// src/jaccard_sparse.cpp

#include <Rcpp.h>
#include <RcppEigen.h>
#include <limits>
using namespace Rcpp;
using namespace Eigen;

// [[Rcpp::depends(RcppEigen)]]

// [[Rcpp::export]]
S4 jaccard_sparse_cpp_serial(S4 mat) {
  // Extract lgCMatrix components
  IntegerVector i = mat.slot("i");
  IntegerVector p = mat.slot("p");
  IntegerVector dims = mat.slot("Dim");
  int nrows = dims[0];
  int ncols = dims[1];
  
  // Convert lgCMatrix to Eigen SparseMatrix<double>
  Eigen::SparseMatrix<double> eigen_mat(nrows, ncols);
  eigen_mat.reserve(p[ncols]);
  
  for (int col = 0; col < ncols; col++) {
    for (int idx = p[col]; idx < p[col + 1]; idx++) {
      eigen_mat.insert(i[idx], col) = 1.0;
    }
  }
  eigen_mat.makeCompressed();
  
  // Compute intersection matrix
  Eigen::MatrixXd intersect_mat = (eigen_mat.transpose() * eigen_mat).toDense();
  
  // Column sums
  Eigen::VectorXd col_sums = eigen_mat.transpose() * Eigen::VectorXd::Ones(eigen_mat.rows());
  
  // For dgCMatrix, we need to build column pointers (p) and row indices (i)
  // First, count non-zero elements per column
  std::vector<int> col_counts(ncols, 0);
  double neg_inf = -std::numeric_limits<double>::infinity();
  
  for (int j = 0; j < ncols; ++j) {
    for (int i = 0; i < ncols; ++i) {
      if (i == j) {
        // Diagonal is always stored (even though its -Inf)
        col_counts[j]++;
      } else {
        // Off-diagonal: only store if Jaccard coefficient is non-zero
        double inter = intersect_mat(i, j);
        double uni = col_sums(i) + col_sums(j) - inter;
        double jac = (uni > 0) ? inter / uni : 0.0;
        if (jac != 0.0) {
          col_counts[j]++;
        }
      }
    }
  }
  
  // Build column pointers (p)
  std::vector<int> result_p(ncols + 1);
  result_p[0] = 0;
  for (int j = 0; j < ncols; ++j) {
    result_p[j + 1] = result_p[j] + col_counts[j];
  }
  int total_nnz = result_p[ncols];
  
  // Build row indices (i) and values (x)
  std::vector<int> result_i(total_nnz);
  std::vector<double> result_x(total_nnz);
  std::vector<int> current_pos(ncols, 0);
  
  for (int j = 0; j < ncols; ++j) {
    int pos = result_p[j];
    for (int i = 0; i < ncols; ++i) {
      if (i == j) {
        // Store diagonal
        result_i[pos] = i;
        result_x[pos] = neg_inf;
        pos++;
      } else {
        // Store off-diagonal if non-zero
        double inter = intersect_mat(i, j);
        double uni = col_sums(i) + col_sums(j) - inter;
        double jac = (uni > 0) ? inter / uni : 0.0;
        if (jac != 0.0) {
          result_i[pos] = i;
          result_x[pos] = jac;
          pos++;
        }
      }
    }
  }
  
  // Create dgCMatrix
  S4 result("dgCMatrix");
  result.slot("i") = wrap(result_i);
  result.slot("p") = wrap(result_p);
  result.slot("x") = wrap(result_x);
  result.slot("Dim") = IntegerVector::create(ncols, ncols);
  
  List dimnames = mat.slot("Dimnames");
  if (dimnames.size() > 1 && !Rf_isNull(dimnames[1])) {
    result.slot("Dimnames") = List::create(dimnames[1], dimnames[1]);
  }
  
  return result;
}

//___________________________________
//___________________________________

#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <thread>
#include <mutex>
#include <atomic>
#include <cmath>
#include <limits>
#include <memory>
#include <exception>
#include <system_error>
using namespace Rcpp;

// Ultra-fast Jaccard similarity for sparse matrices
// [[Rcpp::export]]
S4 jaccard_sparse_cpp_multi_thread(S4 mat, int num_threads = -1) {
  // Read R-managed slots on the main R thread.
  IntegerVector i_r = mat.slot("i");
  IntegerVector p_r = mat.slot("p");
  IntegerVector dims = mat.slot("Dim");
  const int ncols = dims[1];

  // Copy the sparse index structure to native C++ memory before spawning any
  // workers. The O(nnz) copy is negligible compared with this function's
  // pairwise-column work and guarantees that workers never touch R objects.
  const std::vector<int> i(i_r.begin(), i_r.end());
  const std::vector<int> p(p_r.begin(), p_r.end());

  if (ncols == 0) {
    S4 result("dgCMatrix");
    result.slot("i") = IntegerVector(0);
    result.slot("p") = IntegerVector::create(0);
    result.slot("x") = NumericVector(0);
    result.slot("Dim") = IntegerVector::create(0, 0);
    return result;
  }

  if (num_threads <= 0) {
    const unsigned int hc = std::thread::hardware_concurrency();
    num_threads = (hc == 0U) ? 1 : static_cast<int>(hc);
  }
  num_threads = std::max(1, std::min(num_threads, ncols));

  // PHASE 1: Precompute column data using pointers into native C++ storage.
  std::vector<int> col_sizes(static_cast<size_t>(ncols), 0);
  std::vector<const int*> col_starts(static_cast<size_t>(ncols), nullptr);
  std::vector<const int*> col_ends(static_cast<size_t>(ncols), nullptr);

  for (int col = 0; col < ncols; ++col) {
    const int start_idx = p[static_cast<size_t>(col)];
    const int end_idx = p[static_cast<size_t>(col + 1)];
    col_sizes[static_cast<size_t>(col)] = end_idx - start_idx;
    if (end_idx > start_idx) {
      col_starts[static_cast<size_t>(col)] = i.data() + start_idx;
      col_ends[static_cast<size_t>(col)] = i.data() + end_idx;
    }
  }

  // PHASE 2: Count non-zeros in the symmetric result.
  std::vector<std::atomic<int>> col_nnz(static_cast<size_t>(ncols));
  for (int j = 0; j < ncols; ++j) {
    col_nnz[static_cast<size_t>(j)].store(1, std::memory_order_relaxed);
  }

  std::atomic<int> next_col_count{0};
  auto count_nnz = [&]() {
    while (true) {
      const int col1 = next_col_count.fetch_add(1, std::memory_order_relaxed);
      if (col1 >= ncols) break;

      if (col_sizes[static_cast<size_t>(col1)] == 0) continue;

      const int* col1_start = col_starts[static_cast<size_t>(col1)];
      const int* col1_end = col_ends[static_cast<size_t>(col1)];
      const int col1_min = *col1_start;
      const int col1_max = *(col1_end - 1);

      for (int col2 = col1 + 1; col2 < ncols; ++col2) {
        if (col_sizes[static_cast<size_t>(col2)] == 0) continue;

        const int* col2_start = col_starts[static_cast<size_t>(col2)];
        const int* col2_end = col_ends[static_cast<size_t>(col2)];
        const int col2_min = *col2_start;
        const int col2_max = *(col2_end - 1);

        if (col1_max < col2_min || col1_min > col2_max) continue;

        const int* ptr1 = col1_start;
        const int* ptr2 = col2_start;
        int intersection = 0;

        while (ptr1 < col1_end && ptr2 < col2_end) {
          if (*ptr1 == *ptr2) {
            ++intersection;
            ++ptr1;
            ++ptr2;
          } else if (*ptr1 < *ptr2) {
            ++ptr1;
          } else {
            ++ptr2;
          }
        }

        if (intersection > 0) {
          col_nnz[static_cast<size_t>(col1)].fetch_add(
            1, std::memory_order_relaxed
          );
          col_nnz[static_cast<size_t>(col2)].fetch_add(
            1, std::memory_order_relaxed
          );
        }
      }
    }
  };

  auto run_dynamic_workers = [&](auto& worker) {
    if (num_threads == 1) {
      worker();
      return;
    }

    std::vector<std::thread> threads;
    threads.reserve(static_cast<size_t>(num_threads));
    std::vector<std::exception_ptr> worker_errors(
      static_cast<size_t>(num_threads)
    );
    bool launch_failed = false;

    for (int t = 0; t < num_threads; ++t) {
      try {
        threads.emplace_back([&, t]() {
          try {
            worker();
          } catch (...) {
            worker_errors[static_cast<size_t>(t)] =
              std::current_exception();
          }
        });
      } catch (const std::system_error&) {
        launch_failed = true;
        break;
      } catch (...) {
        for (auto& thread : threads) {
          if (thread.joinable()) thread.join();
        }
        throw;
      }
    }

    for (auto& thread : threads) {
      if (thread.joinable()) thread.join();
    }

    for (const auto& err : worker_errors) {
      if (err) std::rethrow_exception(err);
    }

    // Because work is dynamically claimed via an atomic counter, the main
    // thread can simply consume whatever remains if worker creation failed.
    if (launch_failed) {
      worker();
    }
  };

  run_dynamic_workers(count_nnz);

  std::vector<int> result_p(static_cast<size_t>(ncols + 1), 0);
  for (int j = 0; j < ncols; ++j) {
    result_p[static_cast<size_t>(j + 1)] =
      result_p[static_cast<size_t>(j)] +
      col_nnz[static_cast<size_t>(j)].load(std::memory_order_relaxed);
  }

  const int total_nnz = result_p[static_cast<size_t>(ncols)];
  std::vector<int> result_i(static_cast<size_t>(total_nnz));
  std::vector<double> result_x(static_cast<size_t>(total_nnz));
  const double neg_inf = -std::numeric_limits<double>::infinity();

  // PHASE 3: Fill the result matrix.
  std::vector<std::atomic<int>> col_positions(static_cast<size_t>(ncols));
  for (int j = 0; j < ncols; ++j) {
    col_positions[static_cast<size_t>(j)].store(
      result_p[static_cast<size_t>(j)],
      std::memory_order_relaxed
    );
  }

  std::atomic<int> next_col_compute{0};
  auto compute_jaccard = [&]() {
    while (true) {
      const int col1 = next_col_compute.fetch_add(1, std::memory_order_relaxed);
      if (col1 >= ncols) break;

      int pos = col_positions[static_cast<size_t>(col1)].fetch_add(
        1, std::memory_order_relaxed
      );
      result_i[static_cast<size_t>(pos)] = col1;
      result_x[static_cast<size_t>(pos)] = neg_inf;

      const int col1_size = col_sizes[static_cast<size_t>(col1)];
      if (col1_size == 0) continue;

      const int* col1_start = col_starts[static_cast<size_t>(col1)];
      const int* col1_end = col_ends[static_cast<size_t>(col1)];
      const int col1_min = *col1_start;
      const int col1_max = *(col1_end - 1);

      for (int col2 = col1 + 1; col2 < ncols; ++col2) {
        const int col2_size = col_sizes[static_cast<size_t>(col2)];
        if (col2_size == 0) continue;

        const int* col2_start = col_starts[static_cast<size_t>(col2)];
        const int* col2_end = col_ends[static_cast<size_t>(col2)];
        const int col2_min = *col2_start;
        const int col2_max = *(col2_end - 1);

        if (col1_max < col2_min || col1_min > col2_max) continue;

        const int* ptr1 = col1_start;
        const int* ptr2 = col2_start;
        int intersection = 0;

        while (ptr1 < col1_end && ptr2 < col2_end) {
          if (*ptr1 == *ptr2) {
            ++intersection;
            ++ptr1;
            ++ptr2;
          } else if (*ptr1 < *ptr2) {
            ++ptr1;
          } else {
            ++ptr2;
          }
        }

        if (intersection > 0) {
          const double union_size = static_cast<double>(
            col1_size + col2_size - intersection
          );
          const double jaccard =
            static_cast<double>(intersection) / union_size;

          const int pos1 =
            col_positions[static_cast<size_t>(col1)].fetch_add(
              1, std::memory_order_relaxed
            );
          result_i[static_cast<size_t>(pos1)] = col2;
          result_x[static_cast<size_t>(pos1)] = jaccard;

          const int pos2 =
            col_positions[static_cast<size_t>(col2)].fetch_add(
              1, std::memory_order_relaxed
            );
          result_i[static_cast<size_t>(pos2)] = col1;
          result_x[static_cast<size_t>(pos2)] = jaccard;
        }
      }
    }
  };

  run_dynamic_workers(compute_jaccard);

  // PHASE 4: Sort rows within each sparse-matrix column.
  auto sort_columns = [&](int start_col, int end_col) {
    std::vector<int> indices_buf;
    std::vector<int> temp_i_buf;
    std::vector<double> temp_x_buf;

    for (int col = start_col; col < end_col; ++col) {
      const int start_idx = result_p[static_cast<size_t>(col)];
      const int end_idx = result_p[static_cast<size_t>(col + 1)];
      const int col_size = end_idx - start_idx;
      if (col_size <= 1) continue;

      const size_t needed = static_cast<size_t>(col_size);
      if (indices_buf.size() < needed) {
        indices_buf.resize(needed);
        temp_i_buf.resize(needed);
        temp_x_buf.resize(needed);
      }

      for (int k = 0; k < col_size; ++k) {
        indices_buf[static_cast<size_t>(k)] = k;
      }

      std::sort(
        indices_buf.begin(),
        indices_buf.begin() + col_size,
        [&](int a, int b) {
          return result_i[static_cast<size_t>(start_idx + a)] <
                 result_i[static_cast<size_t>(start_idx + b)];
        }
      );

      for (int k = 0; k < col_size; ++k) {
        const int orig_idx = indices_buf[static_cast<size_t>(k)];
        temp_i_buf[static_cast<size_t>(k)] =
          result_i[static_cast<size_t>(start_idx + orig_idx)];
        temp_x_buf[static_cast<size_t>(k)] =
          result_x[static_cast<size_t>(start_idx + orig_idx)];
      }

      for (int k = 0; k < col_size; ++k) {
        result_i[static_cast<size_t>(start_idx + k)] =
          temp_i_buf[static_cast<size_t>(k)];
        result_x[static_cast<size_t>(start_idx + k)] =
          temp_x_buf[static_cast<size_t>(k)];
      }
    }
  };

  if (num_threads == 1) {
    sort_columns(0, ncols);
  } else {
    const int cols_per_thread =
      std::max(1, (ncols + num_threads - 1) / num_threads);

    std::vector<std::thread> sort_threads;
    sort_threads.reserve(static_cast<size_t>(num_threads));
    std::vector<std::exception_ptr> sort_errors(
      static_cast<size_t>(num_threads)
    );
    int fallback_from = num_threads;

    for (int t = 0; t < num_threads; ++t) {
      const int start_col = t * cols_per_thread;
      const int end_col = std::min((t + 1) * cols_per_thread, ncols);
      if (start_col >= end_col) continue;

      try {
        sort_threads.emplace_back([&, t, start_col, end_col]() {
          try {
            sort_columns(start_col, end_col);
          } catch (...) {
            sort_errors[static_cast<size_t>(t)] =
              std::current_exception();
          }
        });
      } catch (const std::system_error&) {
        fallback_from = t;
        break;
      } catch (...) {
        for (auto& thread : sort_threads) {
          if (thread.joinable()) thread.join();
        }
        throw;
      }
    }

    for (auto& thread : sort_threads) {
      if (thread.joinable()) thread.join();
    }

    for (const auto& err : sort_errors) {
      if (err) std::rethrow_exception(err);
    }

    for (int t = fallback_from; t < num_threads; ++t) {
      const int start_col = t * cols_per_thread;
      const int end_col = std::min((t + 1) * cols_per_thread, ncols);
      if (start_col < end_col) {
        sort_columns(start_col, end_col);
      }
    }
  }

  // Construct the R object only after all workers have joined.
  S4 result("dgCMatrix");
  result.slot("i") = wrap(result_i);
  result.slot("p") = wrap(result_p);
  result.slot("x") = wrap(result_x);
  result.slot("Dim") = IntegerVector::create(ncols, ncols);

  List dimnames = mat.slot("Dimnames");
  if (dimnames.size() > 1 && !Rf_isNull(dimnames[1])) {
    result.slot("Dimnames") = List::create(dimnames[1], dimnames[1]);
  }

  return result;
}
