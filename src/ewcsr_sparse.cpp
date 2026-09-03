// src/ewcsr_sparse.cpp

#include <RcppEigen.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppEigen)]]

#include <vector>
#include <algorithm>
#include <cmath>
#include <utility>
#include <Eigen/Sparse>
#include <thread>
#include <exception>
#include <system_error>

// Optimized ranking function with caller-owned buffer. Keeping the buffer local
// to each worker avoids global/thread_local state in the package DLL.
inline void compute_ranks_inplace(
    const double* values,
    int n,
    double* ranks,
    std::vector<std::pair<double, int>>& ranking_buffer) {

  if (ranking_buffer.size() < static_cast<size_t>(n)) {
    ranking_buffer.resize(n);
  }

  for (int i = 0; i < n; ++i) {
    ranking_buffer[i] = {values[i], i};
  }

  auto begin = ranking_buffer.begin();
  auto end = begin + n;
  std::sort(begin, end);

  for (int i = 0; i < n; ) {
    int j = i;
    const double current_val = ranking_buffer[i].first;

    while (j < n && ranking_buffer[j].first == current_val) {
      ++j;
    }

    const double rank = i + 1; // min rank for ties
    for (int k = i; k < j; ++k) {
      ranks[ranking_buffer[k].second] = rank;
    }

    i = j;
  }
}

// Process a batch of columns. This function uses only C++/Eigen objects and
// does not call the R API, so it is safe to execute on worker threads.
void process_columns_batch(
    const Eigen::SparseMatrix<double>& mat,
    std::vector<Eigen::Triplet<double>>& triplets,
    int start_col,
    int end_col) {

  // Worker-local reusable buffers. These replace namespace-level thread_local
  // objects, which improves portability of the DLL across Windows toolchains.
  std::vector<std::pair<double, int>> ranking_buffer;
  std::vector<double> ranks_buffer;
  std::vector<double> non_zero_values;
  std::vector<int> non_zero_indices;
  std::vector<double> Ne_values;

  const Eigen::Index ncols = mat.cols();
  const size_t avg_nnz_per_col =
    (ncols > 0)
      ? static_cast<size_t>(mat.nonZeros() / ncols)
      : 0U;

  non_zero_values.reserve(avg_nnz_per_col);
  non_zero_indices.reserve(avg_nnz_per_col);
  Ne_values.reserve(avg_nnz_per_col);
  ranking_buffer.reserve(avg_nnz_per_col);
  triplets.reserve(
    static_cast<size_t>(std::max(0, end_col - start_col)) *
      avg_nnz_per_col
  );

  for (int col = start_col; col < end_col; ++col) {
    non_zero_values.clear();
    non_zero_indices.clear();
    double sum_expr = 0.0;

    for (Eigen::SparseMatrix<double>::InnerIterator it(mat, col); it; ++it) {
      const double val = it.value();
      if (val != 0.0) {
        non_zero_values.push_back(val);
        non_zero_indices.push_back(it.row());
        sum_expr += val;
      }
    }

    const int nnz = static_cast<int>(non_zero_values.size());
    if (nnz == 0 || sum_expr == 0.0) {
      continue;
    }

    if (Ne_values.size() < static_cast<size_t>(nnz)) {
      Ne_values.resize(nnz);
    }
    if (ranks_buffer.size() < static_cast<size_t>(nnz)) {
      ranks_buffer.resize(nnz);
    }

    const double inv_sum_expr = 1.0 / sum_expr;
    for (int i = 0; i < nnz; ++i) {
      Ne_values[i] = non_zero_values[i] * inv_sum_expr;
    }

    compute_ranks_inplace(
      non_zero_values.data(),
      nnz,
      ranks_buffer.data(),
      ranking_buffer
    );

    double mean_rank = 0.0;
    for (int i = 0; i < nnz; ++i) {
      mean_rank += ranks_buffer[i];
    }
    mean_rank /= nnz;

    const double inv_nnz = 1.0 / nnz;
    for (int i = 0; i < nnz; ++i) {
      const double CR = ranks_buffer[i] - mean_rank;
      const double ewcsr_val = CR * inv_nnz * Ne_values[i];

      if (ewcsr_val != 0.0) {
        triplets.emplace_back(
          non_zero_indices[i],
          col,
          ewcsr_val
        );
      }
    }
  }
}

// [[Rcpp::export]]
Eigen::SparseMatrix<double> ewcsr_sparse_cpp(
    const Eigen::SparseMatrix<double>& mat,
    int num_threads = -1) {

  const int nrow = static_cast<int>(mat.rows());
  const int ncol = static_cast<int>(mat.cols());

  if (ncol == 0) {
    return Eigen::SparseMatrix<double>(nrow, 0);
  }

  // -1 (and any non-positive value) means use all logical threads reported by
  // the operating system. Explicit positive requests are honoured, bounded
  // only by the number of independent columns.
  if (num_threads <= 0) {
    const unsigned int hc = std::thread::hardware_concurrency();
    num_threads = (hc == 0U) ? 1 : static_cast<int>(hc);
  }

  num_threads = std::max(1, std::min(num_threads, ncol));

  // True serial path: num_threads = 1 never creates a background std::thread.
  if (num_threads == 1) {
    std::vector<Eigen::Triplet<double>> triplets;
    process_columns_batch(mat, triplets, 0, ncol);

    Eigen::SparseMatrix<double> result(nrow, ncol);
    result.setFromTriplets(triplets.begin(), triplets.end());
    return result;
  }

  const int cols_per_thread =
    (ncol + num_threads - 1) / num_threads;

  std::vector<std::vector<Eigen::Triplet<double>>>
    all_triplets(static_cast<size_t>(num_threads));

  // Capture worker exceptions instead of allowing an uncaught exception in a
  // worker thread to call std::terminate() and kill the R process.
  std::vector<std::exception_ptr>
    worker_errors(static_cast<size_t>(num_threads));

  std::vector<std::thread> threads;
  threads.reserve(static_cast<size_t>(num_threads));

  int fallback_from = num_threads;

  for (int t = 0; t < num_threads; ++t) {
    const int start_col = t * cols_per_thread;
    const int end_col = std::min(
      (t + 1) * cols_per_thread,
      ncol
    );

    if (start_col >= end_col) {
      continue;
    }

    try {
      threads.emplace_back(
        [&, t, start_col, end_col]() {
          try {
            process_columns_batch(
              mat,
              all_triplets[static_cast<size_t>(t)],
              start_col,
              end_col
            );
          } catch (...) {
            worker_errors[static_cast<size_t>(t)] =
              std::current_exception();
          }
        }
      );
    } catch (const std::system_error&) {
      // On high-core Windows systems the OS can refuse creation of another
      // thread. Do not let destruction of already-joinable std::threads call
      // std::terminate(); join them and finish the remaining batches serially.
      fallback_from = t;
      break;
    } catch (...) {
      for (auto& thread : threads) {
        if (thread.joinable()) {
          thread.join();
        }
      }
      throw;
    }
  }

  for (auto& thread : threads) {
    if (thread.joinable()) {
      thread.join();
    }
  }

  // Re-throw worker failures on the main thread so Rcpp can translate them to
  // an ordinary R error instead of terminating the process.
  for (const auto& err : worker_errors) {
    if (err) {
      std::rethrow_exception(err);
    }
  }

  // If Windows could not create every requested worker, preserve correctness
  // by processing all not-yet-launched batches serially.
  for (int t = fallback_from; t < num_threads; ++t) {
    const int start_col = t * cols_per_thread;
    const int end_col = std::min(
      (t + 1) * cols_per_thread,
      ncol
    );

    if (start_col < end_col) {
      process_columns_batch(
        mat,
        all_triplets[static_cast<size_t>(t)],
        start_col,
        end_col
      );
    }
  }

  size_t total_nnz = 0U;
  for (const auto& x : all_triplets) {
    total_nnz += x.size();
  }

  std::vector<Eigen::Triplet<double>> combined_triplets;
  combined_triplets.reserve(total_nnz);

  for (const auto& x : all_triplets) {
    combined_triplets.insert(
      combined_triplets.end(),
      x.begin(),
      x.end()
    );
  }

  Eigen::SparseMatrix<double> result(nrow, ncol);
  result.setFromTriplets(
    combined_triplets.begin(),
    combined_triplets.end()
  );

  return result;
}
