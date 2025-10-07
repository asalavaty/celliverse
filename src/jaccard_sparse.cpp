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
using namespace Rcpp;

// Ultra-fast Jaccard similarity for sparse matrices
// [[Rcpp::export]]
S4 jaccard_sparse_cpp_multi_thread(S4 mat, int num_threads = -1) {
  IntegerVector i = mat.slot("i");
  IntegerVector p = mat.slot("p");
  IntegerVector dims = mat.slot("Dim");
  int ncols = dims[1];
  
  if (num_threads <= 0) {
    num_threads = std::thread::hardware_concurrency();
  }
  num_threads = std::min(num_threads, ncols);
  
  // PHASE 1: Precompute column data using raw pointers for maximum speed
  std::vector<int> col_sizes(ncols, 0);
  std::vector<const int*> col_starts(ncols);
  std::vector<const int*> col_ends(ncols);
  
  // Single-pass to set up column pointers - much faster than building vectors
  for (int col = 0; col < ncols; col++) {
    int start_idx = p[col];
    int end_idx = p[col + 1];
    col_sizes[col] = end_idx - start_idx;
    if (col_sizes[col] > 0) {
      col_starts[col] = &i[start_idx];
      col_ends[col] = &i[end_idx];
    }
  }
  
  // PHASE 2: Precompute non-zero counts for result matrix using atomic operations
  std::vector<std::atomic<int>> col_nnz(ncols);
  for (int j = 0; j < ncols; j++) {
    col_nnz[j].store(1); // Diagonal always present
  }
  
  // Dynamic work scheduling for counting
  std::atomic<int> next_col_count(0);
  auto count_nnz = [&](int thread_id) {
    while (true) {
      int col1 = next_col_count.fetch_add(1);
      if (col1 >= ncols) break;
      
      if (col_sizes[col1] == 0) continue;
      
      const int* col1_start = col_starts[col1];
      const int* col1_end = col_ends[col1];
      int col1_min = *col1_start;
      int col1_max = *(col1_end - 1);
      
      // Process columns in chunks for better cache locality
      for (int col2 = col1 + 1; col2 < ncols; col2++) {
        if (col_sizes[col2] == 0) continue;
        
        // Quick bounds check to skip obviously non-overlapping columns
        const int* col2_start = col_starts[col2];
        const int* col2_end = col_ends[col2];
        int col2_min = *col2_start;
        int col2_max = *(col2_end - 1);
        
        if (col1_max < col2_min || col1_min > col2_max) continue;
        
        // Compute intersection using pointer arithmetic (fastest method)
        const int* ptr1 = col1_start;
        const int* ptr2 = col2_start;
        int intersection = 0;
        
        while (ptr1 < col1_end && ptr2 < col2_end) {
          if (*ptr1 == *ptr2) {
            intersection++;
            ptr1++;
            ptr2++;
          } else if (*ptr1 < *ptr2) {
            ptr1++;
          } else {
            ptr2++;
          }
        }
        
        if (intersection > 0) {
          col_nnz[col1].fetch_add(1);
          col_nnz[col2].fetch_add(1);
        }
      }
    }
  };
  
  // Launch counting threads
  std::vector<std::thread> count_threads;
  for (int t = 0; t < num_threads; t++) {
    count_threads.emplace_back(count_nnz, t);
  }
  for (auto& thread : count_threads) thread.join();
  
  // Build column pointers
  std::vector<int> result_p(ncols + 1, 0);
  for (int j = 0; j < ncols; j++) {
    result_p[j + 1] = result_p[j] + col_nnz[j].load();
  }
  int total_nnz = result_p[ncols];
  
  // Preallocate result arrays
  std::vector<int> result_i(total_nnz);
  std::vector<double> result_x(total_nnz);
  double neg_inf = -std::numeric_limits<double>::infinity();
  
  // PHASE 3: Fill result matrix with Jaccard similarities
  std::vector<std::atomic<int>> col_positions(ncols);
  for (int j = 0; j < ncols; j++) {
    col_positions[j].store(result_p[j]);
  }
  
  // Dynamic work scheduling for computation
  std::atomic<int> next_col_compute(0);
  auto compute_jaccard = [&](int thread_id) {
    // Thread-local buffers to avoid repeated allocations
    std::vector<int> temp_indices;
    std::vector<double> temp_values;
    
    while (true) {
      int col1 = next_col_compute.fetch_add(1);
      if (col1 >= ncols) break;
      
      // Store diagonal
      int pos = col_positions[col1].fetch_add(1);
      result_i[pos] = col1;
      result_x[pos] = neg_inf;
      
      if (col_sizes[col1] == 0) continue;
      
      const int* col1_start = col_starts[col1];
      const int* col1_end = col_ends[col1];
      int col1_size = col_sizes[col1];
      int col1_min = *col1_start;
      int col1_max = *(col1_end - 1);
      
      // Process in chunks for better cache performance
      for (int col2 = col1 + 1; col2 < ncols; col2++) {
        if (col_sizes[col2] == 0) continue;
        
        // Quick bounds check
        const int* col2_start = col_starts[col2];
        const int* col2_end = col_ends[col2];
        int col2_min = *col2_start;
        int col2_max = *(col2_end - 1);
        
        if (col1_max < col2_min || col1_min > col2_max) continue;
        
        // Compute intersection
        const int* ptr1 = col1_start;
        const int* ptr2 = col2_start;
        int intersection = 0;
        
        while (ptr1 < col1_end && ptr2 < col2_end) {
          if (*ptr1 == *ptr2) {
            intersection++;
            ptr1++;
            ptr2++;
          } else if (*ptr1 < *ptr2) {
            ptr1++;
          } else {
            ptr2++;
          }
        }
        
        if (intersection > 0) {
          // FIXED: Use col_sizes[col2] instead of undefined col2_size
          double union_size = col1_size + col_sizes[col2] - intersection;
          double jaccard = static_cast<double>(intersection) / union_size;
          
          // Store symmetric entries
          int pos1 = col_positions[col1].fetch_add(1);
          result_i[pos1] = col2;
          result_x[pos1] = jaccard;
          
          int pos2 = col_positions[col2].fetch_add(1);
          result_i[pos2] = col1;
          result_x[pos2] = jaccard;
        }
      }
    }
  };
  
  // Launch computation threads
  std::vector<std::thread> compute_threads;
  for (int t = 0; t < num_threads; t++) {
    compute_threads.emplace_back(compute_jaccard, t);
  }
  for (auto& thread : compute_threads) thread.join();
  
  // PHASE 4: Parallel column sorting using cache-friendly approach
  auto sort_columns = [&](int start_col, int end_col) {
    // Thread-local buffers to avoid allocations
    std::vector<int> indices_buf;
    std::vector<int> temp_i_buf;
    std::vector<double> temp_x_buf;
    
    for (int col = start_col; col < end_col; col++) {
      int start_idx = result_p[col];
      int end_idx = result_p[col + 1];
      int col_size = end_idx - start_idx;
      
      if (col_size <= 1) continue;
      
      // Ensure buffers are large enough
      if (indices_buf.size() < col_size) {
        indices_buf.resize(col_size);
        temp_i_buf.resize(col_size);
        temp_x_buf.resize(col_size);
      }
      
      // Initialize indices
      for (int i = 0; i < col_size; i++) {
        indices_buf[i] = i;
      }
      
      // Sort indices by row values
      std::sort(indices_buf.begin(), indices_buf.begin() + col_size,
                [&](int a, int b) {
                  return result_i[start_idx + a] < result_i[start_idx + b];
                });
      
      // Apply permutation using buffers
      for (int i = 0; i < col_size; i++) {
        int orig_idx = indices_buf[i];
        temp_i_buf[i] = result_i[start_idx + orig_idx];
        temp_x_buf[i] = result_x[start_idx + orig_idx];
      }
      
      // Copy back
      for (int i = 0; i < col_size; i++) {
        result_i[start_idx + i] = temp_i_buf[i];
        result_x[start_idx + i] = temp_x_buf[i];
      }
    }
  };
  
  // Parallel column sorting
  std::vector<std::thread> sort_threads;
  int cols_per_thread = std::max(1, (ncols + num_threads - 1) / num_threads);
  
  for (int t = 0; t < num_threads; t++) {
    int start_col = t * cols_per_thread;
    int end_col = std::min((t + 1) * cols_per_thread, ncols);
    if (start_col < end_col) {
      sort_threads.emplace_back(sort_columns, start_col, end_col);
    }
  }
  for (auto& thread : sort_threads) thread.join();
  
  // Create final dgCMatrix
  S4 result("dgCMatrix");
  result.slot("i") = wrap(result_i);
  result.slot("p") = wrap(result_p);
  result.slot("x") = wrap(result_x);
  result.slot("Dim") = IntegerVector::create(ncols, ncols);
  
  // Set dimnames
  List dimnames = mat.slot("Dimnames");
  if (dimnames.size() > 1 && !Rf_isNull(dimnames[1])) {
    result.slot("Dimnames") = List::create(dimnames[1], dimnames[1]);
  }
  
  return result;
}

