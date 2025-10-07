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
#include <atomic>
#include <mutex>

// Thread-local storage for buffers to avoid allocations
thread_local std::vector<std::pair<double, int>> ranking_buffer;
thread_local std::vector<double> ranks_buffer;
thread_local std::vector<double> non_zero_values;
thread_local std::vector<int> non_zero_indices;
thread_local std::vector<double> Ne_values;

// Optimized ranking function with pre-allocated memory
inline void compute_ranks_inplace(const double* values, int n, double* ranks) {
  // Resize buffers if needed (only when necessary)
  if (ranking_buffer.size() < static_cast<size_t>(n)) {
    ranking_buffer.resize(n);
  }
  
  // Create index-value pairs
  for (int i = 0; i < n; ++i) {
    ranking_buffer[i] = {values[i], i};
  }
  
  // Sort only the needed portion
  auto begin = ranking_buffer.begin();
  auto end = begin + n;
  std::sort(begin, end);
  
  // Calculate ranks with tie handling
  for (int i = 0; i < n; ) {
    int j = i;
    double current_val = ranking_buffer[i].first;
    while (j < n && ranking_buffer[j].first == current_val) {
      ++j;
    }
    
    const double rank = i + 1;  // min rank for ties
    for (int k = i; k < j; ++k) {
      ranks[ranking_buffer[k].second] = rank;
    }
    
    i = j;
  }
}

// Process a batch of columns (for parallel processing)
void process_columns_batch(const Eigen::SparseMatrix<double>& mat, 
                           std::vector<Eigen::Triplet<double>>& triplets,
                           int start_col, int end_col) {
  const int nrow = mat.rows();
  
  // Pre-allocate thread-local memory
  const size_t avg_nnz_per_col = mat.nonZeros() / mat.cols();
  non_zero_values.reserve(avg_nnz_per_col);
  non_zero_indices.reserve(avg_nnz_per_col);
  Ne_values.reserve(avg_nnz_per_col);
  ranking_buffer.reserve(avg_nnz_per_col);
  triplets.reserve((end_col - start_col) * avg_nnz_per_col);  // Reserve for this batch
  
  for (int col = start_col; col < end_col; ++col) {
    // Clear buffers for reuse
    non_zero_values.clear();
    non_zero_indices.clear();
    double sum_expr = 0.0;
    
    // Extract non-zero values and indices for this column
    for (Eigen::SparseMatrix<double>::InnerIterator it(mat, col); it; ++it) {
      const double val = it.value();
      if (val != 0.0) {
        non_zero_values.push_back(val);
        non_zero_indices.push_back(it.row());
        sum_expr += val;
      }
    }
    
    const int nnz = non_zero_values.size();
    if (nnz == 0 || sum_expr == 0.0) {
      continue; // skip empty or zero-sum columns
    }
    
    // Resize working buffers if needed
    if (Ne_values.size() < static_cast<size_t>(nnz)) {
      Ne_values.resize(nnz);
      ranks_buffer.resize(nnz);
    }
    
    // Calculate Ne values using multiplication instead of division
    const double inv_sum_expr = 1.0 / sum_expr;
    for (int i = 0; i < nnz; ++i) {
      Ne_values[i] = non_zero_values[i] * inv_sum_expr;
    }
    
    // Compute ranks in-place
    compute_ranks_inplace(non_zero_values.data(), nnz, ranks_buffer.data());
    
    // Calculate mean rank
    double mean_rank = 0.0;
    for (int i = 0; i < nnz; ++i) {
      mean_rank += ranks_buffer[i];
    }
    mean_rank /= nnz;
    
    // Calculate and store results
    const double inv_nnz = 1.0 / nnz;
    for (int i = 0; i < nnz; ++i) {
      const double CR = ranks_buffer[i] - mean_rank;
      const double ewcsr_val = CR * inv_nnz * Ne_values[i];
      
      if (ewcsr_val != 0.0) {
        triplets.emplace_back(non_zero_indices[i], col, ewcsr_val);
      }
    }
  }
}

// [[Rcpp::export]]
Eigen::SparseMatrix<double> ewcsr_sparse_cpp(const Eigen::SparseMatrix<double>& mat) {
  const int nrow = mat.rows();
  const int ncol = mat.cols();
  
  // Determine optimal number of threads
  const int num_threads = std::max(1, static_cast<int>(std::thread::hardware_concurrency()));
  const int cols_per_thread = (ncol + num_threads - 1) / num_threads;
  
  // Per-thread triplet vectors
  std::vector<std::vector<Eigen::Triplet<double>>> all_triplets(num_threads);
  
  // Process columns in parallel
  std::vector<std::thread> threads;
  for (int i = 0; i < num_threads; ++i) {
    const int start_col = i * cols_per_thread;
    const int end_col = std::min((i + 1) * cols_per_thread, ncol);
    
    if (start_col < end_col) {
      threads.emplace_back(process_columns_batch, 
                           std::cref(mat), 
                           std::ref(all_triplets[i]), 
                           start_col, end_col);
    }
  }
  
  // Wait for all threads to complete
  for (auto& thread : threads) {
    thread.join();
  }
  
  // Combine all triplets
  size_t total_nnz = 0;
  for (const auto& vec : all_triplets) {
    total_nnz += vec.size();
  }
  
  std::vector<Eigen::Triplet<double>> combined_triplets;
  combined_triplets.reserve(total_nnz);
  for (const auto& vec : all_triplets) {
    combined_triplets.insert(combined_triplets.end(), vec.begin(), vec.end());
  }
  
  // Build the sparse matrix
  Eigen::SparseMatrix<double> result(nrow, ncol);
  result.setFromTriplets(combined_triplets.begin(), combined_triplets.end());
  
  return result;
}
