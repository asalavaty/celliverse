// src/mutual_rank.cpp

#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <thread>
#include <mutex>
#include <cmath>
#include <unordered_map>
#include <queue>
#include <limits>
#include <atomic>
#include <exception>
#include <system_error>
using namespace Rcpp;

namespace {

template <typename Func>
void cv_run_ranges(int n_items, int num_threads, Func&& func) {
  if (n_items <= 0) return;

  if (num_threads <= 1) {
    func(0, n_items);
    return;
  }

  const int items_per_thread =
    std::max(1, (n_items + num_threads - 1) / num_threads);

  std::vector<std::thread> threads;
  threads.reserve(static_cast<size_t>(num_threads));
  std::vector<std::exception_ptr> worker_errors(
    static_cast<size_t>(num_threads)
  );
  int fallback_from = num_threads;

  for (int t = 0; t < num_threads; ++t) {
    const int start = t * items_per_thread;
    const int end = std::min((t + 1) * items_per_thread, n_items);
    if (start >= end) continue;

    try {
      threads.emplace_back([&, t, start, end]() {
        try {
          func(start, end);
        } catch (...) {
          worker_errors[static_cast<size_t>(t)] =
            std::current_exception();
        }
      });
    } catch (const std::system_error&) {
      fallback_from = t;
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

  // If a high-core system cannot create every requested worker, process only
  // the ranges that were never launched, serially on the main thread.
  for (int t = fallback_from; t < num_threads; ++t) {
    const int start = t * items_per_thread;
    const int end = std::min((t + 1) * items_per_thread, n_items);
    if (start < end) func(start, end);
  }
}

} // anonymous namespace


// Custom comparator that handles -Inf properly (treats -Inf as the smallest value)
struct CompareWithInf {
  bool operator()(const std::pair<double, int>& a, const std::pair<double, int>& b) const {
    bool a_inf = std::isinf(a.first) && a.first < 0; // Check if -Inf
    bool b_inf = std::isinf(b.first) && b.first < 0;
    
    if (a_inf && b_inf) return false; // Both -Inf, equal
    else if (a_inf) return false;     // a is -Inf, should come after b
    else if (b_inf) return true;      // b is -Inf, a should come before b
    else return a.first > b.first;    // Normal descending order
  }
};

// Ultra-fast mutual rank for dgCMatrix - OPTIMIZED VERSION
// [[Rcpp::export]]
S4 mutual_rank_sparse_cpp(S4 mat, int num_threads = -1) {
  // Extract dgCMatrix components
  IntegerVector i = mat.slot("i");
  IntegerVector p = mat.slot("p");
  NumericVector x = mat.slot("x");
  IntegerVector dims = mat.slot("Dim");
  int n = dims[0];
  
  if (num_threads <= 0) {
    const unsigned int hc = std::thread::hardware_concurrency();
    num_threads = (hc == 0) ? 1 : static_cast<int>(hc);
  }
  num_threads = std::max(
    1,
    std::min(num_threads, n)
  );
  
  // PHASE 1: Build row data directly without expensive data structures
  // Pre-count row sizes
  std::vector<int> row_sizes(n, 0);
  for (int col = 0; col < n; col++) {
    int start_idx = p[col];
    int end_idx = p[col + 1];
    for (int idx = start_idx; idx < end_idx; idx++) {
      int row = i[idx];
      row_sizes[row]++;
    }
  }
  
  // Pre-allocate row data as flat arrays for cache efficiency
  std::vector<double*> row_values(n);
  std::vector<int*> row_cols(n);
  std::vector<int> row_capacities(n);
  
  for (int row = 0; row < n; row++) {
    if (row_sizes[row] > 0) {
      row_values[row] = new double[row_sizes[row]];
      row_cols[row] = new int[row_sizes[row]];
      row_capacities[row] = row_sizes[row];
    }
  }
  
  std::vector<int> row_positions(n, 0);
  
  // Fill row data - single pass, cache-friendly
  for (int col = 0; col < n; col++) {
    int start_idx = p[col];
    int end_idx = p[col + 1];
    for (int idx = start_idx; idx < end_idx; idx++) {
      int row = i[idx];
      double value = x[idx];
      int pos = row_positions[row]++;
      row_values[row][pos] = value;
      row_cols[row][pos] = col;
    }
  }
  
  // PHASE 2: Parallel ranking with optimized sorting
  std::vector<std::vector<std::pair<int, double>>> rank_maps(n);
  
  auto compute_ranks = [&](int start_row, int end_row) {
    // Thread-local buffers to avoid allocations
    std::vector<std::pair<double, int>> sort_buffer;
    std::vector<double> unique_values;
    
    for (int row = start_row; row < end_row; row++) {
      if (row_sizes[row] == 0) continue;
      
      int row_size = row_sizes[row];
      
      // Resize buffers if needed
      if (sort_buffer.size() < static_cast<size_t>(row_size)) {
        sort_buffer.resize(row_size);
        unique_values.resize(row_size);
      }
      
      // Copy to sort buffer
      for (int k = 0; k < row_size; k++) {
        sort_buffer[k] = {row_values[row][k], row_cols[row][k]};
      }
      
      // Sort with custom comparator
      std::sort(sort_buffer.begin(), sort_buffer.begin() + row_size, CompareWithInf());
      
      // Compute ranks with optimized tie handling
      std::vector<std::pair<int, double>> rank_map;
      rank_map.reserve(row_size);
      
      int pos = 0;
      while (pos < row_size) {
        int tie_start = pos;
        double current_val = sort_buffer[pos].first;
        bool current_is_inf = std::isinf(current_val) && current_val < 0;
        
        if (current_is_inf) {
          // All -Inf values get worst rank
          double rank = row_size;
          for (int k = tie_start; k < row_size; k++) {
            rank_map.emplace_back(sort_buffer[k].second, rank);
          }
          break;
        } else {
          // Find tie range
          while (pos < row_size) {
            bool pos_is_inf = std::isinf(sort_buffer[pos].first) && sort_buffer[pos].first < 0;
            if (pos_is_inf || std::abs(sort_buffer[pos].first - current_val) > 1e-10) {
              break;
            }
            pos++;
          }
          
          double avg_rank = (tie_start + pos - 1) / 2.0 + 1.0;
          for (int k = tie_start; k < pos; k++) {
            rank_map.emplace_back(sort_buffer[k].second, avg_rank);
          }
        }
      }
      
      // Sort rank_map by column for faster lookup
      std::sort(rank_map.begin(), rank_map.end(), 
                [](const auto& a, const auto& b) { return a.first < b.first; });
      
      rank_maps[row] = std::move(rank_map);
    }
  };
  
  // Robust parallel range execution; num_threads == 1 is truly serial.
  cv_run_ranges(n, num_threads, compute_ranks);
  
  // PHASE 3: Precompute output structure efficiently
  // Count non-zeros per column in output using atomic operations
  std::vector<std::atomic<int>> atomic_col_sizes(n);
  for (int i = 0; i < n; i++) atomic_col_sizes[i].store(0);
  
  auto count_nnz = [&](int start_row, int end_row) {
    for (int row = start_row; row < end_row; row++) {
      if (rank_maps[row].empty()) continue;
      
      // Diagonal element
      atomic_col_sizes[row].fetch_add(1, std::memory_order_relaxed);
      
      // Upper triangle elements
      for (const auto& entry : rank_maps[row]) {
        int col = entry.first;
        if (row < col) {
          // Check if mutual rank exists
          if (col < n && !rank_maps[col].empty()) {
            // Use binary search for faster lookup
            auto it = std::lower_bound(rank_maps[col].begin(), rank_maps[col].end(), 
                                       std::make_pair(row, 0.0),
                                       [](const auto& a, const auto& b) { return a.first < b.first; });
            
            if (it != rank_maps[col].end() && it->first == row) {
              atomic_col_sizes[row].fetch_add(1, std::memory_order_relaxed);
              atomic_col_sizes[col].fetch_add(1, std::memory_order_relaxed);
            }
          }
        }
      }
    }
  };
  
  cv_run_ranges(n, num_threads, count_nnz);
  
  // Build column pointers
  std::vector<int> p_new(n + 1, 0);
  for (int col = 0; col < n; col++) {
    p_new[col + 1] = p_new[col] + atomic_col_sizes[col].load();
  }
  
  int total_nnz = p_new[n];
  std::vector<int> i_new(total_nnz);
  std::vector<double> x_new(total_nnz);
  std::vector<std::atomic<int>> col_positions(n);
  for (int i = 0; i < n; i++) col_positions[i].store(0);
  
  // PHASE 4: Fill output matrix with mutual ranks
  auto fill_matrix = [&](int start_row, int end_row) {
    for (int row = start_row; row < end_row; row++) {
      if (rank_maps[row].empty()) continue;
      
      // Add diagonal
      int diag_pos = p_new[row] + col_positions[row].fetch_add(1, std::memory_order_relaxed);
      i_new[diag_pos] = row;
      x_new[diag_pos] = INFINITY;
      
      // Add mutual ranks
      for (const auto& entry : rank_maps[row]) {
        int col = entry.first;
        if (row >= col) continue;
        
        if (col < n && !rank_maps[col].empty()) {
          // Binary search for row in cols rank map
          auto it = std::lower_bound(rank_maps[col].begin(), rank_maps[col].end(), 
                                     std::make_pair(row, 0.0),
                                     [](const auto& a, const auto& b) { return a.first < b.first; });
          
          if (it != rank_maps[col].end() && it->first == row) {
            double rank_ij = entry.second;
            double rank_ji = it->second;
            double mutual_rank = std::sqrt(rank_ij * rank_ji);
            
            // Add to rows column
            int pos1 = p_new[row] + col_positions[row].fetch_add(1, std::memory_order_relaxed);
            i_new[pos1] = col;
            x_new[pos1] = mutual_rank;
            
            // Add to cols column
            int pos2 = p_new[col] + col_positions[col].fetch_add(1, std::memory_order_relaxed);
            i_new[pos2] = row;
            x_new[pos2] = mutual_rank;
          }
        }
      }
    }
  };
  
  cv_run_ranges(n, num_threads, fill_matrix);
  
  // PHASE 5: Sort rows within each column (required for dgCMatrix)
  auto sort_columns = [&](int start_col, int end_col) {
    std::vector<int> indices;
    std::vector<int> temp_i;
    std::vector<double> temp_x;
    
    for (int col = start_col; col < end_col; col++) {
      int start_idx = p_new[col];
      int end_idx = p_new[col + 1];
      int col_size = end_idx - start_idx;
      
      if (col_size <= 1) continue;
      
      // Create index array
      if (indices.size() < static_cast<size_t>(col_size)) {
        indices.resize(col_size);
        temp_i.resize(col_size);
        temp_x.resize(col_size);
      }
      
      for (int i = 0; i < col_size; i++) {
        indices[i] = i;
      }
      
      // Sort indices by row values
      std::sort(indices.begin(), indices.begin() + col_size,
                [&](int a, int b) { return i_new[start_idx + a] < i_new[start_idx + b]; });
      
      // Apply permutation
      for (int i = 0; i < col_size; i++) {
        temp_i[i] = i_new[start_idx + indices[i]];
        temp_x[i] = x_new[start_idx + indices[i]];
      }
      
      for (int i = 0; i < col_size; i++) {
        i_new[start_idx + i] = temp_i[i];
        x_new[start_idx + i] = temp_x[i];
      }
    }
  };
  
  cv_run_ranges(n, num_threads, sort_columns);
  
  // Clean up allocated memory
  for (int row = 0; row < n; row++) {
    if (row_values[row] != nullptr) {
      delete[] row_values[row];
      delete[] row_cols[row];
    }
  }
  
  // Create final dgCMatrix
  S4 result("dgCMatrix");
  result.slot("i") = wrap(i_new);
  result.slot("p") = wrap(p_new);
  result.slot("x") = wrap(x_new);
  result.slot("Dim") = IntegerVector::create(n, n);
  
  // Copy dimnames
  List dimnames = mat.slot("Dimnames");
  if (dimnames.size() > 0) {
    result.slot("Dimnames") = dimnames;
  }
  
  return result;
}
