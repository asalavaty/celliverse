// src/matrix_vector_operations.cpp

#include <RcppEigen.h>
using namespace Rcpp;

// [[Rcpp::export]]
SEXP matrix_to_vector_na_omit(SEXP mat) {
  // Ultra-fast: directly return the x slot without any copying
  // WARNING: This shares memory with the original matrix!
  Rcpp::S4 matrix_s4(mat);
  return matrix_s4.slot("x");
}

//_____________________________________
//_____________________________________

#include <Rcpp.h>
#include <vector>
#include <algorithm>
using namespace Rcpp;

// sparse_compare_threshold for Only including positions that pass the condition (and are not zeros if zero_to_false is true). This does not store entries that fail the condition. 
// Such a function is required for inputting into jaccard_sparse_cpp function. sparse_compare_threshold_same_ipx cannot be input into jaccard_sparse_cpp function

// [[Rcpp::export]]
SEXP sparse_compare_threshold(SEXP mat, std::string op, double threshold, bool zero_to_false = true) {
  S4 mat_obj(mat);
  IntegerVector i = mat_obj.slot("i");
  IntegerVector p = mat_obj.slot("p");
  NumericVector x = mat_obj.slot("x");
  
  IntegerVector dims = mat_obj.slot("Dim");
  int nrow = dims[0];
  int ncol = dims[1];
  
  std::vector<int> new_i;
  std::vector<int> new_p(1, 0);
  
  // Reserve maximum possible size
  new_i.reserve(x.size());
  
  // Define the comparison condition
  auto condition = [&](double val) -> bool {
    if (op == ">") return val > threshold;
    if (op == "<") return val < threshold;
    if (op == ">=") return val >= threshold;
    if (op == "<=") return val <= threshold;
    if (op == "==") return val == threshold;
    if (op == "!=") return val != threshold;
    return false;
  };
  
  // Process each column
  for (int col = 0; col < ncol; col++) {
    int col_start = p[col];
    int col_end = p[col + 1];
    int count_in_col = 0;
    
    for (int idx = col_start; idx < col_end; idx++) {
      double val = x[idx];
      
      bool passes_condition = condition(val);
      bool should_include;
      
      if (zero_to_false && val == 0.0) {
        should_include = false;
      } else {
        should_include = passes_condition;
      }
      
      if (should_include) {
        new_i.push_back(i[idx]);
        count_in_col++;
      }
    }
    
    new_p.push_back(new_p.back() + count_in_col);
  }
  
  // Create logical x vector with all TRUE values
  LogicalVector new_x(new_i.size(), TRUE);
  
  // Create the result matrix
  S4 result("lgCMatrix");
  result.slot("i") = wrap(new_i);
  result.slot("p") = wrap(new_p);
  result.slot("x") = new_x; // Logical vector of TRUE values
  result.slot("Dim") = IntegerVector::create(nrow, ncol);
  result.slot("Dimnames") = mat_obj.slot("Dimnames");
  result.slot("factors") = List::create();
  
  return result;
}

//___________________________________________
//___________________________________________

#include <Rcpp.h>
#include <vector>
#include <algorithm>
using namespace Rcpp;

// sparse_compare_threshold_same_ipx is requied for inputting into the gini_rows_lg_matrix. 
// This Includes all positions from the input matrix in the output, regardless of whether they pass the condition.
// For each input entry, computes a logical value (1 for TRUE, 0 for FALSE) based on the condition, zero handling, and NA checks, and stores it explicitly.
// This results in an output matrix with the same sparsity pattern as the input (same number of stored elements), but with x values set to 0 or 1. This is required for inputting into the gini_rows_lg_matrix.

// [[Rcpp::export]]
SEXP sparse_compare_threshold_same_ipx(SEXP mat, std::string op, double threshold, bool zero_to_false = true) {
  S4 mat_obj(mat);
  IntegerVector i = mat_obj.slot("i");
  IntegerVector p = mat_obj.slot("p");
  NumericVector x = mat_obj.slot("x");
  
  IntegerVector dims = mat_obj.slot("Dim");
  int nrow = dims[0];
  int ncol = dims[1];
  
  std::vector<int> new_i;
  std::vector<int> new_p(1, 0);
  std::vector<int> new_x_vec;
  
  new_i.reserve(x.size());
  new_x_vec.reserve(x.size());
  
  // Define the comparison condition
  auto condition = [&](double val) -> bool {
    if (op == ">") return val > threshold;
    if (op == "<") return val < threshold;
    if (op == ">=") return val >= threshold;
    if (op == "<=") return val <= threshold;
    if (op == "==") return val == threshold;
    if (op == "!=") return val != threshold;
    return false;
  };
  
  // Process each column
  for (int col = 0; col < ncol; col++) {
    int col_start = p[col];
    int col_end = p[col + 1];
    int count_in_col = 0;
    
    for (int idx = col_start; idx < col_end; idx++) {
      double val = x[idx];
      
      bool isna = Rcpp::traits::is_na<REALSXP>(val);
      bool passes_condition = !isna && condition(val);
      
      bool value = passes_condition;
      
      if (zero_to_false && !isna && val == 0.0) {
        value = false;
      }
      
      if (isna) {
        value = false;
      }
      
      new_i.push_back(i[idx]);
      new_x_vec.push_back(value ? 1 : 0);
      count_in_col++;
    }
    
    new_p.push_back(new_p.back() + count_in_col);
  }
  
  // Create logical x vector
  LogicalVector new_x = wrap(new_x_vec);
  
  // Create the result matrix
  S4 result("lgCMatrix");
  result.slot("i") = wrap(new_i);
  result.slot("p") = wrap(new_p);
  result.slot("x") = new_x;
  result.slot("Dim") = IntegerVector::create(nrow, ncol);
  result.slot("Dimnames") = mat_obj.slot("Dimnames");
  result.slot("factors") = List::create();
  
  return result;
}

//_____________________________________
//_____________________________________

#include <Rcpp.h>
#include <vector>
#include <algorithm>
using namespace Rcpp;

// sparse_between_thresholds for Only including positions that pass the condition (and are not zeros if zero_to_false is true). This does not store entries that fail the condition. 
// Such a function is required for inputting into jaccard_sparse_cpp function. sparse_between_thresholds_same_ipx cannot be input into jaccard_sparse_cpp function

// [[Rcpp::export]]
SEXP sparse_between_thresholds(SEXP mat, double lower_threshold, double upper_threshold, bool zero_to_false = true) {
  S4 mat_obj(mat);
  IntegerVector i = mat_obj.slot("i");
  IntegerVector p = mat_obj.slot("p");
  NumericVector x = mat_obj.slot("x");
  
  IntegerVector dims = mat_obj.slot("Dim");
  int nrow = dims[0];
  int ncol = dims[1];
  
  std::vector<int> new_i;
  std::vector<int> new_p(1, 0);
  
  // Reserve maximum possible size
  new_i.reserve(x.size());
  
  // Process each column
  for (int col = 0; col < ncol; col++) {
    int col_start = p[col];
    int col_end = p[col + 1];
    int count_in_col = 0;
    
    // Process non-zero elements in this column
    for (int idx = col_start; idx < col_end; idx++) {
      double val = x[idx];
      
      bool should_include;
      
      // If zero_to_false is true and value is zero, set to FALSE
      if (zero_to_false && val == 0.0) {
        should_include = false;
      } else {
        // Match the R logic: value > lower_threshold AND value < upper_threshold
        should_include = (val > lower_threshold && val < upper_threshold);
      }
      
      if (should_include) {
        new_i.push_back(i[idx]);
        count_in_col++;
      }
    }
    
    new_p.push_back(new_p.back() + count_in_col);
  }
  
  // Create logical x vector with all TRUE values
  LogicalVector new_x(new_i.size(), TRUE);
  
  // Create the result matrix
  S4 result("lgCMatrix");
  result.slot("i") = wrap(new_i);
  result.slot("p") = wrap(new_p);
  result.slot("x") = new_x;
  result.slot("Dim") = IntegerVector::create(nrow, ncol);
  result.slot("Dimnames") = mat_obj.slot("Dimnames");
  result.slot("factors") = List::create();
  
  return result;
}

//___________________________________________
//___________________________________________

// sparse_between_thresholds_same_ipx is requied for inputting into the gini_rows_lg_matrix. 
// This Includes all positions from the input matrix in the output, regardless of whether they pass the condition.
// For each input entry, computes a logical value (1 for TRUE, 0 for FALSE) based on the condition, zero handling, and NA checks, and stores it explicitly.
// This results in an output matrix with the same sparsity pattern as the input (same number of stored elements), but with x values set to 0 or 1. This is required for inputting into the gini_rows_lg_matrix.

// [[Rcpp::export]]
SEXP sparse_between_thresholds_same_ipx(SEXP mat, double lower_threshold, double upper_threshold, bool zero_to_false = true) {
  S4 mat_obj(mat);
  IntegerVector i = mat_obj.slot("i");
  IntegerVector p = mat_obj.slot("p");
  NumericVector x = mat_obj.slot("x");
  
  IntegerVector dims = mat_obj.slot("Dim");
  int nrow = dims[0];
  int ncol = dims[1];
  
  std::vector<int> new_i;
  std::vector<int> new_p(1, 0);
  std::vector<int> new_x_vec;
  
  new_i.reserve(x.size());
  new_x_vec.reserve(x.size());
  
  // Process each column
  for (int col = 0; col < ncol; col++) {
    int col_start = p[col];
    int col_end = p[col + 1];
    int count_in_col = 0;
    
    // Process non-zero elements in this column
    for (int idx = col_start; idx < col_end; idx++) {
      double val = x[idx];
      
      bool isna = Rcpp::traits::is_na<REALSXP>(val);
      bool passes_condition = !isna && (val > upper_threshold && val < lower_threshold);
      
      bool value = passes_condition;
      
      if (zero_to_false && !isna && val == 0.0) {
        value = false;
      }
      
      if (isna) {
        value = false;
      }
      
      new_i.push_back(i[idx]);
      new_x_vec.push_back(value ? 1 : 0);
      count_in_col++;
    }
    
    new_p.push_back(new_p.back() + count_in_col);
  }
  
  // Create logical x vector
  LogicalVector new_x = wrap(new_x_vec);
  
  // Create the result matrix
  S4 result("lgCMatrix");
  result.slot("i") = wrap(new_i);
  result.slot("p") = wrap(new_p);
  result.slot("x") = new_x;
  result.slot("Dim") = IntegerVector::create(nrow, ncol);
  result.slot("Dimnames") = mat_obj.slot("Dimnames");
  result.slot("factors") = List::create();
  
  return result;
}

//___________________________________________
//___________________________________________

#include <Rcpp.h>
#include <vector>

using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector sparse_row_nonzero_count_cpp(
    S4 sp_mat, 
    int noise_thresh) {
  
  IntegerVector i = sp_mat.slot("i");
  IntegerVector p = sp_mat.slot("p");
  IntegerVector dim = sp_mat.slot("Dim");
  
  int n_genes = dim[0];
  int n_cells = dim[1];
  
  // Initialize counts
  std::vector<int> row_counts(n_genes, 0);
  
  // Iterate through all non-zero elements
  // For true dgCMatrix, every stored value is non-zero by definition
  for (int col = 0; col < n_cells; col++) {
    int col_start = p[col];
    int col_end = p[col + 1];
    
    for (int idx = col_start; idx < col_end; idx++) {
      row_counts[i[idx]]++;
    }
  }
  
  // Convert to logical result
  LogicalVector result(n_genes);
  for (int gene = 0; gene < n_genes; gene++) {
    result[gene] = (row_counts[gene] > noise_thresh);
  }
  
  return result;
}

//___________________________________________
//___________________________________________

#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <thread>
#include <atomic>
#include <exception>
#include <system_error>
#include <cstring>
#include <cstdint>
#include <limits>
#include <unordered_map>

namespace {

// Resolve the public thread setting consistently:
//   num_threads <= 0 -> all logical threads reported by the OS
//   num_threads > 0  -> honour the user's request
// The count is bounded only by the amount of independent work available.
inline int cv_resolve_threads(int num_threads, int work_items) {
  if (work_items <= 0) {
    return 1;
  }

  if (num_threads <= 0) {
    const unsigned int hc = std::thread::hardware_concurrency();
    num_threads = (hc == 0U) ? 1 : static_cast<int>(hc);
  }

  return std::max(1, std::min(num_threads, work_items));
}

// Exact R NA detection without calling the R API from a worker thread.
// The NA bit pattern is captured on the main R thread and compared natively.
inline bool cv_is_r_na(double value, std::uint64_t na_bits) {
  std::uint64_t value_bits = 0U;
  std::memcpy(&value_bits, &value, sizeof(double));
  return value_bits == na_bits;
}

} // anonymous namespace

// Ultra-fast parallel version
// [[Rcpp::export]]
Rcpp::S4 replace_na_with_zero_cpp(Rcpp::S4 mat, int num_threads = -1) {
  // All R/Rcpp access happens on the main R thread.
  Rcpp::NumericVector x = mat.slot("x");
  const size_t n = static_cast<size_t>(x.size());

  if (n == 0U) {
    return mat;
  }

  // Obtain the raw data pointer on the main thread. Worker threads only touch
  // disjoint elements through this native pointer and never call the R API.
  double* x_ptr = x.begin();

  // Capture R's exact NA_REAL bit pattern on the main thread so workers can
  // distinguish NA from an arbitrary NaN without calling ISNA/R_IsNA.
  const double na_value = NA_REAL;
  std::uint64_t na_bits = 0U;
  std::memcpy(&na_bits, &na_value, sizeof(double));

  const int max_work_items =
    (n > static_cast<size_t>(std::numeric_limits<int>::max()))
      ? std::numeric_limits<int>::max()
      : static_cast<int>(n);
  num_threads = cv_resolve_threads(num_threads, max_work_items);

  auto process_range = [&](size_t start, size_t end) {
    for (size_t idx = start; idx < end; ++idx) {
      if (cv_is_r_na(x_ptr[idx], na_bits)) {
        x_ptr[idx] = 0.0;
      }
    }
  };

  // True serial path: no std::thread is created when num_threads == 1.
  if (num_threads == 1) {
    process_range(0U, n);
    return mat;
  }

  const size_t chunk_size =
    (n + static_cast<size_t>(num_threads) - 1U) /
    static_cast<size_t>(num_threads);

  std::vector<std::thread> threads;
  threads.reserve(static_cast<size_t>(num_threads));
  std::vector<std::exception_ptr> worker_errors(
    static_cast<size_t>(num_threads)
  );

  int fallback_from = num_threads;

  for (int t = 0; t < num_threads; ++t) {
    const size_t start = static_cast<size_t>(t) * chunk_size;
    const size_t end = std::min(start + chunk_size, n);

    if (start >= end) {
      continue;
    }

    try {
      threads.emplace_back([&, t, start, end]() {
        try {
          process_range(start, end);
        } catch (...) {
          worker_errors[static_cast<size_t>(t)] =
            std::current_exception();
        }
      });
    } catch (const std::system_error&) {
      // Avoid std::terminate() if a high-core system cannot create every
      // requested worker. Join existing workers and finish remaining chunks
      // serially on the main thread.
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

  for (int t = fallback_from; t < num_threads; ++t) {
    const size_t start = static_cast<size_t>(t) * chunk_size;
    const size_t end = std::min(start + chunk_size, n);
    if (start < end) {
      process_range(start, end);
    }
  }

  return mat;
}

//___________________________________________
//___________________________________________

// Fast version with dynamic load balancing
// [[Rcpp::export]]
Rcpp::NumericMatrix compute_centroids_cpp(
    Rcpp::S4 mat,
    Rcpp::CharacterVector sketched_clusters,
    Rcpp::CharacterVector unique_clusters,
    int num_threads = -1) {

  // Extract and resolve all R/Rcpp objects on the main R thread.
  Rcpp::NumericVector x = mat.slot("x");
  Rcpp::IntegerVector i = mat.slot("i");
  Rcpp::IntegerVector p = mat.slot("p");
  Rcpp::IntegerVector dim = mat.slot("Dim");
  const int n_genes = dim[0];

  Rcpp::List dimnames = mat.slot("Dimnames");
  Rcpp::CharacterVector colnames = dimnames[1];

  std::unordered_map<std::string, int> cell_to_index;
  cell_to_index.reserve(static_cast<size_t>(colnames.size()));
  for (R_xlen_t idx = 0; idx < colnames.size(); ++idx) {
    cell_to_index[Rcpp::as<std::string>(colnames[idx])] =
      static_cast<int>(idx);
  }

  std::unordered_map<std::string, std::vector<int>> cluster_to_cells;
  Rcpp::CharacterVector sketched_names = sketched_clusters.names();

  for (R_xlen_t idx = 0; idx < sketched_clusters.size(); ++idx) {
    const std::string cell_name =
      Rcpp::as<std::string>(sketched_names[idx]);
    const std::string cluster_name =
      Rcpp::as<std::string>(sketched_clusters[idx]);

    const auto it = cell_to_index.find(cell_name);
    if (it != cell_to_index.end()) {
      cluster_to_cells[cluster_name].push_back(it->second);
    }
  }

  const int n_clusters = static_cast<int>(unique_clusters.size());
  Rcpp::NumericMatrix centroids(n_genes, n_clusters);
  if (n_clusters == 0 || n_genes == 0) {
    return centroids;
  }

  // Convert character data before spawning workers. Rcpp string conversion
  // must not happen inside secondary threads.
  std::vector<std::string> unique_cluster_names;
  unique_cluster_names.reserve(static_cast<size_t>(n_clusters));
  for (int c_idx = 0; c_idx < n_clusters; ++c_idx) {
    unique_cluster_names.emplace_back(
      Rcpp::as<std::string>(unique_clusters[c_idx])
    );
  }

  // Raw pointers are obtained on the main R thread. Workers only read the
  // sparse input and write disjoint centroid columns; they call no R API.
  const double* x_ptr = x.begin();
  const int* i_ptr = i.begin();
  const int* p_ptr = p.begin();
  double* centroids_ptr = centroids.begin();

  num_threads = cv_resolve_threads(num_threads, n_clusters);
  std::atomic<int> next_cluster{0};

  auto worker = [&]() {
    std::vector<double> local_sums(static_cast<size_t>(n_genes), 0.0);

    while (true) {
      const int c_idx = next_cluster.fetch_add(1, std::memory_order_relaxed);
      if (c_idx >= n_clusters) break;

      const auto cluster_it =
        cluster_to_cells.find(unique_cluster_names[static_cast<size_t>(c_idx)]);
      if (cluster_it == cluster_to_cells.end()) continue;

      const std::vector<int>& cell_indices = cluster_it->second;
      const int n_cells_in_cluster =
        static_cast<int>(cell_indices.size());
      if (n_cells_in_cluster == 0) continue;

      std::fill(local_sums.begin(), local_sums.end(), 0.0);

      for (const int cell_idx : cell_indices) {
        const int col_start = p_ptr[cell_idx];
        const int col_end = p_ptr[cell_idx + 1];

        for (int data_idx = col_start; data_idx < col_end; ++data_idx) {
          const int gene_idx = i_ptr[data_idx];
          local_sums[static_cast<size_t>(gene_idx)] += x_ptr[data_idx];
        }
      }

      const double inv_n_cells = 1.0 / n_cells_in_cluster;
      const size_t out_offset =
        static_cast<size_t>(n_genes) * static_cast<size_t>(c_idx);

      for (int gene_idx = 0; gene_idx < n_genes; ++gene_idx) {
        centroids_ptr[out_offset + static_cast<size_t>(gene_idx)] =
          local_sums[static_cast<size_t>(gene_idx)] * inv_n_cells;
      }
    }
  };

  if (num_threads == 1) {
    worker();
    return centroids;
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

  // Dynamic scheduling makes fallback simple: after existing workers finish,
  // the main thread consumes any remaining clusters.
  if (launch_failed) {
    worker();
  }

  return centroids;
}

//___________________________________________
//___________________________________________

#include <Rcpp.h>
#include <RcppEigen.h>
#include <vector>
#include <algorithm>
#include <thread>
#include <atomic>
#include <cmath>

// [[Rcpp::depends(RcppEigen)]]

using namespace Rcpp;
using Eigen::MatrixXd;
using Eigen::VectorXd;
using Eigen::SparseMatrix;

// More memory-efficient version with dynamic load balancing
// [[Rcpp::export]]
Eigen::MatrixXd sparse_dense_correlation_cpp(
    Rcpp::S4 sp_mat,
    Rcpp::NumericMatrix centroids,
    int num_threads = -1) {

  // Rcpp -> Eigen conversion happens entirely on the main R thread.
  SparseMatrix<double> mat = Rcpp::as<SparseMatrix<double>>(sp_mat);
  MatrixXd cent = Rcpp::as<MatrixXd>(centroids);

  const int n_cells = static_cast<int>(mat.cols());
  const int n_genes = static_cast<int>(mat.rows());
  const int n_clusters = static_cast<int>(cent.cols());

  MatrixXd correlations(n_cells, n_clusters);
  if (n_cells == 0 || n_clusters == 0 || n_genes == 0) {
    return correlations;
  }

  // Precompute centroid statistics on the main thread.
  MatrixXd cent_means = cent.colwise().mean();
  MatrixXd cent_std = MatrixXd::Zero(1, n_clusters);

  if (n_genes > 1) {
    for (int k = 0; k < n_clusters; ++k) {
      VectorXd centered = cent.col(k).array() - cent_means(0, k);
      cent_std(0, k) =
        std::sqrt(centered.squaredNorm() / static_cast<double>(n_genes - 1));
    }
  }

  num_threads = cv_resolve_threads(num_threads, n_cells);
  std::atomic<int> next_cell{0};

  auto worker = [&]() {
    std::vector<double> cell_vals(static_cast<size_t>(n_genes), 0.0);

    while (true) {
      const int cell = next_cell.fetch_add(1, std::memory_order_relaxed);
      if (cell >= n_cells) break;

      std::fill(cell_vals.begin(), cell_vals.end(), 0.0);

      double cell_sum = 0.0;
      for (SparseMatrix<double>::InnerIterator it(mat, cell); it; ++it) {
        const int gene_idx = static_cast<int>(it.row());
        const double val = it.value();
        cell_vals[static_cast<size_t>(gene_idx)] = val;
        cell_sum += val;
      }

      const double cell_mean = cell_sum / static_cast<double>(n_genes);

      double cell_var = 0.0;
      for (int g = 0; g < n_genes; ++g) {
        const double diff = cell_vals[static_cast<size_t>(g)] - cell_mean;
        cell_var += diff * diff;
      }

      const double cell_std =
        (n_genes > 1)
          ? std::sqrt(cell_var / static_cast<double>(n_genes - 1))
          : 0.0;

      for (int k = 0; k < n_clusters; ++k) {
        double covariance = 0.0;
        const double cent_mean = cent_means(0, k);

        for (int g = 0; g < n_genes; ++g) {
          covariance +=
            (cell_vals[static_cast<size_t>(g)] - cell_mean) *
            (cent(g, k) - cent_mean);
        }

        const double denom = cell_std * cent_std(0, k);
        correlations(cell, k) =
          (n_genes > 1 && denom > 1e-10)
            ? covariance /
                (static_cast<double>(n_genes - 1) * denom)
            : 0.0;
      }
    }
  };

  if (num_threads == 1) {
    worker();
    return correlations;
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

  if (launch_failed) {
    worker();
  }

  return correlations;
}

//___________________________________________
//___________________________________________

#include <Rcpp.h>
#include <vector>
#include <unordered_map>
#include <algorithm>
#include <thread>
#include <atomic>

// [[Rcpp::plugins(cpp11)]]

// Ultra-fast parallel centroid computation for dense matrix
// [[Rcpp::export]]
Rcpp::NumericMatrix compute_reduced_centroids_cpp(
    Rcpp::NumericMatrix mat,
    Rcpp::CharacterVector sketched_clusters,
    Rcpp::CharacterVector unique_clusters,
    int num_threads = -1) {

  const int n_cells = mat.rows();
  const int n_dims = mat.cols();
  const int n_clusters = static_cast<int>(unique_clusters.size());

  // Resolve all R/Rcpp metadata on the main R thread.
  Rcpp::List dimnames = mat.attr("dimnames");
  Rcpp::CharacterVector rownames = dimnames[0];

  std::unordered_map<std::string, int> cell_to_index;
  cell_to_index.reserve(static_cast<size_t>(rownames.size()));
  for (R_xlen_t idx = 0; idx < rownames.size(); ++idx) {
    cell_to_index[Rcpp::as<std::string>(rownames[idx])] =
      static_cast<int>(idx);
  }

  std::unordered_map<std::string, std::vector<int>> cluster_to_cells;
  Rcpp::CharacterVector sketched_names = sketched_clusters.names();

  for (R_xlen_t idx = 0; idx < sketched_clusters.size(); ++idx) {
    const std::string cell_name =
      Rcpp::as<std::string>(sketched_names[idx]);
    const std::string cluster_name =
      Rcpp::as<std::string>(sketched_clusters[idx]);

    const auto it = cell_to_index.find(cell_name);
    if (it != cell_to_index.end()) {
      cluster_to_cells[cluster_name].push_back(it->second);
    }
  }

  std::vector<std::string> unique_cluster_names;
  unique_cluster_names.reserve(static_cast<size_t>(n_clusters));
  for (int c_idx = 0; c_idx < n_clusters; ++c_idx) {
    unique_cluster_names.emplace_back(
      Rcpp::as<std::string>(unique_clusters[c_idx])
    );
  }

  Rcpp::NumericMatrix centroids(n_clusters, n_dims);

  if (n_clusters == 0 || n_dims == 0 || n_cells == 0) {
    Rcpp::List result_dimnames =
      Rcpp::List::create(unique_clusters, dimnames[1]);
    centroids.attr("dimnames") = result_dimnames;
    return centroids;
  }

  // Obtain raw pointers on the main thread. Workers use only native pointers,
  // std::string/std::vector containers, and arithmetic.
  const double* mat_ptr = mat.begin();
  double* centroids_ptr = centroids.begin();

  num_threads = cv_resolve_threads(num_threads, n_clusters);

  auto process_cluster_range = [&](int start_cluster, int end_cluster) {
    std::vector<double> dim_sums(static_cast<size_t>(n_dims), 0.0);

    for (int c_idx = start_cluster; c_idx < end_cluster; ++c_idx) {
      const auto cluster_it =
        cluster_to_cells.find(unique_cluster_names[static_cast<size_t>(c_idx)]);
      if (cluster_it == cluster_to_cells.end()) continue;

      const std::vector<int>& cell_indices = cluster_it->second;
      const int n_cells_in_cluster =
        static_cast<int>(cell_indices.size());
      if (n_cells_in_cluster == 0) continue;

      std::fill(dim_sums.begin(), dim_sums.end(), 0.0);

      for (const int cell_idx : cell_indices) {
        for (int dim_idx = 0; dim_idx < n_dims; ++dim_idx) {
          // R matrices are column-major.
          const size_t in_idx =
            static_cast<size_t>(cell_idx) +
            static_cast<size_t>(n_cells) * static_cast<size_t>(dim_idx);
          dim_sums[static_cast<size_t>(dim_idx)] += mat_ptr[in_idx];
        }
      }

      const double inv_n_cells = 1.0 / n_cells_in_cluster;
      for (int dim_idx = 0; dim_idx < n_dims; ++dim_idx) {
        const size_t out_idx =
          static_cast<size_t>(c_idx) +
          static_cast<size_t>(n_clusters) * static_cast<size_t>(dim_idx);
        centroids_ptr[out_idx] =
          dim_sums[static_cast<size_t>(dim_idx)] * inv_n_cells;
      }
    }
  };

  if (num_threads == 1) {
    process_cluster_range(0, n_clusters);
  } else {
    const int clusters_per_thread =
      (n_clusters + num_threads - 1) / num_threads;

    std::vector<std::thread> threads;
    threads.reserve(static_cast<size_t>(num_threads));
    std::vector<std::exception_ptr> worker_errors(
      static_cast<size_t>(num_threads)
    );

    int fallback_from = num_threads;

    for (int t = 0; t < num_threads; ++t) {
      const int start_cluster = t * clusters_per_thread;
      const int end_cluster = std::min(
        (t + 1) * clusters_per_thread,
        n_clusters
      );
      if (start_cluster >= end_cluster) continue;

      try {
        threads.emplace_back([&, t, start_cluster, end_cluster]() {
          try {
            process_cluster_range(start_cluster, end_cluster);
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

    for (int t = fallback_from; t < num_threads; ++t) {
      const int start_cluster = t * clusters_per_thread;
      const int end_cluster = std::min(
        (t + 1) * clusters_per_thread,
        n_clusters
      );
      if (start_cluster < end_cluster) {
        process_cluster_range(start_cluster, end_cluster);
      }
    }
  }

  // Rcpp attribute assignment happens only after all workers have joined.
  Rcpp::List result_dimnames =
    Rcpp::List::create(unique_clusters, dimnames[1]);
  centroids.attr("dimnames") = result_dimnames;

  return centroids;
}
