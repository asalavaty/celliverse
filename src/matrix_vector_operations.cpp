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

// Ultra-fast parallel version
// [[Rcpp::export]]
Rcpp::S4 replace_na_with_zero_cpp(Rcpp::S4 mat, int num_threads = -1) {
  // Extract slots from dgCMatrix
  Rcpp::NumericVector x = mat.slot("x");
  Rcpp::IntegerVector i = mat.slot("i");
  Rcpp::IntegerVector p = mat.slot("p");
  Rcpp::IntegerVector dim = mat.slot("Dim");
  
  // Determine optimal number of threads
  if (num_threads <= 0) {
    num_threads = std::thread::hardware_concurrency();
    if (num_threads == 0) num_threads = 4; // fallback
  }
  
  const size_t n = x.size();
  if (n == 0) return mat; // quick return for empty matrix
  
  // Process in parallel chunks
  const size_t chunk_size = (n + num_threads - 1) / num_threads;
  std::vector<std::thread> threads;
  
  for (int t = 0; t < num_threads; ++t) {
    const size_t start = t * chunk_size;
    const size_t end = std::min((t + 1) * chunk_size, n);
    
    if (start < end) {
      threads.emplace_back([&x, start, end]() {
        // Use local variables for better cache performance
        double* x_ptr = &x[0]; // direct pointer access
        for (size_t idx = start; idx < end; ++idx) {
          // Direct memory comparison for NA (faster than Rcpp::NumericVector::is_na)
          if (ISNA(x_ptr[idx])) {
            x_ptr[idx] = 0.0;
          }
        }
      });
    }
  }
  
  // Wait for all threads to complete
  for (auto& thread : threads) {
    thread.join();
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
  
  // Extract dgCMatrix components
  Rcpp::NumericVector x = mat.slot("x");
  Rcpp::IntegerVector i = mat.slot("i");
  Rcpp::IntegerVector p = mat.slot("p");
  Rcpp::IntegerVector dim = mat.slot("Dim");
  int n_genes = dim[0];
  int n_cells = dim[1];
  
  // Get column names
  Rcpp::List dimnames = mat.slot("Dimnames");
  Rcpp::CharacterVector colnames = dimnames[1];
  
  // Create mapping structures
  std::unordered_map<std::string, int> cell_to_index;
  for (int idx = 0; idx < colnames.size(); ++idx) {
    cell_to_index[Rcpp::as<std::string>(colnames[idx])] = idx;
  }
  
  std::unordered_map<std::string, std::vector<int>> cluster_to_cells;
  Rcpp::CharacterVector sketched_names = sketched_clusters.names();
  
  for (int idx = 0; idx < sketched_clusters.size(); ++idx) {
    std::string cell_name = Rcpp::as<std::string>(sketched_names[idx]);
    std::string cluster_name = Rcpp::as<std::string>(sketched_clusters[idx]);
    
    auto it = cell_to_index.find(cell_name);
    if (it != cell_to_index.end()) {
      cluster_to_cells[cluster_name].push_back(it->second);
    }
  }
  
  // Initialize result
  int n_clusters = unique_clusters.size();
  Rcpp::NumericMatrix centroids(n_genes, n_clusters);
  
  // Auto-detect threads
  if (num_threads <= 0) {
    num_threads = std::thread::hardware_concurrency();
  }
  num_threads = std::min(num_threads, n_clusters);
  
  // Dynamic load balancing
  std::atomic<int> next_cluster{0};
  std::vector<std::thread> threads;
  
  for (int t = 0; t < num_threads; ++t) {
    threads.emplace_back([&]() {
      // Thread-local arrays to avoid contention
      std::vector<double> local_sums(n_genes);
      std::vector<int> local_counts(n_genes);
      
      while (true) {
        int c_idx = next_cluster.fetch_add(1);
        if (c_idx >= n_clusters) break;
        
        std::string cluster_name = Rcpp::as<std::string>(unique_clusters[c_idx]);
        auto cluster_it = cluster_to_cells.find(cluster_name);
        
        if (cluster_it == cluster_to_cells.end()) {
          continue;
        }
        
        const std::vector<int>& cell_indices = cluster_it->second;
        int n_cells_in_cluster = cell_indices.size();
        
        if (n_cells_in_cluster == 0) {
          continue;
        }
        
        // Reset local arrays
        std::fill(local_sums.begin(), local_sums.end(), 0.0);
        std::fill(local_counts.begin(), local_counts.end(), 0);
        
        // Process cells in this cluster
        for (int cell_idx : cell_indices) {
          int col_start = p[cell_idx];
          int col_end = p[cell_idx + 1];
          
          for (int data_idx = col_start; data_idx < col_end; ++data_idx) {
            int gene_idx = i[data_idx];
            double value = x[data_idx];
            local_sums[gene_idx] += value;
            local_counts[gene_idx]++;
          }
        }
        
        // Compute means
        double inv_n_cells = 1.0 / n_cells_in_cluster;
        for (int gene_idx = 0; gene_idx < n_genes; ++gene_idx) {
          centroids(gene_idx, c_idx) = local_sums[gene_idx] * inv_n_cells;
        }
      }
    });
  }
  
  for (auto& thread : threads) {
    thread.join();
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
  
  SparseMatrix<double> mat = Rcpp::as<SparseMatrix<double>>(sp_mat);
  MatrixXd cent = Rcpp::as<MatrixXd>(centroids);
  
  int n_cells = mat.cols();
  int n_genes = mat.rows();
  int n_clusters = cent.cols();
  
  // Precompute centroid statistics
  MatrixXd cent_means = cent.colwise().mean();
  MatrixXd cent_std = MatrixXd::Zero(1, n_clusters);
  
  for (int k = 0; k < n_clusters; ++k) {
    VectorXd centered = cent.col(k).array() - cent_means(0, k);
    cent_std(0, k) = std::sqrt(centered.squaredNorm() / (n_genes - 1));
  }
  
  // Initialize result
  MatrixXd correlations(n_cells, n_clusters);
  
  // Auto-detect threads
  if (num_threads <= 0) {
    num_threads = std::thread::hardware_concurrency();
  }
  num_threads = std::min(num_threads, n_cells);
  
  // Dynamic load balancing
  std::atomic<int> next_cell{0};
  std::vector<std::thread> threads;
  
  for (int t = 0; t < num_threads; ++t) {
    threads.emplace_back([&]() {
      // Thread-local storage
      std::vector<double> cell_vals(n_genes, 0.0);
      
      while (true) {
        int cell = next_cell.fetch_add(1);
        if (cell >= n_cells) break;
        
        // Extract sparse column values
        int nnz = 0;
        double cell_sum = 0.0;
        
        // Reset cell_vals for this cell
        std::fill(cell_vals.begin(), cell_vals.end(), 0.0);
        
        for (SparseMatrix<double>::InnerIterator it(mat, cell); it; ++it) {
          int gene_idx = it.row();
          double val = it.value();
          cell_vals[gene_idx] = val;
          cell_sum += val;
          nnz++;
        }
        
        double cell_mean = cell_sum / n_genes;
        
        // Compute cell standard deviation
        double cell_var = 0.0;
        for (int g = 0; g < n_genes; ++g) {
          double diff = cell_vals[g] - cell_mean;
          cell_var += diff * diff;
        }
        double cell_std = std::sqrt(cell_var / (n_genes - 1));
        
        // Compute correlations with centroids
        for (int k = 0; k < n_clusters; ++k) {
          double covariance = 0.0;
          double cent_mean = cent_means(0, k);
          
          for (int g = 0; g < n_genes; ++g) {
            covariance += (cell_vals[g] - cell_mean) * (cent(g, k) - cent_mean);
          }
          
          double denom = cell_std * cent_std(0, k);
          if (denom > 1e-10) {
            correlations(cell, k) = covariance / ((n_genes - 1) * denom);
          } else {
            correlations(cell, k) = 0.0;
          }
        }
      }
    });
  }
  
  for (auto& thread : threads) {
    thread.join();
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
    Rcpp::CharacterVector unique_clusters) {
  
  int n_cells = mat.rows();
  int n_dims = mat.cols();
  int n_clusters = unique_clusters.size();
  
  // Get row names from reduced matrix
  Rcpp::List dimnames = mat.attr("dimnames");
  Rcpp::CharacterVector rownames = dimnames[0];
  
  // Create mapping from cell name to row index
  std::unordered_map<std::string, int> cell_to_index;
  for (int idx = 0; idx < rownames.size(); ++idx) {
    cell_to_index[Rcpp::as<std::string>(rownames[idx])] = idx;
  }
  
  // Create mapping from cluster to row indices
  std::unordered_map<std::string, std::vector<int>> cluster_to_cells;
  Rcpp::CharacterVector sketched_names = sketched_clusters.names();
  
  for (int idx = 0; idx < sketched_clusters.size(); ++idx) {
    std::string cell_name = Rcpp::as<std::string>(sketched_names[idx]);
    std::string cluster_name = Rcpp::as<std::string>(sketched_clusters[idx]);
    
    auto it = cell_to_index.find(cell_name);
    if (it != cell_to_index.end()) {
      cluster_to_cells[cluster_name].push_back(it->second);
    }
  }
  
  // Initialize result matrix (clusters x dimensions)
  Rcpp::NumericMatrix centroids(n_clusters, n_dims);
  
  // Determine optimal number of threads
  int num_threads = std::max(1, static_cast<int>(std::thread::hardware_concurrency()));
  if (n_clusters < num_threads) {
    num_threads = n_clusters;
  }
  
  // Process clusters in parallel
  std::vector<std::thread> threads;
  int clusters_per_thread = (n_clusters + num_threads - 1) / num_threads;
  
  for (int t = 0; t < num_threads; ++t) {
    int start_cluster = t * clusters_per_thread;
    int end_cluster = std::min((t + 1) * clusters_per_thread, n_clusters);
    
    if (start_cluster < end_cluster) {
      threads.emplace_back([&, start_cluster, end_cluster]() {
        // Thread-local storage for sums
        std::vector<double> dim_sums(n_dims, 0.0);
        
        for (int c_idx = start_cluster; c_idx < end_cluster; ++c_idx) {
          std::string cluster_name = Rcpp::as<std::string>(unique_clusters[c_idx]);
          auto cluster_it = cluster_to_cells.find(cluster_name);
          
          if (cluster_it == cluster_to_cells.end()) {
            continue; // Skip empty clusters
          }
          
          const std::vector<int>& cell_indices = cluster_it->second;
          int n_cells_in_cluster = cell_indices.size();
          
          if (n_cells_in_cluster == 0) {
            continue;
          }
          
          // Reset thread-local array
          std::fill(dim_sums.begin(), dim_sums.end(), 0.0);
          
          // Sum across all dimensions for cells in this cluster
          for (int cell_idx : cell_indices) {
            for (int dim = 0; dim < n_dims; ++dim) {
              dim_sums[dim] += mat(cell_idx, dim);
            }
          }
          
          // Compute means and store in result matrix
          double inv_n_cells = 1.0 / n_cells_in_cluster;
          for (int dim = 0; dim < n_dims; ++dim) {
            centroids(c_idx, dim) = dim_sums[dim] * inv_n_cells;
          }
        }
      });
    }
  }
  
  // Wait for all threads to complete
  for (auto& thread : threads) {
    thread.join();
  }
  
  // Set row and column names
  Rcpp::List result_dimnames = Rcpp::List::create(unique_clusters, dimnames[1]);
  centroids.attr("dimnames") = result_dimnames;
  
  return centroids;
}

