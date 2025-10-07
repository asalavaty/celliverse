// src/gini_rows_matrix.cpp

// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>
#include <numeric>
#include <algorithm>
#include <cmath>
#include <vector>
#include <string>

using namespace Rcpp;

// Calculate Gini for all the rows of a matrix
// [[Rcpp::export]]
NumericVector gini_rows_matrix(IntegerVector i, IntegerVector p, NumericVector x, int m, int n) {
  std::vector<std::vector<int> > freq(m, std::vector<int>(m + 1, 0));
  std::vector<int> row_nnz(m, 0);
  
  for (int col = 0; col < n; ++col) {
    int start = p[col];
    int nnz = p[col + 1] - start;
    int num_zeros = m - nnz;
    
    std::vector<std::pair<double, int> > val_row(nnz);
    for (int k = 0; k < nnz; ++k) {
      val_row[k] = std::make_pair(x[start + k], i[start + k]);
    }
    
    std::sort(val_row.begin(), val_row.end());
    
    int current_less = num_zeros;
    int k = 0;
    while (k < nnz) {
      double curr_val = val_row[k].first;
      int tie_start = k;
      while (k < nnz && val_row[k].first == curr_val) ++k;
      int tie_count = k - tie_start;
      
      int rank = 1 + current_less;
      
      for (int t = 0; t < tie_count; ++t) {
        int g = val_row[tie_start + t].second;
        freq[g][rank]++;
        row_nnz[g]++;
      }
      
      current_less += tie_count;
    }
  }
  
  for (int g = 0; g < m; ++g) {
    freq[g][1] += n - row_nnz[g];
  }
  
  NumericVector gini(m);
  for (int g = 0; g < m; ++g) {
    long long sum_s = 0;
    for (int r = 1; r <= m; ++r) {
      sum_s += (long long) r * freq[g][r];
    }
    if (sum_s == 0) {
      gini[g] = NA_REAL;
      continue;
    }
    
    long long cum = 0;
    long long sum_weighted = 0;
    for (int r = 1; r <= m; ++r) {
      long long fr = freq[g][r];
      if (fr > 0) {
        long long add = fr * cum + fr * (fr + 1LL) / 2;
        sum_weighted += (long long) r * add;
        cum += fr;
      }
    }
    
    double gg = 2.0 * sum_weighted / (n * (double) sum_s) - (n + 1.0) / n;
    gini[g] = gg;
  }
  
  return gini;
}

//=======================================================
//=======================================================

#include <Rcpp.h>
using namespace Rcpp;

// gini_rows_matrix for lgCMatrix

// [[Rcpp::export]]
NumericVector gini_rows_lg_matrix(IntegerVector i, IntegerVector p, LogicalVector x, int m, int n) {
  NumericVector gini(m);
  
  // Calculate row sums (number of TRUE values per row)
  std::vector<int> row_sums(m, 0);
  for (int col = 0; col < n; ++col) {
    int start = p[col];
    int end = p[col + 1];
    for (int idx = start; idx < end; ++idx) {
      if (x[idx]) {  // only count TRUE entries
        row_sums[i[idx]]++;
      }
    }
  }
  
  // Calculate Gini for each row (binary data)
  for (int row = 0; row < m; ++row) {
    int sum = row_sums[row];
    if (sum == 0 || sum == n)
      gini[row] = 0.0; // all FALSE or all TRUE
    else
      gini[row] = 1.0 - static_cast<double>(sum) / n;
  }
  
  return gini;
}
