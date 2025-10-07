// helpers.h
#ifndef HELPERS_H
#define HELPERS_H

#include <Rcpp.h>
#include <numeric>
#include <algorithm>
#include <cmath>
#include <vector>
#include <string>

// compute rank
// Assumes: NA = NA_REAL, ties.method = "min", na.last = "keep"
// Returns vector of ranks, with NA in same positions
std::vector<double> compute_ranks(const std::vector<double>& x) {
  int n = x.size();
  std::vector<std::pair<double, int>> vals;
  vals.reserve(n); // Pre-allocate to avoid reallocations
  
  // Collect non-NA values with indices
  for (int i = 0; i < n; ++i) {
    if (!std::isnan(x[i])) {
      vals.emplace_back(x[i], i);
    }
  }
  
  // Sort by value (ascending)
  std::sort(vals.begin(), vals.end());
  
  // Initialize ranks with NA
  std::vector<double> ranks(n, NA_REAL);
  
  // Assign ranks in a single pass
  if (!vals.empty()) {
    double current_val = vals[0].first;
    size_t start_idx = 0;
    for (size_t i = 0; i <= vals.size(); ++i) {
      if (i == vals.size() || vals[i].first != current_val) {
        // Assign rank to the group [start_idx, i)
        double rank = start_idx + 1; // 1-based ranking
        for (size_t j = start_idx; j < i; ++j) {
          ranks[vals[j].second] = rank;
        }
        if (i < vals.size()) {
          current_val = vals[i].first;
          start_idx = i;
        }
      }
    }
  }
  
  return ranks;
}

//=======================================================
//=======================================================

// Gini index (matching ineq::Gini with NA->0) ---
double gini_impurity(const std::vector<double>& values) {
  int n = values.size();
  if (n == 0) return NA_REAL;
  
  std::vector<double> x = values;
  std::sort(x.begin(), x.end());
  double sumx = std::accumulate(x.begin(), x.end(), 0.0);
  if (sumx == 0.0) return 0.0;
  
  double num = 0.0;
  for (int i = 0; i < n; ++i) {
    num += (2.0 * (i + 1) - n - 1) * x[i];
  }
  return num / (n * sumx);
}

//=======================================================
//=======================================================


#endif // HELPERS_H
