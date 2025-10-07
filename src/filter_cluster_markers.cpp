// src/filter_cluster_markers.cpp

// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
#include <unordered_map>
#include <unordered_set>
#include <algorithm>
#include <string>
#include <vector>
#include <utility>
#include <limits>

using namespace Rcpp;
using std::string;
using std::vector;
using std::unordered_map;
using std::unordered_set;
using std::pair;

// Return true if obj is a data.frame (has class "data.frame")
bool is_data_frame(const SEXP &obj) {
  if (obj == R_NilValue) return false;
  SEXP cls_sexp = Rf_getAttrib(obj, R_ClassSymbol);
  if (Rf_isNull(cls_sexp)) return false;
  CharacterVector cls(cls_sexp);
  for (int i=0;i<cls.size();++i) {
    if (std::string(cls[i]) == "data.frame") return true;
  }
  return false;
}

// Return column names of a DataFrame
CharacterVector df_colnames(const DataFrame &df) {
  return df.names();
}

// Check existence of column
bool df_has_col(const DataFrame &df, const string &col) {
  CharacterVector n = df_colnames(df);
  for (int i=0;i<n.size();++i) if (std::string(n[i]) == col) return true;
  return false;
}

// Subset a DataFrame by a vector<int> of row indices to keep (0-based)
DataFrame df_subset_rows(const DataFrame &df, const std::vector<int> &keep) {
  int ncols = df.size();
  int nkeep = (int)keep.size();
  List out(ncols);
  CharacterVector names = df.names();
  for (int c=0;c<ncols;++c) {
    SEXP col = df[c];
    // detect factor by presence of "levels" attribute
    SEXP lev = Rf_getAttrib(col, Rf_install("levels"));
    bool isFactor = !Rf_isNull(lev);
    if (isFactor) {
      // factor underlying representation is integer
      IntegerVector iv(col);
      IntegerVector nv(nkeep);
      for (int i=0;i<nkeep;++i) nv[i] = iv[ keep[i] ];
      nv.attr("levels") = lev;
      nv.attr("class") = CharacterVector::create("factor");
      out[c] = nv;
    } else if (Rf_isInteger(col)) {
      IntegerVector iv(col);
      IntegerVector nv(nkeep);
      for (int i=0;i<nkeep;++i) nv[i] = iv[ keep[i] ];
      out[c] = nv;
    } else if (Rf_isReal(col)) {
      NumericVector iv(col);
      NumericVector nv(nkeep);
      for (int i=0;i<nkeep;++i) nv[i] = iv[ keep[i] ];
      out[c] = nv;
    } else if (Rf_isLogical(col)) {
      LogicalVector iv(col);
      LogicalVector nv(nkeep);
      for (int i=0;i<nkeep;++i) nv[i] = iv[ keep[i] ];
      out[c] = nv;
    } else {
      CharacterVector iv(col);
      CharacterVector nv(nkeep);
      for (int i=0;i<nkeep;++i) nv[i] = iv[ keep[i] ];
      out[c] = nv;
    }
  }
  out.attr("class") = CharacterVector::create("data.frame");
  out.attr("row.names") = IntegerVector::create(NA_INTEGER, - (int)keep.size());
  out.names() = names;
  return out;
}

// Build dense rank vector (descending) for a numeric vector.
// NA -> NA_INTEGER. Dense ranking: the largest value -> 1, next distinct -> 2, etc.
IntegerVector dense_rank_desc(const NumericVector &x) {
  int n = x.size();
  vector<pair<double, int>> vp;
  vp.reserve(n);
  for (int i = 0; i < n; ++i) {
    if (!NumericVector::is_na(x[i])) {
      vp.push_back({x[i], i});
    }
  }
  if (vp.empty()) {
    return IntegerVector(n, NA_INTEGER);
  }
  // Sort descending by value
  std::sort(vp.begin(), vp.end(), [](const pair<double, int>& a, const pair<double, int>& b) {
    return a.first > b.first;
  });
  IntegerVector ranks(n, NA_INTEGER);
  int current_rank = 1;
  ranks[vp[0].second] = current_rank;
  for (size_t i = 1; i < vp.size(); ++i) {
    if (vp[i].first < vp[i - 1].first) {
      current_rank++;
    }
    ranks[vp[i].second] = current_rank;
  }
  return ranks;
}

// Order indices by integer ranks ascending, with NA last (stable)
std::vector<int> order_by_rank(const IntegerVector &ranks) {
  int n = ranks.size();
  std::vector<int> idx(n);
  for (int i=0;i<n;++i) idx[i] = i;
  std::stable_sort(idx.begin(), idx.end(), [&](int a, int b) {
    int ra = ranks[a], rb = ranks[b];
    bool na = (ra == NA_INTEGER), nb = (rb == NA_INTEGER);
    if (na && nb) return false;
    if (na) return false;
    if (nb) return true;
    return ra < rb;
  });
  return idx;
}

// Subset and add integer Rank column (name "Rank"), return new DataFrame
DataFrame df_add_rank_and_sort(const DataFrame &df, const string &score_col) {
  if (!df_has_col(df, score_col)) return df;
  NumericVector score = df[score_col];
  IntegerVector ranks = dense_rank_desc(score);
  std::vector<int> ord = order_by_rank(ranks);
  int ncol = df.size();
  int nkeep = (int)ord.size();
  
  bool has_rank = df_has_col(df, "Rank");
  int rank_idx = -1;
  CharacterVector names = df.names();
  if (has_rank) {
    for (int c = 0; c < ncol; ++c) {
      if (std::string(names[c]) == "Rank") {
        rank_idx = c;
        break;
      }
    }
  }
  int new_ncol = has_rank ? ncol : ncol + 1;
  List out(new_ncol);
  CharacterVector outnames(new_ncol);
  int out_c = 0;
  for (int c = 0; c < ncol; ++c) {
    if (has_rank && c == rank_idx) continue;
    SEXP col = df[c];
    // detect factor
    SEXP lev = Rf_getAttrib(col, Rf_install("levels"));
    bool isFactor = !Rf_isNull(lev);
    if (isFactor) {
      IntegerVector v(col);
      IntegerVector nv(nkeep);
      for (size_t i = 0; i < ord.size(); ++i) nv[i] = v[ord[i]];
      nv.attr("levels") = lev;
      nv.attr("class") = CharacterVector::create("factor");
      out[out_c] = nv;
    } else if (Rf_isInteger(col)) {
      IntegerVector v(col);
      IntegerVector nv(nkeep);
      for (size_t i = 0; i < ord.size(); ++i) nv[i] = v[ord[i]];
      out[out_c] = nv;
    } else if (Rf_isReal(col)) {
      NumericVector v(col);
      NumericVector nv(nkeep);
      for (size_t i = 0; i < ord.size(); ++i) nv[i] = v[ord[i]];
      out[out_c] = nv;
    } else if (Rf_isLogical(col)) {
      LogicalVector v(col);
      LogicalVector nv(nkeep);
      for (size_t i = 0; i < ord.size(); ++i) nv[i] = v[ord[i]];
      out[out_c] = nv;
    } else {
      CharacterVector v(col);
      CharacterVector nv(nkeep);
      for (size_t i = 0; i < ord.size(); ++i) nv[i] = v[ord[i]];
      out[out_c] = nv;
    }
    outnames[out_c] = names[c];
    out_c++;
  }
  IntegerVector rankcol(nkeep);
  for (size_t i = 0; i < ord.size(); ++i) rankcol[i] = ranks[ord[i]];
  // --- make ranks dense (compact 1,2,3,... without gaps) ---
  {
    std::unordered_map<int, int> densemap;
    int nextdense = 1;
    for (size_t i = 0; i < rankcol.size(); ++i) {
      int r = rankcol[i];
      if (r == NA_INTEGER) continue;
      if (densemap.find(r) == densemap.end()) {
        densemap[r] = nextdense++;
      }
    }
    for (size_t i = 0; i < rankcol.size(); ++i) {
      int r = rankcol[i];
      if (r != NA_INTEGER) rankcol[i] = densemap[r];
    }
  }
  out[out_c] = rankcol;
  outnames[out_c] = "Rank";
  out.names() = outnames;
  out.attr("class") = CharacterVector::create("data.frame");
  out.attr("row.names") = IntegerVector::create(NA_INTEGER, -nkeep);
  return out;
}

//_________________________________________
//_________________________________________


// [[Rcpp::export]]
List filter_cluster_markers_cpp(List pos_markers,
                                List neg_markers,
                                List med_markers) {
  List out_pos = clone(pos_markers);
  List out_neg = clone(neg_markers);
  List out_med = clone(med_markers);
  
  unordered_set<string> cluster_names_set;
  CharacterVector names_pos = out_pos.names();
  CharacterVector names_neg = out_neg.names();
  CharacterVector names_med = out_med.names();
  for (int i=0;i<names_pos.size();++i) cluster_names_set.insert(std::string(names_pos[i]));
  for (int i=0;i<names_neg.size();++i) cluster_names_set.insert(std::string(names_neg[i]));
  for (int i=0;i<names_med.size();++i) cluster_names_set.insert(std::string(names_med[i]));
  
  for (const auto &cur_cl : cluster_names_set) {
    SEXP pos_sexp = R_NilValue, neg_sexp = R_NilValue, med_sexp = R_NilValue;
    if (out_pos.containsElementNamed(cur_cl.c_str())) pos_sexp = out_pos[cur_cl];
    if (out_neg.containsElementNamed(cur_cl.c_str())) neg_sexp = out_neg[cur_cl];
    if (out_med.containsElementNamed(cur_cl.c_str())) med_sexp = out_med[cur_cl];
    
    unordered_map<string, DataFrame> marker_dfs;
    if (pos_sexp != R_NilValue && is_data_frame(pos_sexp)) marker_dfs["curr_pos_markers"] = DataFrame(pos_sexp);
    if (neg_sexp != R_NilValue && is_data_frame(neg_sexp)) marker_dfs["curr_neg_markers"] = DataFrame(neg_sexp);
    if (med_sexp != R_NilValue && is_data_frame(med_sexp)) marker_dfs["curr_med_markers"] = DataFrame(med_sexp);
    
    if (marker_dfs.empty()) continue;
    
    unordered_map<string, DataFrame> with_purity;
    for (auto &kv : marker_dfs) {
      if (df_has_col(kv.second, "Purity")) with_purity[kv.first] = kv.second;
    }
    
    unordered_map<string, int> feature_count;
    unordered_map<string, vector<string>> feature_to_names;
    unordered_map<string, unordered_map<string, double>> purity_maps;
    for (auto &kv : with_purity) {
      DataFrame &df = kv.second;
      CharacterVector feats = df["Feature"];
      NumericVector pur = df["Purity"];
      string nm = kv.first;
      std::unordered_set<string> seen;
      for (int i=0;i<feats.size();++i) {
        if (CharacterVector::is_na(feats[i])) continue;
        string f = std::string(feats[i]);
        if (seen.insert(f).second) {
          feature_count[f] += 1;
          feature_to_names[f].push_back(nm);
          if (!NumericVector::is_na(pur[i])) {
            purity_maps[nm][f] = pur[i];
          }
        }
      }
    }
    
    vector<string> overlapping_features;
    for (auto &kv : feature_count) if (kv.second >= 2) overlapping_features.push_back(kv.first);
    
    unordered_map<string, unordered_set<string>> features_to_remove;
    for (const string &feature : overlapping_features) {
      double best_purity = -std::numeric_limits<double>::infinity();
      string best_name = "";
      bool found_non_na = false;
      for (const string &nm : feature_to_names[feature]) {
        auto it = purity_maps[nm].find(feature);
        if (it != purity_maps[nm].end()) {
          double p = it->second;
          if (!found_non_na || p > best_purity) {
            best_purity = p;
            best_name = nm;
            found_non_na = true;
          }
        }
      }
      if (!found_non_na) continue;
      for (const string &nm : feature_to_names[feature]) {
        if (nm == best_name) continue;
        features_to_remove[nm].insert(feature);
      }
    }
    
    for (auto &kv : with_purity) {
      string nm = kv.first;
      auto it = features_to_remove.find(nm);
      if (it == features_to_remove.end() || it->second.empty()) continue;
      const unordered_set<string> &remove_set = it->second;
      const DataFrame &df = kv.second;
      CharacterVector feats = df["Feature"];
      vector<int> keep;
      for (int r=0;r<feats.size();++r) {
        if (CharacterVector::is_na(feats[r])) {
          keep.push_back(r);
        } else if (remove_set.find(std::string(feats[r])) == remove_set.end()) {
          keep.push_back(r);
        }
      }
      DataFrame newdf = df_subset_rows(df, keep);
      with_purity[nm] = newdf;
    }
    
    for (auto &kv : marker_dfs) {
      string nm = kv.first;
      if (with_purity.find(nm) != with_purity.end()) marker_dfs[nm] = with_purity[nm];
    }
    
    auto apply_back_and_rank = [&](const string &slotname, List &out_list) {
      if (marker_dfs.find(slotname) != marker_dfs.end()) {
        DataFrame df = marker_dfs[slotname];
        if (df_has_col(df, "Purity")) {
          DataFrame ranked = df_add_rank_and_sort(df, "Purity");
          out_list[cur_cl] = ranked;
        } else if (df_has_col(df, "EWCSR")) {
          DataFrame ranked = df_add_rank_and_sort(df, "EWCSR");
          out_list[cur_cl] = ranked;
        } else {
          out_list[cur_cl] = df;
        }
        
        // check if empty dataframe and convert to message if nrow == 0
        SEXP maybe_sexp = out_list[cur_cl];
        if (maybe_sexp != R_NilValue && is_data_frame(maybe_sexp)) {
          DataFrame maybe_df(maybe_sexp);
          SEXP rn = maybe_df.attr("row.names");
          if (Rf_isInteger(rn)) {
            IntegerVector rnv(rn);
            if (rnv.size() == 2 && rnv[1] < 0) {
              int nrows = -rnv[1];
              if (nrows == 0) {
                CharacterVector msg = CharacterVector::create("❗ No specific marker was identified!");
                msg.attr("class") = CharacterVector::create("logMessage");
                out_list[cur_cl] = msg;
              }
            }
          }
        }
      }
    };
    
    apply_back_and_rank("curr_pos_markers", out_pos);
    apply_back_and_rank("curr_neg_markers", out_neg);
    apply_back_and_rank("curr_med_markers", out_med);
  }
  
  return List::create(Named("pos") = out_pos,
                      Named("neg") = out_neg,
                      Named("med") = out_med);
}