library(ineq)
library(data.table)

# Feature selection based on Gini impurity evaluation of expression weighted centered scaled rank (ewcsr)-transformed data

gini.ewcsr.fs <- function(mat, # A matrix with cells/samples on columns and features/genes on rows
                      gini_thresh = 0.5, # The Gini threshold for selecting specific features.
                      ewcsr_high_thresh = NULL, # The expression weighted centered scaled rank higher threshold for converting the ewcsr matrix to a binary matrix (scores higher than this value will be TRUE)
                      ewcsr_low_thresh = NULL, # The expression weighted centered scaled rank lower threshold for converting the ewcsr matrix to a binary matrix (scores lower than this value will be TRUE)
                      noise_thresh = NULL, # Integer, threshold for detecting the noise features having information in less than this number of samples/cells
                      num_threads = -1 # Integer. Number of threads (cores) to use. Default is -1, which uses all available cores.
                      ) {
  
  # Filtering out low quality features
  if(is.null(noise_thresh)) {
    if(ncol(mat) > 100) {
      noise_thresh <- 11
    } else {
      noise_thresh <- round(sqrt(ncol(mat)))
    }
  }
  
  # mat <- suppressWarnings(Matrix::Matrix(mat, sparse = TRUE)) # Required for R version
  
  # feature_filter <- Matrix::rowSums(mat != 0, na.rm = TRUE) >= noise_thresh
  feature_filter <- sparse_row_nonzero_count_cpp(sp_mat = mat, noise_thresh = noise_thresh)
  mat <- mat[feature_filter,]
  
  #============================================================================
  #============================================================================
  
  # R version of gini.ewcsr.fs matrix generation
  
  # # Transform the matrix to EWCSR sparse matrix
  # ewcsr_mat <- ewcsr.sparse(mat)
  # 
  # # Binary transformation
  # if(!is.null(ewcsr_high_thresh) & !is.null(ewcsr_low_thresh)) {
  #   ewcsr_mat <- ewcsr_mat > ewcsr_high_thresh & ewcsr_mat < ewcsr_low_thresh
  # } else if(!is.null(ewcsr_high_thresh)) {
  #   ewcsr_mat <- ewcsr_mat > ewcsr_high_thresh
  # } else if(!is.null(ewcsr_low_thresh)) {
  #   ewcsr_mat <- ewcsr_mat < ewcsr_low_thresh
  # }
  # 
  # ewcsr_mat[is.na(ewcsr_mat)] <- FALSE
  # 
  # # Calculate the gini scores based on expression weighted centered scaled rank
  # ewcsr_gini_scores <- apply(ewcsr_mat, 1, function(x) ineq::Gini(as.numeric(x)))
  # 
  # # Define no_occurrence genes based on NaN Gini values (these features have not been detected as TRUE within any of the cells/samples based on the used threshold)
  # ewcsr_no_occurrence <- names(which(is.nan(ewcsr_gini_scores)))
  # 
  # ewcsr_gini_scores <- stats::na.omit(ewcsr_gini_scores)
  # 
  # ## Refining gini_thresh
  # if(length(which(ewcsr_gini_scores >= gini_thresh)) < nrow(mat)*0.3) {
  #   while (length(which(ewcsr_gini_scores >= gini_thresh)) < nrow(mat)*0.3) {
  #     gini_thresh <- gini_thresh - 0.05
  #   }
  # }
  # 
  # # Define specific genes based on a Gini threshold
  # ewcsr_specific_features <- names(which(ewcsr_gini_scores >= gini_thresh))
  # 
  # # Define non-specific genes based on a Gini threshold
  # ewcsr_non_specific_features <- names(which(ewcsr_gini_scores < gini_thresh))
  # 
  # return(list(specific_features = ewcsr_specific_features,
  #             non_specific_features = ewcsr_non_specific_features,
  #             no_occurrence = ewcsr_no_occurrence))
  
  #============================================================================
  #============================================================================
  
  # C++ version of gini.ewcsr.fs matrix generation
  
  # Transform the matrix to EWCSR sparse matrix
  ewcsr_mat <- ewcsr.sparse(mat)

  # Binary transformation (R version)
  # if(!is.null(ewcsr_high_thresh) & !is.null(ewcsr_low_thresh)) {
  #   ewcsr_mat <- ewcsr_mat > ewcsr_high_thresh & ewcsr_mat < ewcsr_low_thresh & ewcsr_mat != 0
  # } else if(!is.null(ewcsr_high_thresh)) {
  #   ewcsr_mat <- ewcsr_mat > ewcsr_high_thresh
  # } else if(!is.null(ewcsr_low_thresh)) {
  #   ewcsr_mat <- ewcsr_mat < ewcsr_low_thresh
  # }
  
  # Binary transformation (C++ version)
  if(!is.null(ewcsr_high_thresh) & !is.null(ewcsr_low_thresh)) {
    ewcsr_mat <- sparse_between_thresholds_same_ipx(mat = ewcsr_mat,
                                           lower_threshold = ewcsr_low_thresh,
                                           upper_threshold = ewcsr_high_thresh,
                                           zero_to_false = TRUE
    )
  } else if(!is.null(ewcsr_high_thresh)) {
    ewcsr_mat <- sparse_compare_threshold_same_ipx(mat = ewcsr_mat, op = ">", threshold = ewcsr_high_thresh, zero_to_false = TRUE)
  } else if(!is.null(ewcsr_low_thresh)) {
    ewcsr_mat <- sparse_compare_threshold_same_ipx(mat = ewcsr_mat, op = "<", threshold = ewcsr_low_thresh, zero_to_false = TRUE)
  }
  
  # Calculate the gini scores based on ewcsr
  ewcsr_gini_scores <- gini_rows_lg_matrix(ewcsr_mat@i, ewcsr_mat@p, ewcsr_mat@x, nrow(ewcsr_mat), ncol(ewcsr_mat))
  names(ewcsr_gini_scores) <- rownames(ewcsr_mat)
  
  # Define no_occurrence genes based on NaN Gini values (these features have not been detected as TRUE within any of the cells/samples based on the used threshold)
  ewcsr_no_occurrence <- names(which(is.nan(ewcsr_gini_scores)))
  
  ewcsr_gini_scores <- stats::na.omit(ewcsr_gini_scores)
  
  ## Refining gini_thresh
  if(length(which(ewcsr_gini_scores >= gini_thresh)) < nrow(mat)*0.3) {
    while (length(which(ewcsr_gini_scores >= gini_thresh)) < nrow(mat)*0.3) {
      gini_thresh <- gini_thresh - 0.05
    }
  }
  
  # Define specific genes based on a Gini threshold
  ewcsr_specific_features <- names(which(ewcsr_gini_scores >= gini_thresh))
  
  # Define non-specific genes based on a Gini threshold
  ewcsr_non_specific_features <- names(which(ewcsr_gini_scores < gini_thresh))
  
  return(list(specific_features = ewcsr_specific_features,
              non_specific_features = ewcsr_non_specific_features,
              no_occurrence = ewcsr_no_occurrence))
  
}
