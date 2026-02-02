#' Feature selection using Gini coefficient on ranked expression data
#'
#' @description
#' Identifies specific and non-specific features based on the Gini inequality
#' coefficient computed on ranked expression values.
#'
#' @details
#' This method removes globally low-ranked features while retaining features
#' with consistently high ranks across cells for downstream analysis.
#'
#' @param mat
#' A matrix with features as rows and cells as columns.
#'
#' @param gini_thresh
#' Numeric; Gini threshold for selecting specific features.
#'
#' @param noise_thresh
#' Integer; minimum number of cells required for a feature to be retained.
#'
#' @param num_threads
#' Integer; number of threads to use. \code{-1} uses all available cores.
#'
#' @return
#' A list containing \code{specific_features}, \code{non_specific_features},
#' and \code{no_occurrence}.
#'
#' @seealso
#' \code{\link{gini.ewcsr.fs}}
#'
#' @examples
#' \dontrun{
#' fs <- gini.rank.fs(mat)
#' }
#' 
#' @useDynLib celliverse, .registration = TRUE
#' @export

gini.rank.fs <- function(mat, # A matrix with cells/samples on columns and features/genes on rows
                         gini_thresh = 0.5, # The Gini threshold for selecting specific features.
                         noise_thresh = NULL, # Threshold for detecting the noise features having information in less than this number of samples/cells. If NULL, will be set to round(sqrt(ncol(mat))). 
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
  
  # feature_filter = Matrix::rowSums(mat != 0, na.rm = TRUE) > noise_thresh
  feature_filter <- sparse_row_nonzero_count_cpp(sp_mat = mat, noise_thresh = noise_thresh)
  mat <- mat[feature_filter,]
  
  #============================================================================
  #============================================================================
  
  # R version of gini.rank.fs matrix generation

  # # For feature selection (filtration) it is important to set the ties.method = "min"
  # rank_mat <- apply(mat, 2, function(x) data.table::frankv(x, order = 1, na.last = "keep", ties.method = "min"))
  # rownames(rank_mat) <- rownames(mat)
  # rank_mat[is.na(rank_mat)] <- 0
  # 
  # # Calculate the gini scores based on ranks
  # rank_gini_scores <- base::apply(rank_mat, 1, function(x) ineq::Gini(as.numeric(x)))
  # names(rank_gini_scores) <- rownames(rank_mat)
  # 
  # # Define no_occurrence genes based on NaN Gini values (these features have not been detected as TRUE within any of the cells/samples based on the used threshold)
  # rank_no_occurrence <- base::names(which(is.nan(rank_gini_scores)))
  # 
  # rank_gini_scores <- stats::na.omit(rank_gini_scores)
  # 
  # # Define specific genes based on a Gini threshold
  # 
  # ## Refining gini_thresh
  # if(length(which(rank_gini_scores >= gini_thresh)) < nrow(mat)*0.75) {
  #   while (length(which(rank_gini_scores >= gini_thresh)) < nrow(mat)*0.75) {
  #     gini_thresh <- gini_thresh - 0.05
  #   }
  # }
  # 
  # rank_specific_features <- names(which(rank_gini_scores >= gini_thresh))
  # 
  # # Define non-specific genes based on a Gini threshold
  # rank_non_specific_features <- names(which(rank_gini_scores < gini_thresh))
  # 
  # return(list(specific_features = rank_specific_features,
  #             non_specific_features = rank_non_specific_features,
  #             no_occurrence = rank_no_occurrence))
  
  #============================================================================
  #============================================================================
  
  # C++ version of gini.rank.fs matrix generation
  
  # Calculate the gini scores based on ranks
  rank_gini_scores <- gini_rows_matrix(mat@i, mat@p, mat@x, nrow(mat), ncol(mat))
  names(rank_gini_scores) <- rownames(mat)
  
  # Define no_occurrence genes based on NaN Gini values (these features have not been detected as TRUE within any of the cells/samples based on the used threshold)
  rank_no_occurrence <- base::names(which(is.nan(rank_gini_scores)))
  
  rank_gini_scores <- stats::na.omit(rank_gini_scores)
  
  # Define specific genes based on a Gini threshold
  
  ## Refining gini_thresh
  if(length(which(rank_gini_scores >= gini_thresh)) < nrow(mat)*0.3) {
    while (length(which(rank_gini_scores >= gini_thresh)) < nrow(mat)*0.3) {
      gini_thresh <- gini_thresh - 0.05
    }
  }
  
  rank_specific_features <- names(which(rank_gini_scores >= gini_thresh))
  
  # Define non-specific genes based on a Gini threshold
  rank_non_specific_features <- names(which(rank_gini_scores < gini_thresh))
  
  return(list(specific_features = rank_specific_features,
              non_specific_features = rank_non_specific_features,
              no_occurrence = rank_no_occurrence))
}
