#' Compute mutual rank from a similarity matrix
#'
#' @description
#' Computes the mutual rank (MR) for all pairs in a symmetric similarity
#' or dissimilarity matrix, a robust measure frequently used to stabilize
#' pairwise similarity relationships.
#'
#' @param mat
#' Numeric symmetric matrix (dense or sparse) representing pairwise similarities
#' or dissimilarities.
#'
#' @param num_threads
#' Integer; number of threads to use. Default \code{-1} uses all available cores.
#'
#' @return
#' A numeric matrix of mutual ranks with the same dimensions as \code{mat}.
#'
#' @seealso
#' \code{\link{jaccard.sparse}}, \code{\link{markoClust}}
#'
#' @examples
#' utils::data("pbmc_small", package = "SeuratObject")
#'
#' mat <- SeuratObject::LayerData(
#'   pbmc_small,
#'   assay = "RNA",
#'   layer = "counts"
#' )
#'
#' binary_mat <- sign(mat)
#'
#' similarity_matrix <- jaccard.sparse(
#'   binary_mat,
#'   num_threads = 1
#' )
#'
#' mr <- mutual.rank(
#'   similarity_matrix,
#'   num_threads = 1
#' )
#' 
#' @useDynLib celliverse, .registration = TRUE
#' @export

mutual.rank <- function(mat, # A symmetric similarity/dissimilarity matrix
                        num_threads = -1 # Integer. Number of threads (cores) to use. Default is -1, which uses all available cores.
                        ) {
  
  if (!is.matrix(mat) && !inherits(mat, "Matrix")) {
    cli::cli_abort("Input must be a numeric (symmetric) matrix.")
  }
  
  #============================================================================
  #============================================================================
  
  # R version of the function
  
  # r_rank <- base::apply(mat, 1, data.table::frankv, order = -1, ties.method="average") # ties.method = "average" ensures symmetry is preserved
  # rownames(r_rank) <- rownames(mat)
  # mr <- base::sqrt(r_rank * t(r_rank))
  # dimnames(mr) <- dimnames(mat)
  # return(mr)
  
  #============================================================================
  #============================================================================
  
  # C++ version of the function
  
  mr <- mutual_rank_sparse_cpp(mat = mat, num_threads = num_threads)

  return(mr)
}
