#' Compute expression-weighted centered scaled ranks (EWCSR)
#'
#' @description
#' Computes expression-weighted centered scaled ranks for a sparse or dense
#' expression matrix, calculated per column (cell).
#'
#' @details
#' EWCSR transformation emphasizes relatively high and low expression features
#' within each cell while accounting for expression magnitude. The output matrix
#' retains the same dimensions as the input.
#'
#' @param mat
#' A matrix with features (genes) as rows and cells or samples as columns.
#' 
#' @param num_threads
#' Integer; number of threads to use. The default is \code{-1} which uses all available cores.
#'
#' @return
#' A matrix of EWCSR-transformed values with the same dimensions as \code{mat}.
#'
#' @seealso
#' \code{\link{gini.ewcsr.fs}}, \code{\link{markoCell}}
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
#' ewcsr_mat <- ewcsr.sparse(mat, num_threads = 1)
#' 
#' @useDynLib celliverse, .registration = TRUE
#' @export

ewcsr.sparse <- function(
    mat,
    num_threads = -1L
) {
  
  #============================================================================
  #============================================================================
  
  # R version of ewcsr.sparse matrix generation

  # # Converting 0 to NA
  # mat[mat == 0] <- NA
  # 
  # # Calculating expression-weighted centered scaled ranks excluding NA values
  # ewcsr_mat <- apply(mat, 2, function(x) {
  #   LS <- base::sum(x, na.rm = TRUE) # Get the library size
  #   Ne <- x/LS # Get the normalized expression
  #   R <- data.table::frankv(x, order = 1, na.last = "keep", ties.method = "min") # Calculate the ranks
  #   mu_R <- base::mean(R, na.rm = TRUE) # Get the mean of Ranks
  #   CR <- R - mu_R # Get the centered Rank
  #   Le <- sum(!is.na(x)) # Get the Length of expressed features
  #   EWCSR <- (CR / Le) * Ne # Get the centered normalized scaled ranks
  #   return(EWCSR)
  # })
  # 
  # rownames(ewcsr_mat) <- rownames(mat)

  #============================================================================
  #============================================================================
  
  # C++ version of ewcsr.sparse matrix generation
  
  ewcsr_mat <- ewcsr_sparse_cpp(
    mat,
    num_threads = num_threads
  )
  
  rownames(ewcsr_mat) <- rownames(mat)
  colnames(ewcsr_mat) <- colnames(mat)
  
  return(ewcsr_mat)
}
