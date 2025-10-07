# Calculating the expression-weighted centered scaled ranks of a binary matrix (per column (cell))

library(Matrix)
library(Rcpp)
library(RcppEigen)

ewcsr.sparse <- function(mat # A matrix with cells/samples on columns and features/genes on rows
                           ){
  
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
  
  ewcsr_mat <- ewcsr_sparse_cpp(mat)
  
  rownames(ewcsr_mat) <- rownames(mat)
  colnames(ewcsr_mat) <- colnames(mat)
  
  return(ewcsr_mat)
}
