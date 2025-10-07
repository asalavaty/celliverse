library(Matrix)


jaccard.sparse <- function(mat,
                           num_threads = -1 # Integer. Number of threads (cores) to use. Default is -1, which uses all available cores.
                           ) {
  
  if (!inherits(mat, "Matrix")) {
    stop("Input must be a sparse matrix from the 'Matrix' package.")
  }

  # Ensure matrix is binary (0/1)
  if (!all(mat@x %in% c(0, 1))) {
    stop("Matrix must be binary (0/1) for Jaccard similarity.")
  }
  
  #============================================================================
  #============================================================================
  
  # R version of jaccard.sparse matrix generation

  # # Column-wise crossproduct gives intersections
  # intersect_mat <- Matrix::crossprod(mat)
  # 
  # # Column sums for union calculation
  # col_sums <- Matrix::colSums(mat)
  # union_mat <- outer(col_sums, col_sums, "+") - intersect_mat
  # 
  # # Jaccard similarity
  # jaccard_sim <- intersect_mat / union_mat
  # 
  # # Assign dimnames
  # dimnames(jaccard_sim) <- list(colnames(mat), colnames(mat))
  # 
  # # Convert NA and NaN values to 0
  # jaccard_sim[is.na(jaccard_sim)] <- 0
  
  #============================================================================
  #============================================================================
  
  # C++ version of jaccard.sparse matrix generation
  
  if (ncol(mat) <= 20000) {
    jaccard_sim <- suppressWarnings(jaccard_sparse_cpp_serial(mat))
  } else {
    jaccard_sim <- suppressWarnings(jaccard_sparse_cpp_multi_thread(mat, num_threads = num_threads))
  }

  return(jaccard_sim)
}
