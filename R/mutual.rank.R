# Calculating the mutual rank on top of a similarity matrix

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
