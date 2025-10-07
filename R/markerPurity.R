library(Matrix)
library(igraph)
library(cli)
library(data.table)
library(dplyr)
library(magrittr)

markerPurity <- function(
    so = NULL, # A Seurat object. Either `so` or `data` argument should be specified, but not both.
    assay = "RNA", # The assay we want to use for assessing marker purities.
    layer = "counts", # The layer of the assay we want to use for assessing marker purities (this can be a normalized layer).
    data = NULL, # matrix, the data to be used for assessing marker purity.
    desired_markers = NULL, # A character vector of the names of desired markers. If not specified, the purity of all features of the input data in each desired cluster and the desired cell subsets will be assessed. 
    cluster_labels = NULL, # Optional. Mandatory if desired_clusters is specified. The column name of cluster labels in the meta.data of `so`, or a character vector of cluster labels with the same as the number of columns/cells of the `data` argument.
    desired_clusters = NULL, # Optional. Mandatory if desired_cells is not specified. If not specified, the purity of desired markers will be assessed only in the desired_cells. 
    desired_cells = NULL, # Optional. Mandatory if desired_clusters is not specified. A named list of character vectors of the names of desired cells from the column names of the input data.
    log1p = TRUE, # Weather to log1p transform the data or not
    remove_quiescent_cells = TRUE, # Whether to remove quiescent_cells before marker identification or not.
    high_quantile = 0.25, # The quantile threshold for choosing highly positive expression-weighted centered scaled ranks required for filtering the data and for selecting the positive markers. Higher values label more features as features with high expression-weighted centered scaled ranks.
    low_quantile = 0.25, # The quantile threshold for choosing highly negative expression-weighted centered scaled ranks required for filtering the data and for selecting the negative markers. Lower values label more features as features with low expression-weighted centered scaled ranks.
    noise_feature_thresh = 4, # The threshold for detecting noise features/genes (i.e. features that have non-zero expression in more than this number of samples/cells).
    num_threads = -1, # Integer. Number of threads (cores) to use. Default is -1, which uses all available cores.
    seed = 121, # The seed for randomization and making consistent results
    verbose = TRUE # Logical, whether to show progress messages
) {
  
  #________________________________________
  # Dealing with warnings
  ## Save current warning setting and disable warnings
  old_warn <- getOption("warn")
  options(warn = -1)   # -1 = suppress all warnings
  
  on.exit(options(warn = old_warn), add = TRUE)  # restore when function exits
  
  #________________________________________
  
  # Defining the default logs for info messages
  log_message <- function(...) {
    if (verbose) cli::cli_alert_info(...)
  }
  
  #_____________
  
  log_progress_step <- function(...) {
    if (verbose) cli::cli_progress_step(..., spinner = TRUE)
  }
  
  #_____________
  
  log_progress_done <- function() {
    if(verbose) cli::cli_progress_done()
  }
  
  #_____________
  
  log_h1 <- function(...) {
    if(verbose) cli::cli_h1(...)
  }
  
  #_____________
  
  log_h2 <- function(...) {
    if(verbose) cli::cli_h2(...)
  }
  
  #_____________
  
  log_space <- function() {
    if(verbose) cli::cli_text("")
  }
  
  #________________________________________
  
  # Setting the seed
  set.seed(seed)
  
  # Start of function
  if(verbose) {
    cli::cli_rule(left = cli::style_italic(cli::style_bold("Starting MarkerPurity!")), right = cli::col_silver(Sys.time()))
  }
  
  if(!is.null(desired_clusters)) {
    cli::cli_h1("Calculating the Purity of Markers Within Desired Cluster")
  } else if(!is.null(desired_cells)) {
    cli::cli_h1("Calculating the Purity of Markers Within Desired Cell Subsets")
  }
  
  log_h2("Preparing the input data")
  
  if(!is.null(cluster_labels) & is.null(desired_clusters)) {
    log_message("While cluster_labels is specified, desired_clusters is not specified, so none of the clusters will be inspected!")
  }
  
  log_progress_step("Inspecting the input data")
  
  if((is.null(so) & is.null(data)) | (!is.null(so) & !is.null(data))) {
    cli::cli_abort("Either 'data' or 'so' should be specified, not both or neither.")
  }
  
  if(is.null(desired_markers)) {
    if(is.null(data)) {
      desired_markers <- rownames(so)
    } else {
      desired_markers <- rownames(data)
    }
  }
  
  if(!is.null(so)) {
    if(length(cluster_labels) > 1 | !inherits(cluster_labels, "character")) {
      cli::cli_abort("The 'cluster_labels' argument should be one of the column names in the meta.data of the specified 'so' object!")
    }
  } else if(length(cluster_labels) != ncol(data) | !inherits(cluster_labels, "character")) {
    cli::cli_abort("The 'cluster_labels' argument should be a character vector with a length equal to the number of columns (or cells) in the input 'data' matrix!")
  }
  
  # SO quality control
  if(!is.null(so)) {
    if(length(grep(assay, Seurat::Assays(so))) != 1) {
      cli::cli_abort("The specified 'assay' name is not among the list of assays of the Seurat object.\n\nYou can check the list of available assays using the command Seurat::Assays(so)")
    } else if(nrow(so[[assay]][layer]) == 0) {
      cli::cli_abort("The specified 'layer' name is not among the list of layers of the specified 'assay' of the Seurat object.")
    }
  }
  
  log_progress_done()
  
  if(verbose) {
    qc_status_id <- cli::cli_status("Extracting expression matrix...")
  }
  
  if(is.null(so)) {
    if(!inherits(data, c("matrix", "Matrix"))) {
      expr_mat <- as.matrix(data) # This will be treated both as the expr_mat and norm_expr_mat
    } else {
      expr_mat <- data
    }
  } else {
    if(!inherits(so[[assay]][layer], c("matrix", "Matrix"))) {
      expr_mat <- as.matrix(so[[assay]][layer]) # This will be treated both as the expr_mat and norm_expr_mat
    } else {
      expr_mat <- so[[assay]][layer]
    }
  }
  
  if(any(is.null(colnames(expr_mat))) | any(is.null(rownames(expr_mat)))) {
    cli::cli_abort("All rows and columns of the input data should have (unique) names.")
  }
  
  if(is.null(desired_cells) & is.null(desired_clusters)) {
    cli::cli_abort("You must specify either desired_clusters, desired_cells, or both. Both parameters cannot be left unspecified!")
  } else if(is.null(cluster_labels) & !is.null(desired_clusters)) {
    cli::cli_abort("cluster_labels is not specified. Please provide the cluster_labels!")
  } else if(!is.null(desired_cells) & !base::inherits(desired_cells, "list")) {
    
    tmp_desired_cells_A <- base::sample(colnames(expr_mat), 3)
    tmp_desired_cells_A <- paste("'", tmp_desired_cells_A, "'", sep = "")
    tmp_desired_cells_A <- paste0(tmp_desired_cells_A, collapse = ", ")
    
    tmp_desired_cells_B <- base::sample(colnames(expr_mat), 4)
    tmp_desired_cells_B <- paste("'", tmp_desired_cells_B, "'", sep = "")
    tmp_desired_cells_B <- paste0(tmp_desired_cells_B, collapse = ", ")
    
    cli::cli_abort(paste0("desired_cells should be a named list of the names of desired cells!\n\nFor example: list(type_A = c(", 
                          tmp_desired_cells_A, 
                          "), type_B = c(", tmp_desired_cells_B, ")"))
  }
  
  #____________________
  
  # Input clusters
  if(!is.null(cluster_labels) & !is.null(desired_clusters)) {
    if(is.null(so)) {
      major_clusters <- cluster_labels %>% as.character() %>% setNames(colnames(expr_mat))
      major_clusters <- major_clusters[major_clusters %in% desired_clusters]
    } else {
      major_clusters <- so[[cluster_labels]] %>% 
        unlist() %>% as.vector() %>% as.character() %>% stats::setNames(colnames(expr_mat))
      major_clusters <- major_clusters[major_clusters %in% desired_clusters]
    }
    major_clusters[is.na(major_clusters)] <- "NA"
  } else {
    major_clusters <- NULL
  }
  
  #____________________
  
  # Input cell subsets
  if(!is.null(desired_cells)) {
    cell_subsets <- lapply(names(desired_cells), function(subset_name) {
      curr_subset <- desired_cells[[subset_name]]
      curr_subset <- stats::setNames(rep(subset_name, length(curr_subset)), curr_subset)
      curr_subset
    }) %>% unlist()
  } else {
    cell_subsets <- NULL
  }
  
  #____________________
  
  if(verbose) {
    cli::cli_status_update(qc_status_id, "Removing noise genes...")
  }
  
  noise_features <- rownames(expr_mat)[Matrix::rowSums(expr_mat > 0) <= noise_feature_thresh]
  
  if(length(noise_features) > 0) {
    if(!is.null(desired_markers) & any(desired_markers %in% noise_features)) {
      noise_features <- noise_features[-which(noise_features %in% desired_markers)]
    } else if(is.null(desired_markers)) {
      desired_markers <- desired_markers[-which(desired_markers %in% noise_features)]
    }
  }
  
  # Removing noise genes
  if(length(noise_features) > 0) {
    expr_mat <- expr_mat[-which(rownames(expr_mat) %in% noise_features),]
  }
  
  # Step 1: Log1p transformation and converting zeros to NA
  if(log1p) {
    expr_mat <- log1p(expr_mat)
  }
  
  expr_mat <- Matrix(expr_mat, sparse=TRUE)
  
  #____________________
  
  # Defining the expression-weighted centered scaled rank thresholds
  
  log_progress_step("Expression-weighted centered scaled rank normalizing cells")
  
  # Global expression-weighted centered scaled rank data
  ewcsr_mat <- ewcsr.sparse(expr_mat)
  
  ewcsr_vec <- matrix_to_vector_na_omit(ewcsr_mat)
  ewcsr_vec_pos <- ewcsr_vec[ewcsr_vec > 0] %>% sort()
  ewcsr_vec_neg <- ewcsr_vec[ewcsr_vec < 0] %>% sort() %>% rev()
  
  ewcsr_high_thresh <- quantile(ewcsr_vec_pos, probs = high_quantile)
  ewcsr_low_thresh <- quantile(ewcsr_vec_neg, probs = low_quantile)
  
  log_progress_done()
  
  #___________________________________
  
  # Generating the final global matrices
  
  log_progress_step("Generating EWCSR-based logical matrices")
  
  ## pos_mat
  # pos_mat <- ewcsr_mat > ewcsr_high_thresh
  # pos_mat[is.na(pos_mat)] <- FALSE
  pos_mat <- sparse_compare_threshold(mat = ewcsr_mat, op = ">", threshold = ewcsr_high_thresh, zero_to_false = TRUE)
  
  ### Detecting quiescent cells 
  quiescent_cells <- colnames(pos_mat)[which(Matrix::colSums(pos_mat) == 0)]
  if(remove_quiescent_cells) {
    if(length(quiescent_cells) > 0) {
      
      # Updating the pos_mat
      pos_mat <- pos_mat[,-which(colnames(pos_mat) %in% quiescent_cells)]
      
      # Updating the ewcsr_mat
      ewcsr_mat <- ewcsr_mat[,-which(colnames(ewcsr_mat) %in% quiescent_cells)]
    }
  }
  
  #_______________
  
  ## neg_mat
  # neg_mat <- ewcsr_mat < ewcsr_low_thresh
  # neg_mat[is.na(neg_mat)] <- FALSE
  neg_mat <- sparse_compare_threshold(mat = ewcsr_mat, op = "<", threshold = ewcsr_low_thresh, zero_to_false = TRUE)
  
  ### Detecting and removing no_neg_cells cells 
  no_neg_cells <- colnames(neg_mat)[which(Matrix::colSums(neg_mat) == 0)]
  if(length(no_neg_cells) > 0) {
    
    # Updating the neg_mat
    neg_mat <- neg_mat[,-which(colnames(neg_mat) %in% no_neg_cells)]
    
  }
  
  #_______________
  
  ## med_mat
  # med_mat <- ewcsr_mat > ewcsr_low_thresh/2 & ewcsr_mat < ewcsr_high_thresh/2 # We use half of the defined thresholds for defining the med markers
  # med_mat[is.na(med_mat)] <- FALSE
  med_mat <- sparse_between_thresholds(mat = ewcsr_mat, 
                                       lower_threshold = ewcsr_low_thresh/2, upper_threshold = ewcsr_high_thresh/2, # We use half of the defined thresholds for defining the med markers
                                       zero_to_false = TRUE  # zero_to_false = TRUE
  )
  
  ### Detecting and removing no_med_cells cells 
  no_med_cells <- colnames(med_mat)[which(Matrix::colSums(med_mat) == 0)]
  if(length(no_med_cells) > 0) {
    
    # Updating the med_mat
    med_mat <- med_mat[,-which(colnames(med_mat) %in% no_med_cells)]
    
  }
  
  log_progress_done()
  
  #_____________________________________
  
  # Calculating the purity of desired markers within desired clusters
  
  if(!is.null(major_clusters)) {
    
    log_progress_step("Calculating the purity of desired markers within desired clusters")
    
    major_cluster_ids <- sort(unique(major_clusters))
    
    ## For each cluster, calculate pos marker purity
    
    #_______________
    
    if(verbose) {
      major_cluster_marker_status <- cli::cli_status("Investigating the purity of desired markers from a 'Positive Marker' perspective ...")
    }
    
    ### Ensure only clustered cells are used
    pos_mat_major_clustered <- pos_mat[, which(colnames(pos_mat) %in% names(major_clusters)), drop = FALSE]
    
    #_____________________________________
    
    # Calculate the purity of Markers per Desired Cluster
    
    ## pos markers
    if(nrow(pos_mat_major_clustered) > 0) {
      major_cluster_pos_markers <- lapply(major_cluster_ids, function(cid) {
        cells <- names(major_clusters)[major_clusters == cid] %>% na.omit()
        cells <- cells[cells %in% colnames(pos_mat_major_clustered)]
        
        curr_pos_mat <- pos_mat_major_clustered[desired_markers, cells, drop = FALSE]
        
        if(ncol(curr_pos_mat) > 1) {
          # curr_cluster_gini_scores <- apply(curr_pos_mat, 1, function(x) ineq::Gini(x))
          curr_cluster_gini_scores <- gini_rows_lg_matrix(curr_pos_mat@i, curr_pos_mat@p, curr_pos_mat@x, nrow(curr_pos_mat), ncol(curr_pos_mat))
          names(curr_cluster_gini_scores) <- rownames(curr_pos_mat)
          curr_cluster_gini_scores <- data.frame(Feature = names(curr_cluster_gini_scores), 
                                                 Gini_Score = curr_cluster_gini_scores,
                                                 Purity = 1 - curr_cluster_gini_scores,
                                                 Rank = data.table::frankv(curr_cluster_gini_scores, ties.method="dense")) %>% dplyr::arrange(Rank) %>% 
            dplyr::filter(!is.na(Gini_Score))
          rownames(curr_cluster_gini_scores) <- NULL
          curr_cluster_gini_scores
        } else if(length(cells) > 0) {
          curr_cluster_gini_scores <- ewcsr_mat[rownames(curr_pos_mat)[as.vector(curr_pos_mat)], cells]
          curr_cluster_gini_scores <- data.frame(Feature = names(curr_cluster_gini_scores), 
                                                 EWCSR = curr_cluster_gini_scores,
                                                 Rank = data.table::frankv(curr_cluster_gini_scores, order = -1, ties.method="dense")) %>% dplyr::arrange(Rank)
          rownames(curr_cluster_gini_scores) <- NULL
          curr_cluster_gini_scores
        } else {
          return(base::structure("❗ No specific marker was identified!", class = "logMessage"))
        }
      })
    } else {
      major_cluster_pos_markers <- lapply(major_cluster_ids, function(cid) {
        return(base::structure("❗ No specific marker was identified!", class = "logMessage"))
      })
    }
    
    names(major_cluster_pos_markers) <- major_cluster_ids
    
    #____________________
    
    ## neg markers
    
    if(verbose) {
      major_cluster_marker_status <- cli::cli_status("Investigating the purity of desired markers from a 'Negative Marker' perspective ...")
    }
    
    ### Ensure only clustered cells are used
    neg_mat_major_clustered <- neg_mat[, which(colnames(neg_mat) %in% names(major_clusters)), drop = FALSE]
    
    if(nrow(neg_mat_major_clustered) > 0) {
      major_cluster_neg_markers <- lapply(major_cluster_ids, function(cid) {
        cells <- names(major_clusters)[major_clusters == cid] %>% na.omit()
        cells <- cells[cells %in% colnames(neg_mat_major_clustered)]
        
        curr_neg_mat <- neg_mat_major_clustered[desired_markers, cells, drop = FALSE]
        
        if(ncol(curr_neg_mat) > 1) {
          # curr_cluster_gini_scores <- apply(curr_neg_mat, 1, function(x) ineq::Gini(x))
          curr_cluster_gini_scores <- gini_rows_lg_matrix(curr_neg_mat@i, curr_neg_mat@p, curr_neg_mat@x, nrow(curr_neg_mat), ncol(curr_neg_mat))
          names(curr_cluster_gini_scores) <- rownames(curr_neg_mat)
          curr_cluster_gini_scores <- data.frame(Feature = names(curr_cluster_gini_scores), 
                                                 Gini_Score = curr_cluster_gini_scores,
                                                 Purity = 1 - curr_cluster_gini_scores,
                                                 Rank = data.table::frankv(curr_cluster_gini_scores, ties.method="dense")) %>% dplyr::arrange(Rank) %>% 
            dplyr::filter(!is.na(Gini_Score))
          rownames(curr_cluster_gini_scores) <- NULL
          curr_cluster_gini_scores
        } else if(length(cells) > 0) {
          curr_cluster_gini_scores <- ewcsr_mat[rownames(curr_neg_mat)[as.vector(curr_neg_mat)], cells]
          curr_cluster_gini_scores <- data.frame(Feature = names(curr_cluster_gini_scores), 
                                                 EWCSR = curr_cluster_gini_scores,
                                                 Rank = data.table::frankv(curr_cluster_gini_scores, order = -1, ties.method="dense")) %>% dplyr::arrange(Rank)
          rownames(curr_cluster_gini_scores) <- NULL
          curr_cluster_gini_scores
        } else {
          return(base::structure("❗ No specific marker was identified!", class = "logMessage"))
        }
      })
    } else {
      major_cluster_neg_markers <- lapply(major_cluster_ids, function(cid) {
        return(base::structure("❗ No specific marker was identified!", class = "logMessage"))
      })
    }
    
    names(major_cluster_neg_markers) <- major_cluster_ids
    
    #____________________
    
    ## med markers
    if(verbose) {
      major_cluster_marker_status <- cli::cli_status("Investigating the purity of desired markers from a 'Medium Marker' perspective ...")
    }
    
    ### Ensure only clustered cells are used
    med_mat_major_clustered <- med_mat[, which(colnames(med_mat) %in% names(major_clusters)), drop = FALSE]
    
    if(nrow(med_mat_major_clustered) > 0) {
      major_cluster_med_markers <- lapply(major_cluster_ids, function(cid) {
        cells <- names(major_clusters)[major_clusters == cid] %>% na.omit()
        cells <- cells[cells %in% colnames(med_mat_major_clustered)]
        
        curr_med_mat <- med_mat_major_clustered[desired_markers, cells, drop = FALSE]
        
        if(ncol(curr_med_mat) > 1) {
          # curr_cluster_gini_scores <- apply(curr_med_mat, 1, function(x) ineq::Gini(x))
          curr_cluster_gini_scores <- gini_rows_lg_matrix(curr_med_mat@i, curr_med_mat@p, curr_med_mat@x, nrow(curr_med_mat), ncol(curr_med_mat))
          names(curr_cluster_gini_scores) <- rownames(curr_med_mat)
          curr_cluster_gini_scores <- data.frame(Feature = names(curr_cluster_gini_scores), 
                                                 Gini_Score = curr_cluster_gini_scores,
                                                 Purity = 1 - curr_cluster_gini_scores,
                                                 Rank = data.table::frankv(curr_cluster_gini_scores, ties.method="dense")) %>% dplyr::arrange(Rank) %>% 
            dplyr::filter(!is.na(Gini_Score))
          rownames(curr_cluster_gini_scores) <- NULL
          curr_cluster_gini_scores
        } else if(length(cells) > 0) {
          curr_cluster_gini_scores <- ewcsr_mat[rownames(curr_med_mat)[as.vector(curr_med_mat)], cells]
          curr_cluster_gini_scores <- data.frame(Feature = names(curr_cluster_gini_scores), 
                                                 EWCSR = curr_cluster_gini_scores,
                                                 Rank = data.table::frankv(curr_cluster_gini_scores, order = -1, ties.method="dense")) %>% dplyr::arrange(Rank)
          rownames(curr_cluster_gini_scores) <- NULL
          curr_cluster_gini_scores
        } else {
          return(base::structure("❗ No specific marker was identified!", class = "logMessage"))
        }
      })
    } else {
      major_cluster_med_markers <- lapply(major_cluster_ids, function(cid) {
        return(base::structure("❗ No specific marker was identified!", class = "logMessage"))
      })
    }
    
    names(major_cluster_med_markers) <- major_cluster_ids
    
    #_____________________________________
    
    # Finalizing the major_cluster_markers
    
    ## R Version
    # lapply(names(major_cluster_pos_markers), function(cur_cl) {
    #   
    #   # subsetting the cluster markers
    #   curr_pos_markers <- major_cluster_pos_markers[[cur_cl]]
    #   curr_neg_markers <- major_cluster_neg_markers[[cur_cl]]
    #   curr_med_markers <- major_cluster_med_markers[[cur_cl]]
    #   
    #   # Step 0: Store the original objects in a named list
    #   major_cluster_markers_list <- list(
    #     curr_pos_markers = curr_pos_markers,
    #     curr_neg_markers = curr_neg_markers,
    #     curr_med_markers = curr_med_markers
    #   )
    #   
    #   # Step 1: Keep only data frames
    #   major_cluster_marker_dfs <- lapply(major_cluster_markers_list, function(df) if (is.data.frame(df)) df else NULL)
    #   major_cluster_marker_dfs <- Filter(Negate(is.null), major_cluster_marker_dfs)
    #   
    #   # Step 2: Keep only those with a "Purity" column
    #   major_cluster_with_purity <- major_cluster_marker_dfs[sapply(major_cluster_marker_dfs, function(df) "Purity" %in% colnames(df))]
    #   
    #   # Step 3: Create a named list of Features
    #   major_cluster_features_list <- lapply(major_cluster_with_purity, function(df) df$Feature)
    #   
    #   # Step 4: Count how many data frames each Feature appears in
    #   major_cluster_feature_counts <- table(unlist(major_cluster_features_list))
    #   overlapping_features <- names(major_cluster_feature_counts[major_cluster_feature_counts >= 2])
    #   
    #   # Step 5: For each overlapping feature, retain only in the data frame with highest purity
    #   for (feature in overlapping_features) {
    #     # Collect Purity values across data frames
    #     major_cluster_purity_values <- sapply(names(major_cluster_with_purity), function(name) {
    #       df <- major_cluster_with_purity[[name]]
    #       row <- df[df$Feature == feature, ]
    #       if (nrow(row) > 0) return(row$Purity) else return(NA)
    #     })
    #     
    #     # Identify the data frame with highest purity for this feature
    #     if (all(is.na(major_cluster_purity_values))) next  # skip if all values are NA
    #     major_cluster_max_purity_name <- names(which.max(major_cluster_purity_values))
    #     
    #     # Remove the feature from all other data frames
    #     for (name in setdiff(names(major_cluster_with_purity), major_cluster_max_purity_name)) {
    #       df <- major_cluster_with_purity[[name]]
    #       major_cluster_with_purity[[name]] <- df[df$Feature != feature, ]
    #     }
    #   }
    #   
    #   # Step 6: Merge updated data frames back into the full list
    #   for (name in names(major_cluster_marker_dfs)) {
    #     if (name %in% names(major_cluster_with_purity)) {
    #       major_cluster_marker_dfs[[name]] <- major_cluster_with_purity[[name]]
    #     }
    #     # else: keep the original version without "Purity" column
    #   }
    #   
    #   # Step 7: Assign updated versions back to original variables
    #   if ("curr_pos_markers" %in% names(major_cluster_marker_dfs)) curr_pos_markers <- major_cluster_marker_dfs[["curr_pos_markers"]]
    #   if ("curr_neg_markers" %in% names(major_cluster_marker_dfs)) curr_neg_markers <- major_cluster_marker_dfs[["curr_neg_markers"]]
    #   if ("curr_med_markers" %in% names(major_cluster_marker_dfs)) curr_med_markers <- major_cluster_marker_dfs[["curr_med_markers"]]
    #   
    #   # Updating the cluster markers
    #   major_cluster_pos_markers[[cur_cl]] <<- curr_pos_markers
    #   if(is.data.frame(major_cluster_pos_markers[[cur_cl]])) {
    #     if(nrow(major_cluster_pos_markers[[cur_cl]]) == 0) {
    #       major_cluster_pos_markers[[cur_cl]] <<- base::structure("❗ No specific marker was identified!", class = "logMessage")
    #     }
    #   }
    #   
    #   major_cluster_neg_markers[[cur_cl]] <<- curr_neg_markers
    #   if(is.data.frame(major_cluster_neg_markers[[cur_cl]])) {
    #     if(nrow(major_cluster_neg_markers[[cur_cl]]) == 0) {
    #       major_cluster_neg_markers[[cur_cl]] <<- base::structure("❗ No specific marker was identified!", class = "logMessage")
    #     }
    #   }
    #   
    #   major_cluster_med_markers[[cur_cl]] <<- curr_med_markers
    #   if(is.data.frame(major_cluster_med_markers[[cur_cl]])) {
    #     if(nrow(major_cluster_med_markers[[cur_cl]]) == 0) {
    #       major_cluster_med_markers[[cur_cl]] <<- base::structure("❗ No specific marker was identified!", class = "logMessage")
    #     }
    #   }
    # })
    
    ## C++ version
    cpp_major_markers_list <- filter_cluster_markers_cpp(pos_markers = major_cluster_pos_markers, 
                                                         neg_markers = major_cluster_neg_markers, 
                                                         med_markers = major_cluster_med_markers)
    
    major_cluster_pos_markers <- cpp_major_markers_list$pos
    major_cluster_neg_markers <- cpp_major_markers_list$neg
    major_cluster_med_markers <- cpp_major_markers_list$med
  }
  
  #_____________________________________
  
  # Calculating the purity of desired markers within desired cell subsets
  
  if(!is.null(cell_subsets)) {
    
    log_progress_step("Calculating the purity of desired markers within desired cell subsets")
    
    cell_subset_ids <- sort(unique(cell_subsets))
    
    ## For each subset, calculate pos marker purity
    
    #____________
    
    if(verbose) {
      cell_subset_marker_status <- cli::cli_status("Investigating the purity of desired markers from a 'Positive Marker' perspective ...")
    }
    
    ### Ensure only subsetted cells are used
    pos_mat_cell_subsetted <- pos_mat[, which(colnames(pos_mat) %in% names(cell_subsets)), drop = FALSE]
    
    #_____________________________________
    
    # Calculate the purity of Markers per Desired cell subset
    
    ## pos markers
    if(nrow(pos_mat_cell_subsetted) > 0) {
      cell_subset_pos_markers <- lapply(cell_subset_ids, function(cid) {
        cells <- names(cell_subsets)[cell_subsets == cid] %>% na.omit()
        cells <- cells[cells %in% colnames(pos_mat_cell_subsetted)]
        
        curr_pos_mat <- pos_mat_cell_subsetted[desired_markers, cells, drop = FALSE]
        
        if(ncol(curr_pos_mat) > 1) {
          # curr_subset_gini_scores <- apply(curr_pos_mat, 1, function(x) ineq::Gini(x))
          curr_subset_gini_scores <- gini_rows_lg_matrix(curr_pos_mat@i, curr_pos_mat@p, curr_pos_mat@x, nrow(curr_pos_mat), ncol(curr_pos_mat))
          names(curr_subset_gini_scores) <- rownames(curr_pos_mat)
          curr_subset_gini_scores <- data.frame(Feature = names(curr_subset_gini_scores), 
                                                Gini_Score = curr_subset_gini_scores,
                                                Purity = 1 - curr_subset_gini_scores,
                                                Rank = data.table::frankv(curr_subset_gini_scores, ties.method="dense")) %>% dplyr::arrange(Rank) %>% 
            dplyr::filter(!is.na(Gini_Score))
          rownames(curr_subset_gini_scores) <- NULL
          curr_subset_gini_scores
        } else if(length(cells) > 0) {
          curr_subset_gini_scores <- ewcsr_mat[rownames(curr_pos_mat)[as.vector(curr_pos_mat)], cells]
          curr_subset_gini_scores <- data.frame(Feature = names(curr_subset_gini_scores), 
                                                EWCSR = curr_subset_gini_scores,
                                                Rank = data.table::frankv(curr_subset_gini_scores, order = -1, ties.method="dense")) %>% dplyr::arrange(Rank)
          rownames(curr_subset_gini_scores) <- NULL
          curr_subset_gini_scores
        } else {
          return(base::structure("❗ No specific marker was identified!", class = "logMessage"))
        }
      })
    } else {
      cell_subset_pos_markers <- lapply(cell_subset_ids, function(cid) {
        return(base::structure("❗ No specific marker was identified!", class = "logMessage"))
      })
    }
    
    names(cell_subset_pos_markers) <- cell_subset_ids
    
    #____________________
    
    ## neg markers
    
    if(verbose) {
      cell_subset_marker_status <- cli::cli_status("Investigating the purity of desired markers from a 'Negative Marker' perspective ...")
    }
    
    ### Ensure only subsetted cells are used
    neg_mat_cell_subsetted <- neg_mat[, which(colnames(neg_mat) %in% names(cell_subsets)), drop = FALSE]
    
    if(nrow(neg_mat_cell_subsetted) > 0) {
      cell_subset_neg_markers <- lapply(cell_subset_ids, function(cid) {
        cells <- names(cell_subsets)[cell_subsets == cid] %>% na.omit()
        cells <- cells[cells %in% colnames(neg_mat_cell_subsetted)]
        
        curr_neg_mat <- neg_mat_cell_subsetted[desired_markers, cells, drop = FALSE]
        
        if(ncol(curr_neg_mat) > 1) {
          # curr_subset_gini_scores <- apply(curr_neg_mat, 1, function(x) ineq::Gini(x))
          curr_subset_gini_scores <- gini_rows_lg_matrix(curr_neg_mat@i, curr_neg_mat@p, curr_neg_mat@x, nrow(curr_neg_mat), ncol(curr_neg_mat))
          names(curr_subset_gini_scores) <- rownames(curr_neg_mat)
          curr_subset_gini_scores <- data.frame(Feature = names(curr_subset_gini_scores), 
                                                Gini_Score = curr_subset_gini_scores,
                                                Purity = 1 - curr_subset_gini_scores,
                                                Rank = data.table::frankv(curr_subset_gini_scores, ties.method="dense")) %>% dplyr::arrange(Rank) %>% 
            dplyr::filter(!is.na(Gini_Score))
          rownames(curr_subset_gini_scores) <- NULL
          curr_subset_gini_scores
        } else if(length(cells) > 0) {
          curr_subset_gini_scores <- ewcsr_mat[rownames(curr_neg_mat)[as.vector(curr_neg_mat)], cells]
          curr_subset_gini_scores <- data.frame(Feature = names(curr_subset_gini_scores), 
                                                EWCSR = curr_subset_gini_scores,
                                                Rank = data.table::frankv(curr_subset_gini_scores, order = -1, ties.method="dense")) %>% dplyr::arrange(Rank)
          rownames(curr_subset_gini_scores) <- NULL
          curr_subset_gini_scores
        } else {
          return(base::structure("❗ No specific marker was identified!", class = "logMessage"))
        }
      })
    } else {
      cell_subset_neg_markers <- lapply(cell_subset_ids, function(cid) {
        return(base::structure("❗ No specific marker was identified!", class = "logMessage"))
      })
    }
    
    names(cell_subset_neg_markers) <- cell_subset_ids
    
    #____________________
    
    ## med markers
    
    if(verbose) {
      cell_subset_marker_status <- cli::cli_status("Investigating the purity of desired markers from a 'Medium Marker' perspective ...")
    }
    
    ### Ensure only subsetted cells are used
    med_mat_cell_subsetted <- med_mat[, which(colnames(med_mat) %in% names(cell_subsets)), drop = FALSE]
    
    if(nrow(med_mat_cell_subsetted) > 0) {
      cell_subset_med_markers <- lapply(cell_subset_ids, function(cid) {
        cells <- names(cell_subsets)[cell_subsets == cid] %>% na.omit()
        cells <- cells[cells %in% colnames(med_mat_cell_subsetted)]
        
        curr_med_mat <- med_mat_cell_subsetted[desired_markers, cells, drop = FALSE]
        
        if(ncol(curr_med_mat) > 1) {
          # curr_subset_gini_scores <- apply(curr_med_mat, 1, function(x) ineq::Gini(x))
          curr_cluster_gini_scores <- gini_rows_lg_matrix(curr_med_mat@i, curr_med_mat@p, curr_med_mat@x, nrow(curr_med_mat), ncol(curr_med_mat))
          names(curr_cluster_gini_scores) <- rownames(curr_med_mat)
          curr_subset_gini_scores <- data.frame(Feature = names(curr_subset_gini_scores), 
                                                Gini_Score = curr_subset_gini_scores,
                                                Purity = 1 - curr_subset_gini_scores,
                                                Rank = data.table::frankv(curr_subset_gini_scores, ties.method="dense")) %>% dplyr::arrange(Rank) %>% 
            dplyr::filter(!is.na(Gini_Score))
          rownames(curr_subset_gini_scores) <- NULL
          curr_subset_gini_scores
        } else if(length(cells) > 0) {
          curr_subset_gini_scores <- ewcsr_mat[rownames(curr_med_mat)[as.vector(curr_med_mat)], cells]
          curr_subset_gini_scores <- data.frame(Feature = names(curr_subset_gini_scores), 
                                                EWCSR = curr_subset_gini_scores,
                                                Rank = data.table::frankv(curr_subset_gini_scores, order = -1, ties.method="dense")) %>% dplyr::arrange(Rank)
          rownames(curr_subset_gini_scores) <- NULL
          curr_subset_gini_scores
        } else {
          return(base::structure("❗ No specific marker was identified!", class = "logMessage"))
        }
      })
    } else {
      cell_subset_med_markers <- lapply(cell_subset_ids, function(cid) {
        return(base::structure("❗ No specific marker was identified!", class = "logMessage"))
      })
    }
    
    names(cell_subset_med_markers) <- cell_subset_ids
    
    #_____________________________________
    
    # Finalizing the cell_subset_markers
    
    ## R Version
    # lapply(names(cell_subset_pos_markers), function(cur_subset) {
    #   
    #   # subsetting the subset markers
    #   curr_pos_markers <- cell_subset_pos_markers[[cur_subset]]
    #   curr_neg_markers <- cell_subset_neg_markers[[cur_subset]]
    #   curr_med_markers <- cell_subset_med_markers[[cur_subset]]
    #   
    #   # Step 0: Store the original objects in a named list
    #   cell_subset_markers_list <- list(
    #     curr_pos_markers = curr_pos_markers,
    #     curr_neg_markers = curr_neg_markers,
    #     curr_med_markers = curr_med_markers
    #   )
    #   
    #   # Step 1: Keep only data frames
    #   cell_subset_marker_dfs <- lapply(cell_subset_markers_list, function(df) if (is.data.frame(df)) df else NULL)
    #   cell_subset_marker_dfs <- Filter(Negate(is.null), cell_subset_marker_dfs)
    #   
    #   # Step 2: Keep only those with a "Purity" column
    #   cell_subset_with_purity <- cell_subset_marker_dfs[sapply(cell_subset_marker_dfs, function(df) "Purity" %in% colnames(df))]
    #   
    #   # Step 3: Create a named list of Features
    #   cell_subset_features_list <- lapply(cell_subset_with_purity, function(df) df$Feature)
    #   
    #   # Step 4: Count how many data frames each Feature appears in
    #   cell_subset_feature_counts <- table(unlist(cell_subset_features_list))
    #   overlapping_features <- names(cell_subset_feature_counts[cell_subset_feature_counts >= 2])
    #   
    #   # Step 5: For each overlapping feature, retain only in the data frame with highest purity
    #   for (feature in overlapping_features) {
    #     # Collect Purity values across data frames
    #     cell_subset_purity_values <- sapply(names(cell_subset_with_purity), function(name) {
    #       df <- cell_subset_with_purity[[name]]
    #       row <- df[df$Feature == feature, ]
    #       if (nrow(row) > 0) return(row$Purity) else return(NA)
    #     })
    #     
    #     # Identify the data frame with highest purity for this feature
    #     if (all(is.na(cell_subset_purity_values))) next  # skip if all values are NA
    #     cell_subset_max_purity_name <- names(which.max(cell_subset_purity_values))
    #     
    #     # Remove the feature from all other data frames
    #     for (name in setdiff(names(cell_subset_with_purity), cell_subset_max_purity_name)) {
    #       df <- cell_subset_with_purity[[name]]
    #       cell_subset_with_purity[[name]] <- df[df$Feature != feature, ]
    #     }
    #   }
    #   
    #   # Step 6: Merge updated data frames back into the full list
    #   for (name in names(cell_subset_marker_dfs)) {
    #     if (name %in% names(cell_subset_with_purity)) {
    #       cell_subset_marker_dfs[[name]] <- cell_subset_with_purity[[name]]
    #     }
    #     # else: keep the original version without "Purity" column
    #   }
    #   
    #   # Step 7: Assign updated versions back to original variables
    #   if ("curr_pos_markers" %in% names(cell_subset_marker_dfs)) curr_pos_markers <- cell_subset_marker_dfs[["curr_pos_markers"]]
    #   if ("curr_neg_markers" %in% names(cell_subset_marker_dfs)) curr_neg_markers <- cell_subset_marker_dfs[["curr_neg_markers"]]
    #   if ("curr_med_markers" %in% names(cell_subset_marker_dfs)) curr_med_markers <- cell_subset_marker_dfs[["curr_med_markers"]]
    #   
    #   # Updating the subset markers
    #   cell_subset_pos_markers[[cur_subset]] <<- curr_pos_markers
    #   if(is.data.frame(cell_subset_pos_markers[[cur_subset]])) {
    #     if(nrow(cell_subset_pos_markers[[cur_subset]]) == 0) {
    #       cell_subset_pos_markers[[cur_subset]] <<- base::structure("❗ No specific marker was identified!", class = "logMessage")
    #     }
    #   }
    #   
    #   cell_subset_neg_markers[[cur_subset]] <<- curr_neg_markers
    #   if(is.data.frame(cell_subset_neg_markers[[cur_subset]])) {
    #     if(nrow(cell_subset_neg_markers[[cur_subset]]) == 0) {
    #       cell_subset_neg_markers[[cur_subset]] <<- base::structure("❗ No specific marker was identified!", class = "logMessage")
    #     }
    #   }
    #   
    #   cell_subset_med_markers[[cur_subset]] <<- curr_med_markers
    #   if(is.data.frame(cell_subset_med_markers[[cur_subset]])) {
    #     if(nrow(cell_subset_med_markers[[cur_subset]]) == 0) {
    #       cell_subset_med_markers[[cur_subset]] <<- base::structure("❗ No specific marker was identified!", class = "logMessage")
    #     }
    #   }
    # })
    
    ## C++ version
    cpp_cell_subset_markers_list <- filter_cluster_markers_cpp(pos_markers = cell_subset_pos_markers, 
                                                               neg_markers = cell_subset_neg_markers, 
                                                               med_markers = cell_subset_med_markers)
    
    cell_subset_pos_markers <- cpp_cell_subset_markers_list$pos
    cell_subset_neg_markers <- cpp_cell_subset_markers_list$neg
    cell_subset_med_markers <- cpp_cell_subset_markers_list$med
  }
  
  #_____________________________________
  
  # Preparing the Results Lists
  
  if(!is.null(major_clusters) & !is.null(cell_subsets)) {
    final_results_list <- list(
      within_clusters = list(
        positive_markers = major_cluster_pos_markers,
        negative_markers = major_cluster_neg_markers,
        medium_markers = major_cluster_med_markers
      ),
      within_cell_subsets = list(
        positive_markers = cell_subset_pos_markers,
        negative_markers = cell_subset_neg_markers,
        medium_markers = cell_subset_med_markers
      )
    )
  } else if(!is.null(major_clusters)) {
    final_results_list <- list(
      within_clusters = list(
        positive_markers = major_cluster_pos_markers,
        negative_markers = major_cluster_neg_markers,
        medium_markers = major_cluster_med_markers
      )
    )
  } else if(!is.null(cell_subsets)) {
    final_results_list <- list(
      within_cell_subsets = list(
        positive_markers = cell_subset_pos_markers,
        negative_markers = cell_subset_neg_markers,
        medium_markers = cell_subset_med_markers
      )
    )
  }
  
  #___________________________________
  
  # Finding features from the desired markers vector that are not identified as markers
  
  identified_featuers <- final_results_list %>% unlist() %>% unname() %>% unique()
  identified_featuers <- identified_featuers[which(is.na(suppressWarnings(as.numeric(identified_featuers))))]
  if(any(grepl("No specific marker was identified", identified_featuers))) {
    identified_featuers <- identified_featuers[-grep("No specific marker was identified", identified_featuers)]
  }
  
  non_identified_featuers <- desired_markers[-which(desired_markers %in% identified_featuers)]
  if(length(non_identified_featuers) > 0) {
    final_results_list$non_markers <- non_identified_featuers
  }
  
  #___________________________________
  
  # Adding Quiescent Cells
  
  if(length(quiescent_cells) > 0) {
    final_results_list$quiescent_cells <- quiescent_cells
  }
  
  #___________________________________
  
  if(verbose) {
    log_space()
    cli::cli_rule(left = cli::col_green("SUCCESS"), right = cli::col_silver(Sys.time()))
    cli::cli_alert_success(cli::style_italic(cli::style_bold("MarkerPurity finished successfully!")))
  }
  
  # Return results
  structure(final_results_list,
            class = "MarkerPurity")
  
}