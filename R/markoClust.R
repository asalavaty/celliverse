library(Matrix)
library(igraph)
library(cli)
library(data.table)
library(dplyr)
library(magrittr)

#___________________________

markoClust <- function(
    so = NULL, # A Seurat object. Either `so` or `data` argument should be specified, but not both.
    assay = "RNA", # The assay we want to use for evaluating the clusters.
    layer = "counts", # The layer of the assay we want to use for evaluating the clusters (this can be a normalized layer).
    norm_assay = "RNA", # The assay that include a normalized layer and we want to use for the detection of highly variable genes (HVGs). This can be the same as the 'assay'.
    norm_layer = "data", # The normalized layer of the assay that we want to use for the detection of highly variable genes (HVGs).
    data = NULL, # matrix, It is recommended to input normalized data (at least lib size normalized) if you have set the subset_to_HVG = TRUE.
    cluster_labels, # A character vector of cluster labels (its length should be the same as the number of columns/cells of the data) or the column name of cluster labels in the meta.data of so.
    log1p = TRUE, # Weather to log1p transform the data or not.
    remove_quiescent_cells = TRUE, # Whether to remove quiescent_cells before marker identification or not.
    high_quantile = 0.25, # The quantile threshold for choosing highly positive expression-weighted centered scaled ranks required for filtering the data and for selecting the positive markers. Higher values label more features as features with high expression-weighted centered scaled ranks.
    low_quantile = 0.25, # The quantile threshold for choosing highly negative expression-weighted centered scaled ranks required for filtering the data and for selecting the negative markers. Lower values label more features as features with low expression-weighted centered scaled ranks.
    subset_to_HVG = FALSE, # Weather to subset the input data to highly variable genes or use all the genes.
    hvg_selection.method = c("vst", "mean.var.plot", "dispersion"), # How to choose top variable features. Choose one of 'vst', 'mean.var.plot', or 'dispersion'
    hvg_var_thresh = 1, # The variance threshold for choosing HVGs (genes whose variability is more than this threshold standard deviation above the expected technical noise).
    gini_thresh = 0.5, # The Gini threshold for detecting non-specific (global) markers.
    noise_feature_thresh = 4, # The threshold for detecting noise features/genes (i.e. features that have non-zero expression in more than this number of samples/cells).
    random_marker_thresh = 5, # Markers detected at lower than this number of cell are considered as non-marker genes
    mr_thresh = NULL, # The threshold for choosing the cell-to-cell similarities with lower than selected thresh (if it is null it will be set to the square root of the number of cells by default).
    isolated_cluster_thresh = 5, # Major clusters with lower than this number of cells (default is set to 5) will be considered as isolated cells.
    leiden_obj_function = c("modularity", "CPM"), # Whether to use the Constant Potts Model (CPM) or modularity.
    leiden_resolution = 1, # The resolution parameter to use. Higher resolutions lead to more smaller communities, while lower resolutions lead to fewer larger communities.
    leiden_n_iterations = 5, # the number of iterations to iterate the Leiden algorithm. Each iteration may improve the partition further.
    identify_subclusters = TRUE, # Whether to identify subclusters as well or not
    subcluster_resolution_weight = 0.75, # A multiplier for `leiden_resolution` to adjust the resolution used for sub-cluster identification. Higher values result in more and smaller sub-clusters, while lower values yield fewer and larger sub-clusters. For large datasets (e.g., hundreds of thousands of cells), it is recommended to use smaller weights (e.g. 0.6).
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
  
  # Setting the args
  
  hvg_selection.method <- match.arg(hvg_selection.method)
  leiden_obj_function <- match.arg(leiden_obj_function)
  
  
  #________________________________________
  
  # Checking arguments
  
  cluster_labels_missing <- missing(cluster_labels)
  
  #________________________________________
  
  # Setting the seed
  set.seed(seed)
  
  # Start of function
  if(verbose) {
    cli::cli_rule(left = cli::style_italic(cli::style_bold("Starting MarkoClust!")), right = cli::col_silver(Sys.time()))
  }
  
  cli::cli_h1("Identification of Major Cluster Markers")
  
  log_h2("Preparing the input data")
  
  log_progress_step("Inspecting the input data")
  
  if((is.null(so) & is.null(data)) | (!is.null(so) & !is.null(data))) {
    cli::cli_abort("Either 'data' or 'so' should be specified, not both or neither.")
  }
  
  if(cluster_labels_missing) {
    cli::cli_abort("The cluster_labels cannot be left unspecified!")
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
    } else if(subset_to_HVG) {
      if(length(grep(norm_assay, Seurat::Assays(so))) != 1) {
        cli::cli_abort("The specified 'norm_assay' name is not among the list of assays of the Seurat object.\n\nYou can check the list of available assays using the command Seurat::Assays(so)")
      } else if(nrow(so[[norm_assay]][norm_layer]) == 0) {
        cli::cli_abort("The specified 'norm_layer' name is not among the list of layers of the specified 'norm_assay' of the Seurat object.")
      }
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
    if(subset_to_HVG) {
      if(!inherits(so[[norm_assay]][norm_layer], c("matrix", "Matrix"))) {
        norm_expr_mat <- as.matrix(so[[norm_assay]][norm_layer])
      } else {
        norm_expr_mat <- so[[norm_assay]][norm_layer]
      }
    }
  }
  
  if(any(is.null(colnames(expr_mat))) | any(is.null(rownames(expr_mat)))) {
    cli::cli_abort("All rows and columns of the input data should have (unique) names.")
  }
  
  # Input clusters
  if(is.null(so)) {
    major_clusters <- cluster_labels %>% as.character() %>% setNames(colnames(expr_mat))
  } else {
    major_clusters <- so[[cluster_labels]] %>% 
      unlist() %>% as.vector() %>% as.character() %>% stats::setNames(colnames(expr_mat))
  }
  
  major_clusters[is.na(major_clusters)] <- "NA"
  
  if(any(expr_mat < 0)) {
    cli::cli_abort("Input data contains negative values. The pipeline requires non-negative input (raw or log-normalized counts).")
  }
  
  if(remove_quiescent_cells) {
    major_cluster_quiescent_cells <- major_clusters[major_clusters == "Quiescent"] %>% names()
    if(length(major_cluster_quiescent_cells) > 0) {
      
      # Updating the expr_mat
      expr_mat <- expr_mat[,-which(colnames(expr_mat) %in% major_cluster_quiescent_cells)]
      
      # Update the major_clusters
      major_clusters <- major_clusters[-match(major_cluster_quiescent_cells, names(major_clusters))]
    }
  }
  
  if(verbose) {
    cli::cli_status_update(qc_status_id, "Removing noise genes...")
  }
  
  # Removing noise genes
  expr_mat <- expr_mat[Matrix::rowSums(expr_mat > 0) > noise_feature_thresh,]
  
  # Setting the default mr_thresh
  original_mr_thresh <- mr_thresh
  if(is.null(original_mr_thresh)) {
    mr_thresh <- sqrt(ncol(expr_mat))
  }
  
  if(subset_to_HVG) {
    
    log_progress_step("Finding highly variable genes (HVGs)...")
    
    # Detection of highly variable genes (HVGs)
    if(is.null(so)) {
      base::suppressWarnings(
        hvgs <- Seurat::FindVariableFeatures(expr_mat, selection.method = hvg_selection.method, nfeatures = nrow(expr_mat), verbose = FALSE)
      )
      hvgs <- hvgs %>% dplyr::filter(.data[[grep("variance.standardized", colnames(.), value = TRUE)]] >= hvg_var_thresh)
      hvgs <- rownames(hvgs)
    } else {
      
      # Updating the norm_expr_mat
      norm_expr_mat <- norm_expr_mat[rownames(expr_mat),]
      base::suppressWarnings(
        hvgs <- Seurat::FindVariableFeatures(norm_expr_mat, selection.method = hvg_selection.method, nfeatures = nrow(norm_expr_mat), verbose = FALSE)
      )
      hvgs <- hvgs %>% dplyr::filter(.data[[grep("variance.standardized", colnames(.), value = TRUE)]] >= hvg_var_thresh)
      hvgs <- rownames(hvgs)
    }
    
    log_progress_done()
    
    ## Filtering the expr_mat to include only the hvgs
    expr_mat <- expr_mat[hvgs,] 
  }
  
  # Step 1: Log1p transformation and converting zeros to NA
  if(log1p) {
    expr_mat <- log1p(expr_mat)
  }
  
  if (!inherits(expr_mat, "Matrix")) {
    expr_mat <- Matrix::Matrix(expr_mat, sparse = TRUE)
  }
  
  #_____________________
  
  # Rank-based feature filtration
  
  if(verbose) {
    feature_filter_status <- cli::cli_status("Feature filtration...")
  }
  
  if(verbose) {
    cli::cli_status_update(feature_filter_status, "Rank-based feature filtration...")
  }
  
  global_rank_flt <- gini.rank.fs(expr_mat, gini_thresh = gini_thresh, num_threads = num_threads)
  
  # Set the globally pure features
  if(length(global_rank_flt$non_specific_features) > 0) {
    globally_pure_ranked <- global_rank_flt$non_specific_features
  } else {
    globally_pure_ranked <- NULL
  }
  
  # Filter the data based on global_rank_flt
  expr_mat <- expr_mat[global_rank_flt$specific_features, ]
  
  #_____________________
  
  # Defining the expression-weighted centered scaled rank thresholds
  
  log_progress_step("EWCSR normalizing cells")
  
  # Global expression-weighted centered scaled rank data
  ewcsr_mat <- ewcsr.sparse(expr_mat)
  
  ewcsr_vec <- matrix_to_vector_na_omit(ewcsr_mat)
  ewcsr_vec_pos <- ewcsr_vec[ewcsr_vec > 0] %>% sort()
  ewcsr_vec_neg <- ewcsr_vec[ewcsr_vec < 0] %>% sort() %>% rev()
  
  ewcsr_high_thresh <- quantile(ewcsr_vec_pos, probs = high_quantile)
  ewcsr_low_thresh <- quantile(ewcsr_vec_neg, probs = low_quantile)
  
  log_progress_done()
  
  #_____________________
  
  # EWCSR-based feature filtration
  
  if(verbose) {
    cli::cli_status_update(feature_filter_status, "EWCSR-based feature filtration...")
  }
  
  if(verbose) {
    cli::cli_status_update(feature_filter_status, "Feature filtration based on highly positive EWCSRs...")
  }
  
  ## For high expression-weighted centered scaled ranks
  global_ewcsr_high_flt <- gini.ewcsr.fs(expr_mat, gini_thresh = gini_thresh, ewcsr_high_thresh = ewcsr_high_thresh, num_threads = num_threads)
  
  if(verbose) {
    cli::cli_status_update(feature_filter_status, "Feature filtration based on medium EWCSRs...")
  }
  
  ## For medium expression-weighted centered scaled ranks (for the medium markers we divide the upper and lower thresholds by 2)
  global_ewcsr_medium_flt <- gini.ewcsr.fs(expr_mat, gini_thresh = gini_thresh, ewcsr_high_thresh = ewcsr_low_thresh/2, ewcsr_low_thresh = ewcsr_high_thresh/2, num_threads = num_threads)
  
  ### Filter the data based on specific features
  global_combined_specific_features <- unique(c(global_ewcsr_high_flt$specific_features, 
                                                global_ewcsr_medium_flt$specific_features
  ))
  
  # Set the globally pure features
  if(length(global_ewcsr_high_flt$non_specific_features) > 0) {
    globally_pure_high <- global_ewcsr_high_flt$non_specific_features
  } else {
    globally_pure_high <- NULL
  }
  
  if(length(global_ewcsr_medium_flt$non_specific_features) > 0) {
    globally_pure_medium <- global_ewcsr_medium_flt$non_specific_features
  } else {
    globally_pure_medium <- NULL
  }
  
  expr_mat <- expr_mat[global_combined_specific_features, ]
  
  #_____________________
  
  # Updating the expression-weighted centered scaled rank thresholds
  
  if(verbose) {
    cli::cli_status_update(feature_filter_status, "Updating the EWCSR thresholds...")
  }
  
  # Update the ewcsr_mat
  ewcsr_mat <- ewcsr.sparse(expr_mat)
  
  ewcsr_vec <- matrix_to_vector_na_omit(ewcsr_mat)
  ewcsr_vec_pos <- ewcsr_vec[ewcsr_vec > 0] %>% sort()
  ewcsr_vec_neg <- ewcsr_vec[ewcsr_vec < 0] %>% sort() %>% rev()
  
  ewcsr_high_thresh <- quantile(ewcsr_vec_pos, probs = high_quantile)
  ewcsr_low_thresh <- quantile(ewcsr_vec_neg, probs = low_quantile)
  
  ewcsr_mat <- Matrix::Matrix(ewcsr_mat,sparse=TRUE)
  
  if(verbose) {
    cli::cli_status_clear(feature_filter_status)
  }
  
  if(verbose) {
    cli::cli_alert_success("Feature filtration")
  }
  
  #_____________________________________
  
  # Generating the final global matrices
  
  log_progress_step("Generating EWCSR-based logical matrices")
  
  ## pos_mat
  # pos_mat <- ewcsr_mat > ewcsr_high_thresh
  # pos_mat[is.na(pos_mat)] <- FALSE
  pos_mat <- sparse_compare_threshold(mat = ewcsr_mat, op = ">", threshold = ewcsr_high_thresh, zero_to_false = TRUE)
  
  # Detecting random markers and removing them (this can positively impact on the similarity scores)
  global_pos_random_markers <- which(Matrix::rowSums(pos_mat, na.rm = TRUE) < random_marker_thresh)
  if(length(global_pos_random_markers) > 0) {
    pos_mat <- pos_mat[-global_pos_random_markers,]
  }
  
  if(nrow(pos_mat) < 10) {
    cli::cli_abort("The selected high_quantile is too low and less than 10 features met the selected arguments! You may consider increasing the high_quantile.")
  }
  
  ### Detecting and removing quiescent cells 
  quiescent_cells <- colnames(pos_mat)[which(Matrix::colSums(pos_mat) == 0)]
  
  if(remove_quiescent_cells) {
    
    if(length(quiescent_cells) > 0) {
      
      # Updating the pos_mat
      pos_mat <- pos_mat[,-which(colnames(pos_mat) %in% quiescent_cells)]
      
      # Updating the ewcsr_mat
      ewcsr_mat <- ewcsr_mat[,-which(colnames(ewcsr_mat) %in% quiescent_cells)]
      
      # Update the major_clusters
      major_clusters <- major_clusters[-match(quiescent_cells, names(major_clusters))]
    }
    quiescent_cells <- unique(c(quiescent_cells, major_cluster_quiescent_cells))
  }
  
  # Removing to reduce memory occupied.
  rm(expr_mat)
  
  #_______________
  
  ## neg_mat
  # neg_mat <- ewcsr_mat < ewcsr_low_thresh
  # neg_mat[is.na(neg_mat)] <- FALSE
  neg_mat <- sparse_compare_threshold(mat = ewcsr_mat, op = "<", threshold = ewcsr_low_thresh, zero_to_false = TRUE)
  
  global_neg_random_markers <- which(Matrix::rowSums(neg_mat, na.rm = TRUE) < random_marker_thresh)
  if(length(global_neg_random_markers) > 0) {
    neg_mat <- neg_mat[-global_neg_random_markers,]
  }
  
  if(nrow(neg_mat) < 10) {
    cli::cli_abort("The selected low_quantile is too high and less than 10 features met the selected arguments! You may consider decreasing the low_quantile.")
  }
  
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
  
  # Detecting random markers and removing them (this can positively impact on the similarity scores)
  global_med_random_markers <- which(Matrix::rowSums(med_mat, na.rm = TRUE) < random_marker_thresh)
  if(length(global_med_random_markers) > 0) {
    med_mat <- med_mat[-global_med_random_markers,]
  }
  
  ### Detecting and removing no_med_cells cells 
  no_med_cells <- colnames(med_mat)[which(Matrix::colSums(med_mat) == 0)]
  if(length(no_med_cells) > 0) {
    
    # Updating the med_mat
    med_mat <- med_mat[,-which(colnames(med_mat) %in% no_med_cells)]
    
  }
  
  log_progress_done()
  
  #_____________________________________
  
  # Detecting specific markers of major clusters
  
  # Filtering features at the cluster pseudobulk level
  
  log_progress_step("Detecting cluster markers")
  
  major_cluster_ids <- sort(unique(major_clusters))
  
  ## For each cluster, calculate pos marker frequency (fraction of cells where gene is TRUE)
  
  if(verbose) {
    major_cluster_marker_status <- cli::cli_status("Finding positive markers of clusters...")
  }
  
  ### Ensure only clustered cells are used
  pos_mat_major_clustered <- pos_mat[, which(colnames(pos_mat) %in% names(major_clusters)), drop = FALSE]
  
  if(length(major_cluster_ids) > 1) {
    
    pos_marker_freq_mat <- sapply(major_cluster_ids, function(cid) {
      cells <- names(major_clusters)[major_clusters == cid] %>% na.omit()
      cells <- cells[cells %in% colnames(pos_mat_major_clustered)]
      
      if (length(cells) == 1) {
        # Avoid single-column matrix error
        as.numeric(pos_mat_major_clustered[, cells])
      } else {
        Matrix::rowMeans(pos_mat_major_clustered[, cells, drop = FALSE])
      }
    })
    
    if(inherits(pos_marker_freq_mat, "matrix")) {
      
      colnames(pos_marker_freq_mat) <- paste0("Cluster_", major_cluster_ids)
      rownames(pos_marker_freq_mat) <- rownames(pos_mat_major_clustered)
      
      ### Calculaing gini scores
      # pos_cluster_gini_scores <- apply(pos_marker_freq_mat, 1, function(x) ineq::Gini(x))
      pos_cluster_gini_scores <- gini_rows_freq_mat(pos_marker_freq_mat)
      pos_clusters_specific_features <- names(pos_cluster_gini_scores)[pos_cluster_gini_scores >= gini_thresh/2] %>% na.omit()
      pos_clusters_non_specific_features <- pos_cluster_gini_scores[pos_cluster_gini_scores < gini_thresh] %>% na.omit()
      pos_clusters_non_specific_features <- data.frame(Feature = names(pos_clusters_non_specific_features), 
                                                       Gini_Score = pos_clusters_non_specific_features,
                                                       Rank = data.table::frankv(pos_clusters_non_specific_features, ties.method="dense")) %>% dplyr::arrange(Rank) %>% 
        dplyr::filter(!is.na(Gini_Score))
      rownames(pos_clusters_non_specific_features) <- NULL
      
      ### Filtering the pos_mat_major_clustered
      pos_mat_major_clustered <- pos_mat_major_clustered[pos_clusters_specific_features, , drop = FALSE]
      pos_marker_freq_mat <- pos_marker_freq_mat[pos_clusters_specific_features, , drop = FALSE]
      
    } else {
      pos_mat_major_clustered <- data.frame()
      pos_marker_freq_mat <- data.frame()
      pos_clusters_non_specific_features <- NULL
    }
    
  } else {
    pos_clusters_non_specific_features <- NULL
  }
  
  #______
  
  # Get Top Markers per Major Cluster
  
  ## pos markers
  if(nrow(pos_mat_major_clustered) > 0) {
    major_cluster_pos_markers <- lapply(major_cluster_ids, function(cid) {
      cells <- names(major_clusters)[major_clusters == cid] %>% na.omit()
      cells <- cells[cells %in% colnames(pos_mat_major_clustered)]
      
      curr_pos_mat <- pos_mat_major_clustered[, cells, drop = FALSE]
      
      if(ncol(curr_pos_mat) > 1) {
        # curr_cluster_gini_scores <- apply(curr_pos_mat, 1, function(x) ineq::Gini(x))
        curr_cluster_gini_scores <- gini_rows_lg_marker_matrix(curr_pos_mat@i, curr_pos_mat@p, curr_pos_mat@x, 
                                                               nrow(curr_pos_mat), ncol(curr_pos_mat), 
                                                               curr_pos_mat@Dimnames[[1]])
        
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
  
  ## For each cluster, calculate neg marker frequency (fraction of cells where gene is TRUE)
  
  if(verbose) {
    cli::cli_status_update(major_cluster_marker_status, "Finding negative markers of clusters...")
  }
  
  ### Ensure only clustered cells are used
  neg_mat_major_clustered <- neg_mat[, which(colnames(neg_mat) %in% names(major_clusters)), drop = FALSE]
  
  if(length(major_cluster_ids) > 1) {
    
    neg_marker_freq_mat <- sapply(major_cluster_ids, function(cid) {
      cells <- names(major_clusters)[major_clusters == cid] %>% na.omit()
      cells <- cells[cells %in% colnames(neg_mat_major_clustered)]
      
      if (length(cells) == 1) {
        # Avoid single-column matrix error
        as.numeric(neg_mat_major_clustered[, cells])
      } else {
        Matrix::rowMeans(neg_mat_major_clustered[, cells, drop = FALSE])
      }
    })
    
    if(inherits(neg_marker_freq_mat, "matrix")) {
      
      colnames(neg_marker_freq_mat) <- paste0("Cluster_", major_cluster_ids)
      rownames(neg_marker_freq_mat) <- rownames(neg_mat_major_clustered)
      
      ### Calculaing gini scores
      # neg_cluster_gini_scores <- apply(neg_marker_freq_mat, 1, function(x) ineq::Gini(x))
      neg_cluster_gini_scores <- gini_rows_freq_mat(neg_marker_freq_mat)
      neg_clusters_specific_features <- names(neg_cluster_gini_scores)[neg_cluster_gini_scores >= gini_thresh/2] %>% na.omit()
      neg_clusters_non_specific_features <- neg_cluster_gini_scores[neg_cluster_gini_scores < gini_thresh] %>% na.omit()
      neg_clusters_non_specific_features <- data.frame(Feature = names(neg_clusters_non_specific_features), 
                                                       Gini_Score = neg_clusters_non_specific_features,
                                                       Rank = data.table::frankv(neg_clusters_non_specific_features, ties.method="dense")) %>% dplyr::arrange(Rank) %>% 
        dplyr::filter(!is.na(Gini_Score))
      rownames(neg_clusters_non_specific_features) <- NULL
      
      ### Filtering the neg_mat_major_clustered
      neg_mat_major_clustered <- neg_mat_major_clustered[neg_clusters_specific_features, , drop = FALSE]
      neg_marker_freq_mat <- neg_marker_freq_mat[neg_clusters_specific_features, , drop = FALSE]
      
    } else {
      neg_mat_major_clustered <- data.frame()
      neg_marker_freq_mat <- data.frame()
      neg_clusters_non_specific_features <- NULL
    }
    
  } else {
    neg_clusters_non_specific_features <- NULL
  }
  
  #______
  
  # Get Top Markers per Major Cluster
  
  ## neg markers
  if(nrow(neg_mat_major_clustered) > 0) {
    major_cluster_neg_markers <- lapply(major_cluster_ids, function(cid) {
      cells <- names(major_clusters)[major_clusters == cid] %>% na.omit()
      cells <- cells[cells %in% colnames(neg_mat_major_clustered)]
      
      curr_neg_mat <- neg_mat_major_clustered[, cells, drop = FALSE]
      
      if(ncol(curr_neg_mat) > 1) {
        # curr_cluster_gini_scores <- apply(curr_neg_mat, 1, function(x) ineq::Gini(x))
        curr_cluster_gini_scores <- gini_rows_lg_marker_matrix(curr_neg_mat@i, curr_neg_mat@p, curr_neg_mat@x, 
                                                               nrow(curr_neg_mat), ncol(curr_neg_mat), 
                                                               curr_neg_mat@Dimnames[[1]])
        
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
                                               Rank = data.table::frankv(curr_cluster_gini_scores, order = 1, ties.method="dense")) %>% dplyr::arrange(Rank)
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
  
  ## For each cluster, calculate med marker frequency (fraction of cells where gene is TRUE)
  
  if(verbose) {
    cli::cli_status_update(major_cluster_marker_status, "Finding medium markers of clusters...")
  }
  
  ### Ensure only clustered cells are used
  med_mat_major_clustered <- med_mat[, which(colnames(med_mat) %in% names(major_clusters)), drop = FALSE]
  
  if(length(major_cluster_ids) > 1) {
    
    med_marker_freq_mat <- sapply(major_cluster_ids, function(cid) {
      cells <- names(major_clusters)[major_clusters == cid] %>% na.omit()
      cells <- cells[cells %in% colnames(med_mat_major_clustered)]
      
      if (length(cells) == 1) {
        # Avoid single-column matrix error
        as.numeric(med_mat_major_clustered[, cells])
      } else {
        Matrix::rowMeans(med_mat_major_clustered[, cells, drop = FALSE])
      }
    })
    
    if(inherits(med_marker_freq_mat, "matrix")) {
      
      colnames(med_marker_freq_mat) <- paste0("Cluster_", major_cluster_ids)
      rownames(med_marker_freq_mat) <- rownames(med_mat_major_clustered)
      
      ### Calculaing gini scores
      # med_cluster_gini_scores <- apply(med_marker_freq_mat, 1, function(x) ineq::Gini(x))
      med_cluster_gini_scores <- gini_rows_freq_mat(med_marker_freq_mat)
      med_clusters_specific_features <- names(med_cluster_gini_scores)[med_cluster_gini_scores >= gini_thresh/2] %>% na.omit()
      med_clusters_non_specific_features <- med_cluster_gini_scores[med_cluster_gini_scores < gini_thresh] %>% na.omit()
      med_clusters_non_specific_features <- data.frame(Feature = names(med_clusters_non_specific_features), 
                                                       Gini_Score = med_clusters_non_specific_features,
                                                       Rank = data.table::frankv(med_clusters_non_specific_features, ties.method="dense")) %>% dplyr::arrange(Rank) %>% 
        dplyr::filter(!is.na(Gini_Score))
      rownames(med_clusters_non_specific_features) <- NULL
      
      ### Filtering the med_mat_major_clustered
      med_mat_major_clustered <- med_mat_major_clustered[med_clusters_specific_features, , drop = FALSE]
      med_marker_freq_mat <- med_marker_freq_mat[med_clusters_specific_features, , drop = FALSE]
      
    } else {
      med_mat_major_clustered <- data.frame()
      med_marker_freq_mat <- data.frame()
      med_clusters_non_specific_features <- NULL
    }
    
  } else {
    med_clusters_non_specific_features <- NULL
  }
  
  #______
  
  # Get Top Markers per Major Cluster
  
  ## med markers
  if(nrow(med_mat_major_clustered) > 0) {
    major_cluster_med_markers <- lapply(major_cluster_ids, function(cid) {
      cells <- names(major_clusters)[major_clusters == cid] %>% na.omit()
      cells <- cells[cells %in% colnames(med_mat_major_clustered)]
      
      curr_med_mat <- med_mat_major_clustered[, cells, drop = FALSE]
      
      if(ncol(curr_med_mat) > 1) {
        # curr_cluster_gini_scores <- apply(curr_med_mat, 1, function(x) ineq::Gini(x))
        curr_cluster_gini_scores <- gini_rows_lg_marker_matrix(curr_med_mat@i, curr_med_mat@p, curr_med_mat@x, 
                                                               nrow(curr_med_mat), ncol(curr_med_mat), 
                                                               curr_med_mat@Dimnames[[1]])
        
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
  #   if ("curr_pos_markers" %in% names(major_cluster_marker_dfs)) {
  #     curr_pos_markers <- major_cluster_marker_dfs[["curr_pos_markers"]]
  #     if(grepl("Purity", colnames(curr_pos_markers)) %>% any()) {
  #       curr_pos_markers <- curr_pos_markers %>% 
  #         dplyr::mutate(Rank = data.table::frankv(Purity, order = -1, ties.method="dense")) %>% 
  #         dplyr::arrange(Rank)
  #     } else if(grepl("EWCSR", colnames(curr_pos_markers)) %>% any()) {
  #       curr_pos_markers <- curr_pos_markers %>% 
  #         dplyr::mutate(Rank = data.table::frankv(EWCSR, order = -1, ties.method="dense")) %>% 
  #         dplyr::arrange(Rank)
  #     }
  #   }
  #   
  #   if ("curr_neg_markers" %in% names(major_cluster_marker_dfs)) {
  #     curr_neg_markers <- major_cluster_marker_dfs[["curr_neg_markers"]]
  #     if(grepl("Purity", colnames(curr_neg_markers)) %>% any()) {
  #       curr_neg_markers <- curr_neg_markers %>% 
  #         dplyr::mutate(Rank = data.table::frankv(Purity, order = -1, ties.method="dense")) %>% 
  #         dplyr::arrange(Rank)
  #     } else if(grepl("EWCSR", colnames(curr_neg_markers)) %>% any()) {
  #       curr_neg_markers <- curr_neg_markers %>% 
  #         dplyr::mutate(Rank = data.table::frankv(EWCSR, order = -1, ties.method="dense")) %>% 
  #         dplyr::arrange(Rank)
  #     }
  #   }
  #   
  #   if ("curr_med_markers" %in% names(major_cluster_marker_dfs)) {
  #     curr_med_markers <- major_cluster_marker_dfs[["curr_med_markers"]]
  #     if(grepl("Purity", colnames(curr_med_markers)) %>% any()) {
  #       curr_med_markers <- curr_med_markers %>% 
  #         dplyr::mutate(Rank = data.table::frankv(Purity, order = -1, ties.method="dense")) %>% 
  #         dplyr::arrange(Rank)
  #     } else if(grepl("EWCSR", colnames(curr_med_markers)) %>% any()) {
  #       curr_med_markers <- curr_med_markers %>% 
  #         dplyr::mutate(Rank = data.table::frankv(EWCSR, order = -1, ties.method="dense")) %>% 
  #         dplyr::arrange(Rank)
  #     }
  #   }
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
  
  log_progress_done()
  
  if(verbose) {
    cli::cli_status_clear(major_cluster_marker_status)
  }
  
  #________________________________________
  
  # Detecting sub-clusters
  
  if(identify_subclusters) {
    
    log_space()
    
    log_h1("Identification of Sub-clusters")
    
    log_progress_step("Detecting sub-clusters for each major cluster")
    
    subcluster_list <- lapply(major_cluster_ids, function(cid) {
      
      # Subset the ewcsr_mat
      tmp_ewcsr_mat <- ewcsr_mat[,names(major_clusters)[major_clusters == cid]]
      
      ## Removing unexpressed genes
      tmp_ewcsr_mat <- tmp_ewcsr_mat[Matrix::rowSums(tmp_ewcsr_mat, na.rm = TRUE) != 0,]
      
      tmp_ewcsr_mat <- Matrix::Matrix(tmp_ewcsr_mat,sparse=TRUE)
      
      #_____________
      
      # Define other matrices
      ## pos_mat
      # pos_mat <- tmp_ewcsr_mat > ewcsr_high_thresh
      # pos_mat[is.na(pos_mat)] <- FALSE
      pos_mat <- sparse_compare_threshold(mat = tmp_ewcsr_mat, op = ">", threshold = ewcsr_high_thresh, zero_to_false = TRUE)
      
      ## neg_mat
      # neg_mat <- tmp_ewcsr_mat < ewcsr_low_thresh
      # neg_mat[is.na(neg_mat)] <- FALSE
      neg_mat <- sparse_compare_threshold(mat = tmp_ewcsr_mat, op = "<", threshold = ewcsr_low_thresh, zero_to_false = TRUE)
      
      ## med_mat
      # med_mat <- tmp_ewcsr_mat > ewcsr_low_thresh & tmp_ewcsr_mat < ewcsr_high_thresh # We DO NOT use half of the thresholds and use them as they are for defining the med markers for subclustering
      # med_mat[is.na(med_mat)] <- FALSE
      med_mat <- sparse_between_thresholds(mat = tmp_ewcsr_mat, 
                                           lower_threshold = ewcsr_low_thresh, upper_threshold = ewcsr_high_thresh, # We DO NOT use half of the thresholds and use them as they are for defining the med markers for subclustering
                                           zero_to_false = TRUE  # zero_to_false = TRUE
      )
      
      #_____________
      
      # Calculating and weighting Jaccard similarities
      sim_pos   <- jaccard.sparse(pos_mat, num_threads = num_threads)
      
      sim_med   <- jaccard.sparse(med_mat, num_threads = num_threads)
      
      #__________________________
      
      # Graph construction
      
      ## Resetting the default mr_thresh
      if(is.null(original_mr_thresh)) {
        mr_thresh <- sqrt(ncol(tmp_ewcsr_mat))
      } else {
        mr_thresh <- mr_thresh/2
      }
      
      ## For pos markers
      
      if(!is.null(sim_pos)) {
        # diag(sim_pos) <- -Inf (already done in the jaccard sim function)
        mr_pos <- mutual.rank(sim_pos, num_threads = num_threads)
        # pos_adj_mat <- mr_pos <= mr_thresh
        pos_adj_mat <- sparse_compare_threshold(mat = mr_pos, op = "<=", threshold = mr_thresh, zero_to_false = TRUE)
        # diag(pos_adj_mat) <- FALSE # Not required as the diag has the highest val and will always change to FALSE
        
        if(any(pos_adj_mat)) {
          pos_connected_nodes <- which(Matrix::colSums(pos_adj_mat) > 0)
          pos_adj_mat_trimmed <- pos_adj_mat[pos_connected_nodes, pos_connected_nodes]
          sim_pos_trimmed <- sim_pos[pos_connected_nodes, pos_connected_nodes]
          pos_cell_names <- colnames(pos_adj_mat_trimmed)  # same as rownames
          pos_edge_list <- which(pos_adj_mat_trimmed, arr.ind = TRUE)
          pos_edge_list <- pos_edge_list[pos_edge_list[,1] < pos_edge_list[,2], , drop = FALSE]
          colnames(pos_edge_list) <- rownames(pos_edge_list) <- NULL
          pos_edge_weights <- sim_pos_trimmed[pos_edge_list]
          
          g_pos <- igraph::graph_from_edgelist(pos_edge_list, directed = FALSE)
          igraph::V(g_pos)$name <- pos_cell_names
          igraph::E(g_pos)$weight <- pos_edge_weights
        } else {
          g_pos <- NULL
        }
      } else {
        g_pos <- NULL
      }
      
      #__________
      
      ## For med markers
      
      if(!is.null(sim_med)) {
        # diag(sim_med) <- -Inf (already done in the jaccard sim function)
        mr_med <- mutual.rank(sim_med, num_threads = num_threads)
        # med_adj_mat <- mr_med <= mr_thresh
        med_adj_mat <- sparse_compare_threshold(mat = mr_med, op = "<=", threshold = mr_thresh, zero_to_false = TRUE)
        # diag(med_adj_mat) <- FALSE # Not required as the diag has the highest val and will always change to FALSE
        
        if(any(med_adj_mat)) {
          med_connected_nodes <- which(Matrix::colSums(med_adj_mat) > 0)
          med_adj_mat_trimmed <- med_adj_mat[med_connected_nodes, med_connected_nodes]
          sim_med_trimmed <- sim_med[med_connected_nodes, med_connected_nodes]
          med_cell_names <- colnames(med_adj_mat_trimmed)  # same as rownames
          med_edge_list <- which(med_adj_mat_trimmed, arr.ind = TRUE)
          med_edge_list <- med_edge_list[med_edge_list[,1] < med_edge_list[,2], , drop = FALSE]
          colnames(med_edge_list) <- rownames(med_edge_list) <- NULL
          med_edge_weights <- sim_med_trimmed[med_edge_list]
          
          g_med <- igraph::graph_from_edgelist(med_edge_list, directed = FALSE)
          igraph::V(g_med)$name <- med_cell_names
          igraph::E(g_med)$weight <- med_edge_weights
        } else {
          g_med <- NULL
        }
      } else {
        g_med <- NULL
      }
      
      #__________
      
      # Union graph
      if(!is.null(g_pos) & !is.null(g_med)) {
        g_union <- igraph::union(g_pos, g_med)
      } else if(!is.null(g_pos)) {
        g_union <- g_pos
      } else if(!is.null(g_med)) {
        g_union <- g_med
      } else {
        g_union <- NULL
      }
      
      if(!is.null(g_union)) {
        
        g_union <- igraph::simplify(g_union, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = list(weight = "sum"))
        
        # Identify sub-clusters
        
        # Leiden clustering
        leiden_sub_clusters <- igraph::cluster_leiden(
          graph = g_union,
          objective_function = leiden_obj_function,
          weights = igraph::E(g_union)$weight,
          resolution = leiden_resolution*subcluster_resolution_weight,
          beta = 0.01,
          n_iterations = leiden_n_iterations
        )
        
        sub_clusters <- leiden_sub_clusters$membership
        sub_clusters <- paste("Sub", sub_clusters, sep = "")
        names(sub_clusters) <- leiden_sub_clusters$names
        
        # Identifying non-clusterable/isolated cells
        if(length(sub_clusters) < ncol(tmp_ewcsr_mat)) {
          sub_clust_isolated_cells <- colnames(tmp_ewcsr_mat)[-which(colnames(tmp_ewcsr_mat) %in% names(sub_clusters))]
          sub_clust_isolated_cells_values <- rep("Isolated", length(sub_clust_isolated_cells))
          names(sub_clust_isolated_cells_values) <- sub_clust_isolated_cells
          sub_clusters <- c(sub_clusters, sub_clust_isolated_cells_values)
        }
        
      } else {
        sub_clusters <- rep("Sub1", ncol(tmp_ewcsr_mat))
        names(sub_clusters) <- colnames(tmp_ewcsr_mat)
      }
      
      return(sub_clusters)
      
    })
    
    names(subcluster_list) <- major_cluster_ids
    
    log_progress_done()
    
    ## Merging the sub-clusters
    merged_sub_clusters <- sapply(names(subcluster_list), function(i) {
      tmp_sub_clust <- subcluster_list[[i]]
      final_sub_clust <- paste(i, tmp_sub_clust, sep = "-")
      names(final_sub_clust) <- names(tmp_sub_clust)
      final_sub_clust
    })
    
    merged_sub_clusters <- merged_sub_clusters %>% unname() %>% unlist()
    merged_sub_clusters <- merged_sub_clusters[colnames(ewcsr_mat)]
    
    # Updating the names of subcluster_list
    names(subcluster_list) <- paste(major_cluster_ids, "-Subclusters", sep = "")
    
    #___________________________________
    
    # Detecting specific markers of sub clusters
    
    log_progress_step("Detecting specific markers of sub clusters")
    
    subcluster_markers_list <- lapply(subcluster_list, function(i) {
      
      sub_clusters <- i
      
      # Filtering features at the cluster pseudobulk level
      
      sub_cluster_ids <- sort(unique(sub_clusters))
      
      if(length(sub_cluster_ids) > 1) {
        
        ### Ensure only clustered cells are used
        pos_mat_sub_clustered <- pos_mat[, which(colnames(pos_mat) %in% names(sub_clusters)), drop = FALSE]
        
        #______
        
        # Get Top Markers per sub Cluster
        
        ## pos markers
        if(nrow(pos_mat_sub_clustered) > 0) {
          sub_cluster_pos_markers <- lapply(sub_cluster_ids, function(cid) {
            cells <- names(sub_clusters)[sub_clusters == cid] %>% na.omit()
            cells <- cells[cells %in% colnames(pos_mat_sub_clustered)]
            
            curr_pos_mat <- pos_mat_sub_clustered[, cells, drop = FALSE]
            
            if(ncol(curr_pos_mat) > 1) {
              # curr_cluster_gini_scores <- apply(curr_pos_mat, 1, function(x) ineq::Gini(x))
              curr_cluster_gini_scores <- gini_rows_lg_marker_matrix(curr_pos_mat@i, curr_pos_mat@p, curr_pos_mat@x, 
                                                                     nrow(curr_pos_mat), ncol(curr_pos_mat), 
                                                                     curr_pos_mat@Dimnames[[1]])
              
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
          sub_cluster_pos_markers <- lapply(sub_cluster_ids, function(cid) {
            return(base::structure("❗ No specific marker was identified!", class = "logMessage"))
          })
        }
        
        names(sub_cluster_pos_markers) <- sub_cluster_ids
        
        #____________________
        
        ### Ensure only clustered cells are used
        neg_mat_sub_clustered <- neg_mat[, which(colnames(neg_mat) %in% names(sub_clusters)), drop = FALSE]
        
        #______
        
        # Get Top Markers per sub Cluster
        
        ## neg markers
        if(nrow(neg_mat_sub_clustered) > 0) {
          sub_cluster_neg_markers <- lapply(sub_cluster_ids, function(cid) {
            cells <- names(sub_clusters)[sub_clusters == cid] %>% na.omit()
            cells <- cells[cells %in% colnames(neg_mat_sub_clustered)]
            
            curr_neg_mat <- neg_mat_sub_clustered[, cells, drop = FALSE]
            
            if(ncol(curr_neg_mat) > 1) {
              # curr_cluster_gini_scores <- apply(curr_neg_mat, 1, function(x) ineq::Gini(x)) %>% na.omit()
              curr_cluster_gini_scores <- gini_rows_lg_marker_matrix(curr_neg_mat@i, curr_neg_mat@p, curr_neg_mat@x, 
                                                                     nrow(curr_neg_mat), ncol(curr_neg_mat), 
                                                                     curr_neg_mat@Dimnames[[1]])
              
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
                                                     Rank = data.table::frankv(curr_cluster_gini_scores, order = 1, ties.method="dense")) %>% dplyr::arrange(Rank)
              rownames(curr_cluster_gini_scores) <- NULL
              curr_cluster_gini_scores
            } else {
              return(base::structure("❗ No specific marker was identified!", class = "logMessage"))
            }
          })
        } else {
          sub_cluster_neg_markers <- lapply(sub_cluster_ids, function(cid) {
            return(base::structure("❗ No specific marker was identified!", class = "logMessage"))
          })
        }
        
        names(sub_cluster_neg_markers) <- sub_cluster_ids
        
        #____________________
        
        ### Ensure only clustered cells are used
        med_mat_sub_clustered <- med_mat[, which(colnames(med_mat) %in% names(sub_clusters)), drop = FALSE]
        
        #______
        
        # Get Top Markers per sub Cluster
        
        ## med markers
        if(nrow(med_mat_sub_clustered) > 0) {
          sub_cluster_med_markers <- lapply(sub_cluster_ids, function(cid) {
            cells <- names(sub_clusters)[sub_clusters == cid] %>% na.omit()
            cells <- cells[cells %in% colnames(med_mat_sub_clustered)]
            
            curr_med_mat <- med_mat_sub_clustered[, cells, drop = FALSE]
            
            if(ncol(curr_med_mat) > 1) {
              # curr_cluster_gini_scores <- apply(curr_med_mat, 1, function(x) ineq::Gini(x))
              curr_cluster_gini_scores <- gini_rows_lg_marker_matrix(curr_med_mat@i, curr_med_mat@p, curr_med_mat@x, 
                                                                     nrow(curr_med_mat), ncol(curr_med_mat), 
                                                                     curr_med_mat@Dimnames[[1]])
              
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
          sub_cluster_med_markers <- lapply(sub_cluster_ids, function(cid) {
            return(base::structure("❗ No specific marker was identified!", class = "logMessage"))
          })
        }
        
        names(sub_cluster_med_markers) <- sub_cluster_ids
        
        #_____________________________________
        
        # Finalizing the sub_cluster_markers
        
        ## R Version
        # lapply(names(sub_cluster_pos_markers), function(cur_cl) {
        #   
        #   # subsetting the cluster markers
        #   curr_pos_markers <- sub_cluster_pos_markers[[cur_cl]]
        #   curr_neg_markers <- sub_cluster_neg_markers[[cur_cl]]
        #   curr_med_markers <- sub_cluster_med_markers[[cur_cl]]
        #   
        #   # Step 0: Store the original objects in a named list
        #   sub_cluster_markers_list <- list(
        #     curr_pos_markers = curr_pos_markers,
        #     curr_neg_markers = curr_neg_markers,
        #     curr_med_markers = curr_med_markers
        #   )
        #   
        #   # Step 1: Keep only data frames
        #   sub_cluster_marker_dfs <- lapply(sub_cluster_markers_list, function(df) if (is.data.frame(df)) df else NULL)
        #   sub_cluster_marker_dfs <- Filter(Negate(is.null), sub_cluster_marker_dfs)
        #   
        #   # Step 2: Keep only those with a "Purity" column
        #   sub_cluster_with_purity <- sub_cluster_marker_dfs[sapply(sub_cluster_marker_dfs, function(df) "Purity" %in% colnames(df))]
        #   
        #   # Step 3: Create a named list of Features
        #   sub_cluster_features_list <- lapply(sub_cluster_with_purity, function(df) df$Feature)
        #   
        #   # Step 4: Count how many data frames each Feature appears in
        #   sub_cluster_feature_counts <- table(unlist(sub_cluster_features_list))
        #   overlapping_features <- names(sub_cluster_feature_counts[sub_cluster_feature_counts >= 2])
        #   
        #   # Step 5: For each overlapping feature, retain only in the data frame with highest purity
        #   for (feature in overlapping_features) {
        #     # Collect Purity values across data frames
        #     sub_cluster_purity_values <- sapply(names(sub_cluster_with_purity), function(name) {
        #       df <- sub_cluster_with_purity[[name]]
        #       row <- df[df$Feature == feature, ]
        #       if (nrow(row) > 0) return(row$Purity) else return(NA)
        #     })
        #     
        #     # Identify the data frame with highest purity for this feature
        #     if (all(is.na(sub_cluster_purity_values))) next  # skip if all values are NA
        #     sub_cluster_max_purity_name <- names(which.max(sub_cluster_purity_values))
        #     
        #     # Remove the feature from all other data frames
        #     for (name in setdiff(names(sub_cluster_with_purity), sub_cluster_max_purity_name)) {
        #       df <- sub_cluster_with_purity[[name]]
        #       sub_cluster_with_purity[[name]] <- df[df$Feature != feature, ]
        #     }
        #   }
        #   
        #   # Step 6: Merge updated data frames back into the full list
        #   for (name in names(sub_cluster_marker_dfs)) {
        #     if (name %in% names(sub_cluster_with_purity)) {
        #       sub_cluster_marker_dfs[[name]] <- sub_cluster_with_purity[[name]]
        #     }
        #     # else: keep the original version without "Purity" column
        #   }
        #   
        #   # Step 7: Assign updated versions back to original variables
        #   if ("curr_pos_markers" %in% names(sub_cluster_marker_dfs)) {
        #     curr_pos_markers <- sub_cluster_marker_dfs[["curr_pos_markers"]]
        #     if(grepl("Purity", colnames(curr_pos_markers)) %>% any()) {
        #       curr_pos_markers <- curr_pos_markers %>% 
        #         dplyr::mutate(Rank = data.table::frankv(Purity, order = -1, ties.method="dense")) %>% 
        #         dplyr::arrange(Rank)
        #     } else if(grepl("EWCSR", colnames(curr_pos_markers)) %>% any()) {
        #       curr_pos_markers <- curr_pos_markers %>% 
        #         dplyr::mutate(Rank = data.table::frankv(EWCSR, order = -1, ties.method="dense")) %>% 
        #         dplyr::arrange(Rank)
        #     }
        #   }
        #   
        #   if ("curr_neg_markers" %in% names(sub_cluster_marker_dfs)) {
        #     curr_neg_markers <- sub_cluster_marker_dfs[["curr_neg_markers"]]
        #     if(grepl("Purity", colnames(curr_neg_markers)) %>% any()) {
        #       curr_neg_markers <- curr_neg_markers %>% 
        #         dplyr::mutate(Rank = data.table::frankv(Purity, order = -1, ties.method="dense")) %>% 
        #         dplyr::arrange(Rank)
        #     } else if(grepl("EWCSR", colnames(curr_neg_markers)) %>% any()) {
        #       curr_neg_markers <- curr_neg_markers %>% 
        #         dplyr::mutate(Rank = data.table::frankv(EWCSR, order = -1, ties.method="dense")) %>% 
        #         dplyr::arrange(Rank)
        #     }
        #   }
        #   
        #   if ("curr_med_markers" %in% names(sub_cluster_marker_dfs)) {
        #     curr_med_markers <- sub_cluster_marker_dfs[["curr_med_markers"]]
        #     if(grepl("Purity", colnames(curr_med_markers)) %>% any()) {
        #       curr_med_markers <- curr_med_markers %>% 
        #         dplyr::mutate(Rank = data.table::frankv(Purity, order = -1, ties.method="dense")) %>% 
        #         dplyr::arrange(Rank)
        #     } else if(grepl("EWCSR", colnames(curr_med_markers)) %>% any()) {
        #       curr_med_markers <- curr_med_markers %>% 
        #         dplyr::mutate(Rank = data.table::frankv(EWCSR, order = -1, ties.method="dense")) %>% 
        #         dplyr::arrange(Rank)
        #     }
        #   }
        #   
        #   # Updating the cluster markers
        #   sub_cluster_pos_markers[[cur_cl]] <<- curr_pos_markers
        #   if(is.data.frame(sub_cluster_pos_markers[[cur_cl]])) {
        #     if(nrow(sub_cluster_pos_markers[[cur_cl]]) == 0) {
        #       sub_cluster_pos_markers[[cur_cl]] <<- base::structure("❗ No specific marker was identified!", class = "logMessage")
        #     }
        #   }
        #   
        #   sub_cluster_neg_markers[[cur_cl]] <<- curr_neg_markers
        #   if(is.data.frame(sub_cluster_neg_markers[[cur_cl]])) {
        #     if(nrow(sub_cluster_neg_markers[[cur_cl]]) == 0) {
        #       sub_cluster_neg_markers[[cur_cl]] <<- base::structure("❗ No specific marker was identified!", class = "logMessage")
        #     }
        #   }
        #   
        #   sub_cluster_med_markers[[cur_cl]] <<- curr_med_markers
        #   if(is.data.frame(sub_cluster_med_markers[[cur_cl]])) {
        #     if(nrow(sub_cluster_med_markers[[cur_cl]]) == 0) {
        #       sub_cluster_med_markers[[cur_cl]] <<- base::structure("❗ No specific marker was identified!", class = "logMessage")
        #     }
        #   }
        # })
        
        ## C++ version
        cpp_sub_markers_list <- filter_cluster_markers_cpp(pos_markers = sub_cluster_pos_markers, 
                                                           neg_markers = sub_cluster_neg_markers, 
                                                           med_markers = sub_cluster_med_markers)
        
        sub_cluster_pos_markers <- cpp_sub_markers_list$pos
        sub_cluster_neg_markers <- cpp_sub_markers_list$neg
        sub_cluster_med_markers <- cpp_sub_markers_list$med
        
        #____________________
        
        return(list(
          positive_markers = sub_cluster_pos_markers,
          negative_markers = sub_cluster_neg_markers,
          medium_markers = sub_cluster_med_markers
        )
        )
      } else {
        return(base::structure("❗ The corresponding cluster does not contain any sub-clusters!", class = "logMessage"))
      }
    })
    
    log_progress_done()
    
    names(subcluster_markers_list) <- names(subcluster_list)
  }
  
  #__________________________________________________________
  
  # Preparing the Results Lists
  final_clusters_list <- list(major_clusters = major_clusters)
  
  if(identify_subclusters) {
    final_clusters_list$sub_clusters <- subcluster_list
    final_clusters_list$merged_sub_clusters <- merged_sub_clusters
  }
  
  #_______________
  
  final_markers_list <- list(
    major_clusters = list(
      cluster_specific = list(
        positive_markers = major_cluster_pos_markers,
        negative_markers = major_cluster_neg_markers,
        medium_markers = major_cluster_med_markers
      ),
      cross_cluster = list(
        positive_features = pos_clusters_non_specific_features,
        negative_markers = neg_clusters_non_specific_features,
        medium_markers = med_clusters_non_specific_features
      )
    )
  )
  
  if(identify_subclusters) {
    final_markers_list$sub_clusters <- subcluster_markers_list
  }
  
  #_______________
  
  final_results_list <- list(
    clusters = final_clusters_list,
    markers = final_markers_list
  )
  
  if(length(quiescent_cells) > 0) {
    final_results_list$quiescent_cells <- quiescent_cells
  }
  
  if(length(globally_pure_ranked) > 0) {
    final_results_list$globally_pure_ranked <- globally_pure_ranked
  }
  
  if(length(globally_pure_high) > 0) {
    final_results_list$globally_pure_high <- globally_pure_high
  }
  
  if(length(globally_pure_medium) > 0) {
    final_results_list$globally_pure_medium <- globally_pure_medium
  }
  
  if(verbose) {
    log_space()
    cli::cli_rule(left = cli::col_green("SUCCESS"), right = cli::col_silver(Sys.time()))
    cli::cli_alert_success(cli::style_italic(cli::style_bold("MarkoClust finished successfully!")))
  }
  
  # Return results
  structure(final_results_list,
            class = "ClustoCell")
}

