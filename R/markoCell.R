#' Rank markers for clusters, cell subsets, or individual cells
#'
#' @description
#' Identifies and ranks positive and negative marker genes for a specified
#' set of cells, which may correspond to clusters, sub-clusters, arbitrary
#' cell subsets, or even single cells. Marker ranking is based on
#' expression-weighted centered scaled ranks (EWCSR), with Gini-based specificity assessment,
#' and noise suppression.
#'
#' @details
#' When \code{subset_to_HVG = TRUE}, highly variable genes are detected using
#' the normalized assay and layer specified by \code{norm_assay} and
#' \code{norm_layer}. Gini-based filtering is applied to identify global
#' (non-specific) versus specific markers.
#'
#' @param data Either a \code{Seurat} object or a numeric matrix with features (genes) as rows and cells as columns.
#'   Recommended to provide at least library-size normalized data when
#'   \code{subset_to_HVG = TRUE}.
#'
#' @param assay
#' Character; assay used for marker ranking.
#'
#' @param layer
#' Character; assay layer used for marker ranking. May be normalized.
#'
#' @param norm_assay
#' Character; assay containing a normalized layer used for HVG detection.
#'
#' @param norm_layer
#' Character; normalized layer used for HVG detection.
#'
#' @param cluster_labels
#' Optional; column name in \code{data@meta.data} containing cluster labels,
#' or a character vector of cluster labels with length equal to the number
#' of cells in \code{data}. Required if \code{desired_clusters} is specified.
#'
#' @param desired_clusters
#' Optional; character vector of cluster labels for which markers are ranked.
#' Required if \code{desired_cells} is not specified.
#'
#' @param desired_cells
#' Optional; named list of character vectors specifying cell names for each
#' subset. Required if \code{desired_clusters} is not specified.
#'
#' @param log1p
#' Logical; whether to apply \code{log1p} transformation to the input \code{data}. It is recommended to set this argument to TRUE (default) if the data is not already on a log scale.
#'
#' @param remove_quiescent_cells
#' Logical; whether to remove quiescent cells prior to marker ranking.
#'
#' @param high_quantile
#' Numeric; quantile threshold defining highly positive EWCSR values.
#'
#' @param low_quantile
#' Numeric; quantile threshold defining highly negative EWCSR values.
#'
#' @param subset_to_HVG
#' Logical; whether to restrict analysis to highly variable genes (HVGs).
#'
#' @param hvg_selection.method
#' Character; HVG selection strategy. One of \code{"vst"},
#' \code{"mean.var.plot"}, or \code{"dispersion"}.
#'
#' @param hvg_var_thresh
#' Numeric; variance threshold for selecting HVGs.
#'
#' @param gini_thresh
#' Numeric; Gini coefficient threshold for detecting non-specific markers.
#'
#' @param noise_feature_thresh
#' Integer; features expressed in fewer than this number of cells are
#' considered noise.
#'
#' @param random_marker_thresh
#' Integer; markers detected in fewer than this number of cells are discarded.
#'
#' @param num_threads
#' Integer; number of threads to use. Default \code{-1} uses all available cores.
#'
#' @param seed
#' Integer; random seed for reproducibility.
#'
#' @param verbose
#' Logical; whether to display progress messages.
#'
#' @return
#' An object of class \code{"MarkoCell"} containing ranked marker tables and
#' associated statistics for each requested cell set.
#'
#' @seealso
#' \code{\link{markoClust}}, \code{\link{markerPurity}},
#' \code{\link{gini.ewcsr.fs}}
#'
#' @examples
#' utils::data("pbmc_small", package = "SeuratObject")
#'
#' pbmc_small$example_clusters <- as.character(
#'   SeuratObject::Idents(pbmc_small)
#' )
#'
#' cluster_ids <- utils::head(
#'   unique(pbmc_small$example_clusters),
#'   2
#' )
#'
#' mc <- markoCell(
#'   data = pbmc_small,
#'   cluster_labels = "example_clusters",
#'   desired_clusters = cluster_ids,
#'   num_threads = 1,
#'   verbose = FALSE
#' )
#' 
#' @useDynLib celliverse, .registration = TRUE
#' @export

markoCell <- function(
    data, # Either a Seurat object or a matrix. It is recommended to input normalized data (at least lib size normalized) if you have set the subset_to_HVG = TRUE.
    assay = "RNA", # The assay we want to use for assessing marker purities.
    layer = "counts", # The layer of the assay we want to use for assessing marker purities (this can be a normalized layer).
    norm_assay = "RNA", # The assay that include a normalized layer and we want to use for the detection of highly variable genes (HVGs). This can be the same as the 'assay'.
    norm_layer = "data", # The normalized layer of the assay that we want to use for the detection of highly variable genes (HVGs).
    cluster_labels = NULL, # Optional. Mandatory if desired_clusters is specified. The column name of cluster labels in the meta.data of input Seurat object, or a character vector of cluster labels with the same as the number of columns/cells of the `data` argument.
    desired_clusters = NULL, # Optional. Mandatory if desired_cells is not specified. A character vector of the 'labels' of desired clusters for ranking their corresponding markers. 
    desired_cells = NULL, # Optional. Mandatory if desired_clusters is not specified. A named list of character vectors of the names of desired cells from the column names of the input data.
    log1p = TRUE, # Weather to log1p transform the data or not
    remove_quiescent_cells = TRUE, # Whether to remove quiescent_cells before marker identification or not.
    high_quantile = 0.25, # The quantile threshold for choosing highly positive expression-weighted centered scaled ranks required for filtering the data and for selecting the positive markers. Higher values label more features as features with high expression-weighted centered scaled ranks.
    low_quantile = 0.25, # The quantile threshold for choosing highly negative expression-weighted centered scaled ranks required for filtering the data and for selecting the negative markers. Lower values label more features as features with low expression-weighted centered scaled ranks.
    subset_to_HVG = FALSE, # Weather to subset the input data to highly varialble genes or use all the genes.
    hvg_selection.method = c("vst", "mean.var.plot", "dispersion"), # How to choose top variable features. Choose one of 'vst', 'mean.var.plot', or 'dispersion'
    hvg_var_thresh = 1, # The variance threshold for choosing HVGs (genes whose variability is more than this threshold standard deviation above the expected technical noise).
    gini_thresh = 0.5, # The Gini threshold for detecting non-specific (global) markers.
    noise_feature_thresh = 4, # The threshold for detecting noise features/genes (i.e. features that have non-zero expression in more than this number of samples/cells).
    random_marker_thresh = 5, # Markers detected at lower than this number of cell are considered as non-marker genes
    num_threads = -1, # Integer. Number of threads (cores) to use. Default is -1, which uses all available cores.
    seed = 9999, # The seed for randomization and making consistent results
    verbose = TRUE # Logical, whether to show progress messages
) {
  
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
  
  #________________________________________
  
  # Checking arguments
  
  data_missing <- missing(data)
  
  #________________________________________
  
  # Setting the seed
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Start of function
  if(verbose) {
    cli::cli_rule(left = cli::style_italic(cli::style_bold("Starting MarkoCell!")), right = cli::col_silver(Sys.time()))
  }
  
  if(!is.null(desired_clusters)) {
    cli::cli_h1("Identification of Markers of Desired Clusters")
  } else if(!is.null(desired_cells)) {
    cli::cli_h1("Identification of Markers of Desired Cell Subsets")
  }
  
  log_h2("Preparing the input data")
  
  if(!is.null(cluster_labels) & is.null(desired_clusters)) {
    log_message("While cluster_labels is specified, desired_clusters is NULL, so none of the clusters will be inspected!")
  }
  
  if(!is.null(desired_clusters)) {
    log_message("For more sensitive and precise identification of all cluster-specific markers, it is recommended to use the markoClust function, as it also excludes markers that are common across all clusters!")
  }
  
  log_progress_step("Inspecting the input data")
  
  if(data_missing) {
    cli::cli_abort("The data cannot be left unspecified!")
  }
  
  if(inherits(data, "Seurat") & !is.null(cluster_labels)) {
    if(length(cluster_labels) > 1 | !inherits(cluster_labels, "character")) {
      cli::cli_abort("The 'cluster_labels' argument should be one of the column names in the meta.data of the specified 'Seurat' object!")
    }
  } else if(!inherits(data, "Seurat") & !is.null(cluster_labels)) {
    if(length(cluster_labels) != ncol(data) | !inherits(cluster_labels, "character")) {
      cli::cli_abort("The 'cluster_labels' argument should be a character vector with a length equal to the number of columns (or cells) in the input 'data' matrix!")
    }
  }
  
  # SO quality control
  if(inherits(data, "Seurat")) {
    if(length(grep(assay, Seurat::Assays(data))) != 1) {
      cli::cli_abort("The specified 'assay' name is not among the list of assays of the Seurat object.\n\nYou can check the list of available assays using the command Seurat::Assays(data)")
    } else if(nrow(data[[assay]][layer]) == 0) {
      cli::cli_abort("The specified 'layer' name is not among the list of layers of the specified 'assay' of the Seurat object.")
    } else if(subset_to_HVG) {
      if(length(grep(norm_assay, Seurat::Assays(data))) != 1) {
        cli::cli_abort("The specified 'norm_assay' name is not among the list of assays of the Seurat object.\n\nYou can check the list of available assays using the command Seurat::Assays(data)")
      } else if(nrow(data[[norm_assay]][norm_layer]) == 0) {
        cli::cli_abort("The specified 'norm_layer' name is not among the list of layers of the specified 'norm_assay' of the Seurat object.")
      }
    }
  }
  
  log_progress_done()
  
  if(verbose) {
    qc_status_id <- cli::cli_status("Extracting expression matrix...")
  }
  
  if(!inherits(data, "Seurat")) {
    if(!inherits(data, c("matrix", "Matrix"))) {
      expr_mat <- as.matrix(data) # This will be treated both as the expr_mat and norm_expr_mat
    } else {
      expr_mat <- data
    }
  } else {
    if(!inherits(data[[assay]][layer], c("matrix", "Matrix"))) {
      expr_mat <- as.matrix(data[[assay]][layer]) # This will be treated both as the expr_mat and norm_expr_mat
    } else {
      expr_mat <- data[[assay]][layer]
    }
    if(subset_to_HVG) {
      if(!inherits(data[[norm_assay]][norm_layer], c("matrix", "Matrix"))) {
        norm_expr_mat <- as.matrix(data[[norm_assay]][norm_layer])
      } else {
        norm_expr_mat <- data[[norm_assay]][norm_layer]
      }
    }
  }
  
  if(any(is.null(colnames(expr_mat))) | any(is.null(rownames(expr_mat)))) {
    cli::cli_abort("All rows and columns of the input data should have (unique) names.")
  }
  
  if(is.null(desired_cells) & is.null(desired_clusters)) {
    cli::cli_abort("You must specify either desired_clusters, desired_cells, or both. Both parameters cannot be NULL!")
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
    if(!inherits(data, "Seurat")) {
      major_clusters <- cluster_labels %>% as.character() %>% setNames(colnames(expr_mat))
      major_clusters <- major_clusters[major_clusters %in% desired_clusters]
    } else {
      major_clusters <- data[[cluster_labels]] %>% 
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
  
  if(any(expr_mat < 0)) {
    cli::cli_abort("Input data contains negative values. The pipeline requires non-negative input (raw or log-normalized counts).")
  }
  
  if(verbose) {
    cli::cli_status_update(qc_status_id, "Removing noise genes...")
  }
  
  # Removing noise genes
  expr_mat <- expr_mat[Matrix::rowSums(expr_mat > 0) > noise_feature_thresh,]
  
  if(subset_to_HVG) {
    
    log_progress_step("Finding highly variable genes (HVGs)...")
    
    # Detection of highly variable genes (HVGs)
    if(!inherits(data, "Seurat")) {
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
  
  # Step 1: Log1p transformation
  if(log1p) {
    expr_mat <- log1p(expr_mat)
  }
  
  if (!inherits(expr_mat, "Matrix")) {
    expr_mat <- Matrix::Matrix(expr_mat, sparse = TRUE)
  }
  
  #____________________
  
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
  
  #___________________________________
  
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
  
  # Saving the global matrix for marker detection
  global_neg_mat <- neg_mat
  
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
  
  # Saving the global matrix for marker detection
  global_med_mat <- med_mat
  
  ### Detecting and removing no_med_cells cells 
  no_med_cells <- colnames(med_mat)[which(Matrix::colSums(med_mat) == 0)]
  if(length(no_med_cells) > 0) {
    
    # Updating the med_mat
    med_mat <- med_mat[,-which(colnames(med_mat) %in% no_med_cells)]
    
  }
  
  log_progress_done()
  
  #_____________________________________
  
  # Detecting specific markers of major clusters
  
  if(!is.null(major_clusters)) {
    
    # Filtering features at the cluster pseudobulk level
    
    log_progress_step("Detecting cluster markers")
    
    major_cluster_ids <- sort(unique(major_clusters))
    
    if(verbose) {
      major_cluster_marker_status <- cli::cli_status("Finding positive markers of clusters...")
    }
    
    ### Ensure only clustered cells are used
    pos_mat_major_clustered <- pos_mat[, which(colnames(pos_mat) %in% names(major_clusters)), drop = FALSE]
    
    #______
    
    # Get Top Markers per Major Cluster
    
    ## pos markers
    if(nrow(pos_mat_major_clustered) > 0) {
      major_cluster_pos_markers <- lapply(major_cluster_ids, function(cid) {
        cells <- names(major_clusters)[major_clusters == cid] %>% na.omit()
        cells <- cells[cells %in% colnames(pos_mat_major_clustered)]
        
        curr_pos_mat <- pos_mat_major_clustered[, cells, drop = FALSE]
        
        if(ncol(curr_pos_mat) > 1 & any(curr_pos_mat)) {
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
        } else if(length(cells) > 0 & any(curr_pos_mat)) {
          curr_cluster_gini_scores <- ewcsr_mat[rownames(curr_pos_mat)[as.vector(curr_pos_mat)], cells]
          curr_cluster_gini_scores <- data.frame(Feature = names(curr_cluster_gini_scores), 
                                                 EWCSR = curr_cluster_gini_scores,
                                                 Rank = data.table::frankv(curr_cluster_gini_scores, order = -1, ties.method="dense")) %>% dplyr::arrange(Rank)
          rownames(curr_cluster_gini_scores) <- NULL
          curr_cluster_gini_scores
        } else {
          return(base::structure("Note: No specific marker was identified!", class = "logMessage"))
        }
      })
    } else {
      major_cluster_pos_markers <- lapply(major_cluster_ids, function(cid) {
        return(base::structure("Note: No specific marker was identified!", class = "logMessage"))
      })
    }
    
    names(major_cluster_pos_markers) <- major_cluster_ids
    
    #____________________
    
    if(verbose) {
      cli::cli_status_update(major_cluster_marker_status, "Finding negative markers of clusters...")
    }
    
    ### Ensure only clustered cells are used
    neg_mat_major_clustered <- global_neg_mat[, which(colnames(global_neg_mat) %in% names(major_clusters)), drop = FALSE]
    
    #______
    
    # Get Top Markers per Major Cluster
    
    ## neg markers
    if(nrow(neg_mat_major_clustered) > 0) {
      major_cluster_neg_markers <- lapply(major_cluster_ids, function(cid) {
        cells <- names(major_clusters)[major_clusters == cid] %>% na.omit()
        cells <- cells[cells %in% colnames(neg_mat_major_clustered)]
        
        curr_neg_mat <- neg_mat_major_clustered[, cells, drop = FALSE]
        
        if(ncol(curr_neg_mat) > 1 & any(curr_neg_mat)) {
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
        } else if(length(cells) > 0 & any(curr_neg_mat)) {
          curr_cluster_gini_scores <- ewcsr_mat[rownames(curr_neg_mat)[as.vector(curr_neg_mat)], cells]
          curr_cluster_gini_scores <- data.frame(Feature = names(curr_cluster_gini_scores), 
                                                 EWCSR = curr_cluster_gini_scores,
                                                 Rank = data.table::frankv(curr_cluster_gini_scores, order = 1, ties.method="dense")) %>% dplyr::arrange(Rank)
          rownames(curr_cluster_gini_scores) <- NULL
          curr_cluster_gini_scores
        } else {
          return(base::structure("Note: No specific marker was identified!", class = "logMessage"))
        }
      })
    } else {
      major_cluster_neg_markers <- lapply(major_cluster_ids, function(cid) {
        return(base::structure("Note: No specific marker was identified!", class = "logMessage"))
      })
    }
    
    names(major_cluster_neg_markers) <- major_cluster_ids
    
    #____________________
    
    if(verbose) {
      cli::cli_status_update(major_cluster_marker_status, "Finding medium markers of clusters...")
    }
    
    ### Ensure only clustered cells are used
    med_mat_major_clustered <- global_med_mat[, which(colnames(global_med_mat) %in% names(major_clusters)), drop = FALSE]
    
    #______
    
    # Get Top Markers per Major Cluster
    
    ## med markers
    if(nrow(med_mat_major_clustered) > 0) {
      major_cluster_med_markers <- lapply(major_cluster_ids, function(cid) {
        cells <- names(major_clusters)[major_clusters == cid] %>% na.omit()
        cells <- cells[cells %in% colnames(med_mat_major_clustered)]
        
        curr_med_mat <- med_mat_major_clustered[, cells, drop = FALSE]
        
        if(ncol(curr_med_mat) > 1 & any(curr_med_mat)) {
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
        } else if(length(cells) > 0 & any(curr_med_mat)) {
          curr_cluster_gini_scores <- ewcsr_mat[rownames(curr_med_mat)[as.vector(curr_med_mat)], cells]
          curr_cluster_gini_scores <- data.frame(Feature = names(curr_cluster_gini_scores), 
                                                 EWCSR = curr_cluster_gini_scores,
                                                 Rank = data.table::frankv(curr_cluster_gini_scores, order = -1, ties.method="dense")) %>% dplyr::arrange(Rank)
          rownames(curr_cluster_gini_scores) <- NULL
          curr_cluster_gini_scores
        } else {
          return(base::structure("Note: No specific marker was identified!", class = "logMessage"))
        }
      })
    } else {
      major_cluster_med_markers <- lapply(major_cluster_ids, function(cid) {
        return(base::structure("Note: No specific marker was identified!", class = "logMessage"))
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
    #       major_cluster_pos_markers[[cur_cl]] <<- base::structure("Note: No specific marker was identified!", class = "logMessage")
    #     }
    #   }
    #   
    #   major_cluster_neg_markers[[cur_cl]] <<- curr_neg_markers
    #   if(is.data.frame(major_cluster_neg_markers[[cur_cl]])) {
    #     if(nrow(major_cluster_neg_markers[[cur_cl]]) == 0) {
    #       major_cluster_neg_markers[[cur_cl]] <<- base::structure("Note: No specific marker was identified!", class = "logMessage")
    #     }
    #   }
    #   
    #   major_cluster_med_markers[[cur_cl]] <<- curr_med_markers
    #   if(is.data.frame(major_cluster_med_markers[[cur_cl]])) {
    #     if(nrow(major_cluster_med_markers[[cur_cl]]) == 0) {
    #       major_cluster_med_markers[[cur_cl]] <<- base::structure("Note: No specific marker was identified!", class = "logMessage")
    #     }
    #   }
    # })
    
    ## C++ version
    cpp_major_markers_list <- filter_cluster_markers_cpp(pos_markers = major_cluster_pos_markers, 
                                                         neg_markers = major_cluster_neg_markers, 
                                                         med_markers = major_cluster_med_markers)
    
    major_cluster_pos_markers <- cpp_major_markers_list$pos
    
    major_cluster_pos_markers <- 
      lapply(major_cluster_pos_markers, function(i) {
        if(is.data.frame(i)) {
          if(nrow(i) == 0) {
            return(base::structure("Note: No specific marker was identified!", class = "logMessage"))
          } else {
            return(i)
          }
        } else {
          return(i)
        }
      })
    
    major_cluster_neg_markers <- cpp_major_markers_list$neg
    
    major_cluster_neg_markers <- 
      lapply(major_cluster_neg_markers, function(i) {
        if(is.data.frame(i)) {
          if(nrow(i) == 0) {
            return(base::structure("Note: No specific marker was identified!", class = "logMessage"))
          } else {
            return(i)
          }
        } else {
          return(i)
        }
      })
    
    major_cluster_med_markers <- cpp_major_markers_list$med
    
    major_cluster_med_markers <- 
      lapply(major_cluster_med_markers, function(i) {
        if(is.data.frame(i)) {
          if(nrow(i) == 0) {
            return(base::structure("Note: No specific marker was identified!", class = "logMessage"))
          } else {
            return(i)
          }
        } else {
          return(i)
        }
      })
    
    log_progress_done()
    
    if(verbose) {
      cli::cli_status_clear(major_cluster_marker_status)
    }
  }
  
  #_____________________________________
  
  # Detecting specific markers of cell subsets
  
  if(!is.null(cell_subsets)) {
    
    if(!is.null(desired_clusters)) {
      cli::cli_h1("Identification of Markers of Desired Cell Subsets")
    }
    
    # Filtering features at the cell subset pseudobulk level
    
    log_progress_step("Detecting cell subset markers")
    
    cell_subset_ids <- sort(unique(cell_subsets))
    
    if(verbose) {
      cell_subset_marker_status <- cli::cli_status("Finding positive markers of cell subsets...")
    }
    
    ### Ensure only cell subsetted cells are used
    pos_mat_cell_subsetted <- pos_mat[, which(colnames(pos_mat) %in% names(cell_subsets)), drop = FALSE]
    
    #______
    
    # Get Top Markers per Cell Subset
    
    ## pos markers
    if(nrow(pos_mat_cell_subsetted) > 0) {
      cell_subset_pos_markers <- lapply(cell_subset_ids, function(cid) {
        cells <- names(cell_subsets)[cell_subsets == cid] %>% na.omit()
        cells <- cells[cells %in% colnames(pos_mat_cell_subsetted)]
        
        curr_pos_mat <- pos_mat_cell_subsetted[, cells, drop = FALSE]
        
        if(ncol(curr_pos_mat) > 1 & any(curr_pos_mat)) {
          # curr_subset_gini_scores <- apply(curr_pos_mat, 1, function(x) ineq::Gini(x))
          curr_subset_gini_scores <- gini_rows_lg_marker_matrix(curr_pos_mat@i, curr_pos_mat@p, curr_pos_mat@x, 
                                                                nrow(curr_pos_mat), ncol(curr_pos_mat), 
                                                                curr_pos_mat@Dimnames[[1]])
          
          curr_subset_gini_scores <- data.frame(Feature = names(curr_subset_gini_scores), 
                                                Gini_Score = curr_subset_gini_scores,
                                                Purity = 1 - curr_subset_gini_scores,
                                                Rank = data.table::frankv(curr_subset_gini_scores, ties.method="dense")) %>% dplyr::arrange(Rank) %>% 
            dplyr::filter(!is.na(Gini_Score))
          rownames(curr_subset_gini_scores) <- NULL
          curr_subset_gini_scores
        } else if(length(cells) > 0 & any(curr_pos_mat)) {
          curr_subset_gini_scores <- ewcsr_mat[rownames(curr_pos_mat)[as.vector(curr_pos_mat)], cells]
          curr_subset_gini_scores <- data.frame(Feature = names(curr_subset_gini_scores), 
                                                EWCSR = curr_subset_gini_scores,
                                                Rank = data.table::frankv(curr_subset_gini_scores, order = -1, ties.method="dense")) %>% dplyr::arrange(Rank)
          rownames(curr_subset_gini_scores) <- NULL
          curr_subset_gini_scores
        } else {
          return(base::structure("Note: No specific marker was identified!", class = "logMessage"))
        }
      })
    } else {
      cell_subset_pos_markers <- lapply(cell_subset_ids, function(cid) {
        return(base::structure("Note: No specific marker was identified!", class = "logMessage"))
      })
    }
    
    names(cell_subset_pos_markers) <- cell_subset_ids
    
    #____________________
    
    if(verbose) {
      cli::cli_status_update(cell_subset_marker_status, "Finding negative markers of cell subsets...")
    }
    
    ### Ensure only cell subsetted cells are used
    neg_mat_cell_subsetted <- neg_mat[, which(colnames(neg_mat) %in% names(cell_subsets)), drop = FALSE]
    
    #______
    
    # Get Top Markers per Cell Subset
    
    ## neg markers
    if(nrow(neg_mat_cell_subsetted) > 0) {
      cell_subset_neg_markers <- lapply(cell_subset_ids, function(cid) {
        cells <- names(cell_subsets)[cell_subsets == cid] %>% na.omit()
        cells <- cells[cells %in% colnames(neg_mat_cell_subsetted)]
        
        curr_neg_mat <- neg_mat_cell_subsetted[, cells, drop = FALSE]
        
        if(ncol(curr_neg_mat) > 1 & any(curr_neg_mat)) {
          # curr_subset_gini_scores <- apply(curr_neg_mat, 1, function(x) ineq::Gini(x))
          curr_subset_gini_scores <- gini_rows_lg_marker_matrix(curr_neg_mat@i, curr_neg_mat@p, curr_neg_mat@x, 
                                                                nrow(curr_neg_mat), ncol(curr_neg_mat), 
                                                                curr_neg_mat@Dimnames[[1]])
          
          curr_subset_gini_scores <- data.frame(Feature = names(curr_subset_gini_scores), 
                                                Gini_Score = curr_subset_gini_scores,
                                                Purity = 1 - curr_subset_gini_scores,
                                                Rank = data.table::frankv(curr_subset_gini_scores, ties.method="dense")) %>% dplyr::arrange(Rank) %>% 
            dplyr::filter(!is.na(Gini_Score))
          rownames(curr_subset_gini_scores) <- NULL
          curr_subset_gini_scores
        } else if(length(cells) > 0 & any(curr_neg_mat)) {
          curr_subset_gini_scores <- ewcsr_mat[rownames(curr_neg_mat)[as.vector(curr_neg_mat)], cells]
          curr_subset_gini_scores <- data.frame(Feature = names(curr_subset_gini_scores), 
                                                EWCSR = curr_subset_gini_scores,
                                                Rank = data.table::frankv(curr_subset_gini_scores, order = 1, ties.method="dense")) %>% dplyr::arrange(Rank)
          rownames(curr_subset_gini_scores) <- NULL
          curr_subset_gini_scores
        } else {
          return(base::structure("Note: No specific marker was identified!", class = "logMessage"))
        }
      })
    } else {
      cell_subset_neg_markers <- lapply(cell_subset_ids, function(cid) {
        return(base::structure("Note: No specific marker was identified!", class = "logMessage"))
      })
    }
    
    names(cell_subset_neg_markers) <- cell_subset_ids
    
    #____________________
    
    if(verbose) {
      cli::cli_status_update(cell_subset_marker_status, "Finding 'medium' markers of cell subsets...")
    }
    
    ### Ensure only cell subsetted cells are used
    med_mat_cell_subsetted <- med_mat[, which(colnames(med_mat) %in% names(cell_subsets)), drop = FALSE]
    
    #______
    
    # Get Top Markers per Cell Subset
    
    ## med markers
    if(nrow(med_mat_cell_subsetted) > 0) {
      cell_subset_med_markers <- lapply(cell_subset_ids, function(cid) {
        cells <- names(cell_subsets)[cell_subsets == cid] %>% na.omit()
        cells <- cells[cells %in% colnames(med_mat_cell_subsetted)]
        
        curr_med_mat <- med_mat_cell_subsetted[, cells, drop = FALSE]
        
        if(ncol(curr_med_mat) > 1 & any(curr_med_mat)) {
          # curr_subset_gini_scores <- apply(curr_med_mat, 1, function(x) ineq::Gini(x))
          curr_subset_gini_scores <- gini_rows_lg_marker_matrix(curr_med_mat@i, curr_med_mat@p, curr_med_mat@x, 
                                                                nrow(curr_med_mat), ncol(curr_med_mat), 
                                                                curr_med_mat@Dimnames[[1]])
          
          curr_subset_gini_scores <- data.frame(Feature = names(curr_subset_gini_scores), 
                                                Gini_Score = curr_subset_gini_scores,
                                                Purity = 1 - curr_subset_gini_scores,
                                                Rank = data.table::frankv(curr_subset_gini_scores, ties.method="dense")) %>% dplyr::arrange(Rank) %>% 
            dplyr::filter(!is.na(Gini_Score))
          rownames(curr_subset_gini_scores) <- NULL
          curr_subset_gini_scores
        } else if(length(cells) > 0 & any(curr_med_mat)) {
          curr_subset_gini_scores <- ewcsr_mat[rownames(curr_med_mat)[as.vector(curr_med_mat)], cells]
          curr_subset_gini_scores <- data.frame(Feature = names(curr_subset_gini_scores), 
                                                EWCSR = curr_subset_gini_scores,
                                                Rank = data.table::frankv(curr_subset_gini_scores, order = -1, ties.method="dense")) %>% dplyr::arrange(Rank)
          rownames(curr_subset_gini_scores) <- NULL
          curr_subset_gini_scores
        } else {
          return(base::structure("Note: No specific marker was identified!", class = "logMessage"))
        }
      })
    } else {
      cell_subset_med_markers <- lapply(cell_subset_ids, function(cid) {
        return(base::structure("Note: No specific marker was identified!", class = "logMessage"))
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
    #       cell_subset_pos_markers[[cur_subset]] <<- base::structure("Note: No specific marker was identified!", class = "logMessage")
    #     }
    #   }
    #   
    #   cell_subset_neg_markers[[cur_subset]] <<- curr_neg_markers
    #   if(is.data.frame(cell_subset_neg_markers[[cur_subset]])) {
    #     if(nrow(cell_subset_neg_markers[[cur_subset]]) == 0) {
    #       cell_subset_neg_markers[[cur_subset]] <<- base::structure("Note: No specific marker was identified!", class = "logMessage")
    #     }
    #   }
    #   
    #   cell_subset_med_markers[[cur_subset]] <<- curr_med_markers
    #   if(is.data.frame(cell_subset_med_markers[[cur_subset]])) {
    #     if(nrow(cell_subset_med_markers[[cur_subset]]) == 0) {
    #       cell_subset_med_markers[[cur_subset]] <<- base::structure("Note: No specific marker was identified!", class = "logMessage")
    #     }
    #   }
    # })
    
    ## C++ version
    cpp_cell_subset_markers_list <- filter_cluster_markers_cpp(pos_markers = cell_subset_pos_markers, 
                                                               neg_markers = cell_subset_neg_markers, 
                                                               med_markers = cell_subset_med_markers)
    
    cell_subset_pos_markers <- cpp_cell_subset_markers_list$pos
    
    cell_subset_pos_markers <- 
      lapply(cell_subset_pos_markers, function(i) {
        if(is.data.frame(i)) {
          if(nrow(i) == 0) {
            return(base::structure("Note: No specific marker was identified!", class = "logMessage"))
          } else {
            return(i)
          }
        } else {
          return(i)
        }
      })
    
    cell_subset_neg_markers <- cpp_cell_subset_markers_list$neg
    
    cell_subset_neg_markers <- 
      lapply(cell_subset_neg_markers, function(i) {
        if(is.data.frame(i)) {
          if(nrow(i) == 0) {
            return(base::structure("Note: No specific marker was identified!", class = "logMessage"))
          } else {
            return(i)
          }
        } else {
          return(i)
        }
      })
    
    cell_subset_med_markers <- cpp_cell_subset_markers_list$med
    
    cell_subset_med_markers <- 
      lapply(cell_subset_med_markers, function(i) {
        if(is.data.frame(i)) {
          if(nrow(i) == 0) {
            return(base::structure("Note: No specific marker was identified!", class = "logMessage"))
          } else {
            return(i)
          }
        } else {
          return(i)
        }
      })
    
    log_progress_done()
    
    if(verbose) {
      cli::cli_status_clear(cell_subset_marker_status)
    }
  }
  
  #___________________________________
  
  # Preparing the Results Lists
  
  if(!is.null(major_clusters) & !is.null(cell_subsets)) {
    final_results_list <- list(
      cluster_markers = list(
        positive_markers = major_cluster_pos_markers,
        negative_markers = major_cluster_neg_markers,
        medium_markers = major_cluster_med_markers
      ),
      cell_subset_markers = list(
        positive_markers = cell_subset_pos_markers,
        negative_markers = cell_subset_neg_markers,
        medium_markers = cell_subset_med_markers
      )
    )
  } else if(!is.null(major_clusters)) {
    final_results_list <- list(
      cluster_markers = list(
        positive_markers = major_cluster_pos_markers,
        negative_markers = major_cluster_neg_markers,
        medium_markers = major_cluster_med_markers
      )
    )
  } else if(!is.null(cell_subsets)) {
    final_results_list <- list(
      cell_subset_markers = list(
        positive_markers = cell_subset_pos_markers,
        negative_markers = cell_subset_neg_markers,
        medium_markers = cell_subset_med_markers
      )
    )
  }
  
  #_______________
  
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
  
  #___________________________________
  
  if(verbose) {
    log_space()
    cli::cli_rule(left = cli::col_green("SUCCESS"), right = cli::col_silver(Sys.time()))
    cli::cli_alert_success(cli::style_italic(cli::style_bold("MarkoCell finished successfully!")))
  }
  
  # Return results
  structure(final_results_list,
            class = "MarkoCell")
}
