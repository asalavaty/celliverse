# Function for cell type annotation

library(Matrix)

typoClust <- function(objects = NULL, # A list of one or more objects of class ClustoCell or MarkoCell (e.g. list(obj1, obj2)). This argument is mandatory if desired_pos_markers and desired_neg_markers are not specified.
                      desired_sets = NULL, # Optional. A character vector of the names of desired clusters, sub-clusters, and/or cell-subsets present in the specified `objects`. If not specified, all clusters and cell-subsets in the objects will be annotated.
                      tissue = NULL, # A character vector specifying one or more tissue contexts to be used for identifying specific cell types. If not specified, all available tissues will be examined. The available tissue types can be accessed from the tissueCondition_types object, which can be loaded using data("tissueCondition_types", package = "celliverse").
                      condition = NULL, # A character vector specifying one or more conditions to be used for identifying specific cell types. If not specified, all conditions including 'Healthy' as well as all available diseased conditions will be examined. The available condition types can be accessed from the tissueCondition_types object, which can be loaded using data("tissueCondition_types", package = "celliverse").
                      use_pos_markers = TRUE, # logical, whether to use positive markers for cell type annotation (default if TRUE).
                      use_neg_markers = TRUE, # logical, whether to use negative markers for cell type annotation.
                      desired_pos_markers = NULL, # Optional. Mandatory if `objects` and `desired_neg_markers` are not specified. A named list of character vectors of the desired positive marker panels (e.g. list(cluster1 = c(gene1, gene 2), cluster2 = c(gene5, gene7))). The names of panels in the list should match those of desired_neg_markers, if specified.
                      desired_neg_markers = NULL, # Optional. Mandatory if `objects` and `desired_pos_markers` are not specified. A named list of character vectors of the desired negative marker panels (e.g. list(cluster1 = c(gene1, gene 2), cluster2 = c(gene5, gene7))). The names of panels in the list should match those of desired_pos_markers, if specified.
                      thresh_mode = c("n", "rank"), # Specifies how to select top markers for each cluster or subcluster. Options are:
                      ## "rank": selects all markers with ranks up to the threshold. If multiple markers share the same rank as the cutoff, they are all included.
                      ## "n": selects strictly the top n rows in rank order. Only the first n rows are kept, even if additional rows share the same rank as the n-th row.
                      thresh = 20, # Integer, threshold for choosing the top N rows of the marker tables or top N ranked markers of each cluster, sub-cluster, and cell-subset. Only used if `objects` is specified.
                      mode = c("markerDB", "ceLLMarkup"), # Either `markerDB` (which annotates the cell clusters and cell subsets based on the prepared database of cell-type markers) or ceLLMarkup (which annotates the cell clusters and cell subsets using large language models (LLM))
                      species = c("human", "mouse"), # Character vector, either 'human' or 'mouse'.
                      verbose = TRUE # Logical, whether to show progress messages
                      ) {
  
  suppressWarnings({
  
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
  
  thresh_mode <- match.arg(thresh_mode)
  mode <- match.arg(mode)
  species <- match.arg(species)

  #________________________________________
  
  # Helper Functions ----
  
  ## special_multiply ----
  # Multiplies two numbers with a custom sign rule:
  #   - Both positive  → positive product (as usual)
  #   - One positive, one negative → negative product (as usual)
  #   - Both negative  → negative product (different from standard multiplication)
  # Works element-wise on vectors using ifelse().
  
  special_multiply <- function(x, y) {
    xy <- x * y
    ifelse(x < 0 & y < 0, -xy, xy)
  }
  
  #________________________________________
  
  # Start of function ----
  if(verbose) {
    cli::cli_rule(left = cli::style_italic(cli::style_bold("Starting TypoClust!")), right = cli::col_silver(Sys.time()))
  }
  
  if(!use_pos_markers & !use_neg_markers) {
    cli::cli_abort("Either `use_pos_markers`, `use_neg_markers` or both should be set to TRUE!")
  }
  
  log_h1("Preparing the Input Data")
  
  if(!is.null(objects) & is.null(desired_sets)) {
    log_message("Since `objects` is specified but `desired_sets` is not specified, all clusters and cell subsets of `objects` will be annotated!")
  }
  
  log_progress_step("Inspecting the input data")
  
  if((is.null(objects) & is.null(desired_pos_markers) & is.null(desired_neg_markers))) {
    cli::cli_abort("At least one of `objects`, `desired_pos_markers`, or `desired_neg_markers` should be specified!")
  }
  
  if(!is.null(objects)) {
    if(!inherits(objects, "ClustoCell") & !inherits(objects, "MarkoCell") & !inherits(objects, "list")) {
      cli::cli_abort("The `objects` argument should be a list of one or more objects of class ClustoCell or MarkoCell (e.g. list(obj1, obj2))!")
    }
  }
  
  if(!is.null(objects) & !inherits(objects, "list")) {
    if(inherits(objects, c("ClustoCell", "MarkoCell"))) {
      objects <- list(objects)
    }
  }
  
  if(!is.null(desired_pos_markers)) {
    if(!inherits(desired_pos_markers, "list")) {
      cli::cli_abort("The `desired_pos_markers` argument should be a named list of character vectors of the desired positive marker panels (e.g. list(cluster1 = c(gene1, gene 2), cluster2 = c(gene5, gene7)))!")
    }
  }
  
  if(!is.null(desired_neg_markers)) {
    if(!inherits(desired_neg_markers, "list")) {
      cli::cli_abort("The `desired_neg_markers` argument should be a named list of character vectors of the desired positive marker panels (e.g. list(cluster1 = c(gene1, gene 2), cluster2 = c(gene5, gene7)))!")
    }
  }
  
  if(!is.null(desired_sets)) {
    if(!inherits(desired_sets, "character")) {
      cli::cli_abort("The `desired_sets` argument should be a character vector of the names of desired clusters, sub-clusters, and/or cell-subsets present in the specified `objects`!")
    }
  }
  
  log_progress_done()
  
  log_progress_step("Preparing the clusters, cell-subsets, and marker panels.")
  
  # Setting the names of all clusters, sub-clusters, cell-subsets, and desired panels ----
  clusters <- NULL
  sub_clusters <- NULL
  cell_subsets <- NULL
  panels <- NULL
  all_clusters_pos <- NULL
  all_clusters_neg <- NULL
  combined_cell_set_names <- NULL
  combined_panels_list <- list(pos_panels = NULL,
                               neg_panels = NULL)
  
  ## Getting the names of all clusters, sub-clusters and cell-subsets ----
  if(!is.null(objects)) {
    
    ### Getting all major_clusters ----
    # Extract names separately
    all_clusters <- unlist(lapply(objects, function(i) {
      if(any(grepl("major_clusters", names(i$markers)))) {
        i$markers$major_clusters$cluster_specific$positive_markers %>% names()
      }
    }), use.names = FALSE)
    all_clusters <- unique(all_clusters)
    
    # Extract pos and neg separately
    all_clusters_pos <- unlist(lapply(objects, function(i) {
      if(any(grepl("major_clusters", names(i$markers)))) {
        tmp_pos <- i$markers$major_clusters$cluster_specific$positive_markers
        names(tmp_pos) <- names(tmp_pos)
        tmp_pos
      } else {
        list()
      }
    }), recursive = FALSE)
    
    all_clusters_neg <- unlist(lapply(objects, function(i) {
      if(any(grepl("major_clusters", names(i$markers)))) {
        tmp_neg <- i$markers$major_clusters$cluster_specific$negative_markers
        names(tmp_neg) <- names(tmp_neg)
        tmp_neg
      } else {
        list()
      }
    }), recursive = FALSE)    
    #____________
    
    ## Getting all sub_clusters ----
    all_sub_clusters <- unlist(lapply(objects, function(j) {
      if(any(grepl("sub_clusters", names(j$markers)))) {
        all_sub_clusters_lst <- gsub(pattern = "-Subclusters", replacement = "-", x = names(j$markers$sub_clusters))
        unlist(lapply(seq_along(all_sub_clusters_lst), function(k) {
          i <- all_sub_clusters_lst[k]
          if(inherits(j$markers$sub_clusters[[k]], "list")) {
            paste(i, names(j$markers$sub_clusters[[k]]$positive_markers), sep = "")
          }
        }), use.names = FALSE)
      }
    }), use.names = FALSE)
    all_sub_clusters <- unique(all_sub_clusters)
    
    # Extract positive markers
    all_sub_clusters_pos <- unlist(lapply(objects, function(j) {
      if(any(grepl("sub_clusters", names(j$markers)))) {
        all_sub_clusters_lst <- gsub(pattern = "-Subclusters", replacement = "-", x = names(j$markers$sub_clusters))
        unlist(lapply(seq_along(all_sub_clusters_lst), function(k) {
          i <- all_sub_clusters_lst[k]
          if(inherits(j$markers$sub_clusters[[k]], "list")) {
            tmp_sub_cluster_names <- paste(i, names(j$markers$sub_clusters[[k]]$positive_markers), sep = "")
            tmp_pos <- j$markers$sub_clusters[[k]]$positive_markers
            names(tmp_pos) <- tmp_sub_cluster_names
            tmp_pos
          } else {
            list()
          }
        }), recursive = FALSE)
      } else {
        list()
      }
    }), recursive = FALSE)
    
    # Extract negative markers
    all_sub_clusters_neg <- unlist(lapply(objects, function(j) {
      if(any(grepl("sub_clusters", names(j$markers)))) {
        all_sub_clusters_lst <- gsub(pattern = "-Subclusters", replacement = "-", x = names(j$markers$sub_clusters))
        unlist(lapply(seq_along(all_sub_clusters_lst), function(k) {
          i <- all_sub_clusters_lst[k]
          if(inherits(j$markers$sub_clusters[[k]], "list")) {
            tmp_sub_cluster_names <- paste(i, names(j$markers$sub_clusters[[k]]$negative_markers), sep = "")
            tmp_neg <- j$markers$sub_clusters[[k]]$negative_markers
            names(tmp_neg) <- tmp_sub_cluster_names
            tmp_neg
          } else {
            list()
          }
        }), recursive = FALSE)
      } else {
        list()
      }
    }), recursive = FALSE)
    
    #____________
    
    ## Getting all MarkoCell_clusters ----
    
    MarkoCell_clusters <- unlist(lapply(objects, function(i) {
      if(any(grepl("cluster_markers", names(i)))) {
        i$cluster_markers$positive_markers %>% names()
      }
    }), use.names = FALSE)
    MarkoCell_clusters <- unique(MarkoCell_clusters)
    
    MarkoCell_pos <- unlist(lapply(objects, function(i) {
      if(any(grepl("cluster_markers", names(i)))) {
        tmp_pos <- i$cluster_markers$positive_markers
        names(tmp_pos) <- names(tmp_pos)
        tmp_pos
      } else {
        list()
      }
    }), recursive = FALSE)
    
    MarkoCell_neg <- unlist(lapply(objects, function(i) {
      if(any(grepl("cluster_markers", names(i)))) {
        tmp_neg <- i$cluster_markers$negative_markers
        names(tmp_neg) <- names(tmp_neg)
        tmp_neg
      } else {
        list()
      }
    }), recursive = FALSE)
    
    #_________
    
    ## Getting all MarkoCell_cell_subset ----
    
    MarkoCell_cell_subset <- unlist(lapply(objects, function(i) {
      if(any(grepl("cell_subset_markers", names(i)))) {
        i$cell_subset_markers$positive_markers %>% names()
      }
    }), use.names = FALSE)
    MarkoCell_cell_subset <- unique(MarkoCell_cell_subset)
    
    MarkoCell_cell_subset_pos <- unlist(lapply(objects, function(i) {
      if(any(grepl("cell_subset_markers", names(i)))) {
        tmp_pos <- i$cell_subset_markers$positive_markers
        names(tmp_pos) <- names(tmp_pos)
        tmp_pos
      } else {
        list()
      }
    }), recursive = FALSE)
    
    MarkoCell_cell_subset_neg <- unlist(lapply(objects, function(i) {
      if(any(grepl("cell_subset_markers", names(i)))) {
        tmp_neg <- i$cell_subset_markers$negative_markers
        names(tmp_neg) <- names(tmp_neg)
        tmp_neg
      } else {
        list()
      }
    }), recursive = FALSE)
    
    #____________
    
    # Combine all positive and negative marker lists
    all_clusters_pos <- c(all_clusters_pos, all_sub_clusters_pos, MarkoCell_pos, MarkoCell_cell_subset_pos)
    all_clusters_neg <- c(all_clusters_neg, all_sub_clusters_neg, MarkoCell_neg, MarkoCell_cell_subset_neg)

    #____________
    
    ## Merging the names of clusters, sub-clusters, and cell-subsets in the objects ----
    combined_cell_set_names <- c(
      all_clusters,
      all_sub_clusters,
      MarkoCell_clusters,
      MarkoCell_cell_subset
    ) %>% unique()
    
    if(!is.null(desired_sets)) {
      if(!all(desired_sets %in% combined_cell_set_names)) {
        wrong_desired_sets <- desired_sets[!(desired_sets %in% combined_cell_set_names)]
        cli::cli_abort(paste0("All cluster, sub-cluster, and cell-subset names specified in the `desired_sets` argument must be present in the `objects`!\n\n",
                              "The following set name(s) are not present in the `objects`: ", paste0(wrong_desired_sets, collapse = ", "),
                              "\n\n", "See below for all clusters, sub-clusters, and cell-subsets present in the `objects`:\n\n",
                              paste0(combined_cell_set_names, collapse = ", ")))
      } else {
        combined_cell_set_names <- desired_sets
      }
    }
    
    #____________
    
    ## Preparing final markers tables based on objects ----
    
    all_clusters_pos <- all_clusters_pos[vapply(all_clusters_pos, function(i) is.data.frame(i) && nrow(i) > 0, logical(1))]
    all_clusters_neg <- all_clusters_neg[vapply(all_clusters_neg, function(i) is.data.frame(i) && nrow(i) > 0, logical(1))]
    
    if(!is.null(desired_sets)) {
      all_clusters_pos <- all_clusters_pos[names(all_clusters_pos) %in% desired_sets]
      all_clusters_neg <- all_clusters_neg[names(all_clusters_neg) %in% desired_sets]
    }
    
    combined_panels_list <- list(pos_panels = all_clusters_pos, neg_panels = all_clusters_neg)
    
    # Process purity and panels in one nested lapply, combining operations
    combined_panels_purity_list <- lapply(combined_panels_list, function(i) {
      lapply(i, function(j) {
        if (thresh_mode == "n") {
          j <- j[seq_len(min(thresh, nrow(j))), , drop = FALSE]  # Use base slicing for speed
        } else if (thresh_mode == "rank") {
          j <- j[j$Rank < thresh, , drop = FALSE]
        }
        if ("Purity" %in% colnames(j)) {
          j[, c("Feature", "Purity"), drop = FALSE]
        } else if ("EWCSR" %in% colnames(j)) {
          j <- j[, c("Feature", "EWCSR"), drop = FALSE]
          j$EWCSR <- abs(j$EWCSR)
          colnames(j)[colnames(j) == "EWCSR"] <- "Purity"
          j
        } else {
          j  # Fallback
        }
      })
    })
    
    combined_panels_list <- lapply(combined_panels_list, function(i) {
      lapply(i, function(j) {
        if (thresh_mode == "n") {
          j$Feature[seq_len(min(thresh, nrow(j)))]  # Base extraction
        } else if (thresh_mode == "rank") {
          j$Feature[j$Rank < thresh]
        }
      })
    })
    
  } else {
    all_clusters_pos <- NULL
    all_clusters_neg <- NULL
    combined_panels_list <- list(pos_panels = NULL,
                                 neg_panels = NULL)
    combined_panels_purity_list <- list(pos_panels = NULL,
                                        neg_panels = NULL)
  }
  
  #____________
  
  # Preparing the desired markers ----
  
  ## Preparing the desired_pos_markers ----
  
  if(!is.null(desired_pos_markers)) {
    combined_panels_list$pos_panels <- append(combined_panels_list$pos_panels, desired_pos_markers)
    combined_cell_set_names <- append(combined_cell_set_names, names(desired_pos_markers))
  }
  
  ## Preparing the desired_neg_markers ----
  
  if(!is.null(desired_neg_markers)) {
    combined_panels_list$neg_panels <- append(combined_panels_list$neg_panels, desired_neg_markers)
    combined_cell_set_names <- append(combined_cell_set_names, names(desired_neg_markers))
  }
  
  #____________
  
  # Finalizing cluster/subset names and markers ----
  
  combined_cell_set_names <- unique(combined_cell_set_names)
  combined_panels_list <- combined_panels_list[!is.null(combined_panels_list)]
  combined_panels_purity_list <- combined_panels_purity_list[!is.null(combined_panels_purity_list)]
  
  #____________________
  
  # Generating the standard marker names dataframe ----
  
  # Check if 'markerDictionary' exists and is of the correct class
  if (!exists("markerDictionary") || !inherits(markerDictionary, "CelliVerse_Data")) {
    
    # Check if the loaded object is of the correct class
    if (exists("markerDictionary") & !inherits(markerDictionary, "CelliVerse_Data")) {
      log_message("The loaded markerDictionary object is not of class 'CelliVerse_Data'.")
    }
    
    # If it doesn't exist or the class is incorrect, load it from the celliverse package
    data("markerDictionary", package = "celliverse")
    log_message("markerDictionary has been successfully loaded and is of class 'CelliVerse_Data'.")
    
  } else {
    log_message("markerDictionary is already loaded and of the correct class 'CelliVerse_Data'.")
  }
  
  #____________________
  
  # Set the curr_markerDictionary ----
  if(species == "human") {
    curr_markerDictionary <- markerDictionary$human
  } else if(species == "mouse") {
    curr_markerDictionary <- markerDictionary$mouse
  } else {
    cli::cli_abort("The 'species' argument should be any of 'human' or 'mouse'!")
  }
  
  # Preapre all markers
  combined_panels_all_markers <- combined_panels_list %>% unlist() %>% unname() %>% unique()
  
  # Split aliases by "|"
  alias_list <- strsplit(curr_markerDictionary$Alias, "\\|")
  
  # Create lookup vector: names are aliases, values are Symbols
  alias_to_symbol <- setNames(rep(curr_markerDictionary$Symbol, lengths(alias_list)),
                              unlist(alias_list))
  
  # Lookup
  combined_markers_symbol <- alias_to_symbol[combined_panels_all_markers]
  
  marker_symbol_df <- data.frame(Input_Marker = combined_panels_all_markers,
                                 Std_Symbol = combined_markers_symbol)
  marker_symbol_df$Available <- TRUE
  marker_symbol_df$Available[is.na(marker_symbol_df$Std_Symbol)] <- FALSE
  
  marker_symbol_df_flt <- marker_symbol_df[!is.na(marker_symbol_df$Std_Symbol),]
  
  log_progress_done()

  #________________________________________________
  
  log_h1("Performing Cell Type Annotation!")

  if(mode == "markerDB") {

    log_message("Setting the cell type annotation mode to markerDB!")

    # Check if 'markerDB' exists and is of the correct class
    if (!exists("markerDB") || !inherits(markerDB, "CelliVerse_Data")) {
      
      # Check if the loaded object is of the correct class
      if (exists("markerDB") & !inherits(markerDB, "CelliVerse_Data")) {
        log_message("The loaded markerDB object is not of class 'CelliVerse_Data'.")
      }
      
      # If it doesn't exist or the class is incorrect, load it from the celliverse package
      data("markerDB", package = "celliverse")
      log_message("markerDB has been successfully loaded and is of class 'CelliVerse_Data'.")

    } else {
      log_message("markerDB is already loaded and of the correct class 'CelliVerse_Data'.")
    }
    
    #____________________
    
    # Check if 'tissueCondition_types' exists and is of the correct class ----
    if(!is.null(tissue)) {
      if (!exists("tissueCondition_types") || !inherits(tissueCondition_types, "CelliVerse_Data")) {
        
        # Check if the loaded object is of the correct class
        if (exists("tissueCondition_types") & !inherits(tissueCondition_types, "CelliVerse_Data")) {
          log_message("The loaded tissueCondition_types object is not of class 'CelliVerse_Data'.")
        }
        
        # If it doesn't exist or the class is incorrect, load it from the celliverse package
        data("tissueCondition_types", package = "celliverse")
        log_message("tissueCondition_types has been successfully loaded and is of class 'CelliVerse_Data'.")
        
      } else {
        log_message("tissueCondition_types is already loaded and of the correct class 'CelliVerse_Data'.")
      }
    }
    
    #____________________

    # Import the occurrence sparse matrices ----
    if(species == "human") {
      pos_sparse_mat <- markerDB$human$positive_db
      neg_sparse_mat <- markerDB$human$negative_db
    } else if(species == "mouse") {
      pos_sparse_mat <- markerDB$mouse$positive_db
      neg_sparse_mat <- markerDB$mouse$negative_db
    } else {
      cli::cli_abort("The 'species' argument should be any of 'human' or 'mouse'!")
    }
    
    if(!use_pos_markers) {
      pos_sparse_mat <- NULL
    } else if(!use_neg_markers) {
      neg_sparse_mat <- NULL
    }
    
    #____________________
    
    # Checking the tissue type
    if(!is.null(tissue)) {
      if(species == "human") {
        curr_tissue_types <- tissueCondition_types$human$all_tissues
        if(all(tissue %in% curr_tissue_types)) {
          log_message(paste0("Tissue type is set to '", paste0(tissue, collapse = ", "), "'."))
        } else {
          cli::cli_abort("The `tissue` argument is incorrect. Please use the available tissue types from the tissueCondition_types object, which can be loaded using data('tissueCondition_types', package = 'celliverse').")
        }
      } else if(species == "mouse") {
        curr_tissue_types <- tissueCondition_types$mouse$all_tissues
        if(all(tissue %in% curr_tissue_types)) {
          log_message(paste0("Tissue type is set to '", paste0(tissue, collapse = ", "), "'."))
        } else {
          cli::cli_abort("The `tissue` argument is incorrect. Please use the available tissue types from the tissueCondition_types object, which can be loaded using data('tissueCondition_types', package = 'celliverse').")
        }
      }
    }
    
    #____________________
    
    # Checking the condition type
    if(!is.null(condition)) {
      if(species == "human") {
        curr_condition_types <- tissueCondition_types$human$all_conditions
        if(all(condition %in% curr_condition_types)) {
          log_message(paste0("Condition is set to '", paste0(condition, collapse = ", "), "'."))
        } else {
          cli::cli_abort("The `condition` argument is incorrect. Please use the available condition types from the tissueCondition_types object, which can be loaded using data('tissueCondition_types', package = 'celliverse').")
        }
      } else if(species == "mouse") {
        curr_condition_types <- tissueCondition_types$mouse$all_conditions
        if(all(condition %in% curr_condition_types)) {
          log_message(paste0("Condition is set to '", paste0(condition, collapse = ", "), "'."))
        } else {
          cli::cli_abort("The `condition` argument is incorrect. Please use the available condition types from the tissueCondition_types object, which can be loaded using data('tissueCondition_types', package = 'celliverse').")
        }
      }
    }

    #_______________
    
    ### Helper: process a sparse matrix and marker panel
    process_matrix <- function(sparse_mat, marker_panel) {
      # Intersect to get valid markers
      marker_panel <- intersect(marker_panel, rownames(sparse_mat))
      
      # Initialize outputs to match original behavior
      n_cols <- ncol(sparse_mat)
      occurrence <- numeric(n_cols)
      matched_markers <- character(n_cols)
      overlaps <- integer(n_cols)
      names(occurrence) <- names(matched_markers) <- names(overlaps) <- colnames(sparse_mat)
      
      # Early return for empty marker_panel
      if (length(marker_panel) == 0) {
        return(list(overlap_counts = overlaps, 
                    overlap_markers = matched_markers, 
                    overlap_occurrence = occurrence))
      }
      
      # Subset sparse matrix
      sub_mat <- sparse_mat[marker_panel, , drop = FALSE]
      
      # Use Matrix::summary for non-zero elements
      summ <- Matrix::summary(sub_mat)
      if (nrow(summ) == 0) {
        return(list(overlap_counts = overlaps, 
                    overlap_markers = matched_markers, 
                    overlap_occurrence = occurrence))
      }
      
      # Group by column index (j)
      col_indices <- summ$j
      row_indices <- summ$i
      values <- summ$x
      
      # Compute matched_markers: paste row names for each column
      marker_names <- rownames(sub_mat)[row_indices]
      markers_by_col <- split(marker_names, col_indices)
      matched_markers[as.integer(names(markers_by_col))] <- vapply(
        markers_by_col, 
        paste, collapse = "|", 
        FUN.VALUE = character(1)
      )
      
      # Compute overlaps: count markers per column
      overlaps[as.integer(names(markers_by_col))] <- lengths(markers_by_col)
      
      # Compute occurrence: sum values per column
      occurrence[as.integer(names(markers_by_col))] <- tapply(values, col_indices, sum, simplify = TRUE)
      
      return(list(overlap_counts = overlaps, 
                  overlap_markers = matched_markers, 
                  overlap_occurrence = occurrence))
    }
    
    #_______________
    
    set_celltype_list <- lapply(combined_cell_set_names, function(curr_set) {
      
      # Setting the marker panels, converting them to standard names, and setting their purity
      
      ## For Positive Markers
      
      ### Finding the marker panel
      pos_marker_panel <- combined_panels_list$pos_panels[[curr_set]]
      
      #_____________
      
      ### Setting the purities
      if(curr_set %in% names(combined_panels_purity_list$pos_panels) & !is.null(pos_marker_panel)) {
        pos_marker_purity_df <- data.frame(
          Input_Marker = marker_symbol_df_flt$Input_Marker[which(marker_symbol_df_flt$Input_Marker %in% pos_marker_panel)],
          Std_Symbol = marker_symbol_df_flt$Std_Symbol[which(marker_symbol_df_flt$Input_Marker %in% pos_marker_panel)]
          )
        pos_marker_purity_df <- pos_marker_purity_df %>% dplyr::mutate(
          Purity = combined_panels_purity_list$pos_panels[[curr_set]][match(Input_Marker, combined_panels_purity_list$pos_panels[[curr_set]][,"Feature"]),
                                                                      "Purity"]
        )
      } else if(!is.null(pos_marker_panel)) {
        pos_marker_purity_df <- data.frame(
          Input_Marker = marker_symbol_df_flt$Input_Marker[which(marker_symbol_df_flt$Input_Marker %in% pos_marker_panel)],
          Std_Symbol = marker_symbol_df_flt$Std_Symbol[which(marker_symbol_df_flt$Input_Marker %in% pos_marker_panel)],
          Purity = NA
          )
      } else {
        pos_marker_purity_df <- NULL
      }
      
      #_____________

      ### Converting names to standard names
      pos_marker_panel <- marker_symbol_df_flt$Std_Symbol[which(marker_symbol_df_flt$Input_Marker %in% pos_marker_panel)]
      
      if(length(pos_marker_panel) == 0) {
        pos_marker_panel <- NULL
      }
      
      #__________________________
      
      ## For Negative Markers
      
      ### Finding the marker panel
      neg_marker_panel <- combined_panels_list$neg_panels[[curr_set]]

      #_____________
      
      ### Setting the purities
      if(curr_set %in% names(combined_panels_purity_list$neg_panels) & !is.null(neg_marker_panel)) {
        neg_marker_purity_df <- data.frame(
          Input_Marker = marker_symbol_df_flt$Input_Marker[which(marker_symbol_df_flt$Input_Marker %in% neg_marker_panel)],
          Std_Symbol = marker_symbol_df_flt$Std_Symbol[which(marker_symbol_df_flt$Input_Marker %in% neg_marker_panel)]
        )
        
        neg_marker_purity_df <- neg_marker_purity_df %>% dplyr::mutate(
          Purity = combined_panels_purity_list$neg_panels[[curr_set]][match(Input_Marker, combined_panels_purity_list$neg_panels[[curr_set]][,"Feature"]),
                                                                      "Purity"]
        )
      } else if(!is.null(neg_marker_panel)) {
        neg_marker_purity_df <- data.frame(
          Input_Marker = marker_symbol_df_flt$Input_Marker[which(marker_symbol_df_flt$Input_Marker %in% neg_marker_panel)],
          Std_Symbol = marker_symbol_df_flt$Std_Symbol[which(marker_symbol_df_flt$Input_Marker %in% neg_marker_panel)],
          Purity = NA
        )
      } else {
        neg_marker_purity_df <- NULL
      }
      
      #_____________
      
      ### Converting names to standard names
      neg_marker_panel <- marker_symbol_df_flt$Std_Symbol[which(marker_symbol_df_flt$Input_Marker %in% neg_marker_panel)]
      
      if(length(neg_marker_panel) == 0) {
        neg_marker_panel <- NULL
      }
      
      #_____________
      
      # Check that at least one marker panel is specified
      if (is.null(pos_marker_panel) && is.null(neg_marker_panel)) {
        cli::cli_abort("At least one of pos_marker_panel or neg_marker_panel must be specified. There is something wrong with one of the desired sets!")
      }
      
      results_list <- list()
      
      #_______
      
      # Initialize column names
      reference_colnames <- NULL
      if (!is.null(pos_sparse_mat) & !is.null(neg_sparse_mat)) { 
        reference_colnames <- c(colnames(pos_sparse_mat), colnames(neg_sparse_mat)) %>% unique()
      } else if (!is.null(pos_sparse_mat)) {
        reference_colnames <- colnames(pos_sparse_mat)
      } else if (!is.null(neg_sparse_mat)) {
        reference_colnames <- colnames(neg_sparse_mat)
      } else cli::cli_abort("At least one of pos_sparse_mat or neg_sparse_mat must be specified!")
      
      df <- data.frame(
        Combo = reference_colnames,
        stringsAsFactors = FALSE
      )
      
      split_combo <- do.call(rbind, strsplit(df$Combo, "_"))
      df$Tissue <- split_combo[, 1]
      df$Condition <- split_combo[, 2]
      df$CellType <- split_combo[, 3]
      
      # Filter the df based on the user specified tissue type
      if(!is.null(tissue)) {
        df <- df %>% dplyr::filter(Tissue %in% tissue)
      }
      
      # Filter the df based on the user specified condition type
      if(!is.null(condition)) {
        df <- df %>% dplyr::filter(Condition %in% condition)
      }
      
      ### Positive overlap
      if (!is.null(pos_marker_panel) && !is.null(pos_sparse_mat) && use_pos_markers) {
        pos_results <- process_matrix(sparse_mat = pos_sparse_mat, marker_panel = pos_marker_panel)
        df$Pos_Markers <- pos_results$overlap_markers[df$Combo]
        df$Pos_Markers[is.na(df$Pos_Markers)] <- ""
        # Pre-create named purity vector for fast lookup
        pos_purity_lookup <- setNames(pos_marker_purity_df$Purity, pos_marker_purity_df$Std_Symbol)
        
        df$Avg_Pos_Purity <- vapply(df$Pos_Markers, function(tmp_marker_set) {
          if (tmp_marker_set == "") return(NA_real_)
          markers <- strsplit(tmp_marker_set, "|", fixed = TRUE)[[1]]  # Drop unique if no dups expected
          mean(pos_purity_lookup[markers], na.rm = TRUE)
        }, FUN.VALUE = numeric(1))  
        df$Pos_Count <- pos_results$overlap_counts[df$Combo]
        df$Pos_Occurrence <- pos_results$overlap_occurrence[df$Combo]
      } else {
        df$Pos_Markers <- ""
        df$Avg_Pos_Purity <- NA
        df$Pos_Count <- 0
        df$Pos_Occurrence <- 0
      }
      
      ### Negative overlap
      if (!is.null(neg_marker_panel) && !is.null(neg_sparse_mat) && use_neg_markers) {
        neg_results <- process_matrix(neg_sparse_mat, neg_marker_panel)
        df$Neg_Markers <- neg_results$overlap_markers[df$Combo]
        df$Neg_Markers[is.na(df$Neg_Markers)] <- ""
        # Pre-create named purity vector for fast lookup
        neg_purity_lookup <- setNames(neg_marker_purity_df$Purity, neg_marker_purity_df$Std_Symbol)
        
        df$Avg_Neg_Purity <- vapply(df$Neg_Markers, function(tmp_marker_set) {
          if (tmp_marker_set == "") return(NA_real_)
          markers <- strsplit(tmp_marker_set, "|", fixed = TRUE)[[1]]  # Drop unique if no dups expected
          mean(neg_purity_lookup[markers], na.rm = TRUE)
        }, FUN.VALUE = numeric(1))
        df$Neg_Count <- neg_results$overlap_counts[df$Combo]
        df$Neg_Occurrence <- neg_results$overlap_occurrence[df$Combo]
      } else {
        df$Neg_Markers <- ""
        df$Avg_Neg_Purity <- NA
        df$Neg_Count <- 0
        df$Neg_Occurrence <- 0
      }
      
      # Wrong markers
      ## Positive markers wrongly found in neg matrix
      if (!is.null(pos_marker_panel) && !is.null(neg_sparse_mat) && use_pos_markers) {
        wrong_pos <- process_matrix(neg_sparse_mat, pos_marker_panel)
        df$Wrong_Positive_Markers <- wrong_pos$overlap_markers[df$Combo]
        df$Wrong_Positive_Markers[is.na(df$Wrong_Positive_Markers)] <- ""
        df$Avg_Wrong_Pos_Purity <- sapply(df$Wrong_Positive_Markers, function(tmp_marker_set) {
          if(tmp_marker_set == "") {
            return(NA)
          } else {
            pos_marker_purity_df$Purity[match(c(strsplit(tmp_marker_set, split = "\\|") %>% unlist() %>% unique()),
                                              pos_marker_purity_df$Std_Symbol)] %>% mean()
          }
        })  
        df$Wrong_Pos_Count <- wrong_pos$overlap_counts[df$Combo]
        df$Wrong_Pos_Occurrence <- wrong_pos$overlap_occurrence[df$Combo]
      } else {
        df$Wrong_Positive_Markers <- ""
        df$Avg_Wrong_Pos_Purity <- NA
        df$Wrong_Pos_Count <- 0
        df$Wrong_Pos_Occurrence <- 0
      }
      
      ## Negative markers wrongly found in pos matrix
      if (!is.null(neg_marker_panel) && !is.null(pos_sparse_mat) && use_neg_markers) {
        wrong_neg <- process_matrix(pos_sparse_mat, neg_marker_panel)
        df$Wrong_Negative_Markers <- wrong_neg$overlap_markers[df$Combo]
        df$Wrong_Negative_Markers[is.na(df$Wrong_Negative_Markers)] <- ""
        df$Avg_Wrong_Neg_Purity <- sapply(df$Wrong_Negative_Markers, function(tmp_marker_set) {
          if(tmp_marker_set == "") {
            return(NA)
          } else {
            neg_marker_purity_df$Purity[match(c(strsplit(tmp_marker_set, split = "\\|") %>% unlist() %>% unique()),
                                              neg_marker_purity_df$Std_Symbol)] %>% mean()
          }
        })  
        df$Wrong_Neg_Count <- wrong_neg$overlap_counts[df$Combo]
        df$Wrong_Neg_Occurrence <- wrong_neg$overlap_occurrence[df$Combo]
      } else {
        df$Wrong_Negative_Markers <- ""
        df$Avg_Wrong_Neg_Purity <- NA
        df$Wrong_Neg_Count <- 0
        df$Wrong_Neg_Occurrence <- 0
      }
      
      # Convert NAs to 0 (for non-purity columns)
      df[,-(grep("Purity", colnames(df)))][is.na(df[,-(grep("Purity", colnames(df)))])] <- 0
      
      # Final columns
      df <- df %>%
        dplyr::mutate(
          Combined_Markers = ifelse(Pos_Markers == "" & Neg_Markers == "", "",
                                    ifelse(Pos_Markers == "", Neg_Markers,
                                           ifelse(Neg_Markers == "", Pos_Markers,
                                                  paste(Pos_Markers, Neg_Markers, sep = "|")))),
          
          Combined_Count = Pos_Count + Neg_Count,
          
          Adjusted_Count = (Pos_Count * 4) * ifelse(is.na(Avg_Pos_Purity), 1, Avg_Pos_Purity) +
            Neg_Count * ifelse(is.na(Avg_Neg_Purity), 1, Avg_Neg_Purity) -
            (Wrong_Pos_Count * 4) * ifelse(is.na(Avg_Wrong_Pos_Purity), 1, Avg_Wrong_Pos_Purity) -
            (Wrong_Neg_Count * 2) * ifelse(is.na(Avg_Wrong_Neg_Purity), 1, Avg_Wrong_Neg_Purity),
          
          Adjusted_Occurrence = (Pos_Occurrence * 4) * ifelse(is.na(Avg_Pos_Purity), 1, Avg_Pos_Purity) +
            Neg_Occurrence * ifelse(is.na(Avg_Neg_Purity), 1, Avg_Neg_Purity) -
            (Wrong_Pos_Occurrence * 4) * ifelse(is.na(Avg_Wrong_Pos_Purity), 1, Avg_Wrong_Pos_Purity) -
            (Wrong_Neg_Occurrence * 2) * ifelse(is.na(Avg_Wrong_Neg_Purity), 1, Avg_Wrong_Neg_Purity),
          
          Combined_Score = special_multiply((Adjusted_Count^2) * sign(Adjusted_Count), log2(Adjusted_Occurrence + 1))
        ) %>%
        dplyr::filter(Combined_Score != 0 & !is.na(Combined_Score)) %>%
        dplyr::select(Tissue, Condition, CellType, 
                      Pos_Markers, Neg_Markers, Combined_Markers, 
                      Pos_Count, Neg_Count, Combined_Count,
                      Pos_Occurrence, Neg_Occurrence,
                      Avg_Pos_Purity, Avg_Neg_Purity, 
                      Wrong_Positive_Markers, Wrong_Negative_Markers,
                      Wrong_Pos_Count, Wrong_Neg_Count,
                      Wrong_Pos_Occurrence, Wrong_Neg_Occurrence,
                      Avg_Wrong_Pos_Purity, Avg_Wrong_Neg_Purity,
                      Adjusted_Count, Adjusted_Occurrence, Combined_Score) %>%
        dplyr::arrange(desc(Combined_Score), desc(Adjusted_Count), desc(Adjusted_Occurrence), desc(Pos_Count), desc(Neg_Count)) %>%
        dplyr::mutate(Rank = row_number())
      return(df)
    })
    
    names(set_celltype_list) <- combined_cell_set_names
    
    #___________________________________
    
    # Preparing the Results Lists
    
    final_results_list <- list(
      cell_types = set_celltype_list,
      metadata = list(desired_sets = combined_cell_set_names,
                      marker_panels = combined_panels_list,
                      marker_symbol_df = marker_symbol_df)
    )

  } else if(mode == "ceLLMarkup") {

    log_message("Setting the cell type annotation mode to ceLLMarkup!")

  } else {
    cli::cli_abort("The `mode` argument should be either 'markerDB', or 'ceLLMarkup'!")
  }
  
  #___________________________________
  
  if(verbose) {
    log_space()
    cli::cli_rule(left = cli::col_green("SUCCESS"), right = cli::col_silver(Sys.time()))
    cli::cli_alert_success(cli::style_italic(cli::style_bold("TypoClust finished successfully!")))
  }
  
  
  })
  
  # Return results
  structure(final_results_list,
            class = "TypoClust")
}
