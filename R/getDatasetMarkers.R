getDatasetMarkers <- function(obj, # an object of class "ClustoCell" generated via either clustoCell or markoClust function
                              clusters = TRUE, # logical, whether to collect the markers of major clusters or not
                              sub_clusters = TRUE, # logical, whether to collect the markers of sub-clusters or not
                              positive_markers = TRUE, # logical, whether to collect the positive markers or not
                              negative_markers = FALSE, # logical, whether to collect the negative markers or not
                              medium_markers = FALSE, # logical, whether to collect the medium markers or not
                              thresh_mode = c("n", "rank"), # Specifies how to select top markers for each cluster or subcluster. Options are:
                              ## "rank": selects all markers with ranks up to the threshold. If multiple markers share the same rank as the cutoff, they are all included.
                              ## "n": selects strictly the top n rows in rank order. Only the first n rows are kept, even if additional rows share the same rank as the n-th row.
                              pos_thresh = 25, # integer, threshold for filtering positive markers. The larger the dataset you may opt for higher thresholds
                              neg_thresh = 20, # integer, threshold for filtering negative markers. The larger the dataset you may opt for higher thresholds
                              med_thresh = 10, # integer, threshold for filtering medium markers. The larger the dataset you may opt for higher thresholds
                              verbose = TRUE # Logical, whether to show progress messages
                              ) {
  
  #________________________________________
  
  # Defining the default logs for info messages

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
  
  #________________________________________
  
  # Start of function
  if(verbose) {
    cli::cli_rule(left = cli::style_italic(cli::style_bold("Starting getDatasetMarkers!")), right = cli::col_silver(Sys.time()))
  }
  
  if(!inherits(obj, "ClustoCell")) {
    cli::cli_abort("The provided `obj` is of the wrong class. It should be a 'ClustoCell' object, created using either the clustoCell or markoClust function!")
  }

  # Getting the list of all cluster-specific markers of all clusters
  if(clusters) {
    
    log_h1("Collecting the Markers of Major Clusters")
    
    ## For Positive Markers
    if(positive_markers) {
      clusters_pos_markers <- sapply(obj$markers$major_clusters$cluster_specific$positive_markers, function(i) {
        if(inherits(i, "data.frame")) {
          if(thresh_mode == "n") {
            i %>% dplyr::slice(1:pos_thresh) %>% dplyr::select(Feature) %>% unlist() %>% unname()
          } else if(thresh_mode == "rank") {
            i %>% dplyr::filter(Rank < pos_thresh) %>% dplyr::select(Feature) %>% unlist() %>% unname()
          }
        }
      }) %>% 
        unlist() %>% 
        unique()
    } else {
      clusters_pos_markers <- NULL
    }
    
    #____________
    
    ## For Negative Markers
    if(negative_markers) {
      clusters_neg_markers <- sapply(obj$markers$major_clusters$cluster_specific$negative_markers, function(i) {
        if(inherits(i, "data.frame")) {
          if(thresh_mode == "n") {
            i %>% dplyr::slice(1:neg_thresh) %>% dplyr::select(Feature) %>% unlist() %>% unname()
          } else if(thresh_mode == "rank") {
            i %>% dplyr::filter(Rank < neg_thresh) %>% dplyr::select(Feature) %>% unlist() %>% unname()
          }
        }
      }) %>% 
        unlist() %>% 
        unique()
    } else {
      clusters_neg_markers <- NULL
    }
    
    #____________
    
    ## For Medium Markers
    if(medium_markers) {
      clusters_med_markers <- sapply(obj$markers$major_clusters$cluster_specific$medium_markers, function(i) {
        if(inherits(i, "data.frame")) {
          if(thresh_mode == "n") {
            i %>% dplyr::slice(1:med_thresh) %>% dplyr::select(Feature) %>% unlist() %>% unname()
          } else if(thresh_mode == "rank") {
            i %>% dplyr::filter(Rank < med_thresh) %>% dplyr::select(Feature) %>% unlist() %>% unname()
          }
        }
      }) %>% 
        unlist() %>% 
        unique()
    } else {
      clusters_med_markers <- NULL
    }
  } else {
    clusters_pos_markers <- NULL
    clusters_neg_markers <- NULL
    clusters_med_markers <- NULL
  }

  #________________________________________________
  
  # Getting the list of all sub-cluster-specific markers of all clusters
  if(sub_clusters) {
    
    log_h1("Collecting the Markers of Sub-clusters")
    
    ## For Positive Markers
    if(positive_markers) {
      sub_clusters_pos_markers <- sapply(obj$markers$sub_clusters, function(i) {
        if(class(i) == "list") {
          tmp_clust <- i$positive_markers
          tmp_marks <- 
            sapply(tmp_clust, function(j) {
              if(inherits(j, "data.frame")) {
                if(thresh_mode == "n") {
                  j %>% dplyr::slice(1:pos_thresh) %>% dplyr::select(Feature) %>% unlist() %>% unname()
                } else if(thresh_mode == "rank") {
                  j %>% dplyr::filter(Rank < pos_thresh) %>% dplyr::select(Feature) %>% unlist() %>% unname()
                }
              }
            })
        } else {
          tmp_marks <- NULL
        }
        tmp_marks
      }) %>% 
        unlist() %>% 
        unique()
    } else {
      sub_clusters_pos_markers <- NULL
    }
    
    #____________
    
    ## For Negative Markers
    if(negative_markers) {
      sub_clusters_neg_markers <- sapply(obj$markers$sub_clusters, function(i) {
        if(class(i) == "list") {
          tmp_clust <- i$negative_markers
          tmp_marks <- 
            sapply(tmp_clust, function(j) {
              if(inherits(j, "data.frame")) {
                if(thresh_mode == "n") {
                  j %>% dplyr::slice(1:neg_thresh) %>% dplyr::select(Feature) %>% unlist() %>% unname()
                } else if(thresh_mode == "rank") {
                  j %>% dplyr::filter(Rank < neg_thresh) %>% dplyr::select(Feature) %>% unlist() %>% unname()
                }
              }
            })
        } else {
          tmp_marks <- NULL
        }
        tmp_marks
      }) %>% 
        unlist() %>% 
        unique()
    } else {
      sub_clusters_neg_markers <- NULL
    }
    
    #____________
    
    ## For Medium Markers
    if(medium_markers) {
      sub_clusters_med_markers <- sapply(obj$markers$sub_clusters, function(i) {
        if(class(i) == "list") {
          tmp_clust <- i$medium_markers
          tmp_marks <- 
            sapply(tmp_clust, function(j) {
              if(inherits(j, "data.frame")) {
                if(thresh_mode == "n") {
                  j %>% dplyr::slice(1:med_thresh) %>% dplyr::select(Feature) %>% unlist() %>% unname()
                } else if(thresh_mode == "rank") {
                  j %>% dplyr::filter(Rank < med_thresh) %>% dplyr::select(Feature) %>% unlist() %>% unname()
                }
              }
            })
        } else {
          tmp_marks <- NULL
        }
        tmp_marks
      }) %>% 
        unlist() %>% 
        unique()
    } else {
      sub_clusters_med_markers <- NULL
    }
  } else {
    sub_clusters_pos_markers <- NULL
    sub_clusters_neg_markers <- NULL
    sub_clusters_med_markers <- NULL
  }

  #________________________________________________
  
  # Unify the marker sets
  combined_markers = unique(c(
    clusters_pos_markers,
    clusters_neg_markers,
    clusters_med_markers,
    sub_clusters_pos_markers,
    sub_clusters_neg_markers,
    sub_clusters_med_markers
  ))
  
  #________________________________________________
  
  # Define the final results
  final_results <- list(combined_markers = combined_markers)
  
  if(!is.null(clusters_pos_markers)) {
    final_results$clusters_pos_markers <- clusters_pos_markers %>% as.vector() %>% unname() %>% unique()
  }
  
  if(!is.null(clusters_neg_markers)) {
    final_results$clusters_neg_markers <- clusters_neg_markers %>% as.vector() %>% unname() %>% unique()
  }
  
  if(!is.null(clusters_med_markers)) {
    final_results$clusters_med_markers <- clusters_med_markers %>% as.vector() %>% unname() %>% unique()
  }
  
  if(!is.null(sub_clusters_pos_markers)) {
    final_results$sub_clusters_pos_markers <- sub_clusters_pos_markers %>% as.vector() %>% unname() %>% unique()
  }
  
  if(!is.null(sub_clusters_neg_markers)) {
    final_results$sub_clusters_neg_markers <- sub_clusters_neg_markers %>% as.vector() %>% unname() %>% unique()
  }
  
  if(!is.null(sub_clusters_med_markers)) {
    final_results$sub_clusters_med_markers <- sub_clusters_med_markers %>% as.vector() %>% unname() %>% unique()
  }
  
  #________________________________________________
  
  if(verbose) {
    log_space()
    cli::cli_rule(left = cli::col_green("SUCCESS"), right = cli::col_silver(Sys.time()))
    cli::cli_alert_success(cli::style_italic(cli::style_bold("getDatasetMarkers finished successfully!")))
  }
  
  # Return results
  structure(final_results,
            class = "DatasetMarkers")
}