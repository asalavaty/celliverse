library(Matrix)
library(igraph)
library(cli)
library(data.table)
library(dplyr)
library(magrittr)

#______________________

clustoCell_TransferLabel <- function(clustoCell, # Ab object of class ClustoCell obtained by running clustoCell on a sketched (sampled) dataset.
                                     query_ewcsr_mat, # A dgCMatrix matrix. Query EWCSR matrix. This is not required if method is set to 'count-knn' and mandatory otherwise. All cells in the clustoCell should be present in the query_ewcsr_mat and query_expr_mat.
                                     query_expr_mat, # A dgCMatrix matrix. Query count matrix. This is mandatory if method is set to 'count-knn' and not required otherwise.
                                     method = c("ewcsr-cor", 
                                                "ewcsr-red-cor", 
                                                "count-knn"), 
                                     # Character string specifying the label transfer method to use. Options include:
                                     #   \item \code{"ewcsr-cor"}: Transfers labels by computing the correlation between each cell in the query expression matrix (\code{query_expr_mat}) and the EWCSR centroids of each cluster in the \code{clustoCell}, using the full expression space (i.e., non-reduced).
                                     #   \item \code{"ewcsr-red-cor"}: Similar to \code{"ewcsr-cor"}, but performs correlation in the reduced dimensional space (PCA embedding), using dimensionally reduced EWCSR centroids.
                                     #   \item \code{"count-knn"}: Transfers labels using the Seurat \code{FindTransferAnchors} and \code{TransferData} pipeline, based on shared features in count-based expression data and k-nearest neighbor matching.
                                     dims = 30, # Integer, number of dimensions used during the sketching.
                                     num_threads = -1, # Integer. Number of threads (cores) to use. Default is -1, which uses all available cores.
                                     inherit_major_clusters = TRUE, # logical, whether to inherit the major cluster labels and label transfer sub-clusters (if present) within each major cluster or transfer labels of all cell based on the skeched data labels regardless of their original major cluster label. This is only used if all cells of the data are available within the major_cluster slot of the clustoCell object.
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
  
  method <- match.arg(method)
  
  #________________________________________
  
  # Setting the seed
  set.seed(seed)
  
  log_space()
  
  # Start of function
  if(verbose) {
    cli::cli_rule(left = cli::style_italic(cli::style_bold("Starting Label Transfer!")), right = cli::col_silver(Sys.time()))
  }
  
  #________________
  
  if(!inherits(clustoCell, "ClustoCell")) {
    cli::cli_abort("The provided `clustoCell` is of the wrong class. It should be a 'ClustoCell' object, created using either the clustoCell or markoClust function!")
  }
  
  #________________

  if(any(grepl("merged_sub_clusters", names(clustoCell$clusters)))) {
    sketched_cluster_source <- "merged_sub_clusters"
    
    if(inherit_major_clusters && !is.null(query_ewcsr_mat) && length(clustoCell$clusters$major_clusters) == ncol(query_ewcsr_mat)) {
      use_major_clusters <- TRUE
      true_major_clusters <- paste(unique(clustoCell$clusters$major_clusters), "-", sep = "")
      all_major_clusters <- clustoCell$clusters$major_clusters
      log_message("All major clusters have been identified and will be used to label the transferred sketch data within each cluster!")
    } else if(inherit_major_clusters && !is.null(query_expr_mat) && length(clustoCell$clusters$major_clusters) == ncol(query_expr_mat)) {
      use_major_clusters <- TRUE
      true_major_clusters <- paste(unique(clustoCell$clusters$major_clusters), "-", sep = "")
      all_major_clusters <- clustoCell$clusters$major_clusters
      log_message("All major clusters have been identified and will be used to label the transferred sketch data within each cluster!")
    } else {
      use_major_clusters <- FALSE
    }
    
  } else {
    sketched_cluster_source <- "major_clusters"
    use_major_clusters <- FALSE
  }
  
  sketched_clusters <- clustoCell$clusters[[sketched_cluster_source]]
  
  # Adding global quiescent_cells & isolated_cells
  
  isolated_cells_vec <- clustoCell$isolated_cells
  isolated_cells_vec <- setNames(rep("Isolated", length(isolated_cells_vec)), isolated_cells_vec)
  
  quiescent_cells_vec <- clustoCell$quiescent_cells
  quiescent_cells_vec <- setNames(rep("Quiescent", length(quiescent_cells_vec)), quiescent_cells_vec)
  
  sketched_clusters <- c(sketched_clusters, quiescent_cells_vec, isolated_cells_vec)
  
  sketch_cells <- names(sketched_clusters)
  
  #________________

  if (method == "ewcsr-cor") {
    
    # query_ewcsr_mat[is.na(query_ewcsr_mat)] <- 0
    query_ewcsr_mat <- replace_na_with_zero_cpp(query_ewcsr_mat, num_threads = num_threads)
    
    # Compute centroids or prepare for method
    unique_clusters <- sketched_clusters %>% unique() %>% sort()
    
    # centroids <- sapply(unique_clusters, function(cl) {
    #   curr_cells <- which(sketched_clusters == cl) %>% names()
    #   rowMeans(query_ewcsr_mat[, curr_cells, drop = FALSE])
    # }, simplify = FALSE)
    # 
    # centroids <- do.call(rbind, centroids) %>% t()
    centroids <- compute_centroids_cpp(mat = query_ewcsr_mat, sketched_clusters = sketched_clusters, unique_clusters = unique_clusters, num_threads = num_threads)

    # Retain only non-sketched data
    query_ewcsr_mat <- query_ewcsr_mat[, !(colnames(query_ewcsr_mat) %in% sketch_cells)]
    
    # Pearson correlation to centroids
    # sim_scores <- cor(as.matrix(query_ewcsr_mat), centroids)  # Cells x Clusters
    if(use_major_clusters) {
      label_transfer_df <- lapply(true_major_clusters, function(i) {
        curr_true_major_cluster_idx <- grep(i, unique_clusters)
        curr_true_major_cluster <- unique_clusters[curr_true_major_cluster_idx]
        curr_non_sketched_cells <- colnames(query_ewcsr_mat)[colnames(query_ewcsr_mat) %in% names(all_major_clusters[all_major_clusters == gsub("-$", "", i)])]
        curr_query_ewcsr_mat <- query_ewcsr_mat[,curr_non_sketched_cells]
        curr_centroids <- centroids[,curr_true_major_cluster_idx, drop = FALSE]
        curr_sim_scores <- sparse_dense_correlation_cpp(sp_mat = curr_query_ewcsr_mat, centroids = curr_centroids, num_threads = num_threads)
        curr_predicted_labels <- curr_true_major_cluster[apply(curr_sim_scores, 1, which.max)]
        curr_confidence <- apply(curr_sim_scores, 1, max)  # Max correlation as confidence
        curr_label_transfer_df <- data.frame(cell = colnames(curr_query_ewcsr_mat),
                                             predicted_cluster = curr_predicted_labels,
                                             confidence = curr_confidence)
        curr_label_transfer_df
      }) %>% do.call(what = rbind)
      label_transfer_df <- label_transfer_df[match(colnames(query_ewcsr_mat), label_transfer_df$cell),]
      predicted_labels <- label_transfer_df$predicted_cluster
    } else {
      sim_scores <- sparse_dense_correlation_cpp(sp_mat = query_ewcsr_mat, centroids = centroids, num_threads = num_threads)
      predicted_labels <- unique_clusters[apply(sim_scores, 1, which.max)]
      confidence <- apply(sim_scores, 1, max)  # Max correlation as confidence
      label_transfer_df <- data.frame(cell = colnames(query_ewcsr_mat),
                                      predicted_cluster = predicted_labels,
                                      confidence = confidence)
    }

    names(predicted_labels) <- colnames(query_ewcsr_mat)
    predicted_labels <- c(predicted_labels, sketched_clusters)
    
  } else if (method == "ewcsr-red-cor") {
    
    # query_ewcsr_mat[is.na(query_ewcsr_mat)] <- 0
    query_ewcsr_mat <- replace_na_with_zero_cpp(query_ewcsr_mat, num_threads = num_threads)
    
    original_seu <- Seurat::CreateSeuratObject(counts = query_ewcsr_mat[, !(colnames(query_ewcsr_mat) %in% sketch_cells)], 
                                                    data = query_ewcsr_mat[, !(colnames(query_ewcsr_mat) %in% sketch_cells)])
    sketched_seu <- Seurat::CreateSeuratObject(counts = query_ewcsr_mat[, (colnames(query_ewcsr_mat) %in% sketch_cells)], 
                                                    data = query_ewcsr_mat[, (colnames(query_ewcsr_mat) %in% sketch_cells)])
    
    all_seu <- suppressWarnings(merge(original_seu, sketched_seu))  # Merge for joint HVG/PCA
    all_seu <- suppressWarnings(Seurat::FindVariableFeatures(all_seu, verbose = FALSE))
    all_seu <- suppressWarnings(Seurat::ScaleData(all_seu, verbose = FALSE))
    all_seu <- suppressWarnings(Seurat::RunPCA(all_seu, npcs = dims, verbose = FALSE))
    
    # Extract reduced data
    original_reduced <- Seurat::Embeddings(all_seu)[colnames(original_seu), ]
    sketched_reduced <- Seurat::Embeddings(all_seu)[colnames(sketched_seu), ]
    
    # Compute centroids or prepare for method
    unique_clusters <- sketched_clusters %>% unique() %>% sort()
    
    # centroids <- sapply(unique_clusters, function(cl) {
    #   curr_cells <- which(sketched_clusters == cl) %>% names()
    #   colMeans(sketched_reduced[curr_cells, , drop = FALSE])  # Mean in reduced space
    # }, simplify = FALSE)
    # centroids <- do.call(rbind, centroids)  # Cluster x Dim matrix
    
    centroids <- compute_reduced_centroids_cpp(mat = sketched_reduced, sketched_clusters = sketched_clusters, unique_clusters = unique_clusters)
    
    if(use_major_clusters) {
      label_transfer_df <- lapply(true_major_clusters, function(i) {
        curr_true_major_cluster_idx <- grep(i, unique_clusters)
        curr_true_major_cluster <- unique_clusters[curr_true_major_cluster_idx]
        curr_non_sketched_cells <- colnames(original_seu)[colnames(original_seu) %in% names(all_major_clusters[all_major_clusters == gsub("-$", "", i)])]
        curr_original_reduced <- original_reduced[curr_non_sketched_cells,]
        curr_centroids <- centroids[curr_true_major_cluster_idx, , drop = FALSE]
        curr_sim_scores <- cor(t(curr_original_reduced), t(curr_centroids))  # Cells x Clusters
        curr_predicted_labels <- curr_true_major_cluster[apply(curr_sim_scores, 1, which.max)]
        curr_confidence <- apply(curr_sim_scores, 1, max)  # Max correlation as confidence
        curr_label_transfer_df <- data.frame(cell = rownames(curr_original_reduced),
                                             predicted_cluster = curr_predicted_labels,
                                             confidence = curr_confidence)
        curr_label_transfer_df
      }) %>% do.call(what = rbind)
      label_transfer_df <- label_transfer_df[match(colnames(original_seu), label_transfer_df$cell),]
      predicted_labels <- label_transfer_df$predicted_cluster
    } else {
      # Pearson correlation to centroids (in reduced space)
      sim_scores <- cor(t(original_reduced), t(centroids))  # Cells x Clusters
      predicted_labels <- unique_clusters[apply(sim_scores, 1, which.max)]
      confidence <- apply(sim_scores, 1, max)  # Max correlation as confidence
      
      label_transfer_df <- data.frame(cell = colnames(original_seu),
                                      predicted_cluster = predicted_labels,
                                      confidence = confidence)
    }
    
    names(predicted_labels) <- colnames(original_seu)
    predicted_labels <- c(predicted_labels, sketched_clusters)
    
  } else if (method == "count-knn") {
    # kNN voting (using Seurat's framework)
    
    original_seu <- Seurat::CreateSeuratObject(counts = query_expr_mat[, !(colnames(query_expr_mat) %in% sketch_cells)])
    sketched_seu <- Seurat::CreateSeuratObject(counts = query_expr_mat[, (colnames(query_expr_mat) %in% sketch_cells)])
    
    original_seu <- Seurat::NormalizeData(original_seu, normalization.method = "LogNormalize", scale.factor = 10000, verbose = FALSE)
    sketched_seu <- Seurat::NormalizeData(sketched_seu, normalization.method = "LogNormalize", scale.factor = 10000, verbose = FALSE)
    
    original_seu <- Seurat::FindVariableFeatures(original_seu, verbose = FALSE)
    sketched_seu <- Seurat::FindVariableFeatures(sketched_seu, verbose = FALSE)
    
    if(use_major_clusters) {
      label_transfer_df <- lapply(true_major_clusters, function(i) {
        curr_true_major_cluster_idx <- grep(i, unique_clusters)
        curr_true_major_cluster <- unique_clusters[curr_true_major_cluster_idx]
        curr_non_sketched_cells <- colnames(original_seu)[colnames(original_seu) %in% names(all_major_clusters[all_major_clusters == gsub("-$", "", i)])]
        curr_original_seu <- original_seu[,curr_non_sketched_cells]
        if(ncol(curr_original_seu) < 70) {
          curr_k.score <- round(ncol(curr_original_seu)/2)
          curr_k.weight <- round(ncol(curr_original_seu)/2)
        } else {
          curr_k.score <- 30
          curr_k.weight <- 50
        }
        curr_anchors <- Seurat::FindTransferAnchors(reference = sketched_seu, query = curr_original_seu, k.score = curr_k.score,
                                                    dims = 1:dims, reduction = "pcaproject", verbose = FALSE)
        curr_predictions <- Seurat::TransferData(anchorset = curr_anchors, refdata = sketched_clusters[colnames(sketched_seu)], k.weight = curr_k.weight,
                                                 weight.reduction = "pcaproject", dims = 1:dims, verbose = FALSE)
        curr_predicted_labels <- curr_predictions$predicted.id
        curr_confidence <- curr_predictions$prediction.score.max
        curr_label_transfer_df <- data.frame(cell = colnames(curr_original_seu),
                                             predicted_cluster = curr_predicted_labels,
                                             confidence = curr_confidence)
        curr_label_transfer_df
      }) %>% do.call(what = rbind)
      label_transfer_df <- label_transfer_df[match(colnames(original_seu), label_transfer_df$cell),]
      predicted_labels <- label_transfer_df$predicted_cluster
    } else {
      if(ncol(original_seu) < 70) {
        curr_k.score <- round(ncol(original_seu)/2)
        curr_k.weight <- round(ncol(original_seu)/2)
      } else {
        curr_k.score <- 30
        curr_k.weight <- 50
      }
      anchors <- Seurat::FindTransferAnchors(reference = sketched_seu, query = original_seu, k.score = curr_k.score,
                                             dims = 1:dims, reduction = "pcaproject", verbose = FALSE)
      
      predictions <- Seurat::TransferData(anchorset = anchors, refdata = sketched_clusters, k.weight = curr_k.weight,
                                          weight.reduction = "pcaproject", dims = 1:dims, verbose = FALSE)
      
      predicted_labels <- predictions$predicted.id
      confidence <- predictions$prediction.score.max
      
      label_transfer_df <- data.frame(cell = colnames(original_seu),
                                      predicted_cluster = predicted_labels,
                                      confidence = confidence)
    }
    
    names(predicted_labels) <- colnames(original_seu)
    predicted_labels <- c(predicted_labels, sketched_clusters)
  }
  
  #______________
  
  predicted_quiescent <- predicted_labels[predicted_labels == "Quiescent"] %>% names()
  if(length(predicted_quiescent) > 0) {
    clustoCell$quiescent_cells <- unique(c(clustoCell$quiescent_cells, predicted_quiescent))
    predicted_labels <- predicted_labels[-which(predicted_labels == "Quiescent")]
  }
  
  if(any(grepl("merged_sub_clusters", names(clustoCell$clusters)))) {
    clustoCell$clusters$merged_sub_clusters <- predicted_labels
    clustoCell$clusters$major_clusters <- gsub("-Sub\\d+|-Isolated", "", predicted_labels)
    major_cl_category_names <- names(clustoCell$clusters$sub_clusters)
    clustoCell$clusters$sub_clusters <- 
      lapply(major_cl_category_names, function(tmp_cl) {
        curr_cl_name <- gsub("Subclusters", "", tmp_cl)
        predicted_labels[grepl(curr_cl_name, predicted_labels)]
      })
    names(clustoCell$clusters$sub_clusters) <- major_cl_category_names
  } else {
    clustoCell$clusters$major_clusters <- predicted_labels
  }
  
  if(length(clustoCell$isolated_cells) > 0) {
    clustoCell$isolated_cells <- NULL # Removing in the label transfer
  }
  
  clustoCell$label_transfer_df <- label_transfer_df
  
  # Return results
  structure(clustoCell,
            class = "ClustoCell")

}
