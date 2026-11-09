#' Add ClustoCell cluster annotations to a Seurat or SingleCellExperiment object
#'
#' @description
#' Adds major cluster and/or sub-cluster labels stored in a \code{ClustoCell}
#' object to the cell-level metadata of a Seurat or SingleCellExperiment object.
#'
#' @details
#' This function transfers clustering results obtained using \code{clustoCell()}
#' or \code{markoClust()} into an existing single-cell object by appending
#' cluster labels as metadata columns. Major clusters and sub-clusters can be
#' added independently and assigned custom column names.
#'
#' @param obj
#' An object of class \code{Seurat} or \code{SingleCellExperiment}.
#'
#' @param clustoCell
#' An object of class \code{ClustoCell}, generated via \code{clustoCell()} or
#' \code{markoClust()}.
#'
#' @param add_major_clusters
#' Logical; whether to add major cluster labels to the metadata of \code{obj}.
#'
#' @param add_sub_clusters
#' Logical; whether to add sub-cluster labels to the metadata of \code{obj}.
#'
#' @param major_cluster_name
#' Character; name of the metadata column to store major cluster labels.
#'
#' @param sub_cluster_name
#' Character; name of the metadata column to store sub-cluster labels.
#'
#' @return
#' The input object \code{obj} with additional metadata columns containing
#' ClustoCell cluster annotations.
#'
#' @seealso
#' \code{\link{clustoCell}}, \code{\link{markoClust}}
#'
#' @examples
#' utils::data("pbmc_small", package = "SeuratObject")
#'
#' pbmc_small_cc <- clustoCell(
#'   data = pbmc_small,
#'   identify_subclusters = TRUE,
#'   num_threads = 1,
#'   verbose = FALSE
#' )
#'
#' pbmc_small <- addClustoData(
#'   obj = pbmc_small,
#'   clustoCell = pbmc_small_cc,
#'   add_major_clusters = TRUE,
#'   add_sub_clusters = TRUE
#' )
#' 
#' @export

addClustoData <- function(
    obj, # An object of class Seurat or SingleCellExperiment (SCE)
    clustoCell, # An object of class generated via either clustoCell or markoClust function.
    add_major_clusters = TRUE, # Logical, whether to add major cluster labels to the metadata of the obj or not.
    add_sub_clusters = TRUE, # Logical, whether to add sub-cluster labels to the metadata of the obj or not.
    major_cluster_name = "ClustoCell_Clusters", # Character, the name of the column to be added to the metadata corresponding to the major cluster labels
    sub_cluster_name = "ClustoCell_SubClusters" # Character, the name of the column to be added to the metadata corresponding to the sub-cluster labels
) {
  
  # Performing initial checks
  
  if(!inherits(obj, c("Seurat", "SingleCellExperiment"))) {
    cli::cli_abort("The provided `obj` is of the wrong class. It should be a 'Seurat' or 'SingleCellExperiment' object!")
  }
  
  if(inherits(obj, "SingleCellExperiment")) {
    if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
      cli::cli_abort("The 'SummarizedExperiment' package is required but not installed.\n",
                     "You can install it using:\n",
                     "  if (!requireNamespace(\"BiocManager\", quietly = TRUE)) install.packages(\"BiocManager\")\n",
                     "  BiocManager::install(\"SummarizedExperiment\")")
    }
  }
  
  if(!inherits(clustoCell, "ClustoCell")) {
    cli::cli_abort("The provided `clustoCell` is of the wrong class. It should be a 'ClustoCell' object, created using either the clustoCell or markoClust function!")
  }
  
  #________________________________________
  
  # A helper functions
  
  convert_to_factor <- function(x) {
    # Keep NAs as they are
    non_na <- x[!is.na(x)]
    
    # Separate special labels first
    special_labels <- non_na[non_na %in% c("Quiescent", "Isolated")]
    cluster_like <- non_na[!non_na %in% c("Quiescent", "Isolated")]
    
    is_subcluster <- any(grepl("-", cluster_like))
    
    if (length(cluster_like) > 0) {
      if (is_subcluster) {
        # Extract major and subcluster numbers safely
        df <- data.frame(
          original = cluster_like,
          stringsAsFactors = FALSE
        )
        df$major <- as.numeric(sub("C([0-9]+)-Sub[0-9]+", "\\1", df$original))
        df$sub   <- as.numeric(sub("C[0-9]+-Sub([0-9]+)", "\\1", df$original))
        
        # Order by major cluster, then subcluster
        df <- df[order(df$major, df$sub), , drop = FALSE]
        
        levels <- unique(c(df$original, sort(special_labels)))
        
      } else {
        # Only major clusters
        df <- data.frame(
          original = cluster_like,
          stringsAsFactors = FALSE
        )
        df$major <- as.numeric(sub("C", "", df$original))
        
        df <- df[order(df$major), , drop = FALSE]
        
        levels <- unique(c(df$original, sort(special_labels)))
      }
    } else {
      # No cluster-like values, only special labels
      levels <- sort(unique(special_labels))
    }
    
    factor(x, levels = levels)
  }
  
  #__________
  
  metaAdder <- function(obj, clustoCell, cluster_type, name) {
    if(inherits(obj, "Seurat")) {
      obj@meta.data[[name]] <- NA
      obj@meta.data[[name]][match(clustoCell$quiescent_cells, colnames(obj))] <- "Quiescent"
      obj@meta.data[[name]][match(clustoCell$isolated_cells, colnames(obj))] <- "Isolated"
      obj@meta.data[[name]][match(names(clustoCell$clusters[[cluster_type]]), colnames(obj))] <- clustoCell$clusters[[cluster_type]]
      
      obj@meta.data[[name]] <- convert_to_factor(obj@meta.data[[name]])
      
    } else if(inherits(obj, "SingleCellExperiment")) {
      SummarizedExperiment::colData(obj)[[name]] <- NA
      SummarizedExperiment::colData(obj)[[name]][match(clustoCell$quiescent_cells, colnames(obj))] <- "Quiescent"
      SummarizedExperiment::colData(obj)[[name]][match(clustoCell$isolated_cells, colnames(obj))] <- "Isolated"
      SummarizedExperiment::colData(obj)[[name]][match(names(clustoCell$clusters[[cluster_type]]), colnames(obj))] <- clustoCell$clusters[[cluster_type]]
      
      SummarizedExperiment::colData(obj)[[name]] <- convert_to_factor(obj@meta.data[[name]])
    }
    return(obj)
  }
  
  #______________________
  
  # Adding major cluster labels
  if(add_major_clusters) {
    obj <- metaAdder(obj = obj, clustoCell = clustoCell, cluster_type = "major_clusters", name = major_cluster_name)
  }
  
  #______________________
  
  # Adding sub_cluster labels
  if(add_sub_clusters) {
    obj <- metaAdder(obj = obj, clustoCell = clustoCell, cluster_type = "merged_sub_clusters", name = sub_cluster_name)
  }
  
  #______________________
  
  return(obj)
}
