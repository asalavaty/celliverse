# A function for adding the major cluster and sub-cluster labels from an object of class ClustoCell to a Seurat object

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
  # Dealing with warnings
  ## Save current warning setting and disable warnings
  old_warn <- getOption("warn")
  options(warn = -1)   # -1 = suppress all warnings
  
  on.exit(options(warn = old_warn), add = TRUE)  # restore when function exits
  
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
