#' Add TypoClust cell type annotations to a single-cell object
#'
#' @description
#' Adds inferred cell type annotations from a \code{TypoClust} object to the
#' metadata of a Seurat or SingleCellExperiment object.
#'
#' @details
#' Cell type labels are assigned to specified cluster or subset columns and
#' appended as new metadata columns. Multiple ranked cell types can be added,
#' and hierarchical refinement can be applied to obtain more specific cell
#' type annotations.
#'
#' @param obj
#' An object of class \code{Seurat} or \code{SingleCellExperiment}.
#'
#' @param typoClust
#' An object of class \code{TypoClust}, generated using \code{typoClust()}.
#'
#' @param clusters
#' Character vector; names of metadata columns in \code{obj} defining clusters
#' or cell subsets to which cell types will be assigned.
#'
#' @param rank_thresh
#' Integer; the top N ranked cell types to add for each cluster, stored as
#' separate metadata columns.
#'
#' @param refine
#' Logical; whether to refine inferred cell types by traversing deeper levels
#' of the cell type hierarchy.
#'
#' @param refine_thresh
#' Integer; depth of (lexical) hierarchical traversal for refinement. Ignored if
#' \code{refine = FALSE}.
#'
#' @param outNames
#' Character vector; names of output metadata columns. If \code{NULL}, defaults
#' to \code{paste0(clusters, "_Celltype")}.
#'
#' @return
#' The input object \code{obj} with additional metadata columns containing
#' inferred cell type annotations.
#'
#' @seealso
#' \code{\link{typoClust}}, \code{\link{typoClustVis}}
#'
#' @examples
#' \dontrun{
#' so <- addTypoData(
#'   obj = so,
#'   typoClust = tc,
#'   clusters = "ClustoCell_Clusters"
#' )
#' }
#'
#' @export

addTypoData <- function(
    obj, # An object of class Seurat or SingleCellExperiment (SCE)
    typoClust, # An object of class TypoClust generated via `typoClust` function.
    clusters, # Character vector, the names of the metadata columns in the provided `obj` that contain the cell subset or cluster names to which you want to assign corresponding cell types.
    rank_thresh = 1, # Integer, the top N ranked cell types for each cluster will be added as N separate columns in the metadata.
    refine = TRUE, # Logical, whether to refine the cell types by going further down the cell type table to find a more specific match for the N-ranked cell type.
    refine_thresh = 1, # Integer, specifies how deep to traverse the hierarchy of cell subtypes. Ignored if refine = FALSE.
    outNames = NULL # Character, the names of the metadata column where the cell types corresponding to the provided clusters will be added. If NULL, the column names will default to the names of the clusters column with '_Celltype' appended.
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
    cli::cli_alert_info(...)
  }
  
  #________________________________________
  
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
  
  if(!inherits(typoClust, "TypoClust")) {
    cli::cli_abort("The provided `typoClust` is of the wrong class. It should be a 'TypoClust' object, created using the typoClust function!")
  }
  
  if(!is.null(outNames) & (length(outNames) != length(clusters))) {
    cli::cli_abort("The length of the `outNames` vector must match the length of the provided `clusters` argument!")
  }
  
  if(rank_thresh < 1) {
    cli::cli_abort("The `rank_thresh` argument must be an integer greater than or equal to one!")
  }
  
  if(refine & refine_thresh < 1) {
    cli::cli_abort("The `refine_thresh` argument must be an integer greater than or equal to one!")
  }
  
  #______________________
  
  # Defining the cell type column names
  
  if(is.null(outNames)) {
    cellType_col_names <- paste(clusters, "_Celltype", sep = "")
  } else {
    cellType_col_names <- outNames
  }
  names(cellType_col_names) <- clusters
  
  #______________________
  
  # A helper function
  metaAdder <- function(obj, typoClust, named_colnames) {
    for (curr_col in seq_along(named_colnames)) {
      curr_clusters_colname <- names(named_colnames)[curr_col]
      curr_cellType_colname <- named_colnames[curr_col]
      
      for (curr_rank in seq_len(rank_thresh)) {
        final_cellType_colname <- paste0(curr_cellType_colname, "_R", curr_rank)
        
        if (inherits(obj, "Seurat")) {
          cur_clusters_names <- intersect(
            unique(obj@meta.data[[curr_clusters_colname]]),
            names(typoClust$cell_types)
          )
          
          tmp_cluster_names <- unique(obj@meta.data[[curr_clusters_colname]])
          if(any(!(tmp_cluster_names %in% names(typoClust$cell_types)))) {
            missing_tmp_cluster_names <- tmp_cluster_names[which(!(tmp_cluster_names %in% names(typoClust$cell_types)))]
            missing_tmp_cluster_names <- missing_tmp_cluster_names[!(missing_tmp_cluster_names %in% c("Quiescent", "Isolated"))]
            if(length(missing_tmp_cluster_names) > 0) {
              log_message(paste0("The following cluster names are not available in the specified `typoClust` object and their corresponding cell types will be set to NA!\n  WARNING: ",
                                 paste0(missing_tmp_cluster_names, collapse = "\n  WARNING: ")))
            }
          }
          
          obj@meta.data[[final_cellType_colname]] <- NA
          obj@meta.data[[final_cellType_colname]][obj@meta.data[[curr_clusters_colname]] == "Quiescent"] <- "Quiescent"
          obj@meta.data[[final_cellType_colname]][obj@meta.data[[curr_clusters_colname]] == "Isolated"] <- "Isolated"
          
          if(refine) {
            final_refined_cellType_colnames <- paste0(final_cellType_colname, "_L", seq_len(refine_thresh))
            
            for(curr_name in final_refined_cellType_colnames) {
              obj@meta.data[[curr_name]] <- NA
              obj@meta.data[[curr_name]][obj@meta.data[[curr_clusters_colname]] == "Quiescent"] <- "Quiescent"
              obj@meta.data[[curr_name]][obj@meta.data[[curr_clusters_colname]] == "Isolated"] <- "Isolated"
            }
          }

          for (cluster_name in cur_clusters_names) {
            celltype_val <- tryCatch(
              typoClust$cell_types[[as.character(cluster_name)]][curr_rank, "CellType"],
              error = function(e) NA
            )
            if (!is.null(celltype_val) && length(celltype_val) > 0) {
              obj@meta.data[[final_cellType_colname]][obj@meta.data[[curr_clusters_colname]] == cluster_name] <- celltype_val
              
              if(refine) {
                # Set the initial values for refining
                curr_refine_index <- curr_rank
                curr_refined_cellType <- celltype_val
                final_refined_cellType <- celltype_val
                
                for (curr_refine in seq_len(refine_thresh)) {
                  curr_refined_cellType <- grep(paste0(c(paste0(gsub("\\+", "\\\\+", (if(!is.na(curr_refined_cellType) & curr_refined_cellType == "Mononuclear Phagocyte") {"Monocyte|Macrophage|Dendritic Cell"} else {curr_refined_cellType})), "$"),
                                                         paste0(gsub("\\+", "\\\\+", (if(!is.na(curr_refined_cellType) & curr_refined_cellType == "Mononuclear Phagocyte") {"Monocyte|Macrophage|Dendritic Cell"} else {curr_refined_cellType})), " "),
                                                         paste0(" ", gsub("\\+", "\\\\+", (if(!is.na(curr_refined_cellType) & curr_refined_cellType == "Mononuclear Phagocyte") {"Monocyte|Macrophage|Dendritic Cell"} else {curr_refined_cellType})))), 
                                                       collapse = "|"),
                                                typoClust$cell_types[[as.character(cluster_name)]][(curr_refine_index + 1):nrow(typoClust$cell_types[[as.character(cluster_name)]]), "CellType"],
                                                value = TRUE)[1]
                  
                  if(!is.na(curr_refined_cellType) & !grepl(gsub("\\+", "\\\\+", curr_refined_cellType), final_refined_cellType)) {
                    final_refined_cellType <- paste0(final_refined_cellType, " -> ", curr_refined_cellType)
                    
                    curr_refine_index <- curr_refine_index + grep(paste0(c(paste0(gsub("\\+", "\\\\+", curr_refined_cellType), "$"),
                                                                           paste0(gsub("\\+", "\\\\+", curr_refined_cellType), " "),
                                                                           paste0(" ", gsub("\\+", "\\\\+", curr_refined_cellType))), 
                                                                         collapse = "|"),
                                                                  typoClust$cell_types[[as.character(cluster_name)]][(curr_refine_index + 1):nrow(typoClust$cell_types[[as.character(cluster_name)]]), "CellType"])[1]
                    
                    obj@meta.data[[final_refined_cellType_colnames[curr_refine]]][obj@meta.data[[curr_clusters_colname]] == cluster_name] <- final_refined_cellType
                  } else {
                    obj@meta.data[[final_refined_cellType_colnames[curr_refine]]][obj@meta.data[[curr_clusters_colname]] == cluster_name] <- final_refined_cellType
                  }
                }
              }
            }
          }
        } else if(inherits(obj, "SingleCellExperiment")) {
          
          cur_clusters_names <- intersect(
            unique(SummarizedExperiment::colData(obj)[[curr_clusters_colname]]),
            names(typoClust$cell_types)
          )
          
          tmp_cluster_names <- unique(SummarizedExperiment::colData(obj)[[curr_clusters_colname]])
          if(any(!(tmp_cluster_names %in% names(typoClust$cell_types)))) {
            missing_tmp_cluster_names <- tmp_cluster_names[which(!(tmp_cluster_names %in% names(typoClust$cell_types)))]
            missing_tmp_cluster_names <- missing_tmp_cluster_names[!(missing_tmp_cluster_names %in% c("Quiescent", "Isolated"))]
            if(length(missing_tmp_cluster_names) > 0) {
              log_message(paste0("The following cluster names are not available in the specified `typoClust` object and their corresponding cell types will be set to NA!\n  WARNING: ",
                                 paste0(missing_tmp_cluster_names, collapse = "\n  WARNING: ")))
            }
          }
          
          SummarizedExperiment::colData(obj)[[final_cellType_colname]] <- NA
          SummarizedExperiment::colData(obj)[[final_cellType_colname]][SummarizedExperiment::colData(obj)[[curr_clusters_colname]] == "Quiescent"] <- "Quiescent"
          SummarizedExperiment::colData(obj)[[final_cellType_colname]][SummarizedExperiment::colData(obj)[[curr_clusters_colname]] == "Isolated"] <- "Isolated"
          
          if(refine) {
            final_refined_cellType_colnames <- paste0(final_cellType_colname, "_L", seq_len(refine_thresh))
            
            for(curr_name in final_refined_cellType_colnames) {
              SummarizedExperiment::colData(obj)[[curr_name]] <- NA
              SummarizedExperiment::colData(obj)[[curr_name]][SummarizedExperiment::colData(obj)[[curr_clusters_colname]] == "Quiescent"] <- "Quiescent"
              SummarizedExperiment::colData(obj)[[curr_name]][SummarizedExperiment::colData(obj)[[curr_clusters_colname]] == "Isolated"] <- "Isolated"
            }
          }
          for (cluster_name in cur_clusters_names) {
            celltype_val <- tryCatch(
              typoClust$cell_types[[as.character(cluster_name)]][curr_rank, "CellType"],
              error = function(e) NA
            )
            if (!is.null(celltype_val) && length(celltype_val) > 0) {
              SummarizedExperiment::colData(obj)[[final_cellType_colname]][SummarizedExperiment::colData(obj)[[curr_clusters_colname]] == cluster_name] <- celltype_val
              
              if(refine) {
                # Set the initial values for refining
                curr_refine_index <- curr_rank
                curr_refined_cellType <- celltype_val
                final_refined_cellType <- celltype_val
                
                for (curr_refine in seq_len(refine_thresh)) {
                  curr_refined_cellType <- grep(paste0(c(paste0(gsub("\\+", "\\\\+", (if(!is.na(curr_refined_cellType) & curr_refined_cellType == "Mononuclear Phagocyte") {"Monocyte|Macrophage|Dendritic Cell"} else {curr_refined_cellType})), "$"),
                                                         paste0(gsub("\\+", "\\\\+", (if(!is.na(curr_refined_cellType) & curr_refined_cellType == "Mononuclear Phagocyte") {"Monocyte|Macrophage|Dendritic Cell"} else {curr_refined_cellType})), " "),
                                                         paste0(" ", gsub("\\+", "\\\\+", (if(!is.na(curr_refined_cellType) & curr_refined_cellType == "Mononuclear Phagocyte") {"Monocyte|Macrophage|Dendritic Cell"} else {curr_refined_cellType})))), 
                                                       collapse = "|"),
                                                typoClust$cell_types[[as.character(cluster_name)]][(curr_refine_index + 1):nrow(typoClust$cell_types[[as.character(cluster_name)]]), "CellType"],
                                                value = TRUE)[1]
                  
                  if(!is.na(curr_refined_cellType) & !grepl(gsub("\\+", "\\\\+", curr_refined_cellType), final_refined_cellType)) {
                    final_refined_cellType <- paste0(final_refined_cellType, " -> ", curr_refined_cellType)
                    
                    curr_refine_index <- curr_refine_index + grep(paste0(c(paste0(gsub("\\+", "\\\\+", curr_refined_cellType), "$"),
                                                                           paste0(gsub("\\+", "\\\\+", curr_refined_cellType), " "),
                                                                           paste0(" ", gsub("\\+", "\\\\+", curr_refined_cellType))), 
                                                                         collapse = "|"),
                                                                  typoClust$cell_types[[as.character(cluster_name)]][(curr_refine_index + 1):nrow(typoClust$cell_types[[as.character(cluster_name)]]), "CellType"])[1]
                    
                    SummarizedExperiment::colData(obj)[[final_refined_cellType_colnames[curr_refine]]][SummarizedExperiment::colData(obj)[[curr_clusters_colname]] == cluster_name] <- final_refined_cellType
                  } else {
                    SummarizedExperiment::colData(obj)[[final_refined_cellType_colnames[curr_refine]]][SummarizedExperiment::colData(obj)[[curr_clusters_colname]] == cluster_name] <- final_refined_cellType
                  }
                }
              }
            }
          }
        }
      }
    }
    obj
  }
  #______________________
  
  # Adding celltype labels
  obj <- metaAdder(obj = obj, typoClust = typoClust, named_colnames = cellType_col_names)
  
  #______________________
  
  return(obj)
}
