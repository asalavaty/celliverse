#=============================================================================
#
#    markerDB
#
#=============================================================================

#' Cell-type marker database
#'
#' A curated database of positive and negative cell-type marker genes for
#' human and mouse, stored in sparse matrix format. This dataset is used
#' internally by \code{typoClust()} for marker-based cell-type annotation.
#'
#' @format
#' An object of class \code{CelliVerse_Data}, implemented as a named list with
#' two elements:
#' \describe{
#'   \item{human}{A list containing human marker databases:
#'     \describe{
#'       \item{positive_db}{An object of class \code{CelliVerse_Sparse_Data}
#'       wrapping a sparse \code{dgCMatrix} of dimensions
#'       16,899 genes × 9,410 cell types.}
#'       \item{negative_db}{An object of class \code{CelliVerse_Sparse_Data}
#'       wrapping a sparse \code{dgCMatrix} of dimensions
#'       329 genes × 93 cell types.}
#'     }}
#'   \item{mouse}{A list containing mouse marker databases:
#'     \describe{
#'       \item{positive_db}{An object of class \code{CelliVerse_Sparse_Data}
#'       wrapping a sparse \code{dgCMatrix} of dimensions
#'       4,779 genes × 2,112 cell types.}
#'       \item{negative_db}{An object of class \code{CelliVerse_Sparse_Data}
#'       wrapping a sparse \code{dgCMatrix} of dimensions
#'       32 genes × 33 cell types.}
#'     }}
#' }
#'
#' @details
#' Rows correspond to marker genes and columns correspond to annotated
#' cell types. Matrix values encode marker presence or strength as defined
#' during database construction. Sparse representation is used to minimize
#' memory usage.
#' 
#' @keywords datasets
#'
#' @name markerDB
#' @docType data
#'
#' @seealso
#' \code{\link{typoClust}}, \code{\link{markerDictionary}},
#' \code{\link{tissueCondition_types}}
#'
#' @source
#' Curated from published cell-type marker resources and expert annotation.
NULL


#=============================================================================
#
#    markerDictionary
#
#=============================================================================

#' Marker gene dictionary
#'
#' A species-specific dictionary mapping marker identifiers to standardized
#' gene annotations, including gene symbols, aliases, and database identifiers.
#' This dataset supports marker harmonization and identifier resolution within
#' the \code{celliverse} framework.
#'
#' @format
#' An object of class \code{CelliVerse_Data}, implemented as a named list with
#' two elements:
#' \describe{
#'   \item{human}{A data frame with 20,887 rows and 6 variables:
#'     \describe{
#'       \item{Marker}{Internal marker identifier}
#'       \item{Symbol}{Official gene symbol}
#'       \item{Alias}{Alternative gene symbols or aliases}
#'       \item{Entrez}{Entrez Gene identifier}
#'       \item{Ensembl}{Ensembl gene identifier}
#'       \item{UniProt}{UniProt protein identifier}
#'     }}
#'   \item{mouse}{A data frame with 4,779 rows and 6 variables:
#'     \describe{
#'       \item{Marker}{Internal marker identifier}
#'       \item{Symbol}{Official gene symbol}
#'       \item{Alias}{Alternative gene symbols or aliases}
#'       \item{Entrez}{Entrez Gene identifier}
#'       \item{Ensembl}{Ensembl gene identifier}
#'       \item{UniProt}{UniProt protein identifier}
#'     }}
#' }
#'
#' @details
#' This dictionary is used to standardize marker gene identifiers across
#' datasets and species, enabling consistent matching between user-provided
#' markers and curated cell-type marker databases.
#' 
#' @keywords datasets
#'
#' @name markerDictionary
#' @docType data
#'
#' @seealso
#' \code{\link{markerDB}}, \code{\link{typoClust}}
#'
#' @source
#' Integrated from public gene annotation resources including Ensembl,
#' Entrez Gene, and UniProt.
NULL


#=============================================================================
#
#    tissueCondition_types
#
#=============================================================================

#' Tissue and condition reference catalog
#'
#' A reference catalog of tissues and disease conditions for human and mouse,
#' used to contextualize cell-type annotation by tissue and pathological state.
#'
#' @format
#' An object of class \code{CelliVerse_Data}, implemented as a named list with
#' two elements:
#' \describe{
#'   \item{human}{A list containing:
#'     \describe{
#'       \item{all_tissues}{Character vector of all supported tissues (length 396)}
#'       \item{healthy_tissue}{Character vector of healthy tissues (length 367)}
#'       \item{diseased_tissue}{Character vector of diseased tissues (length 103)}
#'       \item{all_conditions}{Character vector of all supported conditions (length 265)}
#'       \item{diseased_tissueCondition}{A data frame with two variables:
#'         \describe{
#'           \item{Tissue}{Tissue name}
#'           \item{Condition}{Associated disease condition}
#'         }}
#'     }}
#'   \item{mouse}{A list containing:
#'     \describe{
#'       \item{all_tissues}{Character vector of all supported tissues (length 110)}
#'       \item{healthy_tissue}{Character vector of healthy tissues (length 108)}
#'       \item{diseased_tissue}{Character vector of diseased tissues (length 19)}
#'       \item{all_conditions}{Character vector of all supported conditions (length 51)}
#'       \item{diseased_tissueCondition}{A data frame with two variables:
#'         \describe{
#'           \item{Tissue}{Tissue name}
#'           \item{Condition}{Associated disease condition}
#'         }}
#'     }}
#' }
#'
#' @details
#' This dataset is used by \code{typoClust()} to restrict or guide cell-type
#' annotation according to tissue context and disease state.
#' 
#' @keywords datasets
#'
#' @name tissueCondition_types
#' @docType data
#'
#' @seealso
#' \code{\link{typoClust}}, \code{\link{markerDB}}
#'
#' @source
#' Curated from public tissue ontologies and disease annotation resources.
NULL

#=============================================================================
#
#    pbmc3k
#
#=============================================================================

#' PBMC 3k single-cell RNA-seq dataset
#'
#' A standard peripheral blood mononuclear cell (PBMC) single-cell RNA-seq
#' dataset consisting of approximately 3,000 cells profiled using the
#' 10x Genomics platform. The dataset is provided as a sparse gene-by-cell
#' expression matrix and is intended for demonstration, benchmarking, and
#' vignette examples within \pkg{celliverse}.
#'
#' @format
#' A sparse matrix of class \code{dgCMatrix} with:
#' \describe{
#'   \item{rows}{13,714 genes}
#'   \item{columns}{2,700 cells}
#' }
#'
#' @details
#' Rows correspond to genes and columns correspond to individual cells.
#' Matrix entries represent UMI counts. The sparse representation minimizes
#' memory usage and enables efficient downstream analysis.
#'
#' This dataset is commonly used in single-cell analysis workflows and is
#' included here as a lightweight example for illustrating the functionality
#' of \pkg{celliverse} without requiring external data downloads.
#'
#' @keywords datasets
#'
#' @name pbmc3k
#' @docType data
#'
#' @seealso
#' \code{\link{ClustoCell}}, \code{\link{typoClust}}
#'
#' @source
#' Originally distributed via the \pkg{SeuratData} package and derived from
#' 10x Genomics PBMC 3k data.
NULL

