library(Matrix)
library(igraph)
library(cli)
library(data.table)
library(dplyr)
library(magrittr)

# Similarity analysis between clusters/cell subsets with each other

clustoSim <- function(
    so = NULL, # A Seurat object
    assay = "RNA", # The assay we want to use for assessing marker purities.
    layer = "counts", # The layer of the assay we want to use for assessing marker purities (this can be a normalized layer).
    data = NULL, # It is recommended to input normalized data (at least lib size normalized) if you have set the subset_to_HVG = TRUE.
    desired_markers = NULL, # A character vector of the names of desired markers. If NULL, the purity of all features of the input data in each desired cluster and the desired cells will be assessed. 
    desired_clusters = NULL, # A character vector of the names of desired clusters. If NULL, the purity of desired markers will be assessed in all clusters. 
    desired_cells = NULL, # A character vector of the names of desired cells from the column names of the input data. If NULL, the purity of desired markers will be assessed in only the desired_clusters.
    log1p = TRUE, # Weather to log1p transform the data or not
    high_quantile = 0.25, # The quantile threshold for choosing highly positive z-scores required for filtering the data and for selecting the positive markers. Higher values label more features as features with high z-scores.
    low_quantile = 0.25, # The quantile threshold for choosing highly negative z-scores required for filtering the data and for selecting the negative markers. Lower values label more features as features with low z-scores.
    network = TRUE,
    heatmap = TRUE,
    sankey = TRUE,
    verbose = TRUE
) {
  
}