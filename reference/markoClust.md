# Evaluate and refine cell clusters using marker ranking and graph partitioning

Identifies markers in predefined cell clusters, and optionally
identifying sub-clusters using Leiden community detection. Supports
large datasets via uniform sketching with label transfer back to the
full dataset.

## Usage

``` r
markoClust(
  data,
  assay = "RNA",
  layer = "counts",
  norm_assay = "RNA",
  norm_layer = "data",
  cluster_labels,
  log1p = TRUE,
  remove_quiescent_cells = TRUE,
  high_quantile = 0.25,
  low_quantile = 0.25,
  subset_to_HVG = FALSE,
  hvg_selection.method = c("vst", "mean.var.plot", "dispersion"),
  hvg_var_thresh = 1,
  gini_thresh = 0.5,
  sketch = FALSE,
  sketch_fraction = 0.5,
  label_transfer_method = c("ewcsr-cor", "seurat-project", "ewcsr-red-cor", "seurat-knn"),
  sketch_pca_dims = 30,
  noise_feature_thresh = 4,
  random_marker_thresh = 5,
  mr_thresh = NULL,
  isolated_cluster_thresh = 5,
  leiden_obj_function = c("modularity", "CPM"),
  leiden_resolution = 0.75,
  leiden_n_iterations = 5,
  identify_subclusters = FALSE,
  num_threads = -1,
  seed = 121,
  verbose = TRUE
)
```

## Arguments

- data:

  Either a `Seurat` object or a numeric matrix with features (genes) as
  rows and cells as columns. Recommended to provide at least
  library-size normalized data when `subset_to_HVG = TRUE`.

- assay:

  Character; assay used for cluster evaluation.

- layer:

  Character; assay layer used for cluster evaluation.

- norm_assay:

  Character; assay containing normalized data for HVG detection.

- norm_layer:

  Character; normalized layer used for HVG detection.

- cluster_labels:

  Character vector. Either the name of the column in data@meta.data
  containing cluster labels, or a character vector of cluster labels
  with length equal to the number of columns (cells) in the \`data\`
  argument.

- log1p:

  Logical; whether to apply `log1p` transformation to the input `data`.
  It is recommended to set this argument to TRUE (default) if the data
  is not already on a log scale.

- remove_quiescent_cells:

  Logical; whether to remove quiescent cells prior to analysis.

- high_quantile:

  Numeric; EWCSR high quantile threshold.

- low_quantile:

  Numeric; EWCSR low quantile threshold.

- subset_to_HVG:

  Logical; whether to restrict analysis to highly variable genes (HVGs).

- hvg_selection.method:

  Character; HVG selection strategy.

- hvg_var_thresh:

  Numeric; HVG variance threshold.

- gini_thresh:

  Numeric; Gini coefficient threshold for detecting global markers.

- sketch:

  Logical; whether to apply uniform sketching for sub-clustering.

- sketch_fraction:

  Numeric between 0 and 1; fraction of cells per cluster to retain in
  sketch.

- label_transfer_method:

  Character; method for transferring labels from sketched to full data.
  One of `"ewcsr-cor"`, `"seurat-project"`, `"ewcsr-red-cor"`, or
  `"seurat-knn"`.

- sketch_pca_dims:

  Integer; number of PCA dimensions used during label transfer.

- noise_feature_thresh:

  Integer; threshold for identifying noise features.

- random_marker_thresh:

  Integer; threshold for discarding weak markers.

- mr_thresh:

  Numeric; mutual-rank threshold for filtering cell similarities.

- isolated_cluster_thresh:

  Integer; clusters with fewer cells are treated as isolated.

- leiden_obj_function:

  Character; Leiden objective function. One of `"modularity"` or
  `"CPM"`.

- leiden_resolution:

  Numeric; Leiden resolution parameter.

- leiden_n_iterations:

  Integer; number of Leiden iterations.

- identify_subclusters:

  Logical; whether to identify sub-clusters.

- num_threads:

  Integer; number of threads to use.

- seed:

  Integer; random seed.

- verbose:

  Logical; whether to display progress messages.

## Value

An object of class `"ClustoCell"` containing refined clusters,
sub-clusters, marker tables, and similarity structures.

## Details

If `sketch = TRUE`, a representative subset of cells is sampled from
each cluster and used for sub-clustering. Labels are transferred back to
the full dataset using the method specified by `label_transfer_method`.
The `"seurat-project"` and `"seurat-knn"` methods operate on the
original expression matrix rather than the EWCSR representation. The
supplied expression data may consist of raw counts, log-normalized
expression values, or SCTransform-normalized data, provided that the
sketched and full datasets were processed using the same normalization
strategy. These methods leverage Seurat's native label transfer
workflows
([`ProjectData`](https://satijalab.org/seurat/reference/ProjectData.html),
[`FindTransferAnchors`](https://satijalab.org/seurat/reference/FindTransferAnchors.html),
and
[`TransferData`](https://satijalab.org/seurat/reference/TransferData.html))
and do not require integer count matrices.

## See also

[`markoCell`](https://asalavaty.github.io/celliverse/reference/markoCell.md),
[`addClustoData`](https://asalavaty.github.io/celliverse/reference/addClustoData.md)

## Examples

``` r
utils::data("pbmc_small", package = "SeuratObject")

pbmc_small$example_clusters <- as.character(
  SeuratObject::Idents(pbmc_small)
)

cc <- markoClust(
  data = pbmc_small,
  cluster_labels = "example_clusters",
  identify_subclusters = FALSE,
  num_threads = 1,
  verbose = FALSE
)
```
