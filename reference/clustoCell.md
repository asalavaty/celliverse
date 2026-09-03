# Clustering and marker discovery for single-cell data using EWCSR-based similarity

Performs unsupervised clustering of single-cell data using
expression-weighted centered scaled ranks (EWCSR), followed by
identification of cluster-specific markers and optional sub-clustering.
The function supports direct clustering on full datasets or scalable
analysis via data sketching with subsequent label transfer to the full
dataset.

## Usage

``` r
clustoCell(
  data,
  assay = "RNA",
  layer = "counts",
  norm_assay = "RNA",
  norm_layer = "data",
  log1p = TRUE,
  subset_to_HVG = FALSE,
  hvg_selection.method = c("vst", "mean.var.plot", "dispersion"),
  hvg_var_thresh = 1,
  high_quantile = 0.25,
  low_quantile = 0.25,
  gini_thresh = 0.5,
  identify_subclusters = TRUE,
  sketch = FALSE,
  sketch_ncells = 5000L,
  label_transfer_method = c("ewcsr-cor", "seurat-project", "ewcsr-red-cor", "seurat-knn"),
  sketch_pca_dims = 30,
  refine_transferred_subClusters = FALSE,
  noise_feature_thresh = 4,
  random_marker_thresh = 5,
  mr_thresh = NULL,
  isolated_cluster_thresh = 5,
  leiden_obj_function = c("modularity", "CPM"),
  leiden_resolution = 1,
  leiden_n_iterations = 5,
  subcluster_resolution_weight = 0.75,
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

  Character string specifying the assay to use for clustering.

- layer:

  Character string specifying the layer of `assay` to use (e.g.,
  `"counts"` or a normalized layer).

- norm_assay:

  Character string specifying the assay used for selecting highly
  variable genes (HVGs).

- norm_layer:

  Character string specifying the normalized layer used for HVG
  detection.

- log1p:

  Logical; whether to apply `log1p` transformation to the input `data`.
  It is recommended to set this argument to TRUE (default) if the data
  is not already on a log scale.

- subset_to_HVG:

  Logical; whether to restrict the analysis to highly variable genes
  (HVGs) or use all features.

- hvg_selection.method:

  Character string specifying the HVG selection strategy. One of
  `"vst"`, `"mean.var.plot"`, or `"dispersion"`.

- hvg_var_thresh:

  Numeric; variance threshold (in standard deviations above expected
  technical noise) for selecting HVGs.

- high_quantile:

  Numeric; upper quantile threshold used for identifying highly positive
  EWCSR values.

- low_quantile:

  Numeric; lower quantile threshold used for identifying highly negative
  EWCSR values.

- gini_thresh:

  Numeric; Gini coefficient threshold for identifying non-specific
  (global) markers.

- identify_subclusters:

  Logical; whether to perform sub-clustering within major clusters.

- sketch:

  Logical; whether to apply Seurat's uniform sketching strategy to
  subsample cells prior to clustering. Recommended for very large
  datasets.

- sketch_ncells:

  Integer; number of cells to sample during sketching. Must be smaller
  than the total number of cells in the dataset.

- label_transfer_method:

  Character string specifying the strategy used to transfer cluster
  labels from the sketched dataset back to the full dataset. Only used
  when `sketch = TRUE`. Options include:

  - `"ewcsr-cor"`: Transfers labels by computing correlations between
    EWCSR profiles of query cells and EWCSR centroids of sketched
    clusters in the full feature space.

  - `"seurat-project"`: Uses Seurat's `ProjectData()` workflow to
    transfer labels by projecting the full expression dataset (raw
    counts, log-normalized, or SCT-normalized) onto the low-dimensional
    embedding learned from the sketched dataset.

  - `"ewcsr-red-cor"`: Similar to `"ewcsr-cor"`, but correlations are
    computed in a reduced dimensional space (PCA embedding).

  - `"seurat-knn"`: Uses Seurat's `FindTransferAnchors()` and
    `TransferData()` workflow to transfer labels from the sketched
    dataset to the full dataset using nearest-neighbor matching.
    Supports standard Seurat normalization workflows (e.g., LogNormalize
    and SCTransform).

- sketch_pca_dims:

  Integer; number of PCA dimensions used during label transfer. Only
  applicable for `"ewcsr-red-cor"` and `"seurat-knn"`.

- refine_transferred_subClusters:

  Logical; whether to re-evaluate and refine transferred sub-cluster
  labels after label transfer.

- noise_feature_thresh:

  Integer; features expressed in fewer than this number of cells are
  treated as noise and excluded.

- random_marker_thresh:

  Integer; markers detected in fewer than this number of cells are
  discarded.

- mr_thresh:

  Numeric; threshold applied to mutual rank similarity. If `NULL`,
  defaults to `sqrt(number of cells)`.

- isolated_cluster_thresh:

  Integer; clusters with fewer than this number of cells are treated as
  isolated.

- leiden_obj_function:

  Character string specifying the Leiden objective function. One of
  `"modularity"` or `"CPM"`.

- leiden_resolution:

  Numeric; resolution parameter controlling cluster granularity.

- leiden_n_iterations:

  Integer; number of Leiden algorithm iterations.

- subcluster_resolution_weight:

  Numeric; multiplier applied to `leiden_resolution` for sub-cluster
  detection.

- num_threads:

  Integer; number of CPU threads to use. Default `-1` uses all available
  cores.

- seed:

  Integer; random seed for reproducibility.

- verbose:

  Logical; whether to print progress messages.

## Value

An object of class `ClustoCell` containing:

- major and sub-cluster assignments

- EWCSR-transformed data

- cluster- and subcluster-specific marker tables

- similarity and mutual-rank matrices

## Details

If `sketch = TRUE`, a representative subset of cells is sampled from the
input data and used for clustering. Labels are transferred back to the
full dataset using the method specified by `label_transfer_method`. The
`"seurat-project"` and `"seurat-knn"` methods operate on the original
expression matrix rather than the EWCSR representation. The supplied
expression data may consist of raw counts, log-normalized expression
values, or SCTransform-normalized data, provided that the sketched and
full datasets were processed using the same normalization strategy.
These methods leverage Seurat's native label transfer workflows
([`ProjectData`](https://satijalab.org/seurat/reference/ProjectData.html),
[`FindTransferAnchors`](https://satijalab.org/seurat/reference/FindTransferAnchors.html),
and
[`TransferData`](https://satijalab.org/seurat/reference/TransferData.html))
and do not require integer count matrices.

## See also

[`typoClust`](https://asalavaty.github.io/celliverse/reference/typoClust.md),
[`markoClust`](https://asalavaty.github.io/celliverse/reference/markoClust.md)

## Examples

``` r
utils::data("pbmc_small", package = "SeuratObject")

cc <- clustoCell(
  data = pbmc_small,
  identify_subclusters = FALSE,
  num_threads = 1,
  verbose = FALSE
)
```
