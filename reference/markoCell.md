# Rank markers for clusters, cell subsets, or individual cells

Identifies and ranks positive and negative marker genes for a specified
set of cells, which may correspond to clusters, sub-clusters, arbitrary
cell subsets, or even single cells. Marker ranking is based on
expression-weighted centered scaled ranks (EWCSR), with Gini-based
specificity assessment, and noise suppression.

## Usage

``` r
markoCell(
  data,
  assay = "RNA",
  layer = "counts",
  norm_assay = "RNA",
  norm_layer = "data",
  cluster_labels = NULL,
  desired_clusters = NULL,
  desired_cells = NULL,
  log1p = TRUE,
  remove_quiescent_cells = TRUE,
  high_quantile = 0.25,
  low_quantile = 0.25,
  subset_to_HVG = FALSE,
  hvg_selection.method = c("vst", "mean.var.plot", "dispersion"),
  hvg_var_thresh = 1,
  gini_thresh = 0.5,
  noise_feature_thresh = 4,
  random_marker_thresh = 5,
  num_threads = -1,
  seed = 9999,
  verbose = TRUE
)
```

## Arguments

- data:

  Either a `Seurat` object or a numeric matrix with features (genes) as
  rows and cells as columns. Recommended to provide at least
  library-size normalized data when `subset_to_HVG = TRUE`.

- assay:

  Character; assay used for marker ranking.

- layer:

  Character; assay layer used for marker ranking. May be normalized.

- norm_assay:

  Character; assay containing a normalized layer used for HVG detection.

- norm_layer:

  Character; normalized layer used for HVG detection.

- cluster_labels:

  Optional; column name in `data@meta.data` containing cluster labels,
  or a character vector of cluster labels with length equal to the
  number of cells in `data`. Required if `desired_clusters` is
  specified.

- desired_clusters:

  Optional; character vector of cluster labels for which markers are
  ranked. Required if `desired_cells` is not specified.

- desired_cells:

  Optional; named list of character vectors specifying cell names for
  each subset. Required if `desired_clusters` is not specified.

- log1p:

  Logical; whether to apply `log1p` transformation to the input `data`.
  It is recommended to set this argument to TRUE (default) if the data
  is not already on a log scale.

- remove_quiescent_cells:

  Logical; whether to remove quiescent cells prior to marker ranking.

- high_quantile:

  Numeric; quantile threshold defining highly positive EWCSR values.

- low_quantile:

  Numeric; quantile threshold defining highly negative EWCSR values.

- subset_to_HVG:

  Logical; whether to restrict analysis to highly variable genes (HVGs).

- hvg_selection.method:

  Character; HVG selection strategy. One of `"vst"`, `"mean.var.plot"`,
  or `"dispersion"`.

- hvg_var_thresh:

  Numeric; variance threshold for selecting HVGs.

- gini_thresh:

  Numeric; Gini coefficient threshold for detecting non-specific
  markers.

- noise_feature_thresh:

  Integer; features expressed in fewer than this number of cells are
  considered noise.

- random_marker_thresh:

  Integer; markers detected in fewer than this number of cells are
  discarded.

- num_threads:

  Integer; number of threads to use. Default `-1` uses all available
  cores.

- seed:

  Integer; random seed for reproducibility.

- verbose:

  Logical; whether to display progress messages.

## Value

An object of class `"MarkoCell"` containing ranked marker tables and
associated statistics for each requested cell set.

## Details

When `subset_to_HVG = TRUE`, highly variable genes are detected using
the normalized assay and layer specified by `norm_assay` and
`norm_layer`. Gini-based filtering is applied to identify global
(non-specific) versus specific markers.

## See also

[`markoClust`](https://asalavaty.github.io/celliverse/reference/markoClust.md),
[`markerPurity`](https://asalavaty.github.io/celliverse/reference/markerPurity.md),
[`gini.ewcsr.fs`](https://asalavaty.github.io/celliverse/reference/gini.ewcsr.fs.md)

## Examples

``` r
utils::data("pbmc_small", package = "SeuratObject")

pbmc_small$example_clusters <- as.character(
  SeuratObject::Idents(pbmc_small)
)

cluster_ids <- utils::head(
  unique(pbmc_small$example_clusters),
  2
)

mc <- markoCell(
  data = pbmc_small,
  cluster_labels = "example_clusters",
  desired_clusters = cluster_ids,
  num_threads = 1,
  verbose = FALSE
)
#> 
#> ── Identification of Markers of Desired Clusters ───────────────────────────────
```
