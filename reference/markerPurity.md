# Assess marker purity across clusters or cell subsets

Quantifies marker purity by evaluating expression specificity within
clusters or user-defined cell subsets.

## Usage

``` r
markerPurity(
  data,
  assay = "RNA",
  layer = "counts",
  desired_markers = NULL,
  cluster_labels = NULL,
  desired_clusters = NULL,
  desired_cells = NULL,
  log1p = TRUE,
  remove_quiescent_cells = TRUE,
  high_quantile = 0.25,
  low_quantile = 0.25,
  noise_feature_thresh = 4,
  num_threads = -1,
  seed = 121,
  verbose = TRUE
)
```

## Arguments

- data:

  Either a `Seurat` object or a numeric matrix with features (genes) as
  rows and cells as columns.

- assay:

  Assay name used for marker purity assessment.

- layer:

  Data layer used for assessment.

- desired_markers:

  Character vector of markers to assess.

- cluster_labels:

  Character vector. Required if \`desired_clusters\` is specified.
  Either the name of the column in data@meta.data containing cluster
  labels, or a character vector of cluster labels with length equal to
  the number of columns (cells) in the \`data\` argument.

- desired_clusters:

  Character vector of clusters to assess. Required if \`desired_cells\`
  is not specified. If not provided, marker purity will be assessed
  solely within \`desired_cells\`.

- desired_cells:

  Named list of character vectors specifying the names of the desired
  cells. Required if \`desired_clusters\` is not specified.

- log1p:

  Logical; whether to apply `log1p` transformation to the input `data`.
  It is recommended to set this argument to TRUE (default) if the data
  is not already on a log scale.

- remove_quiescent_cells:

  Logical; whether to remove quiescent cells.

- high_quantile:

  Quantile for defining high EWCSR values.

- low_quantile:

  Quantile for defining low EWCSR values.

- noise_feature_thresh:

  Threshold for filtering noise features.

- num_threads:

  Number of threads to use.

- seed:

  Random seed.

- verbose:

  Logical; whether to display progress messages.

## Value

An object of class `MarkerPurity`.

## Details

Marker purity is assessed using EWCSR-based filtering and supports both
Seurat objects and matrix-based inputs.

## See also

[`markoCell`](https://asalavaty.github.io/celliverse/reference/markoCell.md),
[`getDatasetMarkers`](https://asalavaty.github.io/celliverse/reference/getDatasetMarkers.md)

## Examples

``` r
if (FALSE) { # \dontrun{
mp <- markerPurity(data = my_seurat_obj, cluster_labels = "seurat_clusters")
} # }
```
