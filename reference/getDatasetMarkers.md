# Collect marker genes from a ClustoCell object

Extracts positive, negative, and/or medium markers from major clusters
and sub-clusters stored in a `ClustoCell` object.

## Usage

``` r
getDatasetMarkers(
  obj,
  clusters = TRUE,
  sub_clusters = TRUE,
  positive_markers = TRUE,
  negative_markers = FALSE,
  medium_markers = FALSE,
  thresh_mode = c("n", "rank"),
  pos_thresh = 25,
  neg_thresh = 20,
  med_thresh = 10,
  verbose = TRUE
)
```

## Arguments

- obj:

  An object of class `ClustoCell`.

- clusters:

  Logical; whether to collect markers from major clusters.

- sub_clusters:

  Logical; whether to collect markers from sub-clusters.

- positive_markers:

  Logical; whether to collect positive markers.

- negative_markers:

  Logical; whether to collect negative markers.

- medium_markers:

  Logical; whether to collect medium markers.

- thresh_mode:

  Character; marker selection strategy. One of:

  - `"rank"`: include all markers with ranks up to the threshold.

  - `"n"`: include only the top n markers (rows) in rank order..

- pos_thresh:

  Integer; threshold for positive markers.

- neg_thresh:

  Integer; threshold for negative markers.

- med_thresh:

  Integer; threshold for medium markers.

- verbose:

  Logical; whether to display progress messages.

## Value

An object of class `DatasetMarkers`.

## Details

Marker selection can be controlled using rank-based or fixed-size
thresholds. Separate thresholds are applied for positive, negative, and
medium markers.

## See also

[`clustoCell`](https://asalavaty.github.io/celliverse/reference/clustoCell.md),
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

markers <- getDatasetMarkers(
  obj = cc,
  sub_clusters = FALSE,
  pos_thresh = 20,
  verbose = FALSE
)
```
