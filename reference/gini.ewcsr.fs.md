# Feature selection using Gini coefficient on EWCSR-transformed data

Performs feature selection based on the Gini inequality coefficient
computed on expression-weighted centered scaled rank (EWCSR) data.

## Usage

``` r
gini.ewcsr.fs(
  mat,
  gini_thresh = 0.5,
  ewcsr_high_thresh = NULL,
  ewcsr_low_thresh = NULL,
  noise_thresh = NULL,
  num_threads = -1
)
```

## Arguments

- mat:

  A matrix with features as rows and cells as columns.

- gini_thresh:

  Numeric; Gini threshold for selecting specific features.

- ewcsr_high_thresh:

  Numeric; upper EWCSR threshold for binarization. EWCSR values higher
  than this threshold will be converted to TRUE.

- ewcsr_low_thresh:

  Numeric; lower EWCSR threshold for binarization. EWCSR values lower
  than this threshold will be converted to TRUE.

- noise_thresh:

  Integer; minimum number of cells required for a feature to be
  retained.

- num_threads:

  Integer; number of threads to use. `-1` uses all available cores.

## Value

A list containing `specific_features`, `non_specific_features`, and
`no_occurrence`.

## Details

Features are categorized into specific, non-specific, and no-occurrence
groups based on Gini thresholds and optional binarization of EWCSR
values.

## See also

[`ewcsr.sparse`](https://asalavaty.github.io/celliverse/reference/ewcsr.sparse.md),
[`gini.rank.fs`](https://asalavaty.github.io/celliverse/reference/gini.rank.fs.md)

## Examples

``` r
utils::data("pbmc_small", package = "SeuratObject")

mat <- SeuratObject::LayerData(
  pbmc_small,
  assay = "RNA",
  layer = "counts"
)

fs <- gini.ewcsr.fs(
  mat,
  num_threads = 1
)
```
