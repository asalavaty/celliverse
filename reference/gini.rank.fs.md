# Feature selection using Gini coefficient on ranked expression data

Identifies specific and non-specific features based on the Gini
inequality coefficient computed on ranked expression values.

## Usage

``` r
gini.rank.fs(mat, gini_thresh = 0.5, noise_thresh = NULL, num_threads = -1)
```

## Arguments

- mat:

  A matrix with features as rows and cells as columns.

- gini_thresh:

  Numeric; Gini threshold for selecting specific features.

- noise_thresh:

  Integer; minimum number of cells required for a feature to be
  retained.

- num_threads:

  Integer; number of threads to use. `-1` uses all available cores.

## Value

A list containing `specific_features`, `non_specific_features`, and
`no_occurrence`.

## Details

This method removes globally low-ranked features while retaining
features with consistently high ranks across cells for downstream
analysis.

## See also

[`gini.ewcsr.fs`](https://asalavaty.github.io/celliverse/reference/gini.ewcsr.fs.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fs <- gini.rank.fs(mat)
} # }
```
