# Compute Jaccard similarity for sparse matrices

Computes pairwise Jaccard similarity between columns of a sparse matrix.

## Usage

``` r
jaccard.sparse(mat, num_threads = -1)
```

## Arguments

- mat:

  A sparse matrix of class `Matrix`.

- num_threads:

  Integer; number of threads to use. `-1` uses all available cores.

## Value

A numeric matrix of Jaccard similarity scores.

## See also

[`mutual.rank`](https://asalavaty.github.io/celliverse/reference/mutual.rank.md)

## Examples

``` r
utils::data("pbmc_small", package = "SeuratObject")

mat <- SeuratObject::LayerData(
  pbmc_small,
  assay = "RNA",
  layer = "counts"
)

binary_mat <- mat
binary_mat@x[] <- 1

jac <- jaccard.sparse(
  binary_mat,
  num_threads = 1
)
```
