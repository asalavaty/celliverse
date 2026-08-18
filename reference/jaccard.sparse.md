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
if (FALSE) { # \dontrun{
jac <- jaccard.sparse(mat)
} # }
```
