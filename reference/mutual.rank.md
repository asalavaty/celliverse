# Compute mutual rank from a similarity matrix

Computes the mutual rank (MR) for all pairs in a symmetric similarity or
dissimilarity matrix, a robust measure frequently used to stabilize
pairwise similarity relationships.

## Usage

``` r
mutual.rank(mat, num_threads = -1)
```

## Arguments

- mat:

  Numeric symmetric matrix (dense or sparse) representing pairwise
  similarities or dissimilarities.

- num_threads:

  Integer; number of threads to use. Default `-1` uses all available
  cores.

## Value

A numeric matrix of mutual ranks with the same dimensions as `mat`.

## See also

[`jaccard.sparse`](https://asalavaty.github.io/celliverse/reference/jaccard.sparse.md),
[`markoClust`](https://asalavaty.github.io/celliverse/reference/markoClust.md)

## Examples

``` r
if (FALSE) { # \dontrun{
mr <- mutual.rank(similarity_matrix)
} # }
```
