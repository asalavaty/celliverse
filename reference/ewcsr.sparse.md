# Compute expression-weighted centered scaled ranks (EWCSR)

Computes expression-weighted centered scaled ranks for a sparse or dense
expression matrix, calculated per column (cell).

## Usage

``` r
ewcsr.sparse(mat)
```

## Arguments

- mat:

  A matrix with features (genes) as rows and cells or samples as
  columns.

## Value

A matrix of EWCSR-transformed values with the same dimensions as `mat`.

## Details

EWCSR transformation emphasizes relatively high and low expression
features within each cell while accounting for expression magnitude. The
output matrix retains the same dimensions as the input.

## See also

[`gini.ewcsr.fs`](https://asalavaty.github.io/celliverse/reference/gini.ewcsr.fs.md),
[`markoCell`](https://asalavaty.github.io/celliverse/reference/markoCell.md)

## Examples

``` r
if (FALSE) { # \dontrun{
ewcsr_mat <- ewcsr.sparse(mat)
} # }
```
