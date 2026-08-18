# Describe a bare count matrix, dense or sparse.

Genes are rows in CelliVerse's convention, which is why \`n_features\`
is \`nrow()\`. The \`\*\_head\` previews exist so the model can
recognise what it is looking at – gene symbols versus Ensembl ids,
barcodes versus sample names – without any of the values being sent.
Five elements is enough to tell those apart and small enough to cost
nothing.

## Usage

``` r
cv_describe_matrix(x)
```

## Arguments

- x:

  a matrix or dgCMatrix.

## Value

a named list of type-specific descriptor fields.
