# How much EXTRA the matrix-\>Seurat conversion costs, as a multiple of the matrix's own size.

1.5, measured: holding a 0.8 GB dgCMatrix cost 1.98 GB and holding it
alongside the Seurat built from it cost 3.21 GB, so the Seurat's own
footprint is ~1.23 GB – about 1.5x the matrix. Dropping the matrix
afterwards freed nothing, so it really is a second copy plus Seurat's
scaffolding, not shared storage.

## Usage

``` r
CV_SEURAT_EXTRA_FACTOR
```

## Format

An object of class `numeric` of length 1.

## Details

ROUND LXXXIII CORRECTION. This was 2 and was described as an increment,
but 2 is the TOTAL of matrix-plus-Seurat. The matrix is already resident
when the question is asked, so charging the total overstated the
additional requirement by a third – the user was told a 3.7 GB matrix
needed "7.5 GB more" when the honest figure is about 5.5 GB. Labelling a
total as an increment is the kind of error that reads as precision.
