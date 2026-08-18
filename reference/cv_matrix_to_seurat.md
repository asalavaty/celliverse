# Convert a (sparse) matrix or data.frame of counts into a Seurat object.

Agent-layer helper (does NOT touch CelliVerse analysis functions).
Assumes the CelliVerse genes-x-cells convention. A data.frame is coerced
to a numeric matrix; a non-numeric frame returns NULL so the caller
skips conversion.

## Usage

``` r
cv_matrix_to_seurat(x)
```
