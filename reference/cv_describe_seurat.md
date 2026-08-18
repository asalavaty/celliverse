# Describe a Seurat object.

\`dim()\` on a Seurat is features x cells, the opposite of the
cells-as-rows convention a reader might expect, so the two counts are
assigned explicitly rather than positionally. \`layers\` is read from
the DEFAULT assay only – enumerating every assay's layers is unbounded
on a multimodal object, and the default assay is what a tool will
actually operate on. \`data_kind\` matters more than it looks: it is
what stops a tool from log-transforming values that are already
log-transformed (see \`cv_detect_log_transformed()\`).

## Usage

``` r
cv_describe_seurat(x)
```

## Arguments

- x:

  a Seurat object.

## Value

a named list of type-specific descriptor fields.
