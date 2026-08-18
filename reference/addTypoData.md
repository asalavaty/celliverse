# Add TypoClust cell type annotations to a single-cell object

Adds inferred cell type annotations from a `TypoClust` object to the
metadata of a Seurat or SingleCellExperiment object.

## Usage

``` r
addTypoData(
  obj,
  typoClust,
  clusters,
  rank_thresh = 1,
  refine = TRUE,
  refine_thresh = 1,
  outNames = NULL
)
```

## Arguments

- obj:

  An object of class `Seurat` or `SingleCellExperiment`.

- typoClust:

  An object of class `TypoClust`, generated using
  [`typoClust()`](https://asalavaty.github.io/celliverse/reference/typoClust.md).

- clusters:

  Character vector; names of metadata columns in `obj` defining clusters
  or cell subsets to which cell types will be assigned.

- rank_thresh:

  Integer; the top N ranked cell types to add for each cluster, stored
  as separate metadata columns.

- refine:

  Logical; whether to refine inferred cell types by traversing deeper
  levels of the cell type hierarchy.

- refine_thresh:

  Integer; depth of (lexical) hierarchical traversal for refinement.
  Ignored if `refine = FALSE`.

- outNames:

  Character vector; names of output metadata columns. If `NULL`,
  defaults to `paste0(clusters, "_Celltype")`.

## Value

The input object `obj` with additional metadata columns containing
inferred cell type annotations.

## Details

Cell type labels are assigned to specified cluster or subset columns and
appended as new metadata columns. Multiple ranked cell types can be
added, and hierarchical refinement can be applied to obtain more
specific cell type annotations.

## See also

[`typoClust`](https://asalavaty.github.io/celliverse/reference/typoClust.md),
[`typoClustVis`](https://asalavaty.github.io/celliverse/reference/typoClustVis.md)

## Examples

``` r
if (FALSE) { # \dontrun{
so <- addTypoData(
  obj = so,
  typoClust = tc,
  clusters = "ClustoCell_Clusters"
)
} # }
```
