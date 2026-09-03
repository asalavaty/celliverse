# Add ClustoCell cluster annotations to a Seurat or SingleCellExperiment object

Adds major cluster and/or sub-cluster labels stored in a `ClustoCell`
object to the cell-level metadata of a Seurat or SingleCellExperiment
object.

## Usage

``` r
addClustoData(
  obj,
  clustoCell,
  add_major_clusters = TRUE,
  add_sub_clusters = TRUE,
  major_cluster_name = "ClustoCell_Clusters",
  sub_cluster_name = "ClustoCell_SubClusters"
)
```

## Arguments

- obj:

  An object of class `Seurat` or `SingleCellExperiment`.

- clustoCell:

  An object of class `ClustoCell`, generated via
  [`clustoCell()`](https://asalavaty.github.io/celliverse/reference/clustoCell.md)
  or
  [`markoClust()`](https://asalavaty.github.io/celliverse/reference/markoClust.md).

- add_major_clusters:

  Logical; whether to add major cluster labels to the metadata of `obj`.

- add_sub_clusters:

  Logical; whether to add sub-cluster labels to the metadata of `obj`.

- major_cluster_name:

  Character; name of the metadata column to store major cluster labels.

- sub_cluster_name:

  Character; name of the metadata column to store sub-cluster labels.

## Value

The input object `obj` with additional metadata columns containing
ClustoCell cluster annotations.

## Details

This function transfers clustering results obtained using
[`clustoCell()`](https://asalavaty.github.io/celliverse/reference/clustoCell.md)
or
[`markoClust()`](https://asalavaty.github.io/celliverse/reference/markoClust.md)
into an existing single-cell object by appending cluster labels as
metadata columns. Major clusters and sub-clusters can be added
independently and assigned custom column names.

## See also

[`clustoCell`](https://asalavaty.github.io/celliverse/reference/clustoCell.md),
[`markoClust`](https://asalavaty.github.io/celliverse/reference/markoClust.md)

## Examples

``` r
utils::data("pbmc_small", package = "SeuratObject")

pbmc_small_cc <- clustoCell(
  data = pbmc_small,
  identify_subclusters = TRUE,
  num_threads = 1,
  verbose = FALSE
)
#> Loading required namespace: SeuratObject

pbmc_small <- addClustoData(
  obj = pbmc_small,
  clustoCell = pbmc_small_cc,
  add_major_clusters = TRUE,
  add_sub_clusters = TRUE
)
```
