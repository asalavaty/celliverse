# Transfer ClustoCell cluster labels to a full-resolution dataset

Transfers major and sub-cluster labels from a `ClustoCell` object
generated on a sketched (subsampled) dataset to a full-resolution
dataset using EWCSR-based correlation or Seurat-based projection and
anchor transfer strategies.

## Usage

``` r
clustoCell_TransferLabel(
  clustoCell,
  query_ewcsr_mat = NULL,
  query_expr_mat = NULL,
  assay = "RNA",
  layer = "counts",
  method = c("ewcsr-cor", "seurat-project", "ewcsr-red-cor", "seurat-knn"),
  dims = 30,
  num_threads = -1,
  inherit_major_clusters = TRUE,
  seed = 121,
  verbose = TRUE
)
```

## Arguments

- clustoCell:

  An object of class `ClustoCell` obtained by running
  [`clustoCell()`](https://asalavaty.github.io/celliverse/reference/clustoCell.md)
  on a sketched dataset.

- query_ewcsr_mat:

  A sparse matrix (`dgCMatrix`) containing EWCSR values for the full
  dataset. Required for EWCSR-based methods and ignored when
  `method = "seurat-knn"` or `method = "seurat-project"`.

- query_expr_mat:

  Either a `Seurat` object or a sparse count matrix (`dgCMatrix`) for
  the full dataset. Required for `method = "seurat-knn"` or
  `method = "seurat-project"` and optional otherwise.

- assay:

  Character string specifying the assay used in `query_expr_mat` when a
  `Seurat` object is provided.

- layer:

  Character string specifying the layer of `assay` to use (e.g.,
  `"counts"`).

- method:

  Character string specifying the label transfer strategy. Options
  include:

  - `"ewcsr-cor"`: Transfers labels by computing correlations between
    EWCSR profiles of query cells and EWCSR centroids of sketched
    clusters in the full feature space.

  - `"seurat-project"`: Uses Seurat's `ProjectData()` workflow to
    transfer labels by projecting the full expression dataset (raw
    counts, log-normalized, or SCT-normalized) onto the low-dimensional
    embedding learned from the sketched dataset.

  - `"ewcsr-red-cor"`: Similar to `"ewcsr-cor"`, but correlations are
    computed in a reduced dimensional space (PCA embedding).

  - `"seurat-knn"`: Uses Seurat's `FindTransferAnchors()` and
    `TransferData()` workflow to transfer labels from the sketched
    dataset to the full dataset using nearest-neighbor matching.
    Supports standard Seurat normalization workflows (e.g., LogNormalize
    and SCTransform).

- dims:

  Integer; number of dimensions used during sketching or PCA-based label
  transfer.

- num_threads:

  Integer; number of CPU threads to use. Default `-1` uses all available
  cores.

- inherit_major_clusters:

  Logical; whether to restrict sub-cluster label transfer within
  inherited major clusters when such labels are available.

- seed:

  Integer; random seed for reproducibility.

- verbose:

  Logical; whether to print progress messages.

## Value

An updated object of class `ClustoCell` containing transferred major and
sub-cluster labels for the full dataset.

## Details

The `"seurat-project"` and `"seurat-knn"` methods operate on the
original expression matrix (\`query_expr_mat\`) rather than the EWCSR
representation. The supplied expression data may consist of raw counts,
log-normalized expression values, or SCTransform-normalized data,
provided that the sketched and full datasets were processed using the
same normalization strategy. These methods leverage Seurat's native
label transfer workflows
([`ProjectData`](https://satijalab.org/seurat/reference/ProjectData.html),
[`FindTransferAnchors`](https://satijalab.org/seurat/reference/FindTransferAnchors.html),
and
[`TransferData`](https://satijalab.org/seurat/reference/TransferData.html))
and do not require integer count matrices.

## Examples

``` r
utils::data("pbmc_small", package = "SeuratObject")

reference_cells <- colnames(pbmc_small)[1:60]

pbmc_reference <- pbmc_small[, reference_cells]

cc_reference <- clustoCell(
  data = pbmc_reference,
  identify_subclusters = FALSE,
  num_threads = 1,
  verbose = FALSE
)

full_counts <- SeuratObject::LayerData(
  pbmc_small,
  assay = "RNA",
  layer = "counts"
)

full_ewcsr <- ewcsr.sparse(full_counts, num_threads = 1)

cc_full <- clustoCell_TransferLabel(
  clustoCell = cc_reference,
  query_ewcsr_mat = full_ewcsr,
  method = "ewcsr-cor",
  num_threads = 1,
  verbose = FALSE
)
```
