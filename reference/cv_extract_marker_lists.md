# Extract per-cluster marker gene lists for annotation.

IMPORTANT (verified against the CelliVerse source): - A ClustoCell
stores PER-CLUSTER markers at
x\$markers\$major_clusters\$cluster_specific\$positive_markers which is
a list named by cluster id ("C0","C1",...) of data.frames with columns
\`Feature\`, \`Gini_Score\`, \`Rank\` (sorted by Rank ascending).
Sub-cluster markers live at
x\$markers\$sub_clusters\[\[cluster\]\]\$positive_markers (named by
sub-cluster id). This is the ONLY source of per-cluster markers. - A
DatasetMarkers object (from getDatasetMarkers) is a FLAT, cross-cluster
DEDUPLICATED pool of gene vectors (combined_markers,
clusters_pos_markers, ...). It has NO per-cluster resolution, so from a
DatasetMarkers handle we can only annotate one pooled pseudo-group. We
still support it (degraded) but ClustoCell is strongly preferred.

## Usage

``` r
cv_extract_marker_lists(
  object,
  n_markers = 20L,
  use_neg_markers = FALSE,
  annotate_subclusters = FALSE
)
```

## Value

list(clusters=\<chr\>, pos=\<named list of chr\>, neg=\<named list of
chr\>, level=\<named chr: "major"/"sub" per cluster\>, degraded=\<lgl\>)
