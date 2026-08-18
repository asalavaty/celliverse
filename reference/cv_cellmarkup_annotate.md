# Core ceLLMarkup annotation routine.

Core ceLLMarkup annotation routine.

## Usage

``` r
cv_cellmarkup_annotate(
  object,
  desired_sets = NULL,
  tissue = NULL,
  condition = NULL,
  species = "human",
  n_markers = 20L,
  use_neg_markers = FALSE,
  annotate_subclusters = FALSE,
  top_k = 3L,
  inherit_major_clusters = TRUE,
  config = cv_load_config()
)
```

## Arguments

- object:

  a ClustoCell or DatasetMarkers.

- desired_sets:

  optional character vector of cluster ids to annotate (default: all).
  Unknown ids abort with the available ids.

- tissue, condition, species:

  context strings.

- n_markers:

  top positive markers per cluster to expose.

- use_neg_markers:

  include negative markers.

- annotate_subclusters:

  also do sub-clusters.

- top_k:

  ranked candidates per cluster.

- config:

  LLM config.

## Value

a TypoClust-compatible object.
