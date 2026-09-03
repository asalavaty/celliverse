# LLM-based cell-type annotation of clusters or marker sets (ceLLMarkup)

Annotates cell clusters, sub-clusters, or arbitrary marker sets by
asking a large language model to name the most likely cell type for each
set, given its marker genes and an optional tissue/condition/species
context. Returns ranked candidate cell types per set in a
`TypoClust`-compatible object.

Exactly one of `marker_set_list`, `panels`, or `seuratClusters` must be
provided.

## Usage

``` r
ceLLMarkup(
  sample_source = NULL,
  feature_type = "gene",
  marker_set_list = NULL,
  panels = NULL,
  seuratClusters = NULL,
  padj = 0.05,
  logFC = NULL,
  tissue = NULL,
  condition = NULL,
  species = c("human", "mouse"),
  provider = "ollama",
  model,
  api_key = NULL,
  host = NULL,
  temperature = 0.2,
  top_k = 3L,
  n_markers = 25L,
  max_retries = 1L,
  inherit_major_clusters = TRUE,
  verbose = TRUE
)
```

## Arguments

- sample_source:

  Free-text description of the sample origin shown to the model (e.g.
  `"human peripheral blood"`). Improves accuracy.

- feature_type:

  Feature type of the markers (e.g. `"gene"`, `"protein"`). Default
  `"gene"`.

- marker_set_list:

  Named list of marker data.frames, one per set. Each data.frame should
  have feature names in the first column and purity or fold changes in
  the second. Positive markers (or up-regulated features) are
  recommended.

- panels:

  Named list of marker panels: either a named list of character vectors
  (positive markers per set), or a list with `pos_panels` and/or
  `neg_panels` elements (each a named list of character vectors). This
  is the input `typoClust(mode = "ceLLMarkup")` uses.

- seuratClusters:

  A data.frame obtained from Seurat's `FindAllMarkers()` (must have
  `avg_log2FC`, `p_val_adj`, `cluster`, `gene` columns). Filtered to
  up-regulated markers by `padj`/`logFC`.

- padj:

  Adjusted p-value threshold for filtering `seuratClusters` (default
  0.05; `NULL` disables).

- logFC:

  log-fold-change threshold for filtering `seuratClusters` (default
  `NULL` = no extra filter; only up-regulated genes kept).

- tissue:

  Optional tissue context (e.g. `"blood"`).

- condition:

  Optional condition context (e.g. `"healthy"`).

- species:

  Species for gene-symbol interpretation: `"human"`, `"mouse"`, or your
  desired species name.

- provider:

  LLM provider: one of `"ollama"`, `"lmstudio"`, `"openai"`,
  `"anthropic"`, `"gemini"`, `"deepseek"`, `"groq"`, `"openrouter"`,
  `"cerebras"`.

- model:

  Model id for the provider (e.g. `"qwen3:8b"` for Ollama,
  `"qwen/qwen3-8b"` for LM Studio, `"gpt-4o-mini"` for OpenAI,
  `"anthropic/claude-3-haiku"` for OpenRouter).

- api_key:

  API key for cloud providers. Not needed for `ollama`/`lmstudio`. If
  `NULL`, falls back to the standard environment variable for the
  provider (e.g. `OPENAI_API_KEY`, `OPENROUTER_API_KEY`).

- host:

  Base URL for local providers. Defaults to `http://localhost:11434`
  (Ollama) or `http://localhost:1234/v1` (LM Studio). Ignored for cloud
  providers.

- temperature:

  Sampling temperature (default 0.2).

- top_k:

  Number of ranked candidate cell types per set (default 3).

- n_markers:

  Maximum number of markers per set shown to the model (default 25).

- max_retries:

  Retries per set when the model returns an unparseable or empty answer
  (default 1).

- inherit_major_clusters:

  Logical; whether a sub-cluster should be annotated *within the
  identity of its own major cluster*. Default `TRUE`.

  When `FALSE`, all sets are annotated together in a single request and
  the function behaves exactly as it did before this argument existed.

  When `TRUE`, the set names are inspected for a major/sub-cluster
  hierarchy — a set `"C1"` is the parent of `"C1-Sub1"`, `"C1-Sub2"`,
  and so on. If both levels are present, annotation runs in two stages.
  First the major clusters are annotated from *their own* markers. Then,
  for each major cluster separately, the model is told what that cluster
  was identified as and asked which specific subtype or state of *that*
  cell type each of its sub-clusters represents, given the
  *sub-cluster's own* markers.

  One request is issued per major cluster in the second stage. That is
  what keeps each sub-cluster tied to its own parent: a single combined
  request could not carry a different parent identity for each set. Cost
  is therefore one request plus one per major cluster that has
  sub-clusters, rather than one in total. `metadata$inheritance` records
  which parent was used for each sub-cluster and what it was called.

- verbose:

  Show progress messages (default `TRUE`).

## Value

An object of class `TypoClust`: a list with `cell_types` (named list of
ranked annotation data.frames) and `metadata`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires an externally running Ollama server and an installed
# qwen3:8b model.
markers <- list(
  Cluster1 = c("CD3D", "CD3E", "TRBC1", "IL7R", "LTB"),
  Cluster2 = c("MS4A1", "CD79A", "CD37", "CD74", "HLA-DRA")
)

annotations <- ceLLMarkup(
  panels = markers,
  sample_source = "human peripheral blood",
  feature_type = "gene",
  tissue = "Blood",
  condition = "Healthy",
  species = "human",
  provider = "ollama",
  model = "qwen3:8b",
  top_k = 3
)
} # }
  
```
