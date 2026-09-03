# Cell type annotation of clusters, sub-clusters, or cell subsets

Annotates clusters, sub-clusters, or arbitrary cell subsets using
curated cell-type marker databases or large language model (LLM)–based
annotation. Annotation can be performed either from marker results
stored in `ClustoCell` or `MarkoCell` objects, or directly from
user-specified positive and/or negative marker panels.

## Usage

``` r
typoClust(
  objects = NULL,
  desired_sets = NULL,
  tissue = NULL,
  condition = NULL,
  use_pos_markers = TRUE,
  use_neg_markers = TRUE,
  desired_pos_markers = NULL,
  desired_neg_markers = NULL,
  thresh_mode = c("n", "rank"),
  thresh = 20,
  mode = c("markerDB", "ceLLMarkup"),
  inherit_major_clusters = TRUE,
  inherit_score_ratio = 0.5,
  species = c("human", "mouse"),
  sample_source = NULL,
  feature_type = "gene",
  llm_provider = "ollama",
  llm_model = NULL,
  llm_api_key = NULL,
  llm_host = NULL,
  llm_top_k = 3,
  verbose = TRUE
)
```

## Arguments

- objects:

  A list of one or more objects of class `ClustoCell` or `MarkoCell`
  (e.g. `list(obj1, obj2)`). Mandatory if `desired_pos_markers` and/or
  `desired_neg_markers` are not specified.

- desired_sets:

  Optional character vector specifying the names of clusters,
  sub-clusters, and/or cell subsets to annotate. These names must exist
  in the supplied `objects`. If `NULL`, all available sets are
  annotated.

- tissue:

  Optional character vector specifying one or more tissue contexts used
  for annotation. If `NULL`, all available tissues are examined.
  Available tissue types can be accessed via
  `data("tissueCondition_types", package = "celliverse")`.

- condition:

  Optional character vector specifying one or more conditions (e.g.
  Healthy, disease states) used for annotation. If `NULL`, all available
  conditions are examined. Available condition types can be accessed via
  `data("tissueCondition_types", package = "celliverse")`.

- use_pos_markers:

  Logical; whether to use positive markers for cell-type annotation.
  Default is `TRUE`.

- use_neg_markers:

  Logical; whether to use negative markers for cell-type annotation.
  Default is `TRUE`.

- desired_pos_markers:

  Optional named list of character vectors specifying positive marker
  panels. Each list element corresponds to one cluster or cell subset
  (e.g. `list(cluster1 = c("GeneA", "GeneB"))`). Mandatory if `objects`
  and `desired_neg_markers` are not specified. List names must match
  those of `desired_neg_markers`, if provided.

- desired_neg_markers:

  Optional named list of character vectors specifying negative marker
  panels. Each list element corresponds to one cluster or cell subset.
  Mandatory if `objects` and `desired_pos_markers` are not specified.
  List names must match those of `desired_pos_markers`, if provided.

- thresh_mode:

  Character string specifying how to select top markers. One of:

  - `"rank"`: Selects all markers with ranks up to the threshold. If
    multiple markers share the cutoff rank, all are included.

  - `"n"`: Selects strictly the top `n` markers in rank order, even if
    additional markers share the same rank.

- thresh:

  Integer specifying the marker selection threshold. Interpreted
  according to `thresh_mode`. Only used when `objects` is specified.

- mode:

  Character string specifying the annotation strategy. One of:

  - `"markerDB"`: Annotates cell sets using curated cell-type marker
    databases.

  - `"ceLLMarkup"`: Annotates cell sets using large language models
    (LLM) via
    [`ceLLMarkup`](https://asalavaty.github.io/celliverse/reference/ceLLMarkup.md);
    configure the LLM with `llm_provider`, `llm_model`, `llm_api_key`,
    `llm_host`.

- inherit_major_clusters:

  Logical; whether a sub-cluster should be annotated *within the
  identity of its own major cluster*. Default `TRUE`.

  When `FALSE`, every set is annotated independently and the function
  behaves exactly as it did before this argument existed.

  When `TRUE`, and the input contains both major clusters and
  sub-clusters, annotation becomes two-stage. Each requested
  sub-cluster's parent major cluster is annotated first, from the *major
  cluster's own* top markers; the sub-cluster is then annotated from
  *its own* top markers, constrained by that parent identity:

  - `mode = "markerDB"`: the database is restricted to every cell type
    whose name contains an admitted parent identity as a whole phrase —
    so a parent called `"CD8+ T Cell"` leaves `"CD8+ T Cell"` itself
    plus `"Exhausted CD8+ T Cell"`, `"Memory CD8+ T Cell"`,
    `"GZMK+ CD8+ T Cell"` and so on — and the sub-cluster is scored
    against that restricted database only. Which identities count as
    admitted is set by `inherit_score_ratio`.

  - `mode = "ceLLMarkup"`: the model is told the parent's identity and
    asked which specific subtype or state of *that* cell type the
    sub-cluster represents, given the sub-cluster's own markers.

  A parent major cluster needed for this is annotated and returned even
  when it was not itself named in `desired_sets`; `metadata$inheritance`
  records, per sub-cluster, which parent was used and what it was
  called. The restriction is applied per sub-cluster, so one major
  cluster's identity can never constrain a different major cluster's
  sub-clusters.

- inherit_score_ratio:

  Numeric in `(0, 1]`, default `0.5`. **`mode = "markerDB"` only**, and
  only when `inherit_major_clusters = TRUE`.

  How close to the parent's rank-1 score a runner-up candidate must come
  before it *also* constrains that parent's sub-clusters. At `1` only
  the rank-1 label is used; at `0.6` any candidate scoring at least 60%
  of the rank-1 score is admitted alongside it, and the database is
  restricted to the union of their named varieties.

  This exists because a major cluster's own label is often not certain,
  and treating it as certain propagates the doubt into every sub-cluster
  beneath it. Measured on the bundled pbmc3k `ClustoCell`
  (`thresh = 10`, Blood/Healthy), each parent's rank-2 candidate as a
  fraction of its rank-1 score:

  |            |                                   |           |
  |------------|-----------------------------------|-----------|
  | **Parent** | **rank-1 vs rank-2**              | **ratio** |
  | C1         | NK Cell vs CD8+ Alpha-Beta T Cell | 0.653     |
  | C2         | B Cell vs MS4A1+ B Cell           | 0.248     |
  | C3         | T Cell vs CD4+ Alpha-Beta T Cell  | 0.965     |
  | C4         | Mononuclear Phagocyte vs Monocyte | 0.837     |
  | C5         | Platelet vs Megakaryocyte         | 0.846     |

  C1 is a mixed cytotoxic compartment whose sub-clustering separates
  CD8+ T cells from NK cells; on rank-1 alone its T-cell sub-cluster is
  folded back into NK. C4 is the other end of the same problem: exactly
  one database cell type is named as a variety of
  `"Mononuclear Phagocyte"`, so on rank-1 alone its sub-clusters can
  only repeat the parent's label, while admitting `"Monocyte"` restores
  a vocabulary of 32. At the default, no parent on that dataset admits
  more than three identities.

- species:

  Character string specifying the species (either `"human"` or
  `"mouse"`). Other species names may also be supplied when the mode is
  set to `"ceLLMarkup"`. Default is `"human"`.

- sample_source:

  Free-text description of the sample origin shown to the model (e.g.
  `"human peripheral blood"`). Improves accuracy. Only used when mode is
  set to `"ceLLMarkup"`.

- feature_type:

  Feature type of the markers (e.g. `"gene"`, `"protein"`). Default
  `"gene"`. Only used when mode is set to `"ceLLMarkup"`.

- llm_provider:

  (`mode = "ceLLMarkup"` only) LLM provider. One of `"ollama"`,
  `"lmstudio"`, `"openai"`, `"anthropic"`, `"gemini"`, `"deepseek"`,
  `"groq"`, `"openrouter"`, `"cerebras"`. Default `"ollama"`.

- llm_model:

  (`mode = "ceLLMarkup"` only) Model id for `llm_provider` (e.g.
  `"qwen3:8b"` for Ollama, `"qwen/qwen3-8b"` for LM Studio,
  `"gpt-4o-mini"` for OpenAI, `"anthropic/claude-3-haiku"` for
  OpenRouter). Mandatory when `mode = "ceLLMarkup"`.

- llm_api_key:

  (`mode = "ceLLMarkup"` only) API key for cloud providers (not needed
  for Ollama/LM Studio). If `NULL`, falls back to the provider's
  standard environment variable (e.g. `OPENAI_API_KEY`,
  `OPENROUTER_API_KEY`).

- llm_host:

  (`mode = "ceLLMarkup"` only) Base URL for local providers. Defaults to
  `http://localhost:11434` (Ollama) or `http://localhost:1234/v1` (LM
  Studio).

- llm_top_k:

  (`mode = "ceLLMarkup"` only) Number of ranked candidate cell types
  returned per set. Default 3.

- verbose:

  Logical; whether to display progress messages.

## Value

An object of class `TypoClust` containing ranked cell-type annotations,
supporting marker evidence, and summary statistics for each annotated
set.

## Details

`typoClust()` identifies candidate cell types for each target set by
comparing positive and/or negative marker genes against tissue- and
condition-aware cell-type marker databases. Users may restrict
annotation to specific tissues or conditions, control the number of
markers used per cluster, and choose between rank-based or fixed-size
marker selection.

Exactly one of the following inputs must be provided:

- One or more `ClustoCell` or `MarkoCell` objects via `objects`

- User-defined positive and/or negative marker panels via
  `desired_pos_markers` and/or `desired_neg_markers`

## See also

[`typoClustVis`](https://asalavaty.github.io/celliverse/reference/typoClustVis.md),
[`markoCell`](https://asalavaty.github.io/celliverse/reference/markoCell.md),
[`markoClust`](https://asalavaty.github.io/celliverse/reference/markoClust.md),
[`clustoCell`](https://asalavaty.github.io/celliverse/reference/clustoCell.md)

## Examples

``` r
utils::data("pbmc_small", package = "SeuratObject")

cc <- clustoCell(
  data = pbmc_small,
  identify_subclusters = FALSE,
  num_threads = 1,
  verbose = FALSE
)

desired_set <- utils::head(
  sort(unique(as.character(cc$clusters$major_clusters))),
  1
)

tc <- typoClust(
  objects = list(cc),
  desired_sets = desired_set,
  tissue = "Blood",
  condition = "Healthy",
  use_neg_markers = FALSE,
  thresh = 10,
  mode = "markerDB",
  species = "human",
  verbose = FALSE
)
```
