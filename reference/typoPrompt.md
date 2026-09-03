# Generate an LLM-ready prompt for cell-type annotation

Generates a structured, copy-and-paste-ready prompt for annotation of
clusters, sub-clusters, or cell subsets using any chatbot or large
language model (LLM). Marker information is extracted directly from a
`ClustoCell` or `MarkoCell` object and combined with optional biological
context such as sample source, tissue, condition, and species.

In interactive sessions, the returned `TypoPrompt` object opens as a
polished HTML interface in the RStudio Viewer, or in the default web
browser when the Viewer is unavailable. The interface provides formatted
and raw views, collapsible sections, light/dark appearance, one-click
copying, TXT/HTML downloads, and printing to PDF. The prompt can also be
accessed directly as plain text or saved programmatically as `.txt` or
`.html`.

## Usage

``` r
typoPrompt(
  object,
  desired_sets = NULL,
  sample_source = NULL,
  feature_type = "gene",
  species = "human",
  tissue = NULL,
  condition = NULL,
  use_neg_markers = TRUE,
  thresh_mode = c("n", "rank"),
  thresh = 20,
  top_k = 3,
  verbose = TRUE
)
```

## Arguments

- object:

  An object of class `ClustoCell` or `MarkoCell` containing marker
  results for the clusters, sub-clusters, and/or cell subsets to be
  annotated.

- desired_sets:

  Optional character vector specifying the names of clusters,
  sub-clusters, and/or cell subsets to include in the prompt. Names must
  be present in `object`. If `NULL`, all available sets are included.

- sample_source:

  Optional free-text description of the sample origin provided to the
  LLM (e.g. `"human peripheral blood"` or `"melanoma tumor biopsy"`).
  Providing this context can help improve annotation specificity.

- feature_type:

  Character string describing the marker feature type (e.g. `"gene"` or
  `"protein"`). Default is `"gene"`.

- species:

  Character string specifying the species (e.g. `"human"` or `"mouse"`).
  Other species names may also be supplied. Default is `"human"`.

- tissue:

  Optional character vector specifying one or more tissue contexts to
  provide to the LLM. Available tissue types can be accessed using
  `data("tissueCondition_types", package = "celliverse")`.

- condition:

  Optional character vector specifying one or more biological or disease
  conditions to provide to the LLM. Available condition types can be
  accessed using
  `data("tissueCondition_types", package = "celliverse")`.

- use_neg_markers:

  Logical; whether to include negative markers in the generated prompt.
  Negative markers provide exclusionary evidence that can help
  distinguish closely related cell types, subtypes, or states. Default
  is `TRUE`.

- thresh_mode:

  Character string specifying how markers are selected from each marker
  table. One of:

  - `"n"`: retains strictly the first `thresh` markers in rank order,
    even when additional markers share the final selected rank.

  - `"rank"`: retains all markers with rank less than or equal to
    `thresh`. Ties at the cutoff rank are therefore retained.

  Default is `"n"`.

- thresh:

  Integer specifying the marker-selection threshold. With
  `thresh_mode = "n"`, up to the first `thresh` markers are used for
  each set. With `thresh_mode = "rank"`, all markers with rank less than
  or equal to `thresh` are used. Default is `20`.

- top_k:

  Positive integer specifying the maximum number of ranked candidate
  annotations that the generated prompt asks the LLM to return for each
  set. Default is `3`.

- verbose:

  Logical; whether to display progress and status messages while
  preparing the prompt. Default is `TRUE`.

## Value

An object of class `TypoPrompt`, inheriting from `character`, that
contains the complete LLM-ready annotation prompt.

In an interactive session, printing the object opens a formatted HTML
interface in the RStudio Viewer when available, otherwise in the default
web browser. The interface provides formatted and raw views, collapsible
sections, light/dark appearance, one-click copying, TXT/HTML downloads,
and printing to PDF. In non-interactive sessions, the plain-text prompt
is printed instead. The raw prompt can also be obtained with
[`as.character()`](https://rdrr.io/r/base/character.html) or
[`cat()`](https://rdrr.io/r/base/cat.html), and exported
programmatically with
[`saveTypoPrompt`](https://asalavaty.github.io/celliverse/reference/saveTypoPrompt.md).

## Details

`typoPrompt()` provides a model- and provider-independent workflow for
LLM-assisted cell annotation. Unlike
[`ceLLMarkup`](https://asalavaty.github.io/celliverse/reference/ceLLMarkup.md),
it does not connect to an LLM directly and therefore requires no API
key, model configuration, or local LLM server. Instead, it prepares the
annotation task for submission to the user's preferred chatbot or LLM.

The generated prompt includes:

- positive markers and, optionally, negative markers for each set;

- available sample, tissue, condition, species, and feature context;

- instructions to consider the complete marker profile rather than
  individual markers;

- instructions to distinguish cell types, subtypes, and cellular states
  where supported by the marker evidence;

- a request for up to `top_k` ranked annotations with confidence scores
  and concise biological rationales; and

- a standardized Markdown-table response format followed by an overall
  interpretation.

For `ClustoCell` objects containing both major clusters and
sub-clusters, the prompt additionally describes their hierarchy and asks
the LLM to first establish the identity of each parent cluster and then
interpret its sub-clusters as biologically meaningful subtypes or states
within that context.

Printing a returned `TypoPrompt` object displays its formatted HTML
interface:


    prompt <- typoPrompt(...)
    prompt

The underlying plain-text prompt remains directly accessible with
`cat(prompt)` or `as.character(prompt)`. It can also be saved
programmatically using
[`saveTypoPrompt`](https://asalavaty.github.io/celliverse/reference/saveTypoPrompt.md)
with `format = "txt"` or `format = "html"`. Interactive HTML rendering
and HTML export require the optional htmltools package; when it is
unavailable, printing falls back to the plain-text prompt.

## See also

[`typoClust`](https://asalavaty.github.io/celliverse/reference/typoClust.md),
[`ceLLMarkup`](https://asalavaty.github.io/celliverse/reference/ceLLMarkup.md),
[`typoClustVis`](https://asalavaty.github.io/celliverse/reference/typoClustVis.md),
[`saveTypoPrompt`](https://asalavaty.github.io/celliverse/reference/saveTypoPrompt.md),
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

prompt <- typoPrompt(
  object = cc,
  desired_sets = desired_set,
  sample_source = "human peripheral blood",
  tissue = "Blood",
  condition = "Healthy",
  species = "human",
  use_neg_markers = FALSE,
  thresh = 10,
  top_k = 3,
  verbose = FALSE
)

class(prompt)
#> [1] "TypoPrompt" "character" 
```
