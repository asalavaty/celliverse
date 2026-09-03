# Save a TypoPrompt to a file

Saves a `TypoPrompt` object as either its exact plain-text prompt or a
self-contained interactive HTML page.

## Usage

``` r
saveTypoPrompt(x, file, format = c("txt", "html"))
```

## Arguments

- x:

  A `TypoPrompt` object returned by
  [`typoPrompt`](https://asalavaty.github.io/celliverse/reference/typoPrompt.md).

- file:

  Character string giving the output file path.

- format:

  Output format: `"txt"` for the raw prompt or `"html"` for the
  interactive HTML viewer.

## Value

Invisibly returns the normalized output file path.

## See also

[`typoPrompt`](https://asalavaty.github.io/celliverse/reference/typoPrompt.md)

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
  verbose = FALSE
)

txt_file <- tempfile(fileext = ".txt")

saveTypoPrompt(
  prompt,
  file = txt_file,
  format = "txt"
)

```
