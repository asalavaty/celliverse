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
if (FALSE) { # \dontrun{
prompt <- typoPrompt(clust_obj)
saveTypoPrompt(prompt, "cell_annotation_prompt.txt", format = "txt")
saveTypoPrompt(prompt, "cell_annotation_prompt.html", format = "html")
} # }
```
