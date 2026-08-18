# Strict variant of cv_cellmarkup_parse(): returns NULL when the reply is not parseable JSON with an \`annotations\` array, so the caller can RETRY instead of silently degrading every cluster to "Unknown". On a well-formed reply it behaves identically to cv_cellmarkup_parse() (Unknown-fill for any cluster the model omitted).

Strict variant of cv_cellmarkup_parse(): returns NULL when the reply is
not parseable JSON with an \`annotations\` array, so the caller can
RETRY instead of silently degrading every cluster to "Unknown". On a
well-formed reply it behaves identically to cv_cellmarkup_parse()
(Unknown-fill for any cluster the model omitted).

## Usage

``` r
cv_cellmarkup_parse_strict(text, cluster_names, top_k = 3L)
```
