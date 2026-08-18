# Parse the model's JSON annotation into a per-cluster candidate list. Robust to code fences / stray text around the JSON.

Parse the model's JSON annotation into a per-cluster candidate list.
Robust to code fences / stray text around the JSON.

## Usage

``` r
cv_cellmarkup_parse(text, cluster_names, top_k = 3L)
```
