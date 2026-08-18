# Describe a DatasetMarkers result.

\`\$combined_markers\` is a flat character vector of gene names (the
deduplicated union of cluster and sub-cluster positive markers), so
\`length()\` is the count – not \`nrow()\`, which would be NULL and
surface as a missing count. \`markers_head\` is capped at 15: enough for
the model to see what kind of markers these are, far short of a marker
table.

## Usage

``` r
cv_describe_datasetmarkers(x)
```

## Arguments

- x:

  a DatasetMarkers object.

## Value

a named list of type-specific descriptor fields.
