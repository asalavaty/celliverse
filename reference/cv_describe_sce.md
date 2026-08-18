# Describe a SingleCellExperiment or SpatialExperiment.

Both classes route here: the fields the agent needs (dims, assay names,
colData columns) are read through the same \`SummarizedExperiment\`
accessors for either, so a separate branch would be two copies of one
function. The spatial coordinates of a SpatialExperiment are
deliberately NOT summarized – no tool consumes them yet, and an unused
field in the system prompt is a cost paid on every turn.

## Usage

``` r
cv_describe_sce(x)
```

## Arguments

- x:

  a SingleCellExperiment or SpatialExperiment.

## Value

a named list of type-specific descriptor fields.
