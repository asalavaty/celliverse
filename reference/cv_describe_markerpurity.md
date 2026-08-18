# Describe a MarkerPurity result.

Three levels: the single top-level element names the GROUPING
(\`within_clusters\` or \`within_cells\`), inside it the keys are marker
types, and inside those are the analysed group ids. The ids are unioned
across marker types rather than taken from the first, because a group
can be absent from one type's table while present in another – taking
the first would under-report the set the model is allowed to reference.

## Usage

``` r
cv_describe_markerpurity(x)
```

## Arguments

- x:

  a MarkerPurity object.

## Value

a named list of type-specific descriptor fields.
