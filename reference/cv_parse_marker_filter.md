# Parse a marker-table filter request into a deterministic spec.

Returns a list with \`kind\` ("rows" \| "rank" \| "predicate"), \`n\`,
\`predicates\` (each \`list(col, op, value, value2)\`), \`limit\`,
\`unknown_cols\`, \`source\` ("request_text" \| "filter" \| "mode") and
a human-readable \`interpretation\`.

## Usage

``` r
cv_parse_marker_filter(
  request_text = NULL,
  filter = NULL,
  mode = c("rows", "rank"),
  top_n = 10L,
  columns = character()
)
```
