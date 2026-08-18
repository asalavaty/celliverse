# Does a heavy dispatch on \`args\[\[data_arg\]\]\` fit this machine, and if not, can sketching fix it?

The one function the validate hooks call. Reuses the store's cached
descriptor (\`.cv_arg_descriptor()\`, agent_tools_core.R) for cell
counts rather than re-deriving them, and \`cv_memory_budget_mb()\`
(Round LXXXIII) for the ceiling. Silent (\`fits = TRUE\`) whenever
anything needed cannot be measured – no opinion means proceed, the same
rule \`cv_upload_advice()\` and \`cv_conversion_advice()\` already
follow.

## Usage

``` r
cv_heavy_dispatch_route(
  store,
  args,
  data_arg = "data",
  sketch_arg = "sketch_ncells"
)
```

## Arguments

- sketch_arg:

  name of the integer parameter naming the sketch size, when the tool
  has one. Only consulted when \`args\$sketch\` is TRUE.

## Value

a list: \`fits\` (logical), \`bytes\`, \`n_cells_total\`,
\`n_cells_used\`, \`needs_mb\`, \`budget_mb\`, \`sketch_can_help\`
(logical, only meaningful when \`fits\` is FALSE),
\`suggested_sketch_ncells\` (integer or NA).
