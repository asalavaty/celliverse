# Construct a single registry tool entry

Construct a single registry tool entry

## Usage

``` r
cv_tool(
  name,
  description,
  parameters = list(),
  input_object_types = character(0),
  output_object_type = NA_character_,
  handler,
  cost = c("light", "heavy"),
  produces = "metadata",
  tier = c("core", "advanced"),
  next_suggestions = character(0),
  result_note = NULL,
  dispatch_cost = NULL,
  heavy_impl = NULL,
  prepare = NULL,
  validate = NULL
)
```
