# Construct one parameter spec (JSON-schema-ish)

Construct one parameter spec (JSON-schema-ish)

## Usage

``` r
cv_param(
  type,
  description = "",
  default = NULL,
  required = FALSE,
  enum = NULL,
  handle_types = NULL,
  items = NULL,
  min = NULL,
  max = NULL
)
```

## Arguments

- type:

  "string" \| "integer" \| "number" \| "boolean" \| "array" \| "object"
  \| "handle"

- description:

  human/LLM description.

- default:

  default value (matches the real formals default).

- required:

  logical.

- enum:

  optional allowed values (from \`c(...)\` defaults in formals).

- handle_types:

  if type=="handle", the acceptable object types.

- items:

  element type for arrays.
