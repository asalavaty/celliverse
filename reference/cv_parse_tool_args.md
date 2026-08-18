# Parse a JSON arguments string safely into a named list

Batch 8b: this used to return a bare \`list()\` for SIX different
situations – arguments absent, \`""\`, \`"null"\`, JSON truncated
mid-stream, a JSON array, or a bare scalar – making "the model sent
nothing" and "the model sent something I could not read" the same value.

## Usage

``` r
cv_parse_tool_args(x)
```

## Arguments

- x:

  the raw \`arguments\` field from the provider.

## Value

a list. When the input could not be read as a JSON object, the list is
empty and carries \`attr(., "cv_parse_failed")\`, a short reason string.

## Details

WHY THAT MATTERED, traced end to end. \`clustoCell\` declares exactly
one required parameter (\`data\`, a handle) and defaults for the rest.
Given \`list()\`, \`cv_resolve_args()\` auto-supplies the required
handle when a single typed object is loaded, then fills every other
default. So a \`clustoCell\` call whose arguments were truncated by a
dropped stream ran a full multi-minute clustering on parameters the
model never chose, and reported \*\*success\*\* – the worst failure
shape there is, because nothing anywhere says a value was lost. The
turn's ledger then refused a correctly-formed retry of the same tool, so
the model could not fix it either.

A genuinely empty call is still \`list()\` and still runs, unchanged.
Only the unreadable cases are now marked, with a \`cv_parse_failed\`
attribute carrying the reason; \`cv_run_tool_call()\` turns that into a
tool error telling the model to re-send. Marking rather than throwing
keeps the decision at the call site, where the tool and its arguments
are both in view.
