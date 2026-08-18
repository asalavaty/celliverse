# Try to recover a handle argument unambiguously.

Weak local models frequently (a) omit a required handle, (b) pass the
parameter name as a placeholder, or (c) echo a template string such as
"your_object_handle"/"seurat_object". This helper recovers a real handle
ONLY when the choice is unambiguous, and returns \`NULL\` otherwise so
the caller can raise a clear error and let the model self-correct.

## Usage

``` r
cv_try_resolve_handle(val, nm, p, store)
```

## Arguments

- val:

  the supplied value (use \`NA\`/\`NA_character\_\` for a MISSING arg).

## Details

Policy (in order): 1. Exactly one loaded object matches the required
\`handle_types\` -\> use it. 2. \`val\` is a recognizable
placeholder/template token AND exactly one object exists overall -\> use
it. Anything else (in particular: a placeholder when \>1 typed candidate
exists) -\> NULL. We deliberately do NOT pick the "first" typed
candidate: when the choice is genuinely ambiguous the loop surfaces the
standard "requires a valid handle - available: ..." error to the USER so
they can pick, rather than the agent silently guessing (which previously
tie-broke alphabetically and could operate on the wrong / older object).
