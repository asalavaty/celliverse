# Add one saved prompt.

Validates and returns the WHOLE list rather than just the new row, so
the client never has to merge server state into its own copy – the same
contract the settings endpoint uses, and the reason two open tabs cannot
drift.

## Usage

``` r
cv_prompts_add(
  label,
  text,
  category = CV_PROMPT_DEFAULT_CATEGORY,
  store = cv_prompts_load()
)
```

## Details

Errors are sentences in the approved voice: what happened, what to do
next, no apology and no exclamation mark.
