# Remove one prompt by id.

A BUILT-IN IS HIDDEN, NOT DELETED – there is no row to delete, and
recording the id means a starter the user does not want stays gone
across restarts while the definition of the starter set remains a single
list in R. A user prompt is dropped outright.

## Usage

``` r
cv_prompts_remove(id, store = cv_prompts_load())
```

## Details

Removing something that is already gone succeeds quietly. The user
pressed a button meaning "I do not want to see this", and after the call
they do not; erroring on a double click would be reporting a failure
that is not one.
