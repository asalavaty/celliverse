# Describe a plain data.frame.

Column NAMES only, never a row of values: a data.frame in the store is
typically a marker or annotation table, and its rows are exactly the
data the descriptor exists to keep out of the prompt. \`columns\` is
uncapped because a table wide enough to matter does not occur here, and
the model needs the full set to name a column in a follow-up call.

## Usage

``` r
cv_describe_df(x)
```

## Arguments

- x:

  a data.frame.

## Value

a named list of type-specific descriptor fields.
