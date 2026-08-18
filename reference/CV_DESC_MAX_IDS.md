# Upper bound on how many ids a descriptor stores.

Deliberately far above any realistic clustering (the previous 20/40 were
not). This exists to stop a pathological object from putting an
unbounded string in the system prompt, not to summarize – reaching it is
announced.

## Usage

``` r
CV_DESC_MAX_IDS
```

## Format

An object of class `integer` of length 1.
