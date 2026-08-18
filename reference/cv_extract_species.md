# Extract an explicit \`species=\` directive from a request, if present.

Round LXIV (Batch 1b). The annotation flow now collects species in its
own step before tissue/condition, because the Tissue and Condition
vocabularies are per-species. This is how each stage knows whether that
step has already been answered.

## Usage

``` r
cv_extract_species(msg)
```

## Arguments

- msg:

  The user message.

## Value

The species as a lowercase string, or \`NULL\` when absent/empty.
