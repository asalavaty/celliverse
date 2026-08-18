# Lenient set-id resolver: case/separator-insensitive match against the available ids, returning NA for ids with no unique match (so the caller can report all unknowns at once). Unlike cv_resolve_set_ids it never aborts.

Lenient set-id resolver: case/separator-insensitive match against the
available ids, returning NA for ids with no unique match (so the caller
can report all unknowns at once). Unlike cv_resolve_set_ids it never
aborts.

## Usage

``` r
cv_resolve_set_ids_lenient(requested, available)
```
