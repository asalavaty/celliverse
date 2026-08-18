# Scan the artifacts dir and build the results manifest (single source of truth).

Read-only with respect to objects: it never writes or deletes object
files (safe for a restored session whose store is empty). It DOES
(over)write manifest.json so the on-disk manifest matches what /api
returns and what the zip bundles.

## Usage

``` r
cv_build_manifest(store, artifacts_dir, session_id = NULL)
```

## Details

Kinds: "figure" (svg/png/pdf, grouped by stem), "table" (csv), "rds",
"text" (txt), "other". Object-derived files also carry
handle/type/summary/source.
