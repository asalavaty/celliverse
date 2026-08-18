# Read a delimited count table (csv/tsv/txt/tab, optionally gzipped) into a sparse genes x cells matrix.

\`data.table::fread\` is already an import, auto-detects the separator,
and handles .gz, so no new dependency and no guessing at commas vs tabs.
The first column is taken as row names when it is not numeric – the near
universal layout for an exported count table.

## Usage

``` r
cv_read_delim_counts(path)
```
