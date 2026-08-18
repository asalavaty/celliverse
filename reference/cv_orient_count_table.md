# Decide whether a delimited count table is genes x cells (the convention) or cells x genes, and return it genes x cells either way.

Barcode-shaped names decide it outright when present. Otherwise the
larger dimension is taken to be genes, which is right for essentially
every real scRNA-seq matrix. Whatever is chosen is REPORTED to the user
rather than assumed silently: a wrong guess here transposes the whole
analysis, so it has to be visible and correctable, not buried.

## Usage

``` r
cv_orient_count_table(m)
```

## Value

list(mat = \<genes x cells\>, note = \<chr\>)
