# The rank-1 row of one TypoClust set's candidate table.

Round LV (Batch 5a): this four-line idiom existed verbatim in two
files - here (inside cv_typoclust_top_labels) and in
.cv_typoclust_tissue_warning() (agent_tools_core.R). Byte-identical in
both, so extracting it changes no behaviour; the point is that a
non-obvious idiom with a tryCatch fallback now has ONE definition. Batch
3a found the copy-paste that had already drifted; this is the same
shape, caught before it did.

## Usage

``` r
.cv_typoclust_rank1_row(df)
```

## Arguments

- df:

  one set's candidate data.frame.

## Value

a one-row data.frame.

## Details

The fallbacks are deliberate and are the reason this is not a one-liner:
a TypoClust may come from celliverse::typoClust() (which ranks) or from
annotateCellsLLM (which may not), and a malformed Rank column must
degrade to "first row" rather than error.
