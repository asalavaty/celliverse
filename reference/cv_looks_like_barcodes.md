# Do these look like cell barcodes rather than gene names?

10x barcodes are long ACGT runs with an optional \`-1\` lane suffix,
which no gene symbol resembles. Used to decide a count table's
ORIENTATION, which is the one genuinely ambiguous thing about a CSV of
counts.

## Usage

``` r
cv_looks_like_barcodes(x, min_frac = 0.8)
```
