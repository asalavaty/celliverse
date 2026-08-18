# Derive major-cluster parentage from a vector of set ids.

Returns a named character vector, one entry per id, holding that id's
parent or \`NA\` when it has none. A set \`P\` is the parent of \`S\`
when \`S\` begins with \`paste0(P, "-")\` and \`P\` is itself one of the
ids. The LONGEST candidate wins, so a cluster called \`"C1"\` cannot
claim \`"C10-Sub1"\` – which a \`"-Sub\[0-9\]+\$"\` pattern would also
get right, but only by accident of the naming, whereas matching against
the real ids is exact.

## Usage

``` r
.cv_cellmarkup_parentage(set_names, enabled = TRUE)
```

## Details

Both levels have to be present. A call carrying only sub-clusters has no
parent identity available to inherit, so every entry comes back \`NA\`
and the caller takes its ordinary single-request path.
