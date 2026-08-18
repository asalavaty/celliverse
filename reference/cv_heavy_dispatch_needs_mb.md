# The memory a heavy dispatch on an already-resident object needs, in MB.

Two terms, not one:

## Usage

``` r
cv_heavy_dispatch_needs_mb(bytes, n_cells_used, n_cells_total)
```

## Arguments

- bytes:

  the object's own resident size (\`cv_object_bytes()\`).

- n_cells_used:

  how many cells the dispatch will actually cluster (the sketch size, or
  the total when not sketching).

- n_cells_total:

  the object's total cell count.

## Value

numeric MB, or \`NA_real\_\` when \`bytes\` cannot be measured.

## Details

FLOOR – \`cv_heavy_dispatch_floor_mb(bytes)\`. Paid on the FULL object
(\`n_cells_total\`), before any sketch is taken, so no \`sketch_ncells\`
reduces it. SCALED – \`bytes \* CV_SEURAT_EXTRA_FACTOR \* (n_cells_used
/ n_cells_total)\`. The final clustering step, sized to however many
cells are actually used – all of them, or the sketch. This is the term
\`sketch_ncells\` actually buys down.
