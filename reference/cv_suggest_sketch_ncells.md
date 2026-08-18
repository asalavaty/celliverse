# A sketch size expected to fit this machine's budget, with headroom.

\`NA_integer\_\` when the FLOOR alone already exceeds \`budget_mb\` – no
\`sketch_ncells\` value can help in that case, by construction, since
the floor does not depend on it. Otherwise solves the SCALED term for
the largest fraction of cells the remaining headroom allows, backs off
20 safety margin (this is an estimate, not a guarantee), and never
suggests a number that is not strictly smaller than \`n_cells_total\`
(clustoCell itself refuses \`sketch_ncells \>=\` the cell count – see
\`.cv_assert_sketch_fits\`).

## Usage

``` r
cv_suggest_sketch_ncells(bytes, n_cells_total, budget_mb)
```
