# The FLOOR term shared by \`cv_heavy_dispatch_needs_mb()\`, \`cv_suggest_sketch_ncells()\`, \`cv_heavy_dispatch_route()\` and the abort message in \`.cv_assert_heavy_object_fits()\` (agent_tools_core.R) – factored into one place rather than four so they cannot silently drift apart, which is the failure mode Round LXIV found repeatedly elsewhere in this codebase ("two copies of one rule").

\`bytes \* (1 + CV_SEURAT_EXTRA_FACTOR)\`: one full copy for the callr
child's own \`saveRDS()\`/\`readRDS()\` (Round XXXVIII, unsketchable
because the child does the sketching itself, after this), plus the
preprocessing derived-structure cost already measured for a matrix -\>
Seurat build.

## Usage

``` r
cv_heavy_dispatch_floor_mb(bytes)
```

## Arguments

- bytes:

  the object's own resident size (\`cv_object_bytes()\`).

## Value

numeric MB, or \`NA_real\_\` when \`bytes\` cannot be measured.
