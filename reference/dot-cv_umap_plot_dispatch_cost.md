# Per-call dispatch-cost classifier for the umapPlot tool (cv_tool()'s \`dispatch_cost\` field, consulted by cv_make_dispatcher()). Resolves the target Seurat object via a cheap store lookup (no computation) and checks whether a usable reduction already exists. Falls back to "heavy" – the safe default of timeout + progress + cancel – if the handle can't be resolved or isn't a Seurat object; the real, specific error still surfaces from .cv_umap_plot_compute() a moment later either way, so this classifier's only job is choosing a dispatch path, never validation.

Per-call dispatch-cost classifier for the umapPlot tool (cv_tool()'s
\`dispatch_cost\` field, consulted by cv_make_dispatcher()). Resolves
the target Seurat object via a cheap store lookup (no computation) and
checks whether a usable reduction already exists. Falls back to "heavy"
– the safe default of timeout + progress + cancel – if the handle can't
be resolved or isn't a Seurat object; the real, specific error still
surfaces from .cv_umap_plot_compute() a moment later either way, so this
classifier's only job is choosing a dispatch path, never validation.

## Usage

``` r
.cv_umap_plot_dispatch_cost(store, call_args)
```
