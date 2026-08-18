# Does umapPlot need to COMPUTE a fresh UMAP for this call, or can it just re-draw an existing embedding? Shared by the dispatch-cost classifier and the actual compute function so the two decisions can never disagree.

Does umapPlot need to COMPUTE a fresh UMAP for this call, or can it just
re-draw an existing embedding? Shared by the dispatch-cost classifier
and the actual compute function so the two decisions can never disagree.

## Usage

``` r
.cv_umap_needs_compute(so, reduction_arg)
```
