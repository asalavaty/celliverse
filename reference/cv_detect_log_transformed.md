# Heuristically detect whether a count matrix / Seurat layer holds RAW counts or already NORMALIZED log-transformed values.

WHY: users sometimes load data whose "counts" layer actually holds
log-normalized values. Feeding that to a tool with log1p=TRUE would
double-transform. Heuristic (best-effort): sample the values; data is
treated as LOG/NORMALIZED when (a) any sampled value is non-integer, OR
(b) the max sampled value is suspiciously small for raw UMI counts (\<
~30). Returns "counts", "log", or "unknown" (empty/undetermined).

## Usage

``` r
cv_detect_log_transformed(x, assay = NULL, layer = "counts")
```
