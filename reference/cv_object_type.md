# Return a canonical object-type string used by the typed tool registry

The registry declares which input_object_types a tool accepts and what
output_object_type it produces; matching these prevents invalid tool
chains (e.g. addClustoData before clustoCell).

## Usage

``` r
cv_object_type(value)
```
