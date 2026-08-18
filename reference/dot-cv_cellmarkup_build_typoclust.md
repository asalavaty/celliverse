# Assemble a TypoClust-compatible object with the columns typoClustVis() and addTypoData() expect. LLM-specific fields (Confidence, Reason) are kept; markerDB-style evidence columns are filled from the input panels so the visualization functions work unchanged.

Assemble a TypoClust-compatible object with the columns typoClustVis()
and addTypoData() expect. LLM-specific fields (Confidence, Reason) are
kept; markerDB-style evidence columns are filled from the input panels
so the visualization functions work unchanged.

## Usage

``` r
.cv_cellmarkup_build_typoclust(
  parsed,
  markers,
  tissue,
  condition,
  species,
  pos_panels,
  neg_panels
)
```
