# Assemble a TypoClust-compatible object from parsed annotations. Mirrors typoClust()'s structure: list(cell_types=\<named list of dfs\>, metadata=...), class = "TypoClust". Adds an \`ann_method\` marker so tooling can tell ceLLMarkup annotations from markerDB ones.

Assemble a TypoClust-compatible object from parsed annotations. Mirrors
typoClust()'s structure: list(cell_types=\<named list of dfs\>,
metadata=...), class = "TypoClust". Adds an \`ann_method\` marker so
tooling can tell ceLLMarkup annotations from markerDB ones.

## Usage

``` r
cv_cellmarkup_build_typoclust(parsed, markers, tissue, condition, species)
```
