# Compact "set: rank-1 cell type" map for a TypoClust object.

WHY this exists (Round XVIII): the agent previously surfaced only
"Created TypoClust: N annotated set(s) \[...\]" to the model, NEVER the
predicted labels. The model then HALLUCINATED the annotation table and
defaulted the pbmc3k platelet cluster (C5) to the common "NK cells" -
even though celliverse::typoClust() correctly returns Platelet. Exposing
the rank-1 label per set makes the model report the REAL annotation.

## Usage

``` r
cv_typoclust_top_labels(x)
```

## Arguments

- x:

  a TypoClust object.

## Value

a named character vector: name = set id, value = label string. Sets
whose label could not be read are dropped, so the result may be shorter
than \`names(x\$cell_types)\`.

## Details

Handles both TypoClust flavours: \* markerDB (celliverse::typoClust):
columns CellType/Tissue/Condition/ Combined_Score/Rank -\> "Platelet
(Blood/Healthy)". \* ceLLMarkup (annotateCellsLLM): columns
CellType/Confidence/Rank/Reason -\> "B cell". Tissue/Condition live in
x\$metadata, not per-row.
