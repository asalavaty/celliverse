# Write the celltype-annotation TXT for a TypoClust object.

TypoClust\$cell_types is a named list (one entry per annotated set, e.g.
"C1") whose value is a data.frame of ranked candidate matches with a
\`CellType\` column (row 1 = top call). We emit a small tab-separated
summary: the top cell type for each set. Returns TRUE if a file was
written.

## Usage

``` r
.cv_write_typoclust_txt(val, path)
```
