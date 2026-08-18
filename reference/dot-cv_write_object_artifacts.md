# Write all downloadable files for ONE object; return the relative filenames.

Always writes the portable .rds. Adds a convenience export when the
object is a kind the user typically wants as plain text/table: CellSet
-\> barcodes .txt TypoClust -\> celltypes .txt DatasetMarkers -\>
combined markers .txt data.frame -\> .csv character vector-\> values
.txt

## Usage

``` r
.cv_write_object_artifacts(val, type, handle, artifacts_dir)
```
