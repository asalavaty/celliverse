# Bundle all artifacts (incl. manifest.json) into a temp .zip; return its path.

Prefers the cross-platform \`zip\` package (no system dependency); falls
back to utils::zip (system \`zip\`). Entries are stored with bare
filenames (no nested session path). Returns NULL when there is nothing
to zip or zipping fails.

## Usage

``` r
cv_artifacts_zip_file(artifacts_dir, session_id = NULL)
```
