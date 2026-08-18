# Write the saved-prompts file atomically.

Temp file plus rename, so a second browser tab (or a crash mid-write)
can never leave a half-written JSON document where the favourites live.
Returns the normalised store invisibly.

## Usage

``` r
cv_prompts_save(store, path = cv_prompts_path())
```
