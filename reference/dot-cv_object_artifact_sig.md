# The change-detection signature for one stored object.

Pulled out of cv_sync_object_artifacts() in Round LIV so the index hook
and the materialization path cannot drift apart on what "changed" means.

## Usage

``` r
.cv_object_artifact_sig(rec)
```
