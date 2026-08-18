# Build a handle from a user-supplied display name + the object's type prefix.

The upload "Optional name" field lets the user label an object (e.g.
"pbmc counts"). We turn that into a stable, handle-safe id by prefixing
the object type (mat\_/obj\_/df\_/...) and sanitizing the name:
lowercase, runs of non-alphanumeric characters collapsed to single
underscores, trimmed. Returns NULL when the name is empty/blank (caller
then falls back to a random id).

## Usage

``` r
cv_handle_from_name(value, name)
```
