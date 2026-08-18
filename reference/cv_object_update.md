# Update an EXISTING object in place, reusing the same handle

Used by update-style tools (addClustoData / addTypoData) that add
metadata to an object the user already loaded: instead of storing the
modified object under a fresh handle (which duplicates a potentially
large Seurat/SCE in the session environment), we overwrite the value at
the existing handle and refresh its descriptor. The object TYPE must not
change (a Seurat stays a Seurat) - this is a guardrail against a handler
accidentally swapping the object class under a stable handle the
model/UI are still referencing.

## Usage

``` r
cv_object_update(store, handle, value, source = "")
```

## Arguments

- store:

  object store environment.

- handle:

  an existing handle.

- value:

  the updated R object (same canonical type as the current value).

- source:

  short human string describing the update.

## Value

the (unchanged) handle.
