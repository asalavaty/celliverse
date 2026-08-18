# Handles of all loaded objects whose type is in \`types\`.

A handle whose value cannot be typed is excluded rather than erroring:
this feeds tool-argument auto-resolution, where one unreadable object
must not stop the others from being offered.

## Usage

``` r
cv_objects_of_type(store, types = NULL)
```

## Arguments

- store:

  object store environment.

- types:

  character vector of canonical types (see \`cv_object_type()\`), or
  NULL/empty to return every handle.

## Value

a character vector of handles, possibly empty.
