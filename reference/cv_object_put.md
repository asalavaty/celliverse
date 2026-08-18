# Put an object into the store, returning its handle

Put an object into the store, returning its handle

## Usage

``` r
cv_object_put(store, value, handle = NULL, source = "")
```

## Arguments

- store:

  object store environment.

- value:

  the R object to store.

- handle:

  optional explicit handle; auto-generated if NULL.

- source:

  short human string describing where it came from.

## Value

the handle (character).
