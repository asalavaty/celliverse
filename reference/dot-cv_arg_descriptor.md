# The descriptor behind a handle-typed argument, or NULL when there isn't one.

Returns NULL rather than aborting for every "cannot tell" case – an
absent argument, a handle that is not in the store, a type with no
descriptor. A validator that cannot see the object must stay silent:
refusing on missing information would turn a diagnostic into a new
failure mode.

## Usage

``` r
.cv_arg_descriptor(store, args, data_arg)
```
