# Describe an object of a type the store does not know.

The end of \`cv_describe_object()\`'s switch. \`length()\` is the only
thing safe to ask of an arbitrary R object without risking a throw
(contract rule 2), and an unknown object still gets a handle and a
summary so the user can see that their upload arrived and can be
downloaded again.

## Usage

``` r
cv_describe_fallback(x)
```

## Arguments

- x:

  any object.

## Value

a named list with a single \`length\` field.
