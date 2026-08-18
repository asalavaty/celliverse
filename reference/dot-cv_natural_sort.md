# Sort ids the way a human reads them: C2 before C10, C1-Sub2 before C1-Sub10.

Plain sort() is lexicographic, which both reads wrongly and – when
combined with truncation – drops ids from the MIDDLE of the range rather
than the end. Every digit run is zero-padded to a fixed width to build
the sort key, so the comparison stays a string comparison (no numeric
overflow, no locale surprises) while ordering numerically.

## Usage

``` r
.cv_natural_sort(x)
```

## Arguments

- x:

  character vector of ids.

## Value

\`x\`, reordered.
