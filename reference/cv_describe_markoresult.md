# Describe a MarkoCell result.

The structure is two levels deep and the levels are easy to mistake for
each other: \`\$cell_subset_markers\` is keyed by marker TYPE (positive
/ negative / medium), and the analysed cell-subset names live one level
in. Reading names off the outer list would report "positive, negative"
as if they were subsets. The loop picks the first non-empty type because
every type holds the same set of subsets; which one supplies the names
does not matter, only that an empty one is skipped.

## Usage

``` r
cv_describe_markoresult(x, type)
```

## Arguments

- x:

  a MarkoCell object.

- type:

  the type label from the dispatch switch.

## Value

a named list of type-specific descriptor fields.

## Details

\`type\` is accepted for call-site symmetry with the MarkoClust branch
of \`cv_describe_object()\`'s switch; the returned fields are the same
either way, since a genuine MarkoClust is class ClustoCell and never
reaches here.
