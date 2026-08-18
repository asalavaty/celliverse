# Describe a TypoClust annotation result.

Unlike every other descriptor, this one carries a RESULT and not just a
shape: \`top_labels\` holds the rank-1 cell type per annotated set. That
is deliberate and is the fix from Round XVIII – with only counts
exposed, the model hallucinated the annotation table and reported the
pbmc3k platelet cluster as "NK cells" while \`typoClust()\` had
correctly returned Platelet. The predicted labels are small, and they
are the whole point of the object.

## Usage

``` r
cv_describe_typoclust(x)
```

## Arguments

- x:

  a TypoClust object.

## Value

a named list of type-specific descriptor fields.
