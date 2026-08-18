# Find named gene(s) anywhere in a ClustoCell's / MarkoCell's stored markers.

Searches ALL THREE marker classes at BOTH levels – the whole point of
the item – and reports where each gene really is, with the numbers
already computed and stored on the object. Free: no recomputation, no
worker.

## Usage

``` r
cv_find_marker_in_object(obj, features, sets = NULL)
```

## Arguments

- obj:

  a ClustoCell or MarkoCell.

- features:

  gene symbol(s) to find.

- sets:

  optional set ids to restrict to (e.g. "C1"); NULL searches all.

## Value

a data.frame with Feature, Level, Membership, Type, Gini_Score, Purity,
Rank – the same shape celliverse::featureInspect() returns, so the two
are directly comparable. Zero rows when the gene is in none of them.
