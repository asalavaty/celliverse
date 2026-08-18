# Gene symbols named in a request, VALIDATED against the object itself.

Round LXXVI. Deliberately not a clever gene-symbol regex: a loose shape
test produces candidates, and the object decides which of them are real.
A token that is not a feature of this object simply yields no rows in
cv_find_marker_in_object(), so a false positive is not merely unlikely,
it is structurally impossible. Round LXXIV's \#11 spent a whole
iteration on a false-positive detector; this one cannot have that
failure mode.

## Usage

``` r
.cv_candidate_genes(request_text)
```
