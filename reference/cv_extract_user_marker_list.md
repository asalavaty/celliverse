# Detect a user-supplied marker gene panel in a request (e.g. "annotate using markers CD3E, CD8A, IL7R" or "with markers CD3D CD3E CD8A"). Returns a list with \`markers\` (character vector, possibly length 0) and \`n\` (the count). Round XXI: when the user provides their own markers, n is fixed to the list length and the picker's n field is hidden. A "gene-like" token is 2+ chars of uppercase letters/digits (allowing a trailing digit/letter, e.g. CD3E, IL7R, MS4A1, PF4); we require \>=2 such tokens to avoid false positives.

Detect a user-supplied marker gene panel in a request (e.g. "annotate
using markers CD3E, CD8A, IL7R" or "with markers CD3D CD3E CD8A").
Returns a list with \`markers\` (character vector, possibly length 0)
and \`n\` (the count). Round XXI: when the user provides their own
markers, n is fixed to the list length and the picker's n field is
hidden. A "gene-like" token is 2+ chars of uppercase letters/digits
(allowing a trailing digit/letter, e.g. CD3E, IL7R, MS4A1, PF4); we
require \>=2 such tokens to avoid false positives.

## Usage

``` r
cv_extract_user_marker_list(msg)
```
