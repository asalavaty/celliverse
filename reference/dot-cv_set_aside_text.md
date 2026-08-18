# One "N cells set aside" clause for cv_summary_line(), or nothing.

Round LXX (audit \#16). Silent when the flag is FALSE, because "0
isolated cells" on every ordinary clustering is noise that trains the
reader to skip the whole line. When the flag is TRUE but the count is NA
the wording drops to "some": contract rule 6 says unknown and none must
not be collapsed, and inventing a number here would be exactly that
collapse in the other direction.

## Usage

``` r
.cv_set_aside_text(flag, n, label)
```
