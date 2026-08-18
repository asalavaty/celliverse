# Is this state entry still waiting to be written?

Entries written before Round LIV carry no \`pending\` field at all, and
absent means FALSE (already materialized) – so an existing session's
state file keeps working untouched.

## Usage

``` r
.cv_state_is_pending(entry)
```
