# Detect a markerDB/typoClust annotation request that has NOT yet specified a tissue/condition -\> the agent must ask (dropdowns) before running. Round XIX: an unfiltered cross-tissue search mis-annotated C3 as a Pronephros NK cell, so every markerDB annotation now first collects Tissue + Condition (either may be "All" = no filter). Returns TRUE only when the method is markerDB/typoClust AND no tissue=/condition= directive is present yet.

Detect a markerDB/typoClust annotation request that has NOT yet
specified a tissue/condition -\> the agent must ask (dropdowns) before
running. Round XIX: an unfiltered cross-tissue search mis-annotated C3
as a Pronephros NK cell, so every markerDB annotation now first collects
Tissue + Condition (either may be "All" = no filter). Returns TRUE only
when the method is markerDB/typoClust AND no tissue=/condition=
directive is present yet.

## Usage

``` r
cv_is_markerdb_annotation(msg)
```
