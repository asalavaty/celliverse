# Advisory cross-tissue warning for an UNFILTERED markerDB typoClust run.

Round XIX: with tissue=NULL, typoClust searches every tissue in the
Marker DB and a spurious high-overlap entry from an irrelevant tissue
can out-score the biologically correct one (pbmc3k C3 -\> "NK Cell
(Pronephros/Healthy)" instead of "T Cell (Blood/Healthy)"). When the run
was NOT tissue-filtered and one or more sets' rank-1 tissue differs from
the majority tissue across all sets, append an advisory naming the
outlier set(s), their tissue, the majority tissue, and the fix (re-run
with an explicit tissue=). Advisory only - it never overrides the top
hit. Returns "" when there is nothing to warn about (single tissue, or a
tissue-filtered run). Advisory for a run where the user ASSERTED a
tissue (audit \#15).

## Usage

``` r
.cv_typoclust_explicit_tissue_warning(res, tissue)
```

## Details

Round LXV Batch 2b. When \`tissue\` is set, every markerDB hit comes
from that tissue by construction, so the cross-tissue outlier check is
structurally unable to fire – which is why the advisory was previously
switched off entirely for this case. That left the most likely user
error completely unflagged: asserting "Brain" for a PBMC dataset.

What is checkable here is whether the asserted tissue was PRODUCTIVE. A
tissue with no useful entries for these markers yields Unknown/empty
labels for most sets, and the asserted tissue is then the first thing to
doubt – well before the biology.

Advisory only. It never overrides the result, and it stays silent
whenever the run produced real labels, because a warning that fires on
good runs is how users learn to ignore warnings.
