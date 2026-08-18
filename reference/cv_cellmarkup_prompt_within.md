# The agent's stage-two prompt: sub-clusters read WITHIN a known parent.

Deliberately built by EXTENDING \`cv_cellmarkup_prompt()\` rather than
by writing a second prompt. That builder carries work this one must not
lose – Round XXIII's lineage-exclusion rule (the negative-marker
evidence that separates CD8+ T from NK) and Round LXV's data fencing for
untrusted cluster names and gene symbols. A hand-written copy would
start without either, and nobody would notice until an annotation came
back wrong.

## Usage

``` r
cv_cellmarkup_prompt_within(
  markers,
  tissue,
  condition,
  species,
  top_k,
  parent_id,
  parent_type
)
```

## Details

The core function keeps its own \`.cv_cellmarkup_prompt_within()\`
because the two BASE prompts genuinely differ. What the two
implementations share is the SEQUENCING, and that lives in
\`.cv_cellmarkup_hierarchy()\`.

The escape hatch in the last instruction matters as much as the
constraint: a sub-cluster that really is not a variety of its parent –
contamination, a doublet – must stay reportable, or the constraint
manufactures agreement instead of testing it.
