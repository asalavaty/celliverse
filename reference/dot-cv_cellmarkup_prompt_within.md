# Build the stage-two prompt: sub-clusters read WITHIN a known parent identity.

The difference from \`.cv_cellmarkup_prompt()\` is the whole point of
the feature, so it is stated plainly to the model rather than hinted at:
these sets are sub-populations of one already-identified cell type, the
markers shown are the SUB-CLUSTER'S OWN, and the answer wanted is which
subtype or state of that parent type each one is.

## Usage

``` r
.cv_cellmarkup_prompt_within(
  markers,
  parent_id,
  parent_type,
  sample_source,
  feature_type,
  tissue,
  condition,
  species,
  top_k
)
```

## Details

The escape hatch in the last instruction is deliberate and matches the
guidance in \`typoPrompt()\`: a sub-cluster that genuinely is not a
variety of its parent (contamination, a doublet, a mis-split) must be
reportable as such, or the constraint would turn into a way of
manufacturing agreement.
