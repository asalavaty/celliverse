# Split clusters into batches that each fit \`usable_tokens\`, counting BOTH the prompt and the reply the model is being asked to produce.

Greedy first-fit in cluster order. A single cluster too large to fit
even alone still gets its own batch – there is nothing left to split –
but the generation cap applied alongside means that request is bounded
anyway.

## Usage

``` r
cv_cellmarkup_batch_clusters(
  markers,
  tissue,
  condition,
  species,
  top_k,
  usable_tokens
)
```

## Value

list of character vectors of cluster ids, together covering every input
cluster exactly once, in order.

## Details

The fixed scaffolding cost is measured from the real prompt text (build
a one-cluster prompt, subtract that cluster's block) rather than
hardcoded, so the estimate tracks the system prompt as it is edited
instead of drifting.
