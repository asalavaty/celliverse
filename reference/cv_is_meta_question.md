# Detect an annotation/cell-type request that does NOT name a method.

Returns TRUE when the user's message asks to annotate / label / identify
the cell type of a cluster/set but never mentions a method keyword (LLM,
ceLLMarkup, GPT, MarkerDB, marker database, typoClust). In that case the
agent must ASK which method to use (Marker DB vs LLM) rather than
picking one. Provider/model-agnostic: this is a deterministic pre-flight
check, so a weak model that would otherwise auto-route to
annotateCellsLLM is stopped before any LLM call.

## Usage

``` r
cv_is_meta_question(msg)
```
