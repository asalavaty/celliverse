# Detect a request for an analysis CelliVerse does not perform.

Round LXVII. Reported from live use: "run differential expression
between C1 and C2" produced the generic recovery line ("I wasn't able to
turn that into an action just now. Please restate...") rather than
saying the analysis is out of scope.

## Usage

``` r
cv_out_of_scope_request(msg)
```

## Arguments

- msg:

  The user message.

## Value

\`NULL\` when in scope; otherwise a list(topic=, alternative=).

## Details

Round LXVI added a capability-boundary rule to the system prompt, and
that rule was not the problem – the model never got the chance to
answer. The request reads as ACTIONABLE (an imperative verb plus cluster
ids), so when no tool call followed, \`recovery_exhausted\` fired and
OVERWROTE whatever the model had said with the canned restate-it
message. A prompt rule cannot win against a branch that discards its
output.

So the boundary is enforced deterministically instead, in R, before any
LLM call – the same pattern as the annotation method picker. The model
is not asked to decline; the server declines.
