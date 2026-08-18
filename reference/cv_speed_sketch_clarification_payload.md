# Build the clarification payload for the SPEED-only sketch offer (Round LXXXVI): a large dataset that fits in memory just fine but may be slow.

Unlike \`cv_sketch_size_clarification_payload()\` above, this one DOES
carry a "run on the full dataset" bypass (via \`choices\`) – sketching
here is a preference, not the only way to avoid a crash, so declining it
is a perfectly good answer and must stay one click away. The bypass's
\`resume_message\` is the plain, unmodified request:
\`.cv_offer_speed_sketch()\` (agent_tools_core.R) has already recorded
that this handle+tool was asked about on the session before raising
this, so the resumed call reaches validate() a second time and is let
through silently, however the model happens to phrase "no sketch" – see
that function's own comment for why the argument's resolved value cannot
be trusted to carry that distinction.

## Usage

``` r
cv_speed_sketch_clarification_payload(
  handle,
  tool,
  ncell,
  sketch_kind = c("ncells", "fraction")
)
```

## Details

\`sketch_kind\` picks the field being offered: clustoCell takes a cell
COUNT (\`sketch_ncells\`); markoClust takes a FRACTION per cluster
(\`sketch_fraction\`, only meaningful with
\`identify_subclusters=TRUE\`, which the caller has already checked
before reaching here).
