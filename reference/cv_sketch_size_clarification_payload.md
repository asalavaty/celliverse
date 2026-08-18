# Build the clarification payload for a heavy dispatch that needs a smaller sketch size than the one it was called with.

Only reached when \`route\$fits\` is FALSE and
\`route\$sketch_can_help\` is TRUE – \`.cv_assert_heavy_object_fits()\`
(agent_tools_core.R) handles the "cannot help at all" case as an
ordinary abort, since there is nothing left to offer there but an
explanation. Here there IS a concrete number, so it is offered as an
editable numeric field rather than just stated.

## Usage

``` r
cv_sketch_size_clarification_payload(
  store,
  args,
  tool,
  route,
  data_arg = "data"
)
```

## Details

Deliberately carries NO "proceed anyway" choice, unlike every other
advisory in this codebase (\`cv_upload_advice()\`,
\`cv_conversion_advice()\`) – this is the one check that exists
specifically so the machine never goes down, and a bypass would remove
the one property it is for.

The resume message is built from the RESOLVED call's own arguments, not
re-parsed from the user's original words: by this point the model has
already turned those words into a structured call, and that structure is
the more reliable source. Every other argument the call already carried
(besides the data handle and the sketch settings) is restated verbatim
so Continuing does not silently drop a choice the user already made.
