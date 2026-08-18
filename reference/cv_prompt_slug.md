# Stable, human-readable id for a prompt.

Derived from the LABEL rather than minted from a counter or a timestamp,
so the same prompt gets the same id on every machine, the file stays
diffable, and the tests are deterministic. \`prefix\` separates the two
namespaces: \`builtin:\` ids may appear in the \`hidden\` list,
\`user:\` ids may not.

## Usage

``` r
cv_prompt_slug(label, prefix = "user")
```

## Details

Deliberately NOT random: Round LXXX's logging notes make the same
argument the other way round, and here determinism is what lets a
built-in stay hidden across a restart.
