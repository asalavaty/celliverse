# Single source of truth for every LLM provider the agent can talk to.

One entry per provider, in the order providers should be presented
(\`.cv_supported_providers\` is derived from \`names()\` of this list,
so reordering an entry here reorders the provider list everywhere it's
used).

## Usage

``` r
.cv_provider_registry
```

## Format

An object of class `list` of length 9.

## Details

Fields per entry:

- key_field:

  Config field holding the API key, or \`NULL\` for a local provider
  that needs none (ollama, lmstudio).

- key_envs:

  Character vector of environment variable names checked, in order,
  before falling back to the config-file value. Empty for keyless
  providers.

- needs_key:

  Logical; \`FALSE\` for ollama/lmstudio.

- base_url:

  \`function(config)\` returning this provider's base URL, or \`NULL\`
  if it doesn't route through the generic OpenAI-compatible base-URL
  path (openai uses its adapter's own hardcoded default;
  anthropic/gemini/ollama have dedicated adapters and never look at a
  base URL passed in from here).

- adapter:

  Name (character, NOT a function object) of the \`cv_chat\_\*()\`
  function this provider dispatches to when it doesn't go through the
  generic OpenAI-compatible base-URL path. Stored as a name and resolved
  via \`get()\` at call time – inside \`cv_chat()\`, well after package
  load – so this file's position in R CMD INSTALL's (alphabetical, no
  Collate field) file-collation order never matters.

- models_kind:

  Which model-listing strategy \`cv_api_provider_models()\` uses.
  \`"ollama"\` and \`"lmstudio"\` are handled by that function's own
  dedicated branches (each has a distinct response shape);
  \`"openai_like"\`, \`"anthropic"\`, and \`"gemini"\` dispatch to the
  matching \`cv_models\_\*()\` parser in agent_models.R.
