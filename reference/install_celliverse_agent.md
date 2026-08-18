# Prepare this machine to run the CelliVerse agent

The agent is **cloud-first**: it runs out of the box with a cloud
provider (set an API key in Settings) and needs *no* local model. This
installer verifies the R web-stack, creates `~/.celliverse` + a default
config, and *optionally* sets up a local runtime - it detects Ollama
(and pulls a tier model when found) and detects LM Studio (via its `lms`
CLI). A local model is never required; you can install Ollama and/or LM
Studio at any time later and switch providers in Settings. The config
`default_model` is only pointed at a local Ollama model when Ollama is
actually present (or you pass `model =` explicitly); otherwise the cloud
default is left untouched.

## Usage

``` r
install_celliverse_agent(
  tier = c("auto", "light", "recommended", "strong", "both", "all"),
  model = NULL,
  pull_model = TRUE,
  install_r_deps = TRUE
)
```

## Arguments

- tier:

  which local model tier(s) to pull when Ollama is present: \`"auto"\`
  (default; detect RAM and pick the best tier that fits), \`"light"\`
  (small, fast), \`"recommended"\` (MoE, strong tool-calling, needs ~24
  GB), \`"strong"\` (largest, needs ~36 GB), \`"both"\` (light+strong),
  or \`"all"\` (all three). Ignored if \`model\` is given. No effect
  when Ollama is absent.

- model:

  explicit local Ollama model id to pull. Overrides \`tier\` when
  supplied, and (unlike \`tier\`) also sets the config \`default_model\`
  even if Ollama is not yet installed. Defaults to the
  hardware-recommended tier.

- pull_model:

  actually run \`ollama pull\` if Ollama is found.

- install_r_deps:

  install any missing R web-stack packages from Suggests.

## Value

invisibly, a named list summarising what was found/done.
