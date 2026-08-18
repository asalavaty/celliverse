# The single LLM entry point used everywhere in the agent

The single LLM entry point used everywhere in the agent

## Usage

``` r
cv_chat(
  messages,
  provider,
  model,
  tools = NULL,
  temperature = 0.2,
  stream = FALSE,
  on_delta = NULL,
  config = cv_load_config(),
  response_format = NULL,
  ...
)
```

## Arguments

- messages:

  internal-schema list of messages.

- provider:

  one of .cv_supported_providers.

- model:

  model id string.

- tools:

  optional list of provider-neutral tool specs (from cv_tools_specs()).

- temperature:

  sampling temperature.

- stream:

  logical; stream tokens via on_delta if provided.

- on_delta:

  optional callback(list) for streaming events.

- config:

  effective config (for keys/hosts).

- ...:

  extra provider params.

## Value

internal normalized response list.
