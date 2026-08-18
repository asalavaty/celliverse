# Launch the CelliVerse agent (API + UI)

Starts the local plumber API, serves the prebuilt React UI from
\`inst/react-app/\`, binds to localhost, and (optionally) opens a
browser. The agent runs cloud-first: no local model is required — pick a
cloud provider + API key in Settings, or install Ollama / LM Studio
later for fully-offline local models.

## Usage

``` r
run_celliverse_agent(
  port = NULL,
  host = "127.0.0.1",
  provider = NULL,
  model = NULL,
  browser = interactive(),
  background = FALSE,
  port_scan = TRUE,
  max_port_tries = 20L
)
```

## Arguments

- port:

  TCP port to bind (default from config, else 8000).

- host:

  host to bind; keep 127.0.0.1 for localhost-only (default).

- provider:

  optional provider override for this run (writes to config).

- model:

  optional model override for this run.

- browser:

  open a browser window automatically.

- background:

  run the server in a background process (returns a handle) instead of
  blocking the console.

- port_scan:

  if \`TRUE\` (default) and \`port\` is already in use, bind the next
  free port instead of failing; if \`FALSE\`, a busy port is a hard
  error.

- max_port_tries:

  how many ports above \`port\` to probe when scanning (default 20).

## Value

invisibly, the plumber router (foreground) or a process handle
(background).
