# POST JSON and return parsed body; used by cloud adapters (non-stream path). \`provider\` (optional) lets connection failures name the provider clearly. \`classify_conn=FALSE\` re-raises the raw transport condition unchanged so a caller that owns its own connection wording (e.g. the Ollama wrapper) can classify it once, without this function nesting a generic message first.

POST JSON and return parsed body; used by cloud adapters (non-stream
path). \`provider\` (optional) lets connection failures name the
provider clearly. \`classify_conn=FALSE\` re-raises the raw transport
condition unchanged so a caller that owns its own connection wording
(e.g. the Ollama wrapper) can classify it once, without this function
nesting a generic message first.

## Usage

``` r
cv_http_post_json(
  url,
  body,
  headers = list(),
  timeout = 300,
  provider = NULL,
  classify_conn = TRUE
)
```
