# Turn a CONNECTION-level failure (no HTTP status was ever returned) into a clear, provider-named message.

\`req_perform()\` throws before any status when DNS fails, the
connection is refused, TLS fails, or the request times out. Left raw,
the user sees httr2/curl's "Could not resolve host:
generativelanguage.googleapis.com". This classifies the underlying curl
message and names the provider + the likely cause, and — because
CelliVerse can run fully offline — points cloud failures at Ollama.
Always aborts. \`provider\` may be NULL (generic wording).

## Usage

``` r
cv_llm_connection_error(provider, cond)
```
