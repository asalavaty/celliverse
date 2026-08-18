# Wrap an Ollama request so a CONNECTION failure (server not running, wrong host/port, DNS) becomes an actionable message instead of a raw httr2/curl condition. HTTP status errors (already turned into cv_llm_http_error by the poster / stream path) are re-raised unchanged so their provider hints survive. Only genuine transport failures get the "is ollama serve running?" hint.

Wrap an Ollama request so a CONNECTION failure (server not running,
wrong host/port, DNS) becomes an actionable message instead of a raw
httr2/curl condition. HTTP status errors (already turned into
cv_llm_http_error by the poster / stream path) are re-raised unchanged
so their provider hints survive. Only genuine transport failures get the
"is ollama serve running?" hint.

## Usage

``` r
cv_with_ollama_connect_hint(host, expr)
```
