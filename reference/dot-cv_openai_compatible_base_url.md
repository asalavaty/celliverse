# Base URL for an OpenAI-compatible provider, or NULL if not one.

\`openai\` itself returns NULL (it uses the adapter default base_url);
only the additional OpenAI-compatible providers return an explicit
endpoint here. \`lmstudio\` is the local LM Studio server (default
http://localhost:1234/v1); its host is user-configurable
(\`lmstudio_host\`) so the same provider slot also covers other
OpenAI-compatible local servers (llama.cpp, Jan).

## Usage

``` r
.cv_openai_compatible_base_url(provider, config = NULL)
```
