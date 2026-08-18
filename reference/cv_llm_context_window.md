# Usable context window, in tokens, for the provider this call will use.

Conservative by construction: a local server cannot be asked what
context it loaded a model with, so for lmstudio we assume the small
default unless the user overrides it via \`lmstudio_num_ctx\`. Cloud
providers get a value large enough that the batching below is a no-op at
any realistic cluster count, which keeps the already-working cloud path
on a single call.

## Usage

``` r
cv_llm_context_window(config = cv_load_config())
```
