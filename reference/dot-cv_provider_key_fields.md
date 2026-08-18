# Every provider key-field name in the registry, e.g. \`c("openai_key", "anthropic_key", ...)\` – excludes keyless providers (ollama, lmstudio). Single source for every call site that previously hand-listed the 7 \`\*\_key\` config field names.

Every provider key-field name in the registry, e.g. \`c("openai_key",
"anthropic_key", ...)\` – excludes keyless providers (ollama, lmstudio).
Single source for every call site that previously hand-listed the 7
\`\*\_key\` config field names.

## Usage

``` r
.cv_provider_key_fields()
```
