# Apply each provider's key-field environment-variable override(s) onto \`cfg\`, in place of the value already there (from defaults or the config file) when a matching env var is set and non-empty. Mirrors the pre- registry per-provider \`env_or()\` calls in \`cv_load_config()\` exactly, just driven by the registry instead of one hardcoded line per provider.

Apply each provider's key-field environment-variable override(s) onto
\`cfg\`, in place of the value already there (from defaults or the
config file) when a matching env var is set and non-empty. Mirrors the
pre- registry per-provider \`env_or()\` calls in \`cv_load_config()\`
exactly, just driven by the registry instead of one hardcoded line per
provider.

## Usage

``` r
.cv_provider_apply_env_overrides(cfg)
```
