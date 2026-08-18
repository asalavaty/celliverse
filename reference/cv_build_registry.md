# Build the full tool registry (named list of cv_tool objects)

Defined in agent_tools\_\*.R via cv_register_core_tools() and
cv_register_advanced_tools(). Kept as a function so it is rebuilt fresh
per process and easy to extend (add one entry -\> auto-surfaces to the
LLM).

## Usage

``` r
cv_build_registry()
```
