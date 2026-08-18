# Render a tool's \`next_suggestions\` as an \`info\` warning, or nothing.

Render a tool's \`next_suggestions\` as an \`info\` warning, or nothing.

## Usage

``` r
.cv_next_steps_note(tool, reg = NULL)
```

## Arguments

- tool:

  the tool record just run.

- reg:

  the registry, used to drop any suggestion that is not a real tool.

## Value

a list of cv_warn() entries (empty when there is nothing to say).
