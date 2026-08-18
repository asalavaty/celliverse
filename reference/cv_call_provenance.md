# Build the provenance record for one tool call.

Build the provenance record for one tool call.

## Usage

``` r
cv_call_provenance(tool_name, args = list(), handle_args = character(0))
```

## Arguments

- tool_name:

  the tool that ran.

- args:

  the resolved, dispatched arguments.

- handle_args:

  names of parameters carrying object handles (values kept as the handle
  STRING; anything unserializable is dropped).
