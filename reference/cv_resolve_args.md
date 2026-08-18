# Resolve & validate call arguments against a tool's parameter spec

\- fills defaults, checks required, coerces scalar types - for "handle"
params, verifies the handle exists AND its object type is in the allowed
handle_types (this is the DAG guardrail) Returns a list with \$args
(resolved, handles still as strings) and \$handle_args (names that are
handles) — the handler resolves handles to objects from the store.

## Usage

``` r
cv_resolve_args(tool, args, store, warnings = NULL)
```

## Arguments

- warnings:

  optional collector (cv_warnings_new()). Round LXIX: the three
  handle-recovery paths below are real decisions the agent makes on the
  user's behalf, and every one of them reported to a cli console the
  browser user never sees. Informational rather than invalidating: each
  fires only when the choice was UNAMBIGUOUS – exactly one loaded object
  of the right type – so the run used the object the user meant. What
  was missing was saying so.
