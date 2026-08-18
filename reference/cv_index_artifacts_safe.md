# Index the session's objects, swallowing anything that goes wrong.

Index the session's objects, swallowing anything that goes wrong.

## Usage

``` r
cv_index_artifacts_safe(session_id, when = "turn end")
```

## Arguments

- session_id:

  session id; looked up for its store and artifacts dir.

- when:

  short phrase naming the path that called this, for the warning.
