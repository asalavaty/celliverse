# Read \`.artifacts_state.json\`, degrading to an empty list on anything unusable.

Round LV (Batch 5a): this five-line read-with-fallback appeared verbatim
in three places after Round LIV added the index
(cv_index_object_artifacts, cv_sync_object_artifacts, cv_build_manifest)
plus a fourth variant in cv_materialize_pending_artifact. Own
duplication, introduced one round ago and collapsed here — the state
file is read on every turn and every download, so "what counts as an
unreadable state file" is exactly the decision that should not have four
independent answers.

## Usage

``` r
.cv_read_artifacts_state(artifacts_dir)
```

## Arguments

- artifacts_dir:

  session artifacts dir.

## Value

the parsed state list, or an empty list.
