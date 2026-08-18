# Write the ONE pending object that would produce \`filename\`, if any.

Round LIV: the single-file download path. \`cv_api_serve_artifact()\`
calls this when a requested artifact is missing from disk; it looks the
filename up in the index, and materializes only the owning handle.

## Usage

``` r
cv_materialize_pending_artifact(session_id, filename)
```

## Arguments

- session_id:

  session id.

- filename:

  bare artifact filename (already path-guarded by the caller).

## Value

invisibly TRUE if something was written.

## Details

Returns FALSE (silently, no error) for every case where nothing can be
done: an unknown session, a filename no index entry claims, an entry
that is not pending, or – the important one – a handle that is no longer
in the store, which is what a restored session looks like. The caller
then 404s exactly as it did before this round existed.
