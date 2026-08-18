# Evict the LEAST-RECENTLY-ACCESSED objects from a store once it grows past its cap, so a single long-lived session does not accumulate an unbounded number of potentially large Seurat/SingleCellExperiment/CelliVerse-result objects over its lifetime (this is the object-store sibling of the job-registry leak fixed in Batch 2b item 6, and the session-registry / history leaks fixed alongside this one – see cv_sessions_evict_stale() and cv_history_evict_stale() in agent_session.R).

Unlike the job registry (where a "terminal" record can simply be
deleted), an evicted object's HANDLE is kept, so the model/UI don't lose
track of it entirely: only its (potentially large) \`value\` is dropped.
The record becomes a lightweight sentinel (\`evicted = TRUE\`,
descriptor/created/ source kept) – cv_object_get() gives a clear,
actionable error naming the handle and what regenerates it (see above)
instead of the handle silently vanishing, and cv_object_update() can
resurrect it in place if the same handle is legitimately written to
again. \`keep\` counts only LIVE (non- evicted) objects, since a
sentinel is cheap and doesn't need its own cap.

## Usage

``` r
cv_object_evict_stale(store, keep = NULL)
```

## Arguments

- store:

  object store environment.

- keep:

  max LIVE objects to retain; defaults to the owning session's
  \`object_store_limit\` config (see cv_default_config()).

## Value

invisibly, the number of objects evicted.

## Details

A store created with no session_id (every existing test call site) is
not attached to a real session and is never swept – see
cv_object_store_new().
