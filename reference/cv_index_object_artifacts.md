# Record what each session object COULD be downloaded as, without writing it.

Round LIV replaces the end-of-turn serialization with this. The old hook
called cv_sync_object_artifacts() after EVERY turn, which
gzip-serialized every changed object on the single thread that also
serves HTTP:

## Usage

``` r
cv_index_object_artifacts(store, artifacts_dir, session_id = NULL)
```

## Arguments

- store:

  the session object store (may be NULL/empty -\> no-op).

- artifacts_dir:

  session artifacts dir.

- session_id:

  for building URLs.

## Value

the freshly built manifest (invisibly via cv_build_manifest).

## Details

16 MB object 617 ms 64 MB 1,519 ms 193 MB 4,649 ms

– a multi-second freeze at the end of an ordinary turn, spent on an
object the user had not asked to download. Round XXXIX's reduction cache
made it worse rather than better: persisting a computed embedding back
onto the source object bumps that handle's \`rev\`, which is exactly
what the signature keys on, so the whole Seurat was re-compressed.

This writes only metadata the store already holds – type, summary,
source, revision, and the filenames the object would produce – so its
cost does not depend on object size at all. The bytes are written later,
by cv_sync_object_artifacts() below, from the two download paths in
agent_api.R.

THE TRADE, put to the user and accepted before this was implemented: a
restored session comes back with an EMPTY object store by design
(history and descriptors survive a restart, the objects themselves do
not), so an object that was never downloaded cannot be produced after a
restart. cv_build_manifest() therefore offers a pending row only while
its handle is still live in the store – it never advertises a download
it cannot deliver.
