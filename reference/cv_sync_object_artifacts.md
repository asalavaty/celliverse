# Persist session objects to downloadable files, then rebuild the manifest.

Round LIV: this is no longer the end-of-turn hook (that is
cv_index_object_artifacts() above). It is the MATERIALIZATION step,
called only when a download actually asks for the bytes – one handle at
a time from cv_api_serve_artifact(), or all of them from
cv_api_artifacts_zip(). The work is identical to what it always did;
only when it runs has changed, and that is deliberately the moment the
user is waiting for a file and being told so.

## Usage

``` r
cv_sync_object_artifacts(
  store,
  artifacts_dir,
  session_id = NULL,
  handles = NULL
)
```

## Arguments

- store:

  the session object store (may be NULL/empty -\> no writes).

- artifacts_dir:

  session artifacts dir.

- session_id:

  for building URLs.

- handles:

  optional character vector: materialize only these handles. NULL
  (default) means every handle in the store, which is what the "Download
  all" path wants. A single-file download passes exactly one, so
  clicking one row never pays for every other object in the session.

## Value

the freshly built manifest (invisibly via cv_build_manifest).

## Details

Incremental: an object is (re)serialized only when it is new, has
changed since the last write (tracked by the signature in
.artifacts_state.json), or is still marked \`pending\` by the index. We
do NOT delete files for handles that vanish from the store: a session's
produced objects remain downloadable (in-place updates overwrite the
same handle's file, so there is no duplication for
addClustoData/addTypoData).
