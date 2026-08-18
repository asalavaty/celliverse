# Create a new object store (an environment mapping handle -\> record)

Each record is a list: list(value = \<the R object\>, descriptor =
\<list\>, created = \<time\>, source = \<chr\>).

## Usage

``` r
cv_object_store_new(session_id = NULL)
```

## Arguments

- session_id:

  the id of the session this store belongs to, stashed as an attribute
  so \`cv_object_evict_stale()\` can look up that session's config/jobs
  at eviction time without every \`cv_object_put()\`/
  \`cv_object_update()\` call site needing to pass session_id through
  explicitly (Batch 3b item 2). A store created with no session_id (as
  every existing standalone/test call site does) is simply never swept –
  eviction only ever applies to a store that's actually attached to a
  live session.
