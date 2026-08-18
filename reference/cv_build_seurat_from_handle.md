# Build a Seurat from a stored matrix handle and put it in the store.

ONE implementation behind three doors: the \`toSeurat\` tool the model
calls, the \`/api/objects/to-seurat\` route the Upload page's "Build it
anyway" button posts to, and the automatic conversion on load. Round
LXXXII shipped a decline message telling the user to say "convert
\<handle\> to a Seurat object" and there was NO TOOL that did it – the
model duly emitted a call to a tool that did not exist and the raw
arguments blob leaked into the transcript. A dead end dressed as a next
step; this is the thing that was missing.

## Usage

``` r
cv_build_seurat_from_handle(store, handle, name = NULL)
```

## Arguments

- store:

  the session object store.

- handle:

  a handle naming a matrix / data.frame already in the store.

- name:

  optional display name for the new handle.

## Value

a list with \`handle\`, \`descriptor\` and \`text\`, or a \`condition\`
on failure (callers decide how to report it).

## Details

Deliberately IN-PROCESS rather than in a worker child. Every heavy tool
here runs in a callr child, but that means serialising the matrix into
the child and the Seurat back out – two full copies of a multi-GB object
to avoid one in-place allocation. For this operation the child is
strictly worse.
