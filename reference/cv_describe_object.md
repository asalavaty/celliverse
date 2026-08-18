# Build a compact descriptor for any supported object

A descriptor is a small named list with: handle, type, class, source,
summary (a one-line human string), and type-specific fields (dims,
metadata columns, cluster counts, ...). It NEVER contains the full
matrix / expression data.

## Usage

``` r
cv_describe_object(value, handle = NA_character_, source = "")
```

## Arguments

- value:

  the object to describe.

- handle:

  its store handle, if it has one yet.

- source:

  free-text provenance ("upload", a tool name, ...).

## Value

a named list: the base fields, the type-specific fields, and
\`summary\`.

## The cv_describe\_\* contract

Descriptors are the ONLY thing the language model ever sees about a
user's object – the object itself stays server-side behind a handle.
That makes the rules below load-bearing rather than stylistic: each one
exists because breaking it produced a real, shipped defect. Anyone
adding a type must satisfy all seven, and
\`tests/testthat/test-round60-descriptor-contract-safe.R\` asserts them
mechanically against every branch of the \`switch()\` below.

1.  **Return only type-specific fields.** A \`cv_describe\_\*()\`
    returns a named list and must NOT set \`handle\`, \`type\`,
    \`class\`, \`source\` or \`summary\`. This function supplies those;
    \`summary\` in particular is computed last, from the merged
    descriptor, so a descriptor that sets it is silently overwritten.

2.  **Never throw.** These run on objects of unknown provenance, during
    \`cv_object_put()\`, on the request thread. A throw here fails the
    user's upload – not just its summary. Every field that reaches into
    an object's structure is wrapped in \`tryCatch()\` with a typed
    fallback (\`character(0)\`, \`NA_integer\_\`), which is why the
    bodies below look more defensive than the data warrants.

3.  **Stay bounded.** The descriptor is embedded in the system prompt on
    every turn. Cap any id/name list at \`CV_DESC_MAX_IDS\` and any
    preview at a handful of elements. Never include a matrix, a layer,
    or a full marker table.

4.  **Ship every capped list with its TRUE total.** A field holding a
    capped list must be accompanied by an \`n\_\*\` count computed
    \*before\* capping, and \`cv_summary_line()\` must route the pair
    through \`.cv_id_list_text(ids, total = n\_\*)\`. This is the Round
    LVI invariant: a capped list whose total is derived from the
    already-capped vector can never announce the shortfall, so ids
    vanish silently from a prompt that simultaneously forbids the model
    from guessing an id it cannot see.

5.  **Sort ids with \`.cv_natural_sort()\`.** Lexicographic order both
    reads wrongly (C10 before C2) and, combined with capping, drops ids
    from the MIDDLE of the range instead of the end.

6.  **Distinguish "unknown" from "none".** \`NA_integer\_\` means the
    count could not be determined; \`0L\` means there genuinely are
    none. \`cv_summary_line()\` renders these differently on purpose –
    "sub-clusters n/a" versus "no sub-clusters" – and collapsing them
    produced a summary reading "NA major clusters, NA sub-clusters"
    after a clustering that had in fact succeeded (the original Issue
    3).

7.  **Add a \`cv_summary_line()\` branch too.** A new \`switch()\`
    branch here without a matching branch there does not error; it falls
    through to \`"\<type\> object"\`, and the model is told nothing
    useful about an object it is expected to reason about.
