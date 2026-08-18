# Should the auto-Seurat conversion run for this object?

The agent builds a Seurat from an uploaded bare matrix so the analysis
tools have something they can accept. That is right, and it is worth
doing – but the Seurat holds a SECOND full copy (see
CV_SEURAT_MEMORY_FACTOR), and doing it unconditionally is what turns
"your dataset is large" into "the R session died holding your dataset".

## Usage

``` r
cv_conversion_advice(x, handle = NULL)
```

## Arguments

- x:

  the object about to be converted.

- handle:

  the handle the object was stored under, named in the note so the user
  has the exact sentence that performs the conversion later. Taken as an
  argument rather than left as a \` half-formatted sentence escaping
  into the UI is a failure mode this project has shipped before, and one
  call site is cheaper than one trap.

## Value

a list with \`convert\` (logical), \`bytes\`, \`needs_mb\`,
\`available_mb\` and \`reason\` (NULL when converting).

## Details

Declining is not a failure: the matrix is loaded, it is in the store, it
has a handle, and the note tells the user the one sentence that performs
the conversion when they want it. An R session that is still running is
worth more than a conversion nobody asked for yet.
