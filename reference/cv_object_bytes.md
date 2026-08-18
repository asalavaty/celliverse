# Bytes an in-memory object occupies.

A NEGATIVE, recorded because the first version of this function was
written on an assumption that measurement did not support. It carried a
hand-rolled fast path for dgCMatrix – 8 bytes per stored value, 4 per
row index, 4 per column pointer – on the belief that
\`utils::object.size()\` would walk a 330-million-entry matrix and be
exactly the kind of cost this file exists to avoid. Break-verification
removed the fast path and nothing failed, so it was timed directly: at
8.2 million stored values \`object.size()\` and the arithmetic were BOTH
below the timer's resolution. It does not walk the data; it sums the
slots, which is the same thing the fast path did, in C.

## Usage

``` r
cv_object_bytes(x)
```

## Arguments

- x:

  any object.

## Value

numeric bytes, or \`NA_real\_\` if it cannot be determined.

## Details

So the fast path bought nothing measurable and cost an approximation –
it agreed with \`object.size()\` to within a few percent rather than
exactly. It is gone. What is kept is the one thing that is actually
wanted here: a single place that answers in bytes and returns NA instead
of throwing, so every caller can treat "cannot tell" as "no opinion".
