# The filenames \`.cv_write_object_artifacts()\` WOULD produce, without producing them.

Round LIV: the end-of-turn hook now records what each object can be
exported as, and the actual serialization is deferred until a download
asks for it. That needs the names up front, so the Results tab can list
a row for a file that does not exist yet.

## Usage

``` r
.cv_planned_object_files(val, type, handle)
```

## Details

This mirrors \`.cv_write_object_artifacts()\`'s branching exactly and
must be kept in step with it – a divergence would either show the user a
row whose download produces nothing, or hide one that is genuinely
available. Every condition here is a cheap field read (does this
TypoClust have any cell types at all?), never a serialization, which is
the entire point. \`test-round54-...\` asserts the two functions agree
for every supported type.
