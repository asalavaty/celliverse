# Is sending this file through the browser a reasonable thing to do?

Called by the Upload page the moment a file is CHOSEN – \`File.size\` is
known to the browser without reading a byte – so the advice arrives
before the upload rather than after it fails.

## Usage

``` r
CV_BROWSER_UPLOAD_HARD_LIMIT_BYTES
```

## Format

An object of class `numeric` of length 1.

## Details

The verdict is derived from two measurements and nothing else: the
file's own size, and what this machine currently has available
(\`cv_available_memory_mb\`, Round XXXIX, which counts reclaimable
memory rather than merely free pages). When the machine cannot be
measured – any platform that is not Linux or macOS – \`available_mb\` is
NA and the verdict is \`TRUE\`: no opinion means proceed, exactly as
before this function existed.

The hard ceiling of a browser upload's transport, independent of memory.

ROUND LXXXVI, from live use: \`cv_upload_advice()\` below only ever
compared the upload's memory cost to \`cv_memory_budget_mb()\`, so on a
well-resourced machine a 3.8 GB file was judged "advisable" and the user
only found out it was doomed after clicking Upload and waiting for
plumber to fail on it – the exact round-trip the advisory exists to
avoid.

The failure Round LXXXIV diagnosed has nothing to do with memory:
plumber reads the whole multipart body into ONE raw vector, and the C
paths that touch it are indexed by a 32-bit int, so a body at or above
2^31 bytes throws (\`long vectors not supported yet\`) on ANY machine,
however much RAM it has. More memory cannot fix it, so it is checked
unconditionally, first, and separately from the memory-based advice that
follows.

This is a measured property of R's C internals, not a policy this
project has any say over – the standing "no upload size limit" rule is
about not inventing a threshold to refuse files we simply judge too big,
and nothing below refuses anything: the Upload button stays enabled, the
message names the path box as the way through, and a file just under
this ceiling is still judged purely on memory as before.
