# Cell-count threshold for the SPEED-only sketch offer (Round LXXXVI), independent of \`cv_heavy_dispatch_route()\` above.

That route is about whether the machine can hold the call at all –
derived from THIS machine's own measured budget, never a fixed cell
count, per the Round LXXXV brief. This is a different question:
clustoCell's and markoClust's cell-cell similarity and
network-generation steps scale with cell count in a way plain bytes does
not capture, so a dataset that fits in memory comfortably can still run
for a very long time. From live use: a machine that clustered 208,506
cells without a memory complaint still took long enough at the
similarity/network stage that the user wanted the choice up front rather
than discovering it by waiting.

## Usage

``` r
CV_LARGE_DATASET_SKETCH_HINT_NCELLS
```

## Format

An object of class `integer` of length 1.

## Details

Deliberately a plain constant, not machine-derived: the user who asked
for this named the number themselves, as their own proxy for "this might
take a while", which is a judgement about patience rather than about
what the machine can hold – the thing Round LXXXV's brief was explicit
must never be a hardcoded cell count. That constraint binds the memory
question; it does not extend to a speed preference the person who will
wait for it gets to set.
