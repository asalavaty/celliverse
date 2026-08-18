# Say when a setting had no effect (audit \#13, second half).

\`refine_transferred_subClusters\` only does anything when labels are
being TRANSFERRED, i.e. when sketching is on and sub-clusters are being
detected. With either precondition off it is silently inert – measured:
clustoCell completes normally and says nothing.

## Usage

``` r
.cv_note_inert_refine(args, warnings)
```

## Details

INFO rather than amber, and the Round LXIX rule decides it rather than
taste: the run is correct and complete. Without sketching there is no
transfer to refine, and sub-clusters are detected on the full data
directly, which is the thing refinement approximates. Nobody reading the
numbers draws a wrong conclusion from skipping this note.
