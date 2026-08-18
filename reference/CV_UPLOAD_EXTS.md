# Extensions \`cv_read_dataset_file()\` actually dispatches on.

Kept beside that switch deliberately, and asserted against it by a test:
a format added to the reader and not to this vector would be REFUSED at
upload before the reader ever saw it, which is a far more confusing
failure than the one this exists to prevent.

## Usage

``` r
CV_UPLOAD_EXTS
```

## Format

An object of class `character` of length 10.

## Details

\`h5\` is included even when \`hdf5r\` is absent. Refusing it here would
replace the reader's specific, actionable message ("install hdf5r, or
export as 10x MTX") with a generic list of formats – the pre-check
exists to save the user an upload, not to give a worse answer than the
code behind it.
