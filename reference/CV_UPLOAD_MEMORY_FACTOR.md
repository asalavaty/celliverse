# How many times the file size a BROWSER upload costs in server memory.

Three, measured: the raw request body, the multipart parser's copy of
the file part, and the object once \`readRDS()\` has it. The two staged
disk copies are not counted – disk is not the scarce thing here.

## Usage

``` r
CV_UPLOAD_MEMORY_FACTOR
```

## Format

An object of class `numeric` of length 1.

## Details

Not a tunable. It is a description of what plumber + webutils do, and if
either stops copying, this number should be re-measured and changed, not
adjusted to make a message read better.
