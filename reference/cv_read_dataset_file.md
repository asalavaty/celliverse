# Read a single-cell dataset from a file in any format this build supports.

Returns the loaded R object plus a human-readable note about anything
that had to be inferred (matrix orientation, placeholder names). The
note is surfaced to the user: an assumption they can see is one they can
correct.

## Usage

``` r
cv_read_dataset_file(path, filename = NULL)
```

## Arguments

- path:

  file to read.

- filename:

  original name, when \`path\` is a temp file whose extension may have
  been lost.

## Value

list(object=, note=\<chr or NULL\>, format=\<chr\>)
