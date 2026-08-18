# Wrap data-derived text in a fence and say, once, that it is data.

A DELIMITER ALONE IS NOT A DEFENCE, which is the part that is easy to
get wrong: a value containing the closing delimiter would end the region
early and everything after it would read as instruction again. The one
thing this function does beyond concatenating strings is neutralise
that, and it does it WITHOUT changing any legitimate content – the
delimiter is a run of hyphens around a fixed phrase, which no gene
symbol, cluster name, tissue or cell type can be. Anything that does
contain it was not a name.

## Usage

``` r
cv_fence_data(text, lead = "")
```

## Arguments

- text:

  the data-derived text.

- lead:

  one sentence introducing the region, in the caller's own words.

## Value

a single string: lead, the standing instruction, and the fenced text.

## Details

Nothing else is sanitised or stripped. \`MARCH1\` is a real gene,
\`Tumour (post-treatment)\` is a real condition, and a fence that
mangled them would trade a hypothetical risk for a certain wrong answer.
