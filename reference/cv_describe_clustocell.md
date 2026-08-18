# Describe a ClustoCell (and, since MarkoClust carries that class, a MarkoClust result too).

The one to read carefully. \`n_major_clusters\` / \`n_sub_clusters\` are
the TRUE totals, computed before \`major_labels\` / \`sub_labels\` are
capped, because \`cv_summary_line()\` compares the pair to announce any
shortfall (contract rule 4). \`n_sub_clusters\` also distinguishes 0
from NA (rule 6): a \`markoClust\` run with \`identify_subclusters =
FALSE\` genuinely has none, which must not read as "n/a". And "Isolated"
is excluded from \`sub_labels\` because it is a real label
\`clustoCell()\` assigns to unassigned cells, not a set anything can
annotate.

## Usage

``` r
cv_describe_clustocell(x)
```

## Arguments

- x:

  a ClustoCell object.

## Value

a named list of type-specific descriptor fields.
