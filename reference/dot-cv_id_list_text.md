# Render an id list for a summary line, announcing any truncation.

Render an id list for a summary line, announcing any truncation.

## Usage

``` r
.cv_id_list_text(ids, total = NULL, max_show = NULL)
```

## Arguments

- ids:

  the ids the descriptor actually holds (already bounded).

- total:

  the TRUE number of ids the object has. Compared against
  \`length(ids)\`, so the "+N more" marker is measured against reality
  rather than against the list that was already cut – the bug this
  replaces.

- max_show:

  optional further cap for display; NULL shows everything the descriptor
  holds, which is the default because the model is instructed to take
  set ids from here.

## Value

a bracketed string like " \[C1, C2, C3 ... (+7 more)\]", or "".
