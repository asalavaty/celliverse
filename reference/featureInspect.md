# Inspect the Membership of Features Across ClustoCell Results

`featureInspect()` searches one or more features across all marker
collections contained within a `ClustoCell` object, including global
feature collections, cross-cluster markers, major cluster-specific
markers, and sub-cluster-specific markers. All matches are returned in a
single long-format data frame containing the feature level, membership,
marker type, Gini score, purity, and rank. Optionally, a
publication-quality ggplot2 visualisation summarising feature
memberships can also be generated.

## Usage

``` r
featureInspect(
  clustoCell,
  features,
  level = NULL,
  type = NULL,
  sort_by = c("input", "rank", "gini"),
  plot = FALSE,
  title = NULL,
  subtitle = NULL,
  tag = NULL,
  nrow_panels = NULL,
  dotsize = 3,
  show_purity = TRUE,
  class_palette = NULL,
  color_low = "steelblue",
  color_high = "firebrick",
  panel_border_color = "black",
  panel_border_size = 0.5,
  axis_text_size = 8,
  axis_title_size = 9,
  plot_margin_right = 10,
  xlab = "Rank",
  ylab = "Feature",
  show_legend = TRUE,
  legend_position = "right",
  legend_box = "vertical",
  legend_box_just = "left"
)
```

## Arguments

- clustoCell:

  An object of class `ClustoCell` obtained using
  [`clustoCell`](https://asalavaty.github.io/celliverse/reference/clustoCell.md)
  or
  [`markoClust`](https://asalavaty.github.io/celliverse/reference/markoClust.md).

- features:

  A character vector of feature names (e.g. gene symbols) to inspect
  across the `ClustoCell` object.

- level:

  Character vector specifying which hierarchical level(s) to include in
  the output. One or more of `"Global"`, `"Cross-cluster"`,
  `"Major cluster"`, and `"Sub-cluster"`. If `NULL` (default), results
  from all levels are returned. If none of the queried features are
  found at the specified level(s), a zero-row `data.frame` is returned
  (with a warning) rather than an error.

- type:

  Character vector specifying which marker type(s) to include in the
  output. One or more of `"Positive"`, `"Negative"`, `"Medium"`,
  `"Pure Ranked"`, `"Pure High"`, `"Pure Medium"`, and `"Pure"`. If
  `NULL` (default), results of all types are returned. The value
  `"Pure"` is a convenience shorthand that expands to `"Pure Ranked"`,
  `"Pure High"`, and `"Pure Medium"` simultaneously, including all
  global feature categories. Individual pure types (e.g. `"Pure High"`)
  can also be specified directly. The `level` and `type` filters are
  applied independently; if their combination yields no matching rows, a
  zero-row `data.frame` is returned (with a warning) rather than an
  error.

- sort_by:

  Character string specifying how to order the rows of the output table.
  One of:

  `"input"`

  :   (Default) Rows follow the order of `features` as supplied by the
      user, then by level (Global \\\rightarrow\\ Cross-cluster
      \\\rightarrow\\ Major cluster \\\rightarrow\\ Sub-cluster).

  `"rank"`

  :   Ascending `Rank` (rows with `NA` rank appear last), then by input
      order within ties.

  `"gini"`

  :   Descending `Gini_Score` (rows with `NA` Gini score appear last),
      then by input order within ties.

- plot:

  Logical. If `FALSE` (default), a `data.frame` is returned. If `TRUE`,
  a named list with elements `$table` and `$plot` is returned.

- title:

  Character. Plot title. Ignored when `plot = FALSE`.

- subtitle:

  Character. Plot subtitle. Ignored when `plot = FALSE`.

- tag:

  Character. Plot tag (e.g. panel label). Ignored when `plot = FALSE`.

- nrow_panels:

  Integer. Number of rows used when faceting the plot by `Membership`.
  If `NULL` (default), the number of rows is determined automatically by
  [`facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html).
  Ignored when `plot = FALSE`.

- dotsize:

  Numeric. Controls the size range of the dots in the plot. The actual
  `size` aesthetic is scaled between `dotsize * 0.4` and
  `dotsize * 1.8`. Default is `3`. Ignored when `plot = FALSE`.

- show_purity:

  Logical. If `TRUE` (default), dot colour encodes `Purity` via a
  continuous gradient. If `FALSE`, dot colour encodes `Type` as a
  discrete scale. Ignored when `plot = FALSE`.

- class_palette:

  Optional. Only used when `show_purity = FALSE`. Specifies the colour
  scale for `Type`. Can be one of:

  - A ggplot2 scale object (e.g.
    [`ggplot2::scale_colour_brewer()`](https://ggplot2.tidyverse.org/reference/scale_brewer.html)).

  - A named or unnamed character vector of colours (e.g.
    `c("red", "blue", "green")`), which is passed to
    [`scale_colour_manual`](https://ggplot2.tidyverse.org/reference/scale_manual.html).

  - `NULL` (default): the default ggplot2 discrete colour scale is used.

  Ignored when `plot = FALSE`.

- color_low:

  Character. The low-end colour of the continuous Purity gradient.
  Default is `"steelblue"`. Ignored when `show_purity = FALSE` or
  `plot = FALSE`.

- color_high:

  Character. The high-end colour of the continuous Purity gradient.
  Default is `"firebrick"`. Ignored when `show_purity = FALSE` or
  `plot = FALSE`.

- panel_border_color:

  Character. Colour of the panel border. Default is `"black"`. Ignored
  when `plot = FALSE`.

- panel_border_size:

  Numeric. Line width of the panel border. Default is `0.5`. Ignored
  when `plot = FALSE`.

- axis_text_size:

  Numeric. Font size (in points) for axis tick labels. Default is `8`.
  Ignored when `plot = FALSE`.

- axis_title_size:

  Numeric. Font size (in points) for axis titles. Default is `9`.
  Ignored when `plot = FALSE`.

- plot_margin_right:

  Numeric. Right margin of the plot in points. Default is `10`. Ignored
  when `plot = FALSE`.

- xlab:

  Character. Label for the x-axis. Default is `"Rank"`. Ignored when
  `plot = FALSE`.

- ylab:

  Character. Label for the y-axis. Default is `"Feature"`. Ignored when
  `plot = FALSE`.

- show_legend:

  Logical. Whether to display the plot legend. Default is `TRUE`.
  Ignored when `plot = FALSE`.

- legend_position:

  Character. Position of the legend. One of `"right"` (default),
  `"left"`, `"top"`, `"bottom"`, or `"none"`. Ignored when
  `plot = FALSE`.

- legend_box:

  Character. Arrangement of multiple legend keys. One of `"vertical"`
  (default) or `"horizontal"`. Ignored when `plot = FALSE`.

- legend_box_just:

  Character. Justification of legend boxes. Default is `"left"`. Ignored
  when `plot = FALSE`.

## Value

- If `plot = FALSE`:

  A `data.frame` with one row for each occurrence of each queried
  feature across all (or the selected) marker collections. Columns are:

  `Feature`

  :   Feature name (character).

  `Level`

  :   Hierarchical level at which the feature was found: `"Global"`,
      `"Cross-cluster"`, `"Major cluster"`, or `"Sub-cluster"`
      (character).

  `Membership`

  :   The specific collection in which the feature was found, e.g.
      `"Global Features"`, `"Cross-cluster Marker"`, `"C1"`, or
      `"C1-Sub1"` (character).

  `Type`

  :   Marker type: `"Pure High"`, `"Pure Medium"`, `"Pure Ranked"`,
      `"Positive"`, `"Negative"`, or `"Medium"` (character).

  `Gini_Score`

  :   Gini score of the feature within the collection (numeric). `NA`
      for global features.

  `Purity`

  :   Purity of the feature within the collection (numeric). `NA` for
      global and cross-cluster features.

  `Rank`

  :   Rank of the feature within its specific collection (integer). `NA`
      for global features. See Details.

  If no queried feature is found in any collection (or in the specified
  level(s)), a zero-row `data.frame` with the above columns is returned.

- If `plot = TRUE`:

  A named list with two elements:

  `$table`

  :   The results `data.frame` described above.

  `$plot`

  :   A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
      object visualising the identified feature memberships as a dot
      plot faceted by `Membership`. Dot position (x-axis) encodes
      `Rank`, dot size encodes the inverted `Gini_Score` (larger =
      purer), dot shape encodes `Type`, and dot colour encodes `Purity`
      (or `Type` when `show_purity = FALSE`). Features without a Gini
      score are shown as large semi-transparent grey dots. See Details.

## Details

**Rank interpretation.** The `Rank` column reflects the rank of the
feature *within its specific collection* (i.e. within the combination of
`Level`, `Membership`, and `Type`), as assigned by
[`clustoCell`](https://asalavaty.github.io/celliverse/reference/clustoCell.md)
or
[`markoClust`](https://asalavaty.github.io/celliverse/reference/markoClust.md).
It does *not* represent the row position in the output table returned by
`featureInspect()`. A feature ranked 1st in cluster C1 positive markers
and 5th in sub-cluster C1-Sub1 medium markers will appear in two
separate rows with `Rank` values of 1 and 5, respectively.

**Dot size in the plot.** When `plot = TRUE`, dot size encodes the
*inverted* Gini score: a lower Gini score indicates a purer marker and
is represented by a *larger* dot. The size legend labels display the
original Gini score values for interpretability. Features without a Gini
score (i.e. global features stored in `globally_pure_ranked`,
`globally_pure_high`, or `globally_pure_medium`) are rendered as large,
semi-transparent grey dots with a heavier border stroke to signal that
their size carries no quantitative meaning. These features are also
plotted at \\x = 0\\ because no rank is assigned to them; the `0` tick
label is suppressed in panels that contain only unranked features to
avoid misinterpretation.

**Level and type filtering.** When `level` and/or `type` are specified,
only rows matching the requested value(s) are returned. The two filters
are applied sequentially and independently: `level` is applied first,
then `type`. Specifying `type = "Pure"` expands to all three global
pure-type categories (`"Pure Ranked"`, `"Pure High"`, `"Pure Medium"`)
but does *not* override the `level` filter — if `level` is set to a
non-global level, the combination will yield zero rows (with a warning).
If no features are found after filtering, a zero-row `data.frame` is
returned with a warning rather than an error, allowing
`featureInspect()` to be used safely inside loops or
[`lapply()`](https://rdrr.io/r/base/lapply.html) calls.

## See also

[`clustoCell`](https://asalavaty.github.io/celliverse/reference/clustoCell.md),
[`markoClust`](https://asalavaty.github.io/celliverse/reference/markoClust.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# --- Basic usage: return a table only ---
result_table <- featureInspect(
  clustoCell = my_clustocell_obj,
  features   = c("CD3D", "MS4A1", "FOXP3")
)
print(result_table)

# --- Filter to a single level ---
result_major <- featureInspect(
  clustoCell = my_clustocell_obj,
  features   = c("CD3D", "MS4A1", "FOXP3"),
  level      = "Major cluster"
)

# --- Filter to multiple levels ---
result_sub <- featureInspect(
  clustoCell = my_clustocell_obj,
  features   = c("CD3D", "MS4A1", "FOXP3"),
  level      = c("Major cluster", "Sub-cluster")
)

# --- Return table sorted by Gini score ---
result_gini <- featureInspect(
  clustoCell = my_clustocell_obj,
  features   = c("CD3D", "MS4A1", "FOXP3"),
  sort_by    = "gini"
)

# --- Return table and plot (default aesthetics) ---
result_list <- featureInspect(
  clustoCell = my_clustocell_obj,
  features   = c("CD3D", "MS4A1", "FOXP3"),
  plot       = TRUE
)
result_list$table
result_list$plot

# --- Customise the plot ---
result_custom <- featureInspect(
  clustoCell    = my_clustocell_obj,
  features      = c("CD3D", "MS4A1", "FOXP3"),
  plot          = TRUE,
  title         = "Feature Membership Overview",
  subtitle      = "ClustoCell marker hierarchy",
  show_purity   = TRUE,
  color_low     = "navy",
  color_high    = "gold",
  dotsize       = 4,
  nrow_panels   = 2,
  legend_position = "bottom"
)
result_custom$plot

# --- Colour dots by Type instead of Purity ---
result_type <- featureInspect(
  clustoCell    = my_clustocell_obj,
  features      = c("CD3D", "MS4A1"),
  plot          = TRUE,
  show_purity   = FALSE,
  class_palette = c(
    Positive    = "#2166AC",
    Negative    = "#D6604D",
    Medium      = "#4DAC26",
    "Pure High" = "#762A83"
  )
)
result_type$plot

# --- Filter to positive markers only ---
result_pos <- featureInspect(
  clustoCell = my_clustocell_obj,
  features   = c("CD3D", "MS4A1", "FOXP3"),
  type       = "Positive"
)

# --- Filter to all global (pure) features using the shorthand ---
result_pure <- featureInspect(
  clustoCell = my_clustocell_obj,
  features   = c("CD3D", "MS4A1", "FOXP3"),
  type       = "Pure"
)

# --- Combine level and type filters ---
result_combo <- featureInspect(
  clustoCell = my_clustocell_obj,
  features   = c("CD3D", "MS4A1", "FOXP3"),
  level      = c("Major cluster", "Sub-cluster"),
  type       = c("Positive", "Negative")
)

# --- Safe use when a level or type may not exist ---
# Returns a zero-row data.frame with a warning (no error)
result_empty <- featureInspect(
  clustoCell = my_clustocell_obj,
  features   = c("CD3D"),
  level      = "Cross-cluster"
)

result_empty2 <- featureInspect(
  clustoCell = my_clustocell_obj,
  features   = c("CD3D"),
  type       = "Pure"
)
} # }
```
