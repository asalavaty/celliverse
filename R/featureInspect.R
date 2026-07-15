# _____________________________________________________________________________
# Inspect the Membership of Features Across ClustoCell Results
# _____________________________________________________________________________


# _____________________________________________________________________________
# Internal helpers
# _____________________________________________________________________________

#' @noRd
.fi_empty_df <- function() {
  data.frame(
    Feature    = character(0),
    Level      = character(0),
    Membership = character(0),
    Type       = character(0),
    Gini_Score = numeric(0),
    Purity     = numeric(0),
    Rank       = integer(0),
    stringsAsFactors = FALSE
  )
}


# _____________________________________________________________________________
# Helper 1 — Global features
# _____________________________________________________________________________

#' @noRd
.fi_inspect_global <- function(clustoCell, features) {

  slots <- list(
    globally_pure_ranked = "Pure Ranked",
    globally_pure_high   = "Pure High",
    globally_pure_medium = "Pure Medium"
  )

  out <- .fi_empty_df()

  for (slot_name in names(slots)) {
    vec <- clustoCell[[slot_name]]
    if (is.null(vec) || !is.character(vec)) next

    hits <- features[features %in% vec]
    if (length(hits) == 0L) next

    out <- rbind(
      out,
      data.frame(
        Feature    = hits,
        Level      = "Global",
        Membership = "Global Features",
        Type       = slots[[slot_name]],
        Gini_Score = NA_real_,
        Purity     = NA_real_,
        Rank       = NA_integer_,
        stringsAsFactors = FALSE
      )
    )
  }

  out
}


# _____________________________________________________________________________
# Helper 2 — Cross-cluster markers
# _____________________________________________________________________________

#' @noRd
.fi_inspect_cross_cluster <- function(clustoCell, features) {

  base <- clustoCell$markers$major_clusters$cross_cluster
  if (is.null(base)) return(.fi_empty_df())

  slots <- list(
    positive_features = "Positive",
    negative_markers  = "Negative",
    medium_markers    = "Medium"
  )

  out <- .fi_empty_df()

  for (slot_name in names(slots)) {
    df <- base[[slot_name]]
    if (is.null(df) || !is.data.frame(df)) next
    if (!"Feature" %in% colnames(df))      next

    hits_idx <- which(df$Feature %in% features)
    if (length(hits_idx) == 0L) next

    gini_col <- if ("Freq_Gini_Score" %in% colnames(df)) df$Freq_Gini_Score[hits_idx] else NA_real_
    rank_col <- if ("Rank"            %in% colnames(df)) as.integer(df$Rank[hits_idx]) else NA_integer_

    out <- rbind(
      out,
      data.frame(
        Feature    = df$Feature[hits_idx],
        Level      = "Cross-cluster",
        Membership = "Cross-cluster Marker",
        Type       = slots[[slot_name]],
        Gini_Score = gini_col,
        Purity     = NA_real_,
        Rank       = rank_col,
        stringsAsFactors = FALSE
      )
    )
  }

  out
}


# _____________________________________________________________________________
# Helper 3 — Major cluster-specific markers
# _____________________________________________________________________________

#' @noRd
.fi_inspect_major_clusters <- function(clustoCell, features) {

  base <- clustoCell$markers$major_clusters$cluster_specific
  if (is.null(base)) return(.fi_empty_df())

  marker_types <- list(
    positive_markers = "Positive",
    negative_markers = "Negative",
    medium_markers   = "Medium"
  )

  out <- .fi_empty_df()

  for (mtype in names(marker_types)) {
    mlist <- base[[mtype]]
    if (is.null(mlist)) next

    for (cluster_name in names(mlist)) {
      df <- mlist[[cluster_name]]
      if (is.null(df) || !is.data.frame(df)) next
      if (!"Feature" %in% colnames(df))      next

      hits_idx <- which(df$Feature %in% features)
      if (length(hits_idx) == 0L) next

      gini_col   <- if ("Gini_Score" %in% colnames(df)) df$Gini_Score[hits_idx]      else NA_real_
      purity_col <- if ("Purity"     %in% colnames(df)) df$Purity[hits_idx]           else NA_real_
      rank_col   <- if ("Rank"       %in% colnames(df)) as.integer(df$Rank[hits_idx]) else NA_integer_

      out <- rbind(
        out,
        data.frame(
          Feature    = df$Feature[hits_idx],
          Level      = "Major cluster",
          Membership = cluster_name,
          Type       = marker_types[[mtype]],
          Gini_Score = gini_col,
          Purity     = purity_col,
          Rank       = rank_col,
          stringsAsFactors = FALSE
        )
      )
    }
  }

  out
}


# _____________________________________________________________________________
# Helper 4 — Sub-cluster-specific markers
# _____________________________________________________________________________

#' @noRd
.fi_inspect_sub_clusters <- function(clustoCell, features) {

  base <- clustoCell$markers$sub_clusters
  if (is.null(base)) return(.fi_empty_df())

  marker_types <- list(
    positive_markers = "Positive",
    negative_markers = "Negative",
    medium_markers   = "Medium"
  )

  out <- .fi_empty_df()

  # Iterate over major-cluster groups, e.g. "C1-Subclusters"
  for (group_name in names(base)) {
    group <- base[[group_name]]
    if (is.null(group)) next

    # Derive major cluster prefix: strip trailing "-Subclusters" (case-insensitive)
    major_prefix <- sub("-[Ss]ubclusters?$", "", group_name)

    for (mtype in names(marker_types)) {
      mlist <- group[[mtype]]
      if (is.null(mlist)) next

      for (sub_name in names(mlist)) {
        df <- mlist[[sub_name]]
        if (is.null(df) || !is.data.frame(df)) next
        if (!"Feature" %in% colnames(df))      next

        hits_idx <- which(df$Feature %in% features)
        if (length(hits_idx) == 0L) next

        membership <- paste0(major_prefix, "-", sub_name)

        gini_col   <- if ("Gini_Score" %in% colnames(df)) df$Gini_Score[hits_idx]      else NA_real_
        purity_col <- if ("Purity"     %in% colnames(df)) df$Purity[hits_idx]           else NA_real_
        rank_col   <- if ("Rank"       %in% colnames(df)) as.integer(df$Rank[hits_idx]) else NA_integer_

        out <- rbind(
          out,
          data.frame(
            Feature    = df$Feature[hits_idx],
            Level      = "Sub-cluster",
            Membership = membership,
            Type       = marker_types[[mtype]],
            Gini_Score = gini_col,
            Purity     = purity_col,
            Rank       = rank_col,
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }

  out
}


# _____________________________________________________________________________
# Helper 5 — Plot
# _____________________________________________________________________________

#' @noRd
.fi_make_plot <- function(
    result_df,
    features,
    show_purity        = TRUE,
    class_palette      = NULL,
    color_low          = "steelblue",
    color_high         = "firebrick",
    dotsize            = 3,
    nrow_panels        = NULL,
    title              = NULL,
    subtitle           = NULL,
    tag                = NULL,
    panel_border_color = "black",
    panel_border_size  = 0.5,
    axis_text_size     = 8,
    axis_title_size    = 9,
    plot_margin_right  = 10,
    xlab               = "Rank",
    ylab               = "Feature",
    show_legend        = TRUE,
    legend_position    = "right",
    legend_box         = "vertical",
    legend_box_just    = "left"
) {

  # ---- Prepare data --------------------------------------------------------

  plot_df <- result_df

  # Feature as ordered factor (top-to-bottom = input order)
  feat_levels <- rev(unique(features[features %in% plot_df$Feature]))
  plot_df$Feature <- factor(plot_df$Feature, levels = feat_levels)

  # Flag rows with no Gini score (global features and any other NA-Gini rows)
  plot_df$is_na_gini <- is.na(plot_df$Gini_Score)

  # Flag rows with no Rank (global features)
  plot_df$is_na_rank <- is.na(plot_df$Rank)

  # x position: NA Rank → 0
  plot_df$Rank_plot <- ifelse(plot_df$is_na_rank, 0L, plot_df$Rank)

  # Size variable: INVERTED Gini (lower Gini = larger dot = purer marker).
  # NA-Gini rows receive the maximum possible inverted value so they render
  # at the largest dot size, making them visually prominent.
  gini_max <- max(plot_df$Gini_Score, na.rm = TRUE)
  if (!is.finite(gini_max)) gini_max <- 1  # fallback when all Gini are NA
  plot_df$Size_plot <- ifelse(
    plot_df$is_na_gini,
    gini_max,                        # NA-Gini → largest size bucket
    gini_max - plot_df$Gini_Score    # invert: high Gini → small dot
  )

  # Type_plot: collapse all global-level types ("Pure High", "Pure Medium",
  # "Pure Ranked") into a single "Global" label for the shape aesthetic.
  # This keeps the Type column in the returned table intact while giving a
  # clean, unified legend entry for global features in the plot.
  plot_df$Type_plot <- ifelse(
    plot_df$Level == "Global",
    "Global",
    plot_df$Type
  )

  # Colour variable (uses Type_plot so the discrete legend is also collapsed)
  if (show_purity) {
    plot_df$colour_var <- plot_df$Purity
  } else {
    plot_df$colour_var <- plot_df$Type_plot
  }

  # Split into two layers: rows with Gini data vs. NA-Gini rows
  plot_df_gini    <- plot_df[!plot_df$is_na_gini, , drop = FALSE]
  plot_df_na_gini <- plot_df[ plot_df$is_na_gini, , drop = FALSE]

  # Shape mapping — "Global" replaces the individual pure-* entries so that
  # all global features share one shape and one legend key.
  shape_map <- c(
    "Global"   = 23L,   # filled diamond  (all global features)
    "Positive" = 21L,   # filled circle
    "Negative" = 25L,   # filled triangle-down
    "Medium"   = 22L    # filled square
  )
  all_types_plot <- unique(plot_df$Type_plot)
  missing_types  <- setdiff(all_types_plot, names(shape_map))
  if (length(missing_types) > 0L) {
    extra <- stats::setNames(rep(21L, length(missing_types)), missing_types)
    shape_map <- c(shape_map, extra)
  }
  used_shapes <- shape_map[all_types_plot]

  # ---- Build base plot -----------------------------------------------------

  # Layer 1: rows WITH Gini data (normal rendering)
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = plot_df_gini,
      ggplot2::aes(
        x      = .data$Rank_plot,
        y      = .data$Feature,
        fill   = if (show_purity) .data$colour_var else NULL,
        colour = if (!show_purity) .data$colour_var else NULL,
        size   = .data$Size_plot,
        shape  = .data$Type_plot
      ),
      stroke = 0.4,
      alpha  = 0.9
    )

  # Layer 2: NA-Gini rows (grey fill, reduced alpha, heavier stroke to signal
  # that dot size is not informative for these features).
  # show.legend = TRUE so the "Global" shape key appears in the Type legend.
  if (nrow(plot_df_na_gini) > 0L) {
    p <- p +
      ggplot2::geom_point(
        data = plot_df_na_gini,
        ggplot2::aes(
          x     = .data$Rank_plot,
          y     = .data$Feature,
          size  = .data$Size_plot,
          shape = .data$Type_plot
        ),
        fill   = "grey75",
        colour = "grey40",
        stroke = 1.0,
        alpha  = 0.5
      )
  }

  # ---- Colour scale --------------------------------------------------------

  if (show_purity) {
    p <- p +
      ggplot2::scale_fill_gradient(
        low      = color_low,
        high     = color_high,
        na.value = "grey70",
        breaks   = scales::breaks_pretty(n = 5),
        labels   = scales::label_number(accuracy = 0.01),
        name     = "Purity"
      )
  } else {
    if (!is.null(class_palette)) {
      if (is.character(class_palette)) {
        p <- p + ggplot2::scale_colour_manual(values = class_palette, name = "Type")
      } else {
        p <- p + class_palette
      }
    }
    # If class_palette is NULL, ggplot2 default discrete colour scale is used
  }

  # ---- Size scale ----------------------------------------------------------
  # The size aesthetic encodes inverted Gini score (larger dot = lower Gini =
  # purer marker). The legend labels are therefore shown as the original Gini
  # values (i.e. gini_max minus the plotted value) for interpretability.

  gini_breaks_inv <- scales::breaks_pretty(n = 4)(
    range(plot_df$Size_plot, na.rm = TRUE)
  )
  # Convert back to original Gini values and format to exactly 2 decimal places
  gini_labels <- formatC(
    gini_max - gini_breaks_inv,
    digits = 2,
    format = "f"
  )

  p <- p +
    ggplot2::scale_size_continuous(
      range  = c(dotsize * 0.4, dotsize * 1.8),
      breaks = gini_breaks_inv,
      labels = gini_labels,
      name   = "Gini Score\n(lower = purer)"
    )

  # ---- Shape scale ---------------------------------------------------------

  p <- p +
    ggplot2::scale_shape_manual(
      values = used_shapes,
      breaks = names(used_shapes),   # controls legend key order
      name   = "Type"
    )

  # ---- X axis --------------------------------------------------------------

  x_max <- max(plot_df$Rank_plot, na.rm = TRUE)

  # Determine which Membership panels contain only NA-rank rows; suppress the
  # "0" tick label in those panels by using a custom label function that
  # replaces 0 with "" when the panel is all-NA-rank.
  na_rank_memberships <- unique(
    plot_df$Membership[plot_df$is_na_rank]
  )
  all_na_rank_memberships <- vapply(
    na_rank_memberships,
    function(m) all(plot_df$is_na_rank[plot_df$Membership == m]),
    logical(1L)
  )
  purely_na_rank_panels <- na_rank_memberships[all_na_rank_memberships]

  p <- p +
    ggplot2::scale_x_continuous(
      breaks = scales::pretty_breaks(n = min(max(x_max, 1L), 8L)),
      labels = function(x) ifelse(x == 0, "", as.character(x))
    )

  # ---- Faceting ------------------------------------------------------------
  # Always facet by Membership so the strip label is shown even when only one
  # panel is present — this ensures the user can always identify which
  # membership (e.g. "C1-Sub3") a feature belongs to.

  p <- p +
    ggplot2::facet_wrap(
      ggplot2::vars(.data$Membership),
      nrow   = nrow_panels,
      scales = "free_y"
    )

  # ---- Theme ---------------------------------------------------------------

  p <- p +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.border      = ggplot2::element_rect(
        color     = panel_border_color,
        fill      = NA,
        linewidth = panel_border_size
      ),
      panel.grid.major  = ggplot2::element_blank(),
      panel.grid.minor  = ggplot2::element_blank(),
      strip.background  = ggplot2::element_rect(fill = "grey95", colour = NA),
      strip.text        = ggplot2::element_text(size = axis_text_size + 1L, face = "bold"),
      plot.margin       = ggplot2::margin(r = plot_margin_right),
      axis.title.y      = ggplot2::element_text(size = axis_title_size),
      axis.text.y       = ggplot2::element_text(size = axis_text_size),
      axis.text.x       = ggplot2::element_text(size = axis_text_size),
      axis.title.x      = ggplot2::element_text(
        size   = axis_title_size,
        margin = ggplot2::margin(t = 10)
      ),
      legend.box        = legend_box,
      legend.box.just   = legend_box_just
    )

  if (show_legend) {
    p <- p + ggplot2::theme(legend.position = legend_position)
  } else {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  # ---- Labels --------------------------------------------------------------

  p <- p +
    ggplot2::labs(
      x        = xlab,
      y        = ylab,
      title    = title,
      subtitle = subtitle,
      tag      = tag
    )

  p
}


# _____________________________________________________________________________
# Main function
# _____________________________________________________________________________

#' Inspect the Membership of Features Across ClustoCell Results
#'
#' @description
#' \code{featureInspect()} searches one or more features across all marker
#' collections contained within a \code{ClustoCell} object, including global
#' feature collections, cross-cluster markers, major cluster-specific markers,
#' and sub-cluster-specific markers. All matches are returned in a single
#' long-format data frame containing the feature level, membership, marker
#' type, Gini score, purity, and rank. Optionally, a publication-quality
#' \pkg{ggplot2} visualisation summarising feature memberships can also be
#' generated.
#'
#' @param clustoCell An object of class \code{ClustoCell} obtained using
#'   \code{\link{clustoCell}} or \code{\link{markoClust}}.
#' @param features A character vector of feature names (e.g. gene symbols) to
#'   inspect across the \code{ClustoCell} object.
#' @param level Character vector specifying which hierarchical level(s) to
#'   include in the output. One or more of \code{"Global"},
#'   \code{"Cross-cluster"}, \code{"Major cluster"}, and
#'   \code{"Sub-cluster"}. If \code{NULL} (default), results from all levels
#'   are returned. If none of the queried features are found at the specified
#'   level(s), a zero-row \code{data.frame} is returned (with a warning)
#'   rather than an error.
#' @param type Character vector specifying which marker type(s) to include in
#'   the output. One or more of \code{"Positive"}, \code{"Negative"},
#'   \code{"Medium"}, \code{"Pure Ranked"}, \code{"Pure High"},
#'   \code{"Pure Medium"}, and \code{"Pure"}. If \code{NULL} (default),
#'   results of all types are returned. The value \code{"Pure"} is a
#'   convenience shorthand that expands to \code{"Pure Ranked"},
#'   \code{"Pure High"}, and \code{"Pure Medium"} simultaneously, including
#'   all global feature categories. Individual pure types (e.g.
#'   \code{"Pure High"}) can also be specified directly. The \code{level} and
#'   \code{type} filters are applied independently; if their combination
#'   yields no matching rows, a zero-row \code{data.frame} is returned (with
#'   a warning) rather than an error.
#' @param sort_by Character string specifying how to order the rows of the
#'   output table. One of:
#'   \describe{
#'     \item{\code{"input"}}{(Default) Rows follow the order of \code{features}
#'       as supplied by the user, then by level (Global \eqn{\rightarrow}
#'       Cross-cluster \eqn{\rightarrow} Major cluster \eqn{\rightarrow}
#'       Sub-cluster).}
#'     \item{\code{"rank"}}{Ascending \code{Rank} (rows with \code{NA} rank
#'       appear last), then by input order within ties.}
#'     \item{\code{"gini"}}{Descending \code{Gini_Score} (rows with \code{NA}
#'       Gini score appear last), then by input order within ties.}
#'   }
#' @param plot Logical. If \code{FALSE} (default), a \code{data.frame} is
#'   returned. If \code{TRUE}, a named list with elements \code{$table} and
#'   \code{$plot} is returned.
#' @param title Character. Plot title. Ignored when \code{plot = FALSE}.
#' @param subtitle Character. Plot subtitle. Ignored when \code{plot = FALSE}.
#' @param tag Character. Plot tag (e.g. panel label). Ignored when
#'   \code{plot = FALSE}.
#' @param nrow_panels Integer. Number of rows used when faceting the plot by
#'   \code{Membership}. If \code{NULL} (default), the number of rows is
#'   determined automatically by \code{\link[ggplot2]{facet_wrap}}.
#'   Ignored when \code{plot = FALSE}.
#' @param dotsize Numeric. Controls the size range of the dots in the plot.
#'   The actual \code{size} aesthetic is scaled between \code{dotsize * 0.4}
#'   and \code{dotsize * 1.8}. Default is \code{3}. Ignored when
#'   \code{plot = FALSE}.
#' @param show_purity Logical. If \code{TRUE} (default), dot colour encodes
#'   \code{Purity} via a continuous gradient. If \code{FALSE}, dot colour
#'   encodes \code{Type} as a discrete scale. Ignored when \code{plot = FALSE}.
#' @param class_palette Optional. Only used when \code{show_purity = FALSE}.
#'   Specifies the colour scale for \code{Type}. Can be one of:
#'   \itemize{
#'     \item A \pkg{ggplot2} scale object (e.g.
#'       \code{ggplot2::scale_colour_brewer()}).
#'     \item A named or unnamed character vector of colours (e.g.
#'       \code{c("red", "blue", "green")}), which is passed to
#'       \code{\link[ggplot2]{scale_colour_manual}}.
#'     \item \code{NULL} (default): the default \pkg{ggplot2} discrete colour
#'       scale is used.
#'   }
#'   Ignored when \code{plot = FALSE}.
#' @param color_low Character. The low-end colour of the continuous Purity
#'   gradient. Default is \code{"steelblue"}. Ignored when
#'   \code{show_purity = FALSE} or \code{plot = FALSE}.
#' @param color_high Character. The high-end colour of the continuous Purity
#'   gradient. Default is \code{"firebrick"}. Ignored when
#'   \code{show_purity = FALSE} or \code{plot = FALSE}.
#' @param panel_border_color Character. Colour of the panel border.
#'   Default is \code{"black"}. Ignored when \code{plot = FALSE}.
#' @param panel_border_size Numeric. Line width of the panel border.
#'   Default is \code{0.5}. Ignored when \code{plot = FALSE}.
#' @param axis_text_size Numeric. Font size (in points) for axis tick labels.
#'   Default is \code{8}. Ignored when \code{plot = FALSE}.
#' @param axis_title_size Numeric. Font size (in points) for axis titles.
#'   Default is \code{9}. Ignored when \code{plot = FALSE}.
#' @param plot_margin_right Numeric. Right margin of the plot in points.
#'   Default is \code{10}. Ignored when \code{plot = FALSE}.
#' @param xlab Character. Label for the x-axis. Default is \code{"Rank"}.
#'   Ignored when \code{plot = FALSE}.
#' @param ylab Character. Label for the y-axis. Default is \code{"Feature"}.
#'   Ignored when \code{plot = FALSE}.
#' @param show_legend Logical. Whether to display the plot legend.
#'   Default is \code{TRUE}. Ignored when \code{plot = FALSE}.
#' @param legend_position Character. Position of the legend. One of
#'   \code{"right"} (default), \code{"left"}, \code{"top"}, \code{"bottom"},
#'   or \code{"none"}. Ignored when \code{plot = FALSE}.
#' @param legend_box Character. Arrangement of multiple legend keys.
#'   One of \code{"vertical"} (default) or \code{"horizontal"}.
#'   Ignored when \code{plot = FALSE}.
#' @param legend_box_just Character. Justification of legend boxes.
#'   Default is \code{"left"}. Ignored when \code{plot = FALSE}.
#'
#' @details
#' \strong{Rank interpretation.} The \code{Rank} column reflects the rank of
#' the feature \emph{within its specific collection} (i.e. within the
#' combination of \code{Level}, \code{Membership}, and \code{Type}), as
#' assigned by \code{\link{clustoCell}} or \code{\link{markoClust}}. It does
#' \emph{not} represent the row position in the output table returned by
#' \code{featureInspect()}. A feature ranked 1st in cluster C1 positive
#' markers and 5th in sub-cluster C1-Sub1 medium markers will appear in two
#' separate rows with \code{Rank} values of 1 and 5, respectively.
#'
#' \strong{Dot size in the plot.} When \code{plot = TRUE}, dot size encodes
#' the \emph{inverted} Gini score: a lower Gini score indicates a purer
#' marker and is represented by a \emph{larger} dot. The size legend labels
#' display the original Gini score values for interpretability. Features
#' without a Gini score (i.e. global features stored in
#' \code{globally_pure_ranked}, \code{globally_pure_high}, or
#' \code{globally_pure_medium}) are rendered as large, semi-transparent grey
#' dots with a heavier border stroke to signal that their size carries no
#' quantitative meaning. These features are also plotted at \eqn{x = 0}
#' because no rank is assigned to them; the \code{0} tick label is suppressed
#' in panels that contain only unranked features to avoid misinterpretation.
#'
#' \strong{Level and type filtering.} When \code{level} and/or \code{type}
#' are specified, only rows matching the requested value(s) are returned. The
#' two filters are applied sequentially and independently: \code{level} is
#' applied first, then \code{type}. Specifying \code{type = "Pure"} expands
#' to all three global pure-type categories (\code{"Pure Ranked"},
#' \code{"Pure High"}, \code{"Pure Medium"}) but does \emph{not} override the
#' \code{level} filter — if \code{level} is set to a non-global level, the
#' combination will yield zero rows (with a warning). If no features are found
#' after filtering, a zero-row \code{data.frame} is returned with a warning
#' rather than an error, allowing \code{featureInspect()} to be used safely
#' inside loops or \code{lapply()} calls.
#'
#' @return
#' \describe{
#'   \item{If \code{plot = FALSE}}{A \code{data.frame} with one row for each
#'     occurrence of each queried feature across all (or the selected) marker
#'     collections. Columns are:
#'     \describe{
#'       \item{\code{Feature}}{Feature name (character).}
#'       \item{\code{Level}}{Hierarchical level at which the feature was found:
#'         \code{"Global"}, \code{"Cross-cluster"}, \code{"Major cluster"}, or
#'         \code{"Sub-cluster"} (character).}
#'       \item{\code{Membership}}{The specific collection in which the feature
#'         was found, e.g. \code{"Global Features"},
#'         \code{"Cross-cluster Marker"}, \code{"C1"}, or \code{"C1-Sub1"}
#'         (character).}
#'       \item{\code{Type}}{Marker type: \code{"Pure High"},
#'         \code{"Pure Medium"}, \code{"Pure Ranked"}, \code{"Positive"},
#'         \code{"Negative"}, or \code{"Medium"} (character).}
#'       \item{\code{Gini_Score}}{Gini score of the feature within the
#'         collection (numeric). \code{NA} for global features.}
#'       \item{\code{Purity}}{Purity of the feature within the collection
#'         (numeric). \code{NA} for global and cross-cluster features.}
#'       \item{\code{Rank}}{Rank of the feature within its specific collection
#'         (integer). \code{NA} for global features. See Details.}
#'     }
#'     If no queried feature is found in any collection (or in the specified
#'     level(s)), a zero-row \code{data.frame} with the above columns is
#'     returned.
#'   }
#'   \item{If \code{plot = TRUE}}{A named list with two elements:
#'     \describe{
#'       \item{\code{$table}}{The results \code{data.frame} described above.}
#'       \item{\code{$plot}}{A \code{\link[ggplot2]{ggplot}} object
#'         visualising the identified feature memberships as a dot plot
#'         faceted by \code{Membership}. Dot position (x-axis) encodes
#'         \code{Rank}, dot size encodes the inverted \code{Gini_Score}
#'         (larger = purer), dot shape encodes \code{Type}, and dot colour
#'         encodes \code{Purity} (or \code{Type} when
#'         \code{show_purity = FALSE}). Features without a Gini score are
#'         shown as large semi-transparent grey dots. See Details.}
#'     }
#'   }
#' }
#'
#' @seealso \code{\link{clustoCell}}, \code{\link{markoClust}}
#'
#' @examples
#' \dontrun{
#' # --- Basic usage: return a table only ---
#' result_table <- featureInspect(
#'   clustoCell = my_clustocell_obj,
#'   features   = c("CD3D", "MS4A1", "FOXP3")
#' )
#' print(result_table)
#'
#' # --- Filter to a single level ---
#' result_major <- featureInspect(
#'   clustoCell = my_clustocell_obj,
#'   features   = c("CD3D", "MS4A1", "FOXP3"),
#'   level      = "Major cluster"
#' )
#'
#' # --- Filter to multiple levels ---
#' result_sub <- featureInspect(
#'   clustoCell = my_clustocell_obj,
#'   features   = c("CD3D", "MS4A1", "FOXP3"),
#'   level      = c("Major cluster", "Sub-cluster")
#' )
#'
#' # --- Return table sorted by Gini score ---
#' result_gini <- featureInspect(
#'   clustoCell = my_clustocell_obj,
#'   features   = c("CD3D", "MS4A1", "FOXP3"),
#'   sort_by    = "gini"
#' )
#'
#' # --- Return table and plot (default aesthetics) ---
#' result_list <- featureInspect(
#'   clustoCell = my_clustocell_obj,
#'   features   = c("CD3D", "MS4A1", "FOXP3"),
#'   plot       = TRUE
#' )
#' result_list$table
#' result_list$plot
#'
#' # --- Customise the plot ---
#' result_custom <- featureInspect(
#'   clustoCell    = my_clustocell_obj,
#'   features      = c("CD3D", "MS4A1", "FOXP3"),
#'   plot          = TRUE,
#'   title         = "Feature Membership Overview",
#'   subtitle      = "ClustoCell marker hierarchy",
#'   show_purity   = TRUE,
#'   color_low     = "navy",
#'   color_high    = "gold",
#'   dotsize       = 4,
#'   nrow_panels   = 2,
#'   legend_position = "bottom"
#' )
#' result_custom$plot
#'
#' # --- Colour dots by Type instead of Purity ---
#' result_type <- featureInspect(
#'   clustoCell    = my_clustocell_obj,
#'   features      = c("CD3D", "MS4A1"),
#'   plot          = TRUE,
#'   show_purity   = FALSE,
#'   class_palette = c(
#'     Positive    = "#2166AC",
#'     Negative    = "#D6604D",
#'     Medium      = "#4DAC26",
#'     "Pure High" = "#762A83"
#'   )
#' )
#' result_type$plot
#'
#' # --- Filter to positive markers only ---
#' result_pos <- featureInspect(
#'   clustoCell = my_clustocell_obj,
#'   features   = c("CD3D", "MS4A1", "FOXP3"),
#'   type       = "Positive"
#' )
#'
#' # --- Filter to all global (pure) features using the shorthand ---
#' result_pure <- featureInspect(
#'   clustoCell = my_clustocell_obj,
#'   features   = c("CD3D", "MS4A1", "FOXP3"),
#'   type       = "Pure"
#' )
#'
#' # --- Combine level and type filters ---
#' result_combo <- featureInspect(
#'   clustoCell = my_clustocell_obj,
#'   features   = c("CD3D", "MS4A1", "FOXP3"),
#'   level      = c("Major cluster", "Sub-cluster"),
#'   type       = c("Positive", "Negative")
#' )
#'
#' # --- Safe use when a level or type may not exist ---
#' # Returns a zero-row data.frame with a warning (no error)
#' result_empty <- featureInspect(
#'   clustoCell = my_clustocell_obj,
#'   features   = c("CD3D"),
#'   level      = "Cross-cluster"
#' )
#'
#' result_empty2 <- featureInspect(
#'   clustoCell = my_clustocell_obj,
#'   features   = c("CD3D"),
#'   type       = "Pure"
#' )
#' }
#'
#' @export
featureInspect <- function(
    clustoCell,
    features,
    level              = NULL,
    type               = NULL,
    sort_by            = c("input", "rank", "gini"),
    plot               = FALSE,
    title              = NULL,
    subtitle           = NULL,
    tag                = NULL,
    nrow_panels        = NULL,
    dotsize            = 3,
    show_purity        = TRUE,
    class_palette      = NULL,
    color_low          = "steelblue",
    color_high         = "firebrick",
    panel_border_color = "black",
    panel_border_size  = 0.5,
    axis_text_size     = 8,
    axis_title_size    = 9,
    plot_margin_right  = 10,
    xlab               = "Rank",
    ylab               = "Feature",
    show_legend        = TRUE,
    legend_position    = "right",
    legend_box         = "vertical",
    legend_box_just    = "left"
) {

  # ---- Input validation ----------------------------------------------------

  if (!is.list(clustoCell)) {
    cli::cli_abort(
      "{.arg clustoCell} must be a {.cls ClustoCell} object (a list), not {.cls {class(clustoCell)}}."
    )
  }

  if (!is.character(features) || length(features) == 0L) {
    cli::cli_abort(
      "{.arg features} must be a non-empty character vector of feature names."
    )
  }

  valid_levels <- c("Global", "Cross-cluster", "Major cluster", "Sub-cluster")

  if (!is.null(level)) {
    if (!is.character(level) || length(level) == 0L) {
      cli::cli_abort(
        "{.arg level} must be a character vector or {.val NULL}."
      )
    }
    bad_levels <- setdiff(level, valid_levels)
    if (length(bad_levels) > 0L) {
      cli::cli_abort(
        c(
          "Invalid value{?s} in {.arg level}: {.val {bad_levels}}.",
          "i" = "Must be one or more of: {.val {valid_levels}}."
        )
      )
    }
  }

  # Valid atomic type values (excluding the "Pure" shorthand)
  valid_types_atomic <- c(
    "Positive", "Negative", "Medium",
    "Pure Ranked", "Pure High", "Pure Medium"
  )
  # "Pure" is a convenience shorthand that expands to all three pure variants
  valid_types_all <- c(valid_types_atomic, "Pure")

  # Resolved type filter: the actual Type column values to keep
  type_filter <- NULL

  if (!is.null(type)) {
    if (!is.character(type) || length(type) == 0L) {
      cli::cli_abort(
        "{.arg type} must be a character vector or {.val NULL}."
      )
    }
    bad_types <- setdiff(type, valid_types_all)
    if (length(bad_types) > 0L) {
      cli::cli_abort(
        c(
          "Invalid value{?s} in {.arg type}: {.val {bad_types}}.",
          "i" = "Must be one or more of: {.val {valid_types_all}}."
        )
      )
    }
    # Expand "Pure" shorthand to the three individual pure Type values
    type_expanded <- type
    if ("Pure" %in% type_expanded) {
      type_expanded <- union(
        setdiff(type_expanded, "Pure"),
        c("Pure Ranked", "Pure High", "Pure Medium")
      )
    }
    type_filter <- type_expanded
  }

  sort_by <- match.arg(sort_by)

  if (!is.logical(plot) || length(plot) != 1L) {
    cli::cli_abort(
      "{.arg plot} must be a single logical value ({.val TRUE} or {.val FALSE})."
    )
  }

  # Warn about duplicated features in the input
  dup_feats <- features[duplicated(features)]
  if (length(dup_feats) > 0L) {
    n_dup <- length(dup_feats)
    cli::cli_warn(
      "{cli::qty(n_dup)} Duplicate feature{?s} detected in {.arg features} and will be deduplicated: {.val {dup_feats}}."
    )
    features <- unique(features)
  }

  # ---- Search all sections -------------------------------------------------

  res_global  <- .fi_inspect_global(clustoCell, features)
  res_cross   <- .fi_inspect_cross_cluster(clustoCell, features)
  res_major   <- .fi_inspect_major_clusters(clustoCell, features)
  res_sub     <- .fi_inspect_sub_clusters(clustoCell, features)

  result <- rbind(res_global, res_cross, res_major, res_sub)

  # ---- Apply level filter --------------------------------------------------

  if (!is.null(level) && nrow(result) > 0L) {
    result <- result[result$Level %in% level, , drop = FALSE]
  }

  # ---- Apply type filter ---------------------------------------------------

  if (!is.null(type_filter) && nrow(result) > 0L) {
    result <- result[result$Type %in% type_filter, , drop = FALSE]
  }

  # ---- Warn if nothing found -----------------------------------------------

  # Build a human-readable description of the active filters for warnings
  .fi_filter_desc <- function(level, type) {
    parts <- character(0L)
    if (!is.null(level)) parts <- c(parts, paste0("level = ", paste(level, collapse = ", ")))
    if (!is.null(type))  parts <- c(parts, paste0("type = ",  paste(type,  collapse = ", ")))
    if (length(parts) == 0L) return(NULL)
    paste(parts, collapse = "; ")
  }
  filter_desc <- .fi_filter_desc(level, type)

  if (nrow(result) == 0L) {
    n_feats <- length(features)
    if (!is.null(filter_desc)) {
      cli::cli_warn(
        c(
          "{cli::qty(n_feats)} None of the supplied feature{?s} {?was/were} found with the requested filter{?s} ({filter_desc}).",
          "i" = "A zero-row {.cls data.frame} is returned."
        )
      )
    } else {
      cli::cli_warn(
        c(
          "{cli::qty(n_feats)} None of the supplied feature{?s} {?was/were} found in any marker collection of {.arg clustoCell}.",
          "i" = "A zero-row {.cls data.frame} is returned."
        )
      )
    }
    if (plot) {
      return(list(table = result, plot = NULL))
    }
    return(result)
  }

  # Warn about features not found after all filters
  not_found <- setdiff(features, result$Feature)
  if (length(not_found) > 0L) {
    n_nf <- length(not_found)
    if (!is.null(filter_desc)) {
      cli::cli_warn(
        "{cli::qty(n_nf)} The following feature{?s} {?was/were} not found with the requested filter{?s} ({filter_desc}): {.val {not_found}}."
      )
    } else {
      cli::cli_warn(
        "{cli::qty(n_nf)} The following feature{?s} {?was/were} not found in any marker collection: {.val {not_found}}."
      )
    }
  }

  # ---- Sort ----------------------------------------------------------------

  input_order <- stats::setNames(seq_along(features), features)
  result$`.input_order` <- input_order[result$Feature]

  level_order <- c("Global", "Cross-cluster", "Major cluster", "Sub-cluster")
  result$`.level_order` <- match(result$Level, level_order)

  result <- switch(
    sort_by,
    "input" = result[order(result$`.input_order`, result$`.level_order`), ],
    "rank"  = result[order(
      ifelse(is.na(result$Rank), Inf, result$Rank),
      result$`.input_order`
    ), ],
    "gini"  = result[order(
      ifelse(is.na(result$Gini_Score), Inf, -result$Gini_Score),
      result$`.input_order`
    ), ]
  )

  result$`.input_order` <- NULL
  result$`.level_order` <- NULL
  rownames(result) <- NULL

  # ---- Return --------------------------------------------------------------

  if (!plot) {
    return(result)
  }

  p <- .fi_make_plot(
    result_df          = result,
    features           = features,
    show_purity        = show_purity,
    class_palette      = class_palette,
    color_low          = color_low,
    color_high         = color_high,
    dotsize            = dotsize,
    nrow_panels        = nrow_panels,
    title              = title,
    subtitle           = subtitle,
    tag                = tag,
    panel_border_color = panel_border_color,
    panel_border_size  = panel_border_size,
    axis_text_size     = axis_text_size,
    axis_title_size    = axis_title_size,
    plot_margin_right  = plot_margin_right,
    xlab               = xlab,
    ylab               = ylab,
    show_legend        = show_legend,
    legend_position    = legend_position,
    legend_box         = legend_box,
    legend_box_just    = legend_box_just
  )

  list(table = result, plot = p)
}
