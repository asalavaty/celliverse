#' Visualize per-signature marker expression across clusters
#'
#' @description
#' Generates a dot heatmap summarizing expression of signature-associated features across clusters.
#'
#' @details
#' Dot size typically encodes the percentage of cells expressing a feature,
#' while color intensity reflects average expression.
#'
#' @param seurat_obj
#' A \code{Seurat} object containing expression data and metadata.
#'
#' @param cluster_col
#' Character; column in \code{seurat_obj@meta.data} containing cluster labels. These could correspond to user defined clusters or cell types.
#'
#' @param row_data
#' Data frame mapping features to signatures. Must contain one row per feature.
#'
#' @param features_col
#' Character; column in \code{row_data} containing feature (e.g. gene) IDs corresponding to the row names of the \code{seurat_obj}.
#'
#' @param signature_col
#' Character; column in \code{row_data} containing signature labels.
#'
#' @param cell_type_colors
#' Color specification for cell types.
#'
#' @param signature_colors
#' Color specification for signatures.
#'
#' @param tag
#' Optional plot tag.
#'
#' @param tile_palette
#' Character vector defining tile fill colors.
#'
#' @param tile_alpha_range
#' Numeric vector of length two defining alpha range.
#'
#' @param dot_size_factor
#' Numeric; scaling factor for dot sizes.
#'
#' @param dot_range
#' Numeric vector defining minimum and maximum dot sizes.
#'
#' @param signature_label_size
#' Numeric; font size of signature labels.
#'
#' @param feature_label_size
#' Numeric; font size of feature labels.
#'
#' @param feature_label_angle
#' Numeric; angle of feature labels.
#'
#' @param show_cluster_labels
#' Logical; whether to show cluster labels.
#'
#' @param show_signature_strip
#' Logical; whether to show signature strip.
#'
#' @param show_signature_labels
#' Logical; whether to show signature labels.
#'
#' @param show_cluster_strip
#' Logical; whether to show cluster strip.
#'
#' @param show_cluster_legend
#' Logical; whether to show cluster legend.
#'
#' @param show_signature_legend
#' Logical; whether to show signature legend.
#'
#' @param legend_ncol
#' Integer; number of legend columns.
#'
#' @param expression_legend_title
#' Character; title for expression legend.
#'
#' @param percent_legend_title
#' Character; title for percent expressed legend.
#'
#' @param tile_fill_legend_title
#' Character; title for tile fill legend.
#'
#' @param signature_legend_title
#' Character; title for signature legend.
#'
#' @param vline_color
#' Character; color of vertical separator lines.
#'
#' @param vline_width
#' Numeric; width of vertical separator lines.
#'
#' @param block_border_color
#' Character; color of block borders.
#'
#' @param feature_label_face
#' Character; font face for feature labels.
#'
#' @return
#' A \code{ggplot2} object.
#'
#' @seealso
#' \code{\link{typoClustVis}}, \code{\link{markoCell}}
#'
#' @examples
#' \dontrun{
#' p <- signatureDotHeatmap(
#'   seurat_obj = so,
#'   row_data = signatures,
#'   features_col = "Features",
#'   signature_col = "Signature"
#' )
#' }
#' 
#' @export

signatureDotHeatmap <- function(
    seurat_obj,
    cluster_col,
    row_data,
    features_col  = NULL,
    signature_col = NULL,
    
    # ----- Colors -----
    cell_type_colors = NULL, # A scale specification for coloring cell types. Can be specified in one of two forms: 
    ## A palette function (e.g. scales::hue_pal(), ggsci::pal_igv()). 
    ## A character vector of colors (e.g. c("red", "blue", "green")).
    
    signature_colors = NULL, # A scale specification for coloring signatures. Can be specified in one of two forms: 
    ## A palette function (e.g. scales::hue_pal(), ggsci::pal_igv()). 
    ## A character vector of colors (e.g. c("red", "blue", "green")).
    
    tag = NULL,
    
    # ----- Shading -----
    tile_palette = RColorBrewer::brewer.pal(9, "YlOrRd"),
    tile_alpha_range = c(0.15, 0.65),
    
    # ----- Dot parameters -----
    dot_size_factor = 2,
    dot_range = c(0.5, 3),
    
    # ----- Labels -----
    signature_label_size = 3.3,
    feature_label_size = 6,
    feature_label_angle = 45,
    
    # ----- Layout -----
    show_cluster_labels   = FALSE,
    show_signature_strip   = TRUE,
    show_signature_labels  = TRUE,
    show_cluster_strip    = TRUE,
    show_cluster_legend   = TRUE,
    show_signature_legend  = FALSE,
    legend_ncol            = 2,
    expression_legend_title = "Expression",
    percent_legend_title    = "Percent\nExpressed",
    tile_fill_legend_title  = NULL,
    signature_legend_title  = "Signature",
    vline_color             = "black",
    vline_width             = 0.15,
    block_border_color      = "grey90",
    feature_label_face      = "plain"
) {
  
  #________________________________________
  # Dealing with warnings
  ## Save current warning setting and disable warnings
  old_warn <- getOption("warn")
  options(warn = -1)   # -1 = suppress all warnings
  
  on.exit(options(warn = old_warn), add = TRUE)  # restore when function exits
  
  #________________________________________
  
  # Checking arguments
  
  row_data_missing <- missing(row_data)
  seurat_obj_missing <- missing(seurat_obj)
  cluster_col_missing <- missing(cluster_col)
  
  #________________________________________
  
  # =======================================================
  # Input checks
  # =======================================================
  
  if(seurat_obj_missing) {
    cli::cli_abort("The seurat_obj cannot be left unspecified!")
  }
  
  if(cluster_col_missing) {
    cli::cli_abort("The cluster_col cannot be left unspecified!")
  }
  
  if (!cluster_col %in% base::colnames(seurat_obj@meta.data)) {
    base::stop("`cluster_col` = ", cluster_col,
               " not found in seurat_obj@meta.data.")
  }
  
  if(row_data_missing) {
    cli::cli_abort("The row_data cannot be left unspecified!")
  }
  
  if (is.null(features_col) || is.null(signature_col)) {
    base::stop("`features_col` and `signature_col` must be provided (no defaults).")
  }
  
  if (!features_col %in% base::colnames(row_data)) {
    base::stop("`features_col` = ", features_col,
               " not found in `row_data`.")
  }
  
  if (!signature_col %in% base::colnames(row_data)) {
    base::stop("`signature_col` = ", signature_col,
               " not found in `row_data`.")
  }
  
  # enforce one row per feature (no duplicates across signatures)
  feature_vec <- row_data[[features_col]]
  if (base::length(base::unique(feature_vec)) != base::nrow(row_data)) {
    base::stop(
      "Each feature must appear exactly once in `row_data` (no duplicates).\n",
      "Check the column: ", features_col
    )
  }
  
  # Extract features to pull from Seurat
  features <- feature_vec
  
  # Construct a clean mapping data.frame: Feature + Signature (factor)
  row_data_map <- row_data %>%
    dplyr::transmute(
      Feature   = .data[[features_col]],
      Signature = base::factor(.data[[signature_col]])
    )
  
  # Keep a separate factor (for level order access)
  signature_factor <- row_data_map$Signature
  
  # =======================================================
  # FetchData + reshape
  # =======================================================
  dot_raw <- Seurat::FetchData(
    seurat_obj,
    vars = base::c(features, cluster_col)
  ) %>%
    dplyr::mutate(Cell = base::rownames(.))
  
  dot_long_raw <- dot_raw %>%
    tidyr::pivot_longer(
      cols = tidyselect::all_of(features),
      names_to = "Feature",
      values_to = "Expression"
    ) %>%
    dplyr::mutate(Cluster = .data[[cluster_col]])
  
  if (!base::identical(cluster_col, "Cluster")) {
    dot_long_raw <- dot_long_raw %>%
      dplyr::select(-tidyselect::all_of(cluster_col))
  }
  
  # =======================================================
  # Summary stats (pct_expr, avg_expr)
  # =======================================================
  dot_long <- dot_long_raw %>%
    dplyr::left_join(row_data_map, by = "Feature") %>%
    dplyr::group_by(Cluster, Feature, Signature) %>%
    dplyr::summarise(
      pct_expr = base::mean(Expression > 0),
      avg_expr = dplyr::if_else(
        base::sum(Expression > 0) == 0,
        0,
        base::mean(Expression[Expression > 0])
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(dot_size = pct_expr * dot_size_factor)
  
  # =======================================================
  # Signature positional index (global_x)
  # =======================================================
  signature_index <- row_data_map %>%
    dplyr::distinct(Feature, Signature) %>%
    dplyr::arrange(Signature) %>%
    dplyr::group_by(Signature) %>%
    dplyr::mutate(block_x = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      global_x = block_x +
        (base::as.numeric(Signature) - 1L) * base::max(block_x)
    )
  
  dot_plot_df <- dot_long %>%
    dplyr::left_join(signature_index, by = c("Feature", "Signature"))
  
  # =======================================================
  # Tile fill + alpha
  # =======================================================
  pal_fun <- grDevices::colorRampPalette(tile_palette)
  palette_100 <- pal_fun(100L)
  
  dot_plot_df <- dot_plot_df %>%
    dplyr::mutate(
      tile_fill = palette_100[
        base::round(scales::rescale(avg_expr, to = base::c(1, 100)))
      ],
      tile_alpha = scales::rescale(avg_expr, to = tile_alpha_range)
    )
  
  # =======================================================
  # Main GGplot (dot heatmap)
  # =======================================================
  gg_dot <- ggplot2::ggplot(
    dot_plot_df,
    ggplot2::aes(x = global_x, y = Cluster)
  ) +
    ggplot2::geom_tile(
      ggplot2::aes(fill = tile_fill, alpha = tile_alpha),
      width  = 0.95,
      height = 0.95,
      color  = block_border_color
    ) +
    ggplot2::scale_fill_identity(name = tile_fill_legend_title) +
    ggplot2::scale_alpha_identity() +
    ggnewscale::new_scale_fill() +
    
    ggplot2::geom_point(
      ggplot2::aes(size = pct_expr, fill = avg_expr),
      shape = 21,
      color = "black"
    ) +
    ggplot2::scale_fill_distiller(
      palette   = "YlOrRd",
      direction = 1,
      name      = expression_legend_title
    ) +
    ggplot2::scale_size(
      name  = percent_legend_title,
      range = dot_range
    ) +
    
    ggplot2::geom_vline(
      xintercept = base::c(
        base::max(signature_index$global_x[
          signature_index$Signature ==
            base::levels(signature_factor)[1]
        ]) + 0.5,
        base::max(signature_index$global_x[
          signature_index$Signature ==
            base::levels(signature_factor)[2]
        ]) + 0.5
      ),
      color     = vline_color,
      linewidth = vline_width
    ) +
    
    ggplot2::annotate(
      "text",
      x     = base::tapply(
        signature_index$global_x,
        signature_index$Signature,
        base::mean
      ),
      y     = Inf,
      label = base::levels(signature_factor),
      vjust = -0.5,
      size  = signature_label_size,
      fontface = "bold"
    ) +
    
    ggplot2::scale_x_continuous(
      breaks = signature_index$global_x,
      labels = signature_index$Feature,
      expand = base::c(0, 0)
    ) +
    ggplot2::labs(x = NULL) +
    
    ggplot2::theme_minimal(base_size = 7) +
    ggplot2::theme(
      plot.margin = ggplot2::margin(
        t = base::ifelse(show_signature_legend, -5, -15),
        r = 0, b = 2, l = 0
      ),
      panel.border = ggplot2::element_rect(
        color = "black",
        fill  = NA,
        linewidth = 0.15
      ),
      legend.title     = ggplot2::element_text(face = "italic"),
      legend.key.width = grid::unit(0.25, "cm"),
      legend.key.height= grid::unit(0.5, "cm"),
      axis.title.y     = ggplot2::element_blank(),
      axis.text.y      = if(show_cluster_labels) ggplot2::element_text(size = 7) else ggplot2::element_blank(),
      panel.grid       = ggplot2::element_blank(),
      axis.text.x      = ggplot2::element_text(
        angle = feature_label_angle,
        vjust = 1.18,
        hjust = 1,
        size  = feature_label_size,
        face  = feature_label_face
      )
    ) +
    ggplot2::guides(
      size = ggplot2::guide_legend(ncol = legend_ncol)
    )
  
  # =======================================================
  # Top strip for signatures (color bar)
  # =======================================================
  if (!base::is.null(signature_colors)) {
    if (!base::is.character(signature_colors)) {
      signature_colors <- signature_colors(
        base::length(base::unique(signature_index$Signature))
      )
    }
  } else {
    signature_colors <- scales::viridis_pal()(
      base::length(base::unique(signature_index$Signature))
    )
  }
  
  top_strip <- signature_index %>%
    dplyr::arrange(global_x) %>%
    dplyr::mutate(y_pos = 1L) %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x    = global_x,
        y    = y_pos,
        fill = Signature
      )
    ) +
    ggplot2::geom_tile(width = 1, height = 1) +
    ggplot2::scale_fill_manual(
      values = signature_colors,
      name   = signature_legend_title
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(
        t = 2,
        r = base::ifelse(show_cluster_strip, -4, 0.5),
        b = base::ifelse(show_signature_legend, -18, -7),
        l = 0
      ),
      legend.position   = if (show_signature_legend) "top" else "none",
      legend.margin     = ggplot2::margin(0, 0, b = 2, 0),
      legend.box.margin = ggplot2::margin(0, 0, b = 2, 0),
      legend.box.spacing= grid::unit(0, "pt"),
      legend.text       = ggplot2::element_text(
        size   = 5.5,
        margin = ggplot2::margin(l = 1)
      ),
      legend.title      = ggplot2::element_text(
        face   = "italic",
        size   = 5.5,
        margin = ggplot2::margin(r = 3)
      ),
      legend.key.width  = grid::unit(0.25, "cm"),
      legend.key.height = grid::unit(0.25, "cm")
    )
  
  # =======================================================
  # Signature titles row (text labels above signatures)
  # =======================================================
  signature_labels <- signature_index %>%
    dplyr::group_by(Signature) %>%
    dplyr::summarise(
      x_center = base::mean(global_x),
      .groups  = "drop"
    ) %>%
    dplyr::mutate(y = 1L)
  
  signature_titles <- ggplot2::ggplot(
    signature_labels,
    ggplot2::aes(x = x_center, y = y, label = Signature)
  ) +
    ggplot2::geom_text(
      size     = 2,
      fontface = "bold",
      vjust    = 1.5,
      hjust    = 0.5
    ) +
    ggplot2::scale_x_continuous(
      limits = base::c(
        base::min(signature_index$global_x),
        base::max(signature_index$global_x)
      )
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(
        t = base::ifelse(show_signature_legend, -10, -12),
        r = base::ifelse(show_cluster_strip, -4, 0.5),
        b = base::ifelse(show_signature_legend, -20, -10),
        l = 0
      )
    )
  
  # =======================================================
  # Left strip for Clusters (color-encoded bar)
  # =======================================================
  celltypes <- base::sort(base::unique(dot_plot_df$Cluster))
  
  if (!base::is.null(cell_type_colors)) {
    if (base::is.character(cell_type_colors)) {
      cluster_cols <- stats::setNames(cell_type_colors, celltypes)
    } else {
      cluster_cols <- stats::setNames(
        cell_type_colors(base::length(celltypes)),
        celltypes
      )
    }
  } else {
    cluster_cols <- stats::setNames(
      scales::hue_pal()(base::length(celltypes)),
      celltypes
    )
  }
  
  left_strip <- dot_plot_df %>%
    dplyr::distinct(Cluster) %>%
    dplyr::mutate(x_pos = 1L) %>%
    ggplot2::ggplot(
      ggplot2::aes(x = x_pos, y = Cluster, fill = Cluster)
    ) +
    ggplot2::geom_tile(width = 1, height = 0.95) +
    ggplot2::scale_fill_manual(values = cluster_cols) +
    ggplot2::labs(tag = tag) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.tag        = ggplot2::element_text(size = 7, face = "bold"),
      plot.margin     = ggplot2::margin(0, 0, 0, 0),
      legend.position = if (show_cluster_legend) "right" else "none",
      legend.margin   = ggplot2::margin(0, 0, 0, 0),
      legend.box.margin = ggplot2::margin(0, 0, 0, 5),
      legend.box.spacing= grid::unit(0, "pt"),
      legend.text       = ggplot2::element_text(size = 5.5),
      legend.title      = ggplot2::element_text(
        size   = 5.5,
        margin = ggplot2::margin(b = 2)
      ),
      legend.key.width  = grid::unit(0.25, "cm"),
      legend.key.height = grid::unit(0.25, "cm")
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(ncol = legend_ncol)
    )
  
  # =======================================================
  # FINAL PATCHWORK LAYOUT
  # =======================================================
  if (show_signature_strip && show_signature_labels && show_cluster_strip) {
    
    final_plot <-
      patchwork::wrap_plots(
        (patchwork::wrap_elements(top_strip) /
           patchwork::wrap_elements(signature_titles) +
           patchwork::plot_layout(heights = c(0.1, 15))),
        (left_strip + gg_dot +
           patchwork::plot_layout(
             widths = c(1, 25),
             heights = 1,
             guides = "collect"
           )),
        nrow    = 2,
        heights = c(1, ifelse(show_signature_legend, 4.5, 10))
      ) &
      ggplot2::theme(
        plot.margin      = ggplot2::margin(t = 2, r = 0, b = 2, l = 0),
        legend.margin    = ggplot2::margin(0, 0, 0, 0),
        legend.box.margin= ggplot2::margin(0, 0, 0, 5),
        legend.box.spacing = grid::unit(0, "pt"),
        legend.title     = ggplot2::element_text(
          face   = "italic",
          size   = 5.5,
          margin = ggplot2::margin(b = 2)
        ),
        legend.box       = "vertical"
      )
    
  } else if (show_signature_strip && show_signature_labels) {
    
    final_plot <-
      patchwork::wrap_plots(
        (patchwork::wrap_elements(top_strip) /
           patchwork::wrap_elements(signature_titles) +
           patchwork::plot_layout(heights = c(0.1, 15))),
        gg_dot,
        nrow    = 2,
        heights = c(1, ifelse(show_signature_legend, 4.5, 10))
      ) &
      ggplot2::theme(
        plot.margin      = ggplot2::margin(t = 2, r = 0, b = 2, l = 0),
        legend.margin    = ggplot2::margin(0, 0, 0, 0),
        legend.box.margin= ggplot2::margin(0, 0, 0, 5),
        legend.box.spacing = grid::unit(0, "pt"),
        legend.title     = ggplot2::element_text(
          face   = "italic",
          size   = 5.5,
          margin = ggplot2::margin(b = 2)
        ),
        legend.box       = "vertical"
      )
    
  } else if (show_signature_strip && show_cluster_strip) {
    
    final_plot <-
      patchwork::wrap_plots(
        patchwork::wrap_elements(top_strip),
        (left_strip + gg_dot +
           patchwork::plot_layout(
             widths = c(1, 25),
             heights = 1,
             guides  = "collect"
           )),
        nrow    = 2,
        heights = c(0.5, ifelse(show_signature_legend, 4, 12))
      ) &
      ggplot2::theme(
        plot.margin      = ggplot2::margin(t = 2, r = 0, b = 2, l = 0),
        legend.margin    = ggplot2::margin(0, 0, 0, 0),
        legend.box.margin= ggplot2::margin(0, 0, 0, 5),
        legend.box.spacing = grid::unit(0, "pt"),
        legend.title     = ggplot2::element_text(
          face   = "italic",
          size   = 5.5,
          margin = ggplot2::margin(b = 2)
        ),
        legend.box       = "vertical"
      )
    
  } else if (show_signature_labels && show_cluster_strip) {
    
    final_plot <-
      patchwork::wrap_plots(
        patchwork::wrap_elements(signature_titles),
        (left_strip + gg_dot +
           patchwork::plot_layout(
             widths = c(1, 25),
             heights = 1,
             guides  = "collect"
           )),
        nrow    = 2,
        heights = c(1.5, 9)
      ) &
      ggplot2::theme(
        plot.margin      = ggplot2::margin(t = 2, r = 0, b = 2, l = 0),
        legend.margin    = ggplot2::margin(0, 0, 0, 0),
        legend.box.margin= ggplot2::margin(0, 0, 0, 5),
        legend.box.spacing = grid::unit(0, "pt"),
        legend.title     = ggplot2::element_text(
          face   = "italic",
          size   = 5.5,
          margin = ggplot2::margin(b = 2)
        ),
        legend.box       = "vertical"
      )
    
  } else if (show_signature_strip) {
    
    final_plot <-
      patchwork::wrap_plots(
        patchwork::wrap_elements(
          top_strip + ggplot2::theme(
            plot.margin = ggplot2::margin(t = 2, r = 0, b = -2, l = 0)
          )
        ),
        gg_dot,
        nrow    = 2,
        heights = c(0.5, ifelse(show_signature_legend, 4, 12))
      ) &
      ggplot2::theme(
        plot.margin      = ggplot2::margin(t = 2, r = 0, b = 2, l = 0),
        legend.margin    = ggplot2::margin(0, 0, 0, 0),
        legend.box.margin= ggplot2::margin(0, 0, 0, 5),
        legend.box.spacing = grid::unit(0, "pt"),
        legend.title     = ggplot2::element_text(
          face   = "italic",
          size   = 5.5,
          margin = ggplot2::margin(b = 2)
        ),
        legend.box       = "vertical"
      )
    
  } else if (show_signature_labels) {
    
    final_plot <-
      patchwork::wrap_plots(
        patchwork::wrap_elements(signature_titles),
        gg_dot,
        nrow    = 2,
        heights = c(1.5, 9)
      ) &
      ggplot2::theme(
        plot.margin      = ggplot2::margin(t = 2, r = 0, b = 2, l = 0),
        legend.margin    = ggplot2::margin(0, 0, 0, 0),
        legend.box.margin= ggplot2::margin(0, 0, 0, 5),
        legend.box.spacing = grid::unit(0, "pt"),
        legend.title     = ggplot2::element_text(
          face   = "italic",
          size   = 5.5,
          margin = ggplot2::margin(b = 2)
        ),
        legend.box       = "vertical"
      )
    
  } else if (show_cluster_strip) {
    
    final_plot <-
      (left_strip + gg_dot +
         patchwork::plot_layout(
           widths = c(1, 25),
           heights = 1,
           guides  = "collect"
         )) &
      ggplot2::theme(
        plot.margin      = ggplot2::margin(t = 2, r = 0, b = 2, l = 0),
        legend.margin    = ggplot2::margin(0, 0, 0, 0),
        legend.box.margin= ggplot2::margin(0, 0, 0, 5),
        legend.box.spacing = grid::unit(0, "pt"),
        legend.title     = ggplot2::element_text(
          face   = "italic",
          size   = 5.5,
          margin = ggplot2::margin(b = 2)
        ),
        legend.box       = "vertical"
      )
    
  } else {
    # Fallback: no strips at all, just the main dot plot with consistent theming
    final_plot <- gg_dot &
      ggplot2::theme(
        plot.margin      = ggplot2::margin(t = 2, r = 0, b = 2, l = 0),
        legend.margin    = ggplot2::margin(0, 0, 0, 0),
        legend.box.margin= ggplot2::margin(0, 0, 0, 5),
        legend.box.spacing = grid::unit(0, "pt"),
        legend.title     = ggplot2::element_text(
          face   = "italic",
          size   = 5.5,
          margin = ggplot2::margin(b = 2)
        ),
        legend.box       = "vertical"
      )
  }
  
  return(final_plot)
}
