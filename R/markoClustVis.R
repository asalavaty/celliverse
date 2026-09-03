#' Visualize cluster and cell-subset markers
#'
#' Generates a faceted dot plot for visualizing marker genes across clusters,
#' sub-clusters, or cell subsets stored in a \code{ClustoCell} or \code{MarkoCell}
#' object. Marker selection can be controlled by rank or by selecting the top
#' \code{n} markers per group. Dot size and color can represent marker purity
#' or marker class.
#'
#' @param obj An object of class \code{ClustoCell} or \code{MarkoCell}
#'   containing marker information.
#' @param desired_sets Optional character vector specifying the names of clusters,
#'   sub-clusters, and/or cell subsets to include. If \code{NULL}, all available
#'   sets in \code{obj} are used.
#' @param show_pos_markers Logical; whether to include positive markers.
#'   Default is \code{TRUE}.
#' @param show_neg_markers Logical; whether to include negative markers.
#'   Default is \code{FALSE}.
#' @param show_med_markers Logical; whether to include medium markers.
#'   Default is \code{FALSE}.
#' @param thresh_mode Character; method for selecting top markers. One of:
#'   \itemize{
#'     \item \code{"rank"}: include all markers up to the specified rank threshold.
#'     \item \code{"n"}: include exactly the top \code{n} markers.
#'   }
#' @param thresh Integer; threshold for selecting markers based on
#'   \code{thresh_mode}. Default is \code{5}.
#' @param title Optional character string for the plot title.
#' @param subtitle Optional character string for the plot subtitle.
#' @param tag Optional character string for the plot tag.
#' @param nrow_panels Optional integer specifying the number of rows in the
#'   faceted plot. If \code{NULL}, rows are determined automatically.
#' @param dotsize Numeric; size of the dots in the plot. Default is \code{2}.
#' @param show_purity Logical; if \code{TRUE}, dot color represents marker purity.
#'   If \code{FALSE}, dot color represents marker class. Default is \code{TRUE}.
#' @param class_palette Optional palette used when \code{show_purity = FALSE}.
#'   Can be either:
#'   \itemize{
#'     \item A \code{ggplot2} scale object (e.g., \code{ggplot2::scale_fill_hue()})
#'     \item A character vector of colors
#'   }
#' @param color_low Character; low color for gradient (used when
#'   \code{show_purity = TRUE}). Default is \code{"blue"}.
#' @param color_high Character; high color for gradient (used when
#'   \code{show_purity = TRUE}). Default is \code{"red"}.
#' @param panel_border_color Character; color of panel borders.
#' @param panel_border_size Numeric; size of panel borders.
#' @param axis_text_size Numeric; font size for axis text.
#' @param axis_title_size Numeric; font size for axis titles.
#' @param plot_margin_right Numeric; right margin of the plot.
#' @param xlab Character; label for the x-axis. Default is \code{"Rank"}.
#' @param ylab Character; label for the y-axis. Default is \code{"Marker"}.
#' @param show_legend Logical; whether to display the legend. Default is \code{TRUE}.
#' @param legend_box Character; layout of the legend box (e.g., \code{"vertical"}).
#' @param legend_box_just Character; justification of the legend box.
#' @param legend_position Character; position of the legend (e.g., \code{"right"}).
#'
#' @return A \code{ggplot2} object showing a faceted dot plot of selected markers
#'   across clusters, sub-clusters, or cell subsets.
#'
#' @details
#' This function provides a flexible visualization for exploring marker genes
#' identified in clustering analyses. Marker selection can be based on rank or
#' a fixed number of top markers. The resulting plot is faceted by cluster or
#' subset, enabling comparison across groups.
#'
#' When \code{show_purity = TRUE}, a continuous color scale is used to represent
#' marker purity. Otherwise, discrete colors are used to represent marker classes
#' (e.g., positive, negative, medium).
#'
#' @examples
#' utils::data("pbmc_small", package = "SeuratObject")
#'
#' pbmc_small$example_clusters <- as.character(
#'   SeuratObject::Idents(pbmc_small)
#' )
#'
#' cc <- markoClust(
#'   data = pbmc_small,
#'   cluster_labels = "example_clusters",
#'   identify_subclusters = FALSE,
#'   num_threads = 1,
#'   verbose = FALSE
#' )
#'
#' plt <- markoClustVis(
#'   obj = cc,
#'   show_pos_markers = TRUE,
#'   show_neg_markers = FALSE,
#'   thresh_mode = "n",
#'   thresh = 2
#' )
#'
#' plt
#'
#' @export

markoClustVis <- function(
    obj, # An object of class ClustoCell or MarkoCell.
    desired_sets = NULL, # Optional. A character vector of the names of desired clusters, sub-clusters, and/or cell-subsets present in the specified `obj`. If not specified, all clusters or cell-subsets in the obj will be visualized.

    show_pos_markers = TRUE, # logical, whether to visualize positive markers (default if TRUE).
    show_neg_markers = FALSE, # logical, whether to visualize negative markers (default if FALSE).
    show_med_markers = FALSE, # logical, whether to visualize medium markers (default if FALSE).
    thresh_mode = c("n", "rank"), # Specifies how to select top markers for each cluster, subcluster or cell-subset. Options are:
        ## "rank": selects all markers with ranks up to the threshold. If multiple markers share the same rank as the cutoff, they are all included.
        ## "n": selects strictly the top n rows in rank order. Only the first n rows are kept, even if additional rows share the same rank as the n-th row.
    thresh = 5, # Integer, threshold for choosing the top N rows of the marker tables or top N ranked markers of each cluster, sub-cluster, and cell-subset.
  
    title = NULL, # Character, the plot title
    subtitle = NULL, # Character, the plot subtitle
    tag = NULL, # Character, the plot tag
    nrow_panels = NULL, # Integer, number of rows of the panels when faceting the plot. If NULL, the number of rows is automatically set.
    
    dotsize = 2, # integer, Size of the dots
    show_purity = TRUE, # whether to show the purity of markers as the dot colors or not. If FALSE, dots will be colored based on marker classes.
    class_palette = NULL, # Optional, only used if show_purity is FALSE. A scale specification for coloring classes. Can be specified in one of two forms: 
    ## A ggplot2 scale object (e.g. ggplot2::scale_fill_hue(), ggsci::scale_fill_igv()). 
    ## A character vector of colors (e.g. c("red", "blue", "green")).
    color_low = "blue", # Low gradient color of the dots
    color_high = "red",  # High gradient color of the dots
    panel_border_color = "black",
    panel_border_size = 0.5,
    axis_text_size = 7,
    axis_title_size = 8,
    plot_margin_right = 10,
    xlab = "Rank",
    ylab = "Marker",
    show_legend = TRUE, # logical, whether to show legend or not
    legend_box = "vertical",
    legend_box_just = "left",
    legend_position = "right"
    
                      ) {

  # Setting the args
  
  thresh_mode <- match.arg(thresh_mode)
  
  #________________________________________
  
  # Defining the default logs for info messages
  log_message <- function(...) {
    cli::cli_alert_info(...)
  }

  #________________________________________
  
  # Start of function ----

  if(!show_pos_markers & !show_neg_markers & !show_med_markers) {
    cli::cli_abort("Either `show_pos_markers`, `show_neg_markers`, `show_med_markers` or any combination should be set to TRUE!")
  }
  
  if(is.null(desired_sets)) {
    log_message("Since `desired_sets` is not specified, the top markers of all clusters/cell subsets of `obj` will be visualized!")
  }
  
  if(!is.null(obj)) {
    if(!inherits(obj, "ClustoCell") & !inherits(obj, "MarkoCell")) {
      cli::cli_abort("The `obj` argument should be of class ClustoCell or MarkoCell!")
    }
  }
  
  if(!is.null(desired_sets)) {
    if(!inherits(desired_sets, "character")) {
      cli::cli_abort("The `desired_sets` argument should be a character vector of the names of desired clusters, sub-clusters, and/or cell-subsets present in the specified `obj`!")
    }
  }
  
  # Setting the names of all clusters, sub-clusters, cell-subsets, and desired panels ----
  clusters <- NULL
  sub_clusters <- NULL
  cell_subsets <- NULL
  panels <- NULL
  all_clusters <- NULL
  all_sub_clusters <- NULL
  MarkoCell_clusters <- NULL
  MarkoCell_cell_subset <- NULL
  all_clusters_pos <- NULL
  all_clusters_neg <- NULL
  all_clusters_med <- NULL
  combined_cell_set_names <- NULL
  combined_panels_list <- list(pos_panels = NULL,
                               neg_panels = NULL,
                               med_panels = NULL)
  
  ## Getting the names of all clusters, sub-clusters and cell-subsets ----
    
    ### Getting all major_clusters ----
    # Extract names separately
    if(any(grepl("major_clusters", names(obj$markers)))) {
      all_clusters <- obj$markers$major_clusters$cluster_specific$positive_markers %>% names()
    }
    all_clusters <- unique(all_clusters)
    
    # Extract pos, neg, and med separately
    if(any(grepl("major_clusters", names(obj$markers)))) {
      all_clusters_pos <- obj$markers$major_clusters$cluster_specific$positive_markers
    } else {
      all_clusters_pos <- list()
    }
    
    if(any(grepl("major_clusters", names(obj$markers)))) {
      all_clusters_neg <- obj$markers$major_clusters$cluster_specific$negative_markers
    } else {
      all_clusters_neg <- list()
    }
    
    if(any(grepl("major_clusters", names(obj$markers)))) {
      all_clusters_med <- obj$markers$major_clusters$cluster_specific$medium_markers
    } else {
      all_clusters_med <- list()
    }
    
    #____________
    
    ## Getting all sub_clusters ----
      if(any(grepl("sub_clusters", names(obj$markers)))) {
        all_sub_clusters_lst <- gsub(pattern = "-Subclusters", replacement = "-", x = names(obj$markers$sub_clusters))
        all_sub_clusters <- 
          unlist(lapply(seq_along(all_sub_clusters_lst), function(k) {
            i <- all_sub_clusters_lst[k]
            if(inherits(obj$markers$sub_clusters[[k]], "list")) {
              paste(i, names(obj$markers$sub_clusters[[k]]$positive_markers), sep = "")
            }
          }), use.names = FALSE)
        }

    all_sub_clusters <- unique(all_sub_clusters)
    
    # Extract positive markers
      if(any(grepl("sub_clusters", names(obj$markers)))) {
        all_sub_clusters_lst <- gsub(pattern = "-Subclusters", replacement = "-", x = names(obj$markers$sub_clusters))
        all_sub_clusters_pos <- 
          unlist(lapply(seq_along(all_sub_clusters_lst), function(k) {
            i <- all_sub_clusters_lst[k]
            if(inherits(obj$markers$sub_clusters[[k]], "list")) {
              tmp_sub_cluster_names <- paste(i, names(obj$markers$sub_clusters[[k]]$positive_markers), sep = "")
              tmp_pos <- obj$markers$sub_clusters[[k]]$positive_markers
              names(tmp_pos) <- tmp_sub_cluster_names
              tmp_pos
            } else {
              list()
            }
          }), recursive = FALSE)
        } else {
          all_sub_clusters_pos <- NULL
          }
    
    # Extract negative markers
    if(any(grepl("sub_clusters", names(obj$markers)))) {
      all_sub_clusters_lst <- gsub(pattern = "-Subclusters", replacement = "-", x = names(obj$markers$sub_clusters))
      all_sub_clusters_neg <- 
        unlist(lapply(seq_along(all_sub_clusters_lst), function(k) {
          i <- all_sub_clusters_lst[k]
          if(inherits(obj$markers$sub_clusters[[k]], "list")) {
            tmp_sub_cluster_names <- paste(i, names(obj$markers$sub_clusters[[k]]$negative_markers), sep = "")
            tmp_neg <- obj$markers$sub_clusters[[k]]$negative_markers
            names(tmp_neg) <- tmp_sub_cluster_names
            tmp_neg
          } else {
            list()
          }
        }), recursive = FALSE)
    } else {
      all_sub_clusters_neg <- NULL
    }
    
    # Extract medium markers
    if(any(grepl("sub_clusters", names(obj$markers)))) {
      all_sub_clusters_lst <- gsub(pattern = "-Subclusters", replacement = "-", x = names(obj$markers$sub_clusters))
      all_sub_clusters_med <- 
        unlist(lapply(seq_along(all_sub_clusters_lst), function(k) {
          i <- all_sub_clusters_lst[k]
          if(inherits(obj$markers$sub_clusters[[k]], "list")) {
            tmp_sub_cluster_names <- paste(i, names(obj$markers$sub_clusters[[k]]$medium_markers), sep = "")
            tmp_med <- obj$markers$sub_clusters[[k]]$medium_markers
            names(tmp_med) <- tmp_sub_cluster_names
            tmp_med
          } else {
            list()
          }
        }), recursive = FALSE)
    } else {
      all_sub_clusters_med <- NULL
    }
    
    #____________
    
    ## Getting all MarkoCell_clusters ----
      
      if(any(grepl("cluster_markers", names(obj)))) {
        MarkoCell_clusters <- obj$cluster_markers$positive_markers %>% names()
      } else {
        MarkoCell_clusters <- NULL
      }
    MarkoCell_clusters <- unique(MarkoCell_clusters)
    
    if(any(grepl("cluster_markers", names(obj)))) {
      MarkoCell_pos <- obj$cluster_markers$positive_markers
    } else {
      MarkoCell_pos <- list()
    }
    
    if(any(grepl("cluster_markers", names(obj)))) {
      MarkoCell_neg <- obj$cluster_markers$negative_markers
    } else {
      MarkoCell_neg <- list()
    }
    
    if(any(grepl("cluster_markers", names(obj)))) {
      MarkoCell_med <- obj$cluster_markers$medium_markers
    } else {
      MarkoCell_med <- list()
    }
    
    #_________
    
    ## Getting all MarkoCell_cell_subset ----
      
      if(any(grepl("cell_subset_markers", names(obj)))) {
        MarkoCell_cell_subset <- obj$cell_subset_markers$positive_markers %>% names()
        MarkoCell_cell_subset <- unique(MarkoCell_cell_subset)
      } else {
        MarkoCell_cell_subset <- NULL
      }

      if(any(grepl("cell_subset_markers", names(obj)))) {
        MarkoCell_cell_subset_pos <- obj$cell_subset_markers$positive_markers
      } else {
        MarkoCell_cell_subset_pos <- list()
      }
      
      if(any(grepl("cell_subset_markers", names(obj)))) {
        MarkoCell_cell_subset_neg <- obj$cell_subset_markers$negative_markers
      } else {
        MarkoCell_cell_subset_neg <- list()
      }
      
      if(any(grepl("cell_subset_markers", names(obj)))) {
        MarkoCell_cell_subset_med <- obj$cell_subset_markers$medium_markers
      } else {
        MarkoCell_cell_subset_med <- list()
      }
    
    #____________
    
    # Combine all positive, negative, and medium marker lists
    all_clusters_pos <- c(all_clusters_pos, all_sub_clusters_pos, MarkoCell_pos, MarkoCell_cell_subset_pos)
    all_clusters_neg <- c(all_clusters_neg, all_sub_clusters_neg, MarkoCell_neg, MarkoCell_cell_subset_neg)
    all_clusters_med <- c(all_clusters_med, all_sub_clusters_med, MarkoCell_med, MarkoCell_cell_subset_med)

    #____________
    
    ## Merging the names of clusters, sub-clusters, and cell-subsets in the objects ----
    combined_cell_set_names <- c(
      all_clusters,
      all_sub_clusters,
      MarkoCell_clusters,
      MarkoCell_cell_subset
    ) %>% unique()
    
    if(!is.null(desired_sets)) {
      if(!all(desired_sets %in% combined_cell_set_names)) {
        wrong_desired_sets <- desired_sets[!(desired_sets %in% combined_cell_set_names)]
        cli::cli_abort(paste0("All cluster, sub-cluster, and cell-subset names specified in the `desired_sets` argument must be present in the `obj`!\n\n",
                              "The following set name(s) are not present in the `obj`: ", paste0(wrong_desired_sets, collapse = ", "),
                              "\n\n", "See below for all clusters, sub-clusters, and cell-subsets present in the `obj`:\n\n",
                              paste0(combined_cell_set_names, collapse = ", ")))
      } else {
        combined_cell_set_names <- desired_sets
      }
    }
    
    #____________
    
    ## Preparing final markers tables based on objects ----
    
    all_clusters_pos <- all_clusters_pos[vapply(all_clusters_pos, function(i) is.data.frame(i) && nrow(i) > 0, logical(1))]
    all_clusters_neg <- all_clusters_neg[vapply(all_clusters_neg, function(i) is.data.frame(i) && nrow(i) > 0, logical(1))]
    all_clusters_med <- all_clusters_med[vapply(all_clusters_med, function(i) is.data.frame(i) && nrow(i) > 0, logical(1))]
    
    if(!is.null(desired_sets)) {
      all_clusters_pos <- all_clusters_pos[names(all_clusters_pos) %in% desired_sets]
      all_clusters_neg <- all_clusters_neg[names(all_clusters_neg) %in% desired_sets]
      all_clusters_med <- all_clusters_med[names(all_clusters_med) %in% desired_sets]
    }
    
    combined_panels_list <- list(
      pos_panels = all_clusters_pos,
      neg_panels = all_clusters_neg,
      med_panels = all_clusters_med
    )
    
    # -------------------------------------------------------------------------
    # Prepare marker tables for visualization
    # -------------------------------------------------------------------------
    
    .keep_valid_marker_tables <- function(x) {
      
      if(is.null(x) || length(x) == 0) {
        return(list())
      }
      
      x[vapply(
        x,
        function(i) is.data.frame(i) && nrow(i) > 0,
        logical(1)
      )]
    }
    
    .standardize_marker_table <- function(j, set_name, marker_class) {
      
      if(!is.data.frame(j) || nrow(j) == 0) {
        return(NULL)
      }
      
      if(thresh_mode == "n") {
        j <- j[seq_len(min(thresh, nrow(j))), , drop = FALSE]
      } else if(thresh_mode == "rank") {
        j <- j[j$Rank < thresh, , drop = FALSE]
      }
      
      if(!is.data.frame(j) || nrow(j) == 0) {
        return(NULL)
      }
      
      if("Purity" %in% colnames(j)) {
        j <- j[, c("Feature", "Purity", "Rank"), drop = FALSE]
      } else if("EWCSR" %in% colnames(j)) {
        j <- j[, c("Feature", "EWCSR", "Rank"), drop = FALSE]
        j$EWCSR <- abs(j$EWCSR)
        colnames(j)[colnames(j) == "EWCSR"] <- "Purity"
      } else {
        return(NULL)
      }
      
      j$Name <- set_name
      j$Class <- marker_class
      
      j
    }
    
    .prepare_marker_class <- function(panel_list, marker_class) {
      
      panel_list <- .keep_valid_marker_tables(panel_list)
      
      if(length(panel_list) == 0) {
        return(NULL)
      }
      
      out <- lapply(names(panel_list), function(set_name) {
        .standardize_marker_table(
          j = panel_list[[set_name]],
          set_name = set_name,
          marker_class = marker_class
        )
      })
      
      out <- Filter(Negate(is.null), out)
      
      if(length(out) == 0) {
        return(NULL)
      }
      
      dplyr::bind_rows(out)
    }
    
    marker_tables_to_plot <- list()
    
    if(show_pos_markers) {
      marker_tables_to_plot$Positive <- .prepare_marker_class(
        panel_list = combined_panels_list$pos_panels,
        marker_class = "Positive"
      )
    }
    
    if(show_neg_markers) {
      marker_tables_to_plot$Negative <- .prepare_marker_class(
        panel_list = combined_panels_list$neg_panels,
        marker_class = "Negative"
      )
    }
    
    if(show_med_markers) {
      marker_tables_to_plot$Medium <- .prepare_marker_class(
        panel_list = combined_panels_list$med_panels,
        marker_class = "Medium"
      )
    }
    
    marker_tables_to_plot <- Filter(Negate(is.null), marker_tables_to_plot)
    
    if(length(marker_tables_to_plot) == 0) {
      
      cli::cli_warn(
        c(
          "No marker table was available for visualization after filtering.",
          "i" = "Try increasing {.arg thresh}, changing {.arg thresh_mode}, lowering marker-filtering thresholds in the upstream marker-ranking function, or enabling additional marker classes with {.arg show_neg_markers} or {.arg show_med_markers}."
        )
      )
      
      return(
        ggplot2::ggplot() +
          ggplot2::annotate(
            geom = "text",
            x = 0,
            y = 0,
            label = paste(
              "No marker table was available for visualization.",
              "Try increasing 'thresh' or enabling additional marker classes.",
              sep = "\n"
            ),
            size = 4
          ) +
          ggplot2::theme_void() +
          ggplot2::labs(
            title = ifelse(is.null(title), "No markers available", title),
            subtitle = subtitle,
            tag = tag
          )
      )
    }
    
    combined_panels_purity_list <- dplyr::bind_rows(marker_tables_to_plot)
    
    combined_panels_purity_list$Class <- factor(
      combined_panels_purity_list$Class,
      levels = c("Negative", "Medium", "Positive")
    )
    
    combined_panels_purity_list <- combined_panels_purity_list %>%
      dplyr::group_by(.data$Class) %>%
      dplyr::arrange(.data$Rank, .by_group = TRUE) %>%
      dplyr::mutate(
        Feature_ordered = factor(.data$Feature, levels = unique(.data$Feature))
      ) %>%
      dplyr::ungroup()
  
  #____________________
  
  # Plotting the data ----
  
  combined_panels_plt <- 
    ggplot2::ggplot(combined_panels_purity_list, 
                    ggplot2::aes(Rank, Feature_ordered, fill = if (show_purity) Purity else Class)) + 
    ggplot2::geom_dotplot(dotsize = dotsize) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = length(unique(combined_panels_purity_list$Rank))))
  
  if(show_purity) {
    combined_panels_plt <- combined_panels_plt +
      ggplot2::scale_fill_gradient(
        low = color_low,
        high = color_high,
        breaks = scales::breaks_pretty(n = 5),
        labels = scales::label_number(accuracy = 0.01)
      )
  } else {
    if (!is.null(class_palette)) {
      if (is.character(class_palette)) {
        class_palette <- ggplot2::scale_fill_manual(values = class_palette)
      }
      combined_panels_plt <- combined_panels_plt + class_palette
    }
  }

  combined_panels_plt <- combined_panels_plt +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(color = panel_border_color, 
                                           fill = NA, 
                                           linewidth = panel_border_size),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(r = plot_margin_right),
      axis.title.y = ggplot2::element_text(size = axis_title_size),
      axis.text.y = ggplot2::element_text(size = axis_text_size),
      axis.text.x = ggplot2::element_text(size = axis_text_size),
      axis.title.x = ggplot2::element_text(size = axis_title_size,
                                           margin = ggplot2::margin(t = 10))
    ) +
    ggplot2::labs(x = xlab, y = ylab, 
                  fill = if (show_purity) "Purity" else "Class",
                  title = title, subtitle = subtitle, tag = tag)
  
  if(length(unique(combined_panels_purity_list$Class)) > 1 & length(unique(combined_panels_purity_list$Name)) > 1) {
    combined_panels_plt <- combined_panels_plt + ggplot2::facet_wrap(. ~ Name + Class, nrow = nrow_panels, scales = "free_y")
  } else if(length(unique(combined_panels_purity_list$Class)) > 1) {
    combined_panels_plt <- combined_panels_plt + ggplot2::facet_wrap(. ~ Class, nrow = nrow_panels, scales = "free_y")
  } else if(length(unique(combined_panels_purity_list$Name)) > 1) {
    combined_panels_plt <- combined_panels_plt + ggplot2::facet_wrap(. ~ Name, nrow = nrow_panels, scales = "free_y")
  }
  
  combined_panels_plt <- combined_panels_plt +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "grey95")
    )
  
  if(!show_legend) {
    combined_panels_plt <- combined_panels_plt +
      ggplot2::theme(legend.position = "none")
  }
    
  return(combined_panels_plt)

}
