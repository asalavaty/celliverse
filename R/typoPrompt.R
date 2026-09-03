
#' Generate an LLM-ready prompt for cell-type annotation
#'
#' @description
#' Generates a structured, copy-and-paste-ready prompt for annotation of
#' clusters, sub-clusters, or cell subsets using any chatbot or large language
#' model (LLM). Marker information is extracted directly from a
#' \code{ClustoCell} or \code{MarkoCell} object and combined with optional
#' biological context such as sample source, tissue, condition, and species.
#'
#' In interactive sessions, the returned \code{TypoPrompt} object opens as a
#' polished HTML interface in the RStudio Viewer, or in the default web browser
#' when the Viewer is unavailable. The interface provides formatted and raw views,
#' collapsible sections, light/dark appearance, one-click copying, TXT/HTML
#' downloads, and printing to PDF. The prompt can also be accessed directly as
#' plain text or saved programmatically as \code{.txt} or \code{.html}.
#'
#' @details
#' \code{typoPrompt()} provides a model- and provider-independent workflow for
#' LLM-assisted cell annotation. Unlike \code{\link{ceLLMarkup}}, it does not
#' connect to an LLM directly and therefore requires no API key, model
#' configuration, or local LLM server. Instead, it prepares the annotation task
#' for submission to the user's preferred chatbot or LLM.
#'
#' The generated prompt includes:
#' \itemize{
#'   \item positive markers and, optionally, negative markers for each set;
#'   \item available sample, tissue, condition, species, and feature context;
#'   \item instructions to consider the complete marker profile rather than
#'   individual markers;
#'   \item instructions to distinguish cell types, subtypes, and cellular
#'   states where supported by the marker evidence;
#'   \item a request for up to \code{top_k} ranked annotations with confidence
#'   scores and concise biological rationales; and
#'   \item a standardized Markdown-table response format followed by an overall
#'   interpretation.
#' }
#'
#' For \code{ClustoCell} objects containing both major clusters and
#' sub-clusters, the prompt additionally describes their hierarchy and asks the
#' LLM to first establish the identity of each parent cluster and then interpret
#' its sub-clusters as biologically meaningful subtypes or states within that
#' context.
#'
#' Printing a returned \code{TypoPrompt} object displays its formatted HTML
#' interface:
#'
#' \preformatted{
#' prompt <- typoPrompt(...)
#' prompt
#' }
#'
#' The underlying plain-text prompt remains directly accessible with
#' \code{cat(prompt)} or \code{as.character(prompt)}. It can also be saved
#' programmatically using \code{\link{saveTypoPrompt}} with \code{format = "txt"}
#' or \code{format = "html"}. Interactive HTML rendering and HTML export
#' require the optional \pkg{htmltools} package; when it is unavailable, printing
#' falls back to the plain-text prompt.
#'
#' @param object
#' An object of class \code{ClustoCell} or \code{MarkoCell} containing marker
#' results for the clusters, sub-clusters, and/or cell subsets to be annotated.
#'
#' @param desired_sets
#' Optional character vector specifying the names of clusters, sub-clusters,
#' and/or cell subsets to include in the prompt. Names must be present in
#' \code{object}. If \code{NULL}, all available sets are included.
#'
#' @param sample_source
#' Optional free-text description of the sample origin provided to the LLM
#' (e.g. \code{"human peripheral blood"} or
#' \code{"melanoma tumor biopsy"}). Providing this context can help improve
#' annotation specificity.
#'
#' @param feature_type
#' Character string describing the marker feature type (e.g. \code{"gene"} or
#' \code{"protein"}). Default is \code{"gene"}.
#'
#' @param species
#' Character string specifying the species (e.g. \code{"human"} or
#' \code{"mouse"}). Other species names may also be supplied. Default is
#' \code{"human"}.
#'
#' @param tissue
#' Optional character vector specifying one or more tissue contexts to provide
#' to the LLM. Available tissue types can be accessed using
#' \code{data("tissueCondition_types", package = "celliverse")}.
#'
#' @param condition
#' Optional character vector specifying one or more biological or disease
#' conditions to provide to the LLM. Available condition types can be accessed
#' using
#' \code{data("tissueCondition_types", package = "celliverse")}.
#'
#' @param use_neg_markers
#' Logical; whether to include negative markers in the generated prompt.
#' Negative markers provide exclusionary evidence that can help distinguish
#' closely related cell types, subtypes, or states. Default is \code{TRUE}.
#'
#' @param thresh_mode
#' Character string specifying how markers are selected from each marker table.
#' One of:
#' \itemize{
#'   \item \code{"n"}: retains strictly the first \code{thresh} markers in
#'   rank order, even when additional markers share the final selected rank.
#'   \item \code{"rank"}: retains all markers with rank less than or equal
#'   to \code{thresh}. Ties at the cutoff rank are therefore retained.
#' }
#' Default is \code{"n"}.
#'
#' @param thresh
#' Integer specifying the marker-selection threshold. With
#' \code{thresh_mode = "n"}, up to the first \code{thresh} markers are used
#' for each set. With \code{thresh_mode = "rank"}, all markers with rank
#' less than or equal to \code{thresh} are used. Default is \code{20}.
#'
#' @param top_k
#' Positive integer specifying the maximum number of ranked candidate
#' annotations that the generated prompt asks the LLM to return for each set.
#' Default is \code{3}.
#'
#' @param verbose
#' Logical; whether to display progress and status messages while preparing the
#' prompt. Default is \code{TRUE}.
#'
#' @return
#' An object of class \code{TypoPrompt}, inheriting from \code{character}, that
#' contains the complete LLM-ready annotation prompt.
#'
#' In an interactive session, printing the object opens a formatted HTML
#' interface in the RStudio Viewer when available, otherwise in the default web
#' browser. The interface provides formatted and raw views, collapsible sections,
#' light/dark appearance, one-click copying, TXT/HTML downloads, and printing to
#' PDF. In non-interactive sessions, the plain-text prompt is printed instead.
#' The raw prompt can also be obtained with \code{as.character()} or \code{cat()},
#' and exported programmatically with \code{\link{saveTypoPrompt}}.
#'
#' @seealso
#' \code{\link{typoClust}}, \code{\link{ceLLMarkup}},
#' \code{\link{typoClustVis}}, \code{\link{saveTypoPrompt}},
#' \code{\link{markoCell}}, \code{\link{markoClust}}, \code{\link{clustoCell}}
#'
#' @examples
#' utils::data("pbmc_small", package = "SeuratObject")
#'
#' cc <- clustoCell(
#'   data = pbmc_small,
#'   identify_subclusters = FALSE,
#'   num_threads = 1,
#'   verbose = FALSE
#' )
#'
#' desired_set <- utils::head(
#'   sort(unique(as.character(cc$clusters$major_clusters))),
#'   1
#' )
#'
#' prompt <- typoPrompt(
#'   object = cc,
#'   desired_sets = desired_set,
#'   sample_source = "human peripheral blood",
#'   tissue = "Blood",
#'   condition = "Healthy",
#'   species = "human",
#'   use_neg_markers = FALSE,
#'   thresh = 10,
#'   top_k = 3,
#'   verbose = FALSE
#' )
#'
#' class(prompt)
#' 
#' @export

typoPrompt <- function(
    object, # An object of class ClustoCell or MarkoCell
    desired_sets = NULL, # Optional. A character vector of the names of desired clusters, sub-clusters, and/or cell-subsets present in the specified `object`. If not specified, all clusters and cell-subsets in the object will be used.
    sample_source = NULL,
    feature_type = "gene",
    species = "human", # Character vector, e.g. 'human' or 'mouse'.
    tissue = NULL, # Optional tissue context(s) to include in the LLM prompt.
    condition = NULL, # Optional biological or disease condition context(s) to include in the LLM prompt.
    use_neg_markers = TRUE, # logical, whether to use negative markers for cell type annotation.
    thresh_mode = c("n", "rank"), # Specifies how to select top markers for each cluster or subcluster. Options are:
    ## "rank": selects all markers with ranks up to and including the threshold. If multiple markers share the cutoff rank, they are all included.
    ## "n": selects strictly the top n rows in rank order. Only the first n rows are kept, even if additional rows share the same rank as the n-th row.
    thresh = 20, # Positive integer controlling marker selection according to `thresh_mode`.
    top_k = 3, # Number of ranked candidate annotations requested from the LLM per set.
    verbose = TRUE # Logical, whether to show progress messages
) {
  
  # Setting the args
  
  thresh_mode <- match.arg(thresh_mode)
  
  if (length(thresh) != 1L || is.na(thresh) || !is.numeric(thresh) ||
      !is.finite(thresh) || thresh < 1 || thresh != as.integer(thresh)) {
    cli::cli_abort("The `thresh` argument must be a single positive integer!")
  }
  thresh <- as.integer(thresh)
  
  if (length(top_k) != 1L || is.na(top_k) || !is.numeric(top_k) ||
      !is.finite(top_k) || top_k < 1 || top_k != as.integer(top_k)) {
    cli::cli_abort("The `top_k` argument must be a single positive integer!")
  }
  top_k <- as.integer(top_k)
  
  if (length(use_neg_markers) != 1L || is.na(use_neg_markers) ||
      !is.logical(use_neg_markers)) {
    cli::cli_abort("The `use_neg_markers` argument must be TRUE or FALSE!")
  }
  
  if (length(verbose) != 1L || is.na(verbose) || !is.logical(verbose)) {
    cli::cli_abort("The `verbose` argument must be TRUE or FALSE!")
  }
  
  if (length(feature_type) != 1L || is.na(feature_type) ||
      !is.character(feature_type) || !nzchar(trimws(feature_type))) {
    cli::cli_abort("The `feature_type` argument must be a non-empty character string!")
  }
  
  if (length(species) != 1L || is.na(species) ||
      !is.character(species) || !nzchar(trimws(species))) {
    cli::cli_abort("The `species` argument must be a non-empty character string!")
  }
  
  #________________________________________
  
  # Defining the default logs for info messages
  log_message <- function(...) {
    if (verbose) cli::cli_alert_info(...)
  }
  
  #_____________
  
  log_progress_step <- function(...) {
    if (verbose) cli::cli_progress_step(..., spinner = TRUE)
  }
  
  #_____________
  
  log_progress_done <- function() {
    if(verbose) cli::cli_progress_done()
  }
  
  #_____________
  
  log_h1 <- function(...) {
    if(verbose) cli::cli_h1(...)
  }
  
  #_____________
  
  log_space <- function() {
    if(verbose) cli::cli_text("")
  }
  
  #________________________________________
  
  # Checking arguments
  
  object_missing <- missing(object)
  
  #________________________________________
  
  # Start of function ----
  if(verbose) {
    cli::cli_rule(left = cli::style_italic(cli::style_bold("Starting TypoPrompt!")), right = cli::col_silver(Sys.time()))
  }
  
  if(object_missing) {
    cli::cli_abort("The object cannot be left unspecified!")
  }
  
  log_h1("Preparing the Input Data")
  
  if(!is.null(object) & is.null(desired_sets)) {
    log_message("Since `object` is specified but `desired_sets` is not specified, all clusters and cell subsets of `object` will be used!")
  }
  
  log_progress_step("Inspecting the input data")
  
  if(!inherits(object, "ClustoCell") & !inherits(object, "MarkoCell")) {
    cli::cli_abort("The `object` argument should be an object of class ClustoCell or MarkoCell!")
  }
  
  
  if(inherits(object, c("ClustoCell"))) {
    obj_type <- "ClustoCell"
  } else {
    obj_type <- "MarkoCell"
  }
  
  
  if(inherits(object, c("ClustoCell", "MarkoCell"))) {
    object <- list(object)
  }
  
  if(!is.null(desired_sets)) {
    if(!is.character(desired_sets) || !length(desired_sets) ||
       anyNA(desired_sets) || any(!nzchar(desired_sets))) {
      cli::cli_abort("The `desired_sets` argument must be a non-empty character vector of valid set names!")
    }
    desired_sets <- unique(desired_sets)
  }
  
  log_progress_done()
  
  log_progress_step("Preparing the clusters, sub-clusters, and cell-subsets.")
  
  # Setting the names and marker panels for all available cell sets ----
  all_clusters_pos <- NULL
  all_clusters_neg <- NULL
  combined_cell_set_names <- NULL
  combined_panels_list <- list(pos_panels = NULL,
                               neg_panels = NULL)
  
  ## Getting the names of all clusters, sub-clusters and cell-subsets ----
  
  ### Getting all major_clusters ----
  # Extract names separately
  all_clusters <- unlist(lapply(object, function(i) {
    if(any(grepl("major_clusters", names(i$markers)))) {
      names(i$markers$major_clusters$cluster_specific$positive_markers)
    }
  }), use.names = FALSE)
  all_clusters <- unique(all_clusters)
  
  # Extract pos and neg separately
  all_clusters_pos <- unlist(lapply(object, function(i) {
    if(any(grepl("major_clusters", names(i$markers)))) {
      tmp_pos <- i$markers$major_clusters$cluster_specific$positive_markers
      names(tmp_pos) <- names(tmp_pos)
      tmp_pos
    } else {
      list()
    }
  }), recursive = FALSE)
  
  all_clusters_neg <- unlist(lapply(object, function(i) {
    if(any(grepl("major_clusters", names(i$markers)))) {
      tmp_neg <- i$markers$major_clusters$cluster_specific$negative_markers
      names(tmp_neg) <- names(tmp_neg)
      tmp_neg
    } else {
      list()
    }
  }), recursive = FALSE)    
  #____________
  
  ## Getting all sub_clusters ----
  all_sub_clusters <- unlist(lapply(object, function(j) {
    if(any(grepl("sub_clusters", names(j$markers)))) {
      all_sub_clusters_lst <- gsub(pattern = "-Subclusters", replacement = "-", x = names(j$markers$sub_clusters))
      unlist(lapply(seq_along(all_sub_clusters_lst), function(k) {
        i <- all_sub_clusters_lst[k]
        if(inherits(j$markers$sub_clusters[[k]], "list")) {
          paste(i, names(j$markers$sub_clusters[[k]]$positive_markers), sep = "")
        }
      }), use.names = FALSE)
    }
  }), use.names = FALSE)
  all_sub_clusters <- unique(all_sub_clusters)
  
  # Extract positive markers
  all_sub_clusters_pos <- unlist(lapply(object, function(j) {
    if(any(grepl("sub_clusters", names(j$markers)))) {
      all_sub_clusters_lst <- gsub(pattern = "-Subclusters", replacement = "-", x = names(j$markers$sub_clusters))
      unlist(lapply(seq_along(all_sub_clusters_lst), function(k) {
        i <- all_sub_clusters_lst[k]
        if(inherits(j$markers$sub_clusters[[k]], "list")) {
          tmp_sub_cluster_names <- paste(i, names(j$markers$sub_clusters[[k]]$positive_markers), sep = "")
          tmp_pos <- j$markers$sub_clusters[[k]]$positive_markers
          names(tmp_pos) <- tmp_sub_cluster_names
          tmp_pos
        } else {
          list()
        }
      }), recursive = FALSE)
    } else {
      list()
    }
  }), recursive = FALSE)
  
  # Extract negative markers
  all_sub_clusters_neg <- unlist(lapply(object, function(j) {
    if(any(grepl("sub_clusters", names(j$markers)))) {
      all_sub_clusters_lst <- gsub(pattern = "-Subclusters", replacement = "-", x = names(j$markers$sub_clusters))
      unlist(lapply(seq_along(all_sub_clusters_lst), function(k) {
        i <- all_sub_clusters_lst[k]
        if(inherits(j$markers$sub_clusters[[k]], "list")) {
          tmp_sub_cluster_names <- paste(i, names(j$markers$sub_clusters[[k]]$negative_markers), sep = "")
          tmp_neg <- j$markers$sub_clusters[[k]]$negative_markers
          names(tmp_neg) <- tmp_sub_cluster_names
          tmp_neg
        } else {
          list()
        }
      }), recursive = FALSE)
    } else {
      list()
    }
  }), recursive = FALSE)
  
  #____________
  
  ## Getting all MarkoCell_clusters ----
  
  MarkoCell_clusters <- unlist(lapply(object, function(i) {
    if(any(grepl("cluster_markers", names(i)))) {
      names(i$cluster_markers$positive_markers)
    }
  }), use.names = FALSE)
  MarkoCell_clusters <- unique(MarkoCell_clusters)
  
  MarkoCell_pos <- unlist(lapply(object, function(i) {
    if(any(grepl("cluster_markers", names(i)))) {
      tmp_pos <- i$cluster_markers$positive_markers
      names(tmp_pos) <- names(tmp_pos)
      tmp_pos
    } else {
      list()
    }
  }), recursive = FALSE)
  
  MarkoCell_neg <- unlist(lapply(object, function(i) {
    if(any(grepl("cluster_markers", names(i)))) {
      tmp_neg <- i$cluster_markers$negative_markers
      names(tmp_neg) <- names(tmp_neg)
      tmp_neg
    } else {
      list()
    }
  }), recursive = FALSE)
  
  #_________
  
  ## Getting all MarkoCell_cell_subset ----
  
  MarkoCell_cell_subset <- unlist(lapply(object, function(i) {
    if(any(grepl("cell_subset_markers", names(i)))) {
      names(i$cell_subset_markers$positive_markers)
    }
  }), use.names = FALSE)
  MarkoCell_cell_subset <- unique(MarkoCell_cell_subset)
  
  MarkoCell_cell_subset_pos <- unlist(lapply(object, function(i) {
    if(any(grepl("cell_subset_markers", names(i)))) {
      tmp_pos <- i$cell_subset_markers$positive_markers
      names(tmp_pos) <- names(tmp_pos)
      tmp_pos
    } else {
      list()
    }
  }), recursive = FALSE)
  
  MarkoCell_cell_subset_neg <- unlist(lapply(object, function(i) {
    if(any(grepl("cell_subset_markers", names(i)))) {
      tmp_neg <- i$cell_subset_markers$negative_markers
      names(tmp_neg) <- names(tmp_neg)
      tmp_neg
    } else {
      list()
    }
  }), recursive = FALSE)
  
  #____________
  
  # Combine all positive and negative marker lists
  all_clusters_pos <- c(all_clusters_pos, all_sub_clusters_pos, MarkoCell_pos, MarkoCell_cell_subset_pos)
  all_clusters_neg <- c(all_clusters_neg, all_sub_clusters_neg, MarkoCell_neg, MarkoCell_cell_subset_neg)
  
  #____________
  
  ## Merging the names of clusters, sub-clusters, and cell-subsets in the object ----
  combined_cell_set_names <- unique(c(
    all_clusters,
    all_sub_clusters,
    MarkoCell_clusters,
    MarkoCell_cell_subset
  ))
  
  if(!is.null(desired_sets)) {
    if(!all(desired_sets %in% combined_cell_set_names)) {
      wrong_desired_sets <- desired_sets[!(desired_sets %in% combined_cell_set_names)]
      cli::cli_abort(paste0("All cluster, sub-cluster, and cell-subset names specified in the `desired_sets` argument must be present in the `object`!\n\n",
                            "The following set name(s) are not present in the `object`: ", paste0(wrong_desired_sets, collapse = ", "),
                            "\n\n", "See below for all clusters, sub-clusters, and cell-subsets present in the `object`:\n\n",
                            paste0(combined_cell_set_names, collapse = ", ")))
    } else {
      combined_cell_set_names <- desired_sets
    }
  }
  
  #____________
  
  ## Preparing final markers tables based on object ----
  
  all_clusters_pos <- all_clusters_pos[vapply(all_clusters_pos, function(i) is.data.frame(i) && nrow(i) > 0, logical(1))]
  all_clusters_neg <- all_clusters_neg[vapply(all_clusters_neg, function(i) is.data.frame(i) && nrow(i) > 0, logical(1))]
  
  if(!is.null(desired_sets)) {
    all_clusters_pos <- all_clusters_pos[names(all_clusters_pos) %in% desired_sets]
    all_clusters_neg <- all_clusters_neg[names(all_clusters_neg) %in% desired_sets]
  }
  
  combined_panels_list <- list(pos_panels = all_clusters_pos, neg_panels = all_clusters_neg)
  
  # Select marker features according to the requested threshold mode.
  combined_panels_list <- lapply(combined_panels_list, function(i) {
    lapply(i, function(j) {
      if (thresh_mode == "n") {
        j$Feature[seq_len(min(thresh, nrow(j)))]  # Base extraction
      } else if (thresh_mode == "rank") {
        j$Feature[j$Rank <= thresh]
      }
    })
  })
  
  #____________
  
  # Finalizing cluster/subset names and markers ----
  
  if(!use_neg_markers) {
    combined_panels_list$neg_panels <- NULL
  }
  
  set_names <- unique(c(names(combined_panels_list$pos_panels), names(combined_panels_list$neg_panels)))
  
  markers <- list(clusters = set_names, 
                  pos = combined_panels_list$pos_panels, 
                  neg = combined_panels_list$neg_panels,
                  level = stats::setNames(rep("set", length(set_names)), set_names),
                  degraded = FALSE)
  
  
  #____________________
  
  log_progress_step("Preparing the prompt.")
  
  # Preparing prompts
  
  msgs <- .cv_typoPrompt(obj_type = obj_type, 
                         markers = markers, 
                         sample_source = sample_source,
                         feature_type = feature_type, tissue = tissue,
                         condition = condition, species = species,
                         use_neg_markers = use_neg_markers,
                         top_k = top_k)
  
  if(verbose) {
    log_space()
    cli::cli_rule(left = cli::col_green("SUCCESS"), right = cli::col_silver(Sys.time()))
    cli::cli_alert_success(cli::style_italic(cli::style_bold("typoPrompt finished successfully!")))
  }
  
  return(msgs)
  
}

# ---- Internal helpers ---------------------------------------------------------

# ___________________________________________________________________
# CelliVerse TypoPrompt
# ___________________________________________________________________

.cv_typoPrompt <- function(
    obj_type,
    markers,
    sample_source = NULL,
    feature_type = "gene",
    tissue = NULL,
    condition = NULL,
    species = "human",
    use_neg_markers = TRUE,
    top_k = 3
) {
  
  # ___________________________________________________________________
  # Validate inputs
  # ___________________________________________________________________
  
  if (!is.list(markers)) {
    stop("'markers' must be a list.")
  }
  
  if (is.null(markers$clusters) || !length(markers$clusters)) {
    stop("'markers$clusters' must contain at least one marker set.")
  }
  
  if (is.null(markers$pos)) {
    stop("'markers$pos' is missing.")
  }
  
  if (is.null(markers$neg)) {
    markers$neg <- list()
  }
  
  if (length(top_k) != 1L || is.na(top_k) || !is.numeric(top_k) ||
      !is.finite(top_k) || top_k < 1 || top_k != as.integer(top_k)) {
    stop("'top_k' must be a single positive integer.")
  }
  
  top_k <- as.integer(top_k)
  
  # ___________________________________________________________________
  # Helpers
  # ___________________________________________________________________
  
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0) y else x
  }
  
  has_text <- function(x) {
    !is.null(x) &&
      length(x) > 0 &&
      !is.na(x[1]) &&
      nzchar(as.character(x[1]))
  }
  
  # ___________________________________________________________________
  # Sample context
  # ___________________________________________________________________
  
  context <- c(
    if (has_text(sample_source)) {
      paste0("Sample source: ", sample_source)
    },
    if (has_text(tissue)) {
      paste0("Tissue: ", tissue)
    },
    if (has_text(condition)) {
      paste0("Condition: ", condition)
    },
    paste0("Species: ", species %||% "human")
  )
  
  context_text <- paste(context, collapse = "\n\n")
  
  # ___________________________________________________________________
  # Marker sets
  # ___________________________________________________________________
  
  marker_blocks <- vapply(
    markers$clusters,
    function(cl) {
      
      pos <- markers$pos[[cl]] %||% character(0)
      neg <- markers$neg[[cl]] %||% character(0)
      
      pos <- as.character(pos)
      neg <- as.character(neg)
      
      pos <- pos[!is.na(pos) & nzchar(pos)]
      neg <- neg[!is.na(neg) & nzchar(neg)]
      
      positive_text <- if (length(pos)) {
        paste(pos, collapse = ", ")
      } else {
        "(none)"
      }
      
      block <- paste0(
        "### Set ", cl, "\n",
        "**Positive ", feature_type, "s:** ", positive_text
      )
      
      if (length(neg)) {
        block <- paste0(
          block,
          "\n\n",
          "**Negative ", feature_type, "s:** ",
          paste(neg, collapse = ", ")
        )
      }
      
      block
    },
    character(1)
  )
  
  marker_text <- paste(marker_blocks, collapse = "\n\n")
  
  # ___________________________________________________________________
  # Identify ClustoCell hierarchy
  # ___________________________________________________________________
  
  subcluster_names <- markers$clusters[
    grepl("-Sub[0-9]+$", markers$clusters)
  ]
  
  parent_names <- unique(sub("-Sub[0-9]+$", "", subcluster_names))
  
  major_clusters <- markers$clusters[
    !grepl("-Sub[0-9]+$", markers$clusters) &
      markers$clusters %in% parent_names
  ]
  
  has_subclusters <- identical(obj_type, "ClustoCell") &&
    length(subcluster_names) > 0L &&
    length(major_clusters) > 0L
  
  if (has_subclusters) {
    
    hierarchy <- vapply(
      major_clusters,
      function(cl) {
        
        subs <- markers$clusters[
          grepl(
            paste0("^", cl, "-Sub[0-9]+$"),
            markers$clusters
          )
        ]
        
        if (!length(subs)) {
          return(paste0(cl, ": no sub-clusters"))
        }
        
        paste0(
          cl,
          ": ",
          paste(subs, collapse = ", ")
        )
      },
      character(1)
    )
    
    hierarchy_text <- paste(hierarchy, collapse = "\n\n")
    
  } else {
    
    hierarchy_text <- NULL
  }
  
  # ___________________________________________________________________
  # Core annotation instructions
  # ___________________________________________________________________
  
  instructions <- paste(
    "You are an expert single-cell and spatial-omics cell-type annotator.",
    "",
    "Your task is to identify the most likely cell type, subtype, or cellular",
    "state represented by each marker set below.",
    "",
    "Base your annotations on established marker biology and the overall marker",
    "profile rather than relying on a single marker.",
    "",
    ifelse(use_neg_markers,
           paste0(
             "Positive ", feature_type, "s indicate features supporting a cell identity,",
             " while negative ", feature_type, "s provide exclusionary evidence and",
             " should be considered when distinguishing between closely related cell",
             "types or cellular states."
           ),
           paste0(
             "Positive ", feature_type, "s indicate features supporting a cell identity."
           )
    ),
    "",
    "Use canonical and biologically specific cell-type names whenever the",
    "marker evidence supports them. Examples include:",
    "- CD8+ cytotoxic T cell",
    "- Naive B cell",
    "- Classical monocyte",
    "- Regulatory T cell",
    "- Conventional dendritic cell",
    "",
    "When the marker profile supports a more specific subtype or cellular state",
    "with reasonable confidence, report the more specific annotation rather than",
    "stopping at a broad parent cell type.",
    "",
    sprintf(
      "For each marker set, provide up to %d ranked candidate annotations.",
      top_k
    ),
    "Rank 1 should represent the most likely annotation.",
    "",
    "For every candidate annotation, provide:",
    "1. The proposed cell type, subtype, or cellular state.",
    "2. A confidence score between 0 and 1.",
    "3. A concise biological rationale citing the most informative positive markers and, where relevant, important negative markers.",
    "",
    "Do not force a specific subtype when the marker evidence does not support",
    "it. When the evidence is ambiguous, prefer the most defensible broader",
    "annotation and explain the uncertainty.",
    "",
    "Consider biologically related markers together. Distinguish closely related",
    "populations using combinations of lineage markers, activation markers,",
    "cytotoxic markers, differentiation markers, and exclusionary markers rather",
    "than relying on any single feature.",
    sep = "\n"
  )
  
  # ___________________________________________________________________
  # ClustoCell hierarchical annotation instructions
  # ___________________________________________________________________
  
  if (has_subclusters) {
    
    instructions <- paste(
      instructions,
      "",
      "### ClustoCell hierarchical annotation",
      "",
      "This marker collection contains both ClustoCell major clusters and sub-clusters.",
      "Sub-clusters are identified by names such as C1-Sub1,",
      "C1-Sub2, C2-Sub1, and so on.",
      "",
      "The sub-clusters belong to their corresponding major cluster. Use the",
      "following hierarchical annotation strategy:",
      "",
      "1. First determine the most likely broad cell type of each major cluster such as C1, C2, C3, and so on.",
      "2. Then interpret the corresponding sub-clusters within the biological context of their parent cluster.",
      "3. Sub-clusters should generally represent more specific subtypes, differentiation states, activation states, functional states, or other biologically meaningful subdivisions of their parent population.",
      "4. Do not independently assign a sub-cluster to an unrelated major lineage when its parent cluster provides strong evidence for a particular lineage, unless the marker evidence clearly indicates contamination, a doublet, or another biological explanation.",
      "5. Use the sub-cluster-specific markers to determine what distinguishes each sub-cluster from the other sub-clusters within the same parent.",
      "",
      "For example, if C1 is identified as a T-cell population and C1-Sub1 and",
      "C1-Sub2 are its sub-clusters, first establish C1 as the broad T-cell",
      "population. Then use the markers specific to C1-Sub1 and C1-Sub2 to",
      "determine whether they represent distinct T-cell subtypes or states, such",
      "as naive, memory, activated, regulatory, exhausted, or cytotoxic states.",
      "",
      "Apply this hierarchical reasoning consistently across all major clusters",
      "and their corresponding sub-clusters.",
      "",
      "### Cluster hierarchy",
      "",
      hierarchy_text,
      sep = "\n"
    )
  }
  
  # ___________________________________________________________________
  # Response instructions
  # ___________________________________________________________________
  
  output_instructions <- paste(
    "### Response format",
    "",
    "Return the results as a clear, human-readable Markdown table.",
    "",
    "| Set | Rank | Cell type / subtype | Confidence | Biological rationale |",
    "|---|---:|---|---:|---|",
    "",
    "Include one row for every candidate annotation.",
    "",
    sprintf(
      "There are %d marker sets in total. Annotate every set and provide no",
      length(markers$clusters)
    ),
    sprintf(
      "more than %d candidate annotations for any individual set.",
      top_k
    ),
    "",
    "Keep the biological rationale concise but informative. Cite the most",
    "discriminative markers rather than listing every marker provided.",
    "",
    if (has_subclusters) {
      paste(
        "For ClustoCell sub-clusters, make the relationship between each sub-cluster",
        "and its parent cluster clear in the annotation."
      )
    } else {
      ""
    },
    "",
    "After the table, provide a short section titled:",
    "",
    "### Overall interpretation",
    "",
    if (has_subclusters) {
      paste(
        "Summarise the major cell populations identified, important subtype or",
        "state distinctions, relationships between major clusters and their",
        "sub-clusters, and any annotations that remain uncertain or ambiguous."
      )
    } else {
      paste(
        "Summarise the major cell populations identified, important subtype or",
        "state distinctions, and any annotations that remain uncertain or ambiguous."
      )
    },
    "",
    "Do not return JSON, XML, code, or any machine-readable object. The response",
    "should be formatted for direct reading by a scientist and easy copy-and-paste",
    "into a report, notebook, or downstream discussion.",
    sep = "\n"
  )
  
  # ___________________________________________________________________
  # Construct final prompt
  # ___________________________________________________________________
  
  prompt <- paste(
    "# Cell-Type Annotation Prompt",
    "",
    "## Sample context",
    "",
    context_text,
    "",
    "## Task",
    "",
    instructions,
    "",
    "## Marker sets",
    "",
    marker_text,
    "",
    output_instructions,
    sep = "\n"
  )
  
  # ___________________________________________________________________
  # Add TypoPrompt class
  # ___________________________________________________________________
  
  class(prompt) <- c("TypoPrompt", "character")
  
  prompt
}


# ___________________________________________________________________
# HTML renderer
# ___________________________________________________________________

.cv_typoPrompt_html <- function(x) {
  
  if (!inherits(x, "TypoPrompt")) {
    stop("'x' must be a TypoPrompt object.")
  }
  
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop(
      "The 'htmltools' package is required to render a TypoPrompt.",
      call. = FALSE
    )
  }
  
  raw_prompt <- as.character(x)
  lines <- strsplit(raw_prompt, "\n", fixed = TRUE)[[1]]
  
  escape_html <- function(z) {
    as.character(htmltools::htmlEscape(z))
  }
  
  # Convert raw bytes to base64 without adding another package dependency.
  raw_to_base64 <- function(x) {
    if (!length(x)) {
      return("")
    }
    
    alphabet <- c(
      LETTERS,
      letters,
      as.character(0:9),
      "+",
      "/"
    )
    
    padding <- (3L - (length(x) %% 3L)) %% 3L
    
    if (padding > 0L) {
      x <- c(x, as.raw(rep(0L, padding)))
    }
    
    bytes <- matrix(
      as.integer(x),
      ncol = 3L,
      byrow = TRUE
    )
    
    encoded_index <- cbind(
      bitwShiftR(bytes[, 1L], 2L),
      bitwOr(
        bitwShiftL(bitwAnd(bytes[, 1L], 3L), 4L),
        bitwShiftR(bytes[, 2L], 4L)
      ),
      bitwOr(
        bitwShiftL(bitwAnd(bytes[, 2L], 15L), 2L),
        bitwShiftR(bytes[, 3L], 6L)
      ),
      bitwAnd(bytes[, 3L], 63L)
    )
    
    encoded <- paste0(
      alphabet[as.vector(t(encoded_index)) + 1L],
      collapse = ""
    )
    
    if (padding > 0L) {
      n <- nchar(encoded)
      substr(encoded, n - padding + 1L, n) <- strrep("=", padding)
    }
    
    encoded
  }
  
  # Locate the CelliVerse package logo and embed it directly into the HTML.
  # During development the logo lives in man/figures/Symbol.png. Installed
  # packages may expose documentation figures under help/figures instead.
  get_logo_data_uri <- function() {
    package_root <- tryCatch(
      system.file(package = "celliverse"),
      error = function(e) ""
    )
    
    candidates <- unique(c(
      system.file(
        "help", "figures", "Symbol.png",
        package = "celliverse"
      ),
      system.file(
        "man", "figures", "Symbol.png",
        package = "celliverse"
      ),
      if (nzchar(package_root)) {
        file.path(package_root, "help", "figures", "Symbol.png")
      } else {
        ""
      },
      if (nzchar(package_root)) {
        file.path(package_root, "man", "figures", "Symbol.png")
      } else {
        ""
      },
      file.path("man", "figures", "Symbol.png")
    ))
    
    candidates <- candidates[
      nzchar(candidates) &
        file.exists(candidates)
    ]
    
    if (!length(candidates)) {
      return(NULL)
    }
    
    logo_path <- candidates[[1L]]
    logo_size <- file.info(logo_path)$size
    
    if (is.na(logo_size) || logo_size <= 0) {
      return(NULL)
    }
    
    logo_raw <- readBin(
      con = logo_path,
      what = "raw",
      n = logo_size
    )
    
    paste0(
      "data:image/png;base64,",
      raw_to_base64(logo_raw)
    )
  }
  
  # Render simple Markdown-style bold text after HTML escaping.
  # Escaping is performed first so arbitrary prompt content cannot inject HTML.
  format_inline <- function(z) {
    z <- escape_html(z)
    gsub(
      "\\*\\*(.+?)\\*\\*",
      "<strong>\\1</strong>",
      z,
      perl = TRUE
    )
  }
  
  title <- "Cell-Type Annotation Prompt"
  title_idx <- which(grepl("^# ", lines))[1]
  if (!is.na(title_idx)) {
    title <- sub("^# ", "", lines[title_idx])
  }
  
  set_count <- sum(grepl("^### Set ", lines))
  char_count <- nchar(raw_prompt, type = "chars")
  trimmed <- trimws(raw_prompt)
  word_count <- if (nzchar(trimmed)) {
    length(strsplit(trimmed, "\\s+")[[1]])
  } else {
    0L
  }
  
  logo_uri <- get_logo_data_uri()
  
  logo_html <- if (!is.null(logo_uri)) {
    paste0(
      '<img class="brand-logo" src="',
      logo_uri,
      '" alt="CelliVerse logo">'
    )
  } else {
    ""
  }
  
  # _____________________________
  # Convert the generated Markdown-like prompt into semantic HTML.
  # Each level-2 section is rendered as an independent collapsible card.
  # _____________________________
  
  html_lines <- character(0)
  section_open <- FALSE
  i <- 1L
  
  close_section <- function() {
    if (section_open) "</div></details>" else character(0)
  }
  
  while (i <= length(lines)) {
    
    line <- lines[i]
    
    # Main title is represented by the hero and is not repeated below.
    if (grepl("^# ", line)) {
      i <- i + 1L
      next
    }
    
    # Level-2 heading -> new collapsible section.
    if (grepl("^## ", line)) {
      
      if (section_open) {
        html_lines <- c(html_lines, "</div></details>")
      }
      
      heading <- sub("^## ", "", line)
      html_lines <- c(
        html_lines,
        paste0(
          '<details class="prompt-section" open>',
          '<summary>',
          '<span class="section-title">', escape_html(heading), '</span>',
          '<span class="chevron" aria-hidden="true"></span>',
          '</summary>',
          '<div class="section-body">'
        )
      )
      
      section_open <- TRUE
      i <- i + 1L
      next
    }
    
    # Level-3 heading.
    if (grepl("^### ", line)) {
      heading <- sub("^### ", "", line)
      html_lines <- c(
        html_lines,
        paste0('<h3>', escape_html(heading), '</h3>')
      )
      i <- i + 1L
      next
    }
    
    # Markdown table.
    if (startsWith(line, "|")) {
      
      table_lines <- character(0)
      
      while (i <= length(lines) && startsWith(lines[i], "|")) {
        table_lines <- c(table_lines, lines[i])
        i <- i + 1L
      }
      
      if (length(table_lines) >= 2L) {
        
        table_lines <- table_lines[
          !grepl("^\\|[-:| ]+\\|$", table_lines)
        ]
        
        rows <- lapply(table_lines, function(row) {
          cells <- strsplit(
            sub("^\\||\\|$", "", row),
            "\\|",
            fixed = FALSE
          )[[1]]
          trimws(cells)
        })
        
        header <- rows[[1]]
        body <- rows[-1]
        
        header_html <- paste0(
          "<tr>",
          paste0("<th>", escape_html(header), "</th>", collapse = ""),
          "</tr>"
        )
        
        body_html <- if (length(body)) {
          vapply(
            body,
            function(row) {
              paste0(
                "<tr>",
                paste0("<td>", escape_html(row), "</td>", collapse = ""),
                "</tr>"
              )
            },
            character(1)
          )
        } else {
          character(0)
        }
        
        html_lines <- c(
          html_lines,
          paste0(
            '<div class="table-wrapper"><table>',
            '<thead>', header_html, '</thead>',
            '<tbody>', paste(body_html, collapse = ""), '</tbody>',
            '</table></div>'
          )
        )
      }
      
      next
    }
    
    # Bullet list.
    if (grepl("^- ", line)) {
      
      items <- character(0)
      while (i <= length(lines) && grepl("^- ", lines[i])) {
        items <- c(items, sub("^- ", "", lines[i]))
        i <- i + 1L
      }
      
      html_lines <- c(
        html_lines,
        paste0(
          '<ul>',
          paste0('<li>', escape_html(items), '</li>', collapse = ""),
          '</ul>'
        )
      )
      next
    }
    
    # Numbered list.
    if (grepl("^[0-9]+\\. ", line)) {
      
      items <- character(0)
      while (i <= length(lines) && grepl("^[0-9]+\\. ", lines[i])) {
        items <- c(items, sub("^[0-9]+\\. ", "", lines[i]))
        i <- i + 1L
      }
      
      html_lines <- c(
        html_lines,
        paste0(
          '<ol>',
          paste0('<li>', escape_html(items), '</li>', collapse = ""),
          '</ol>'
        )
      )
      next
    }
    
    # Blank line.
    if (!nzchar(line)) {
      i <- i + 1L
      next
    }
    
    # Join adjacent plain-text lines into a paragraph. This improves readability
    # because many prompt instructions are deliberately line-wrapped in R.
    paragraph <- line
    i <- i + 1L
    
    while (
      i <= length(lines) &&
      nzchar(lines[i]) &&
      !grepl("^#{1,3} ", lines[i]) &&
      !startsWith(lines[i], "|") &&
      !grepl("^- ", lines[i]) &&
      !grepl("^[0-9]+\\. ", lines[i])
    ) {
      paragraph <- paste(paragraph, lines[i])
      i <- i + 1L
    }
    
    html_lines <- c(
      html_lines,
      paste0('<p>', format_inline(paragraph), '</p>')
    )
  }
  
  if (section_open) {
    html_lines <- c(html_lines, "</div></details>")
  }
  
  formatted_prompt <- paste(html_lines, collapse = "\n")
  
  # _____________________________________
  # Complete self-contained HTML document.
  # _____________________________________
  
  html <- paste0(
    '<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>', escape_html(title), '</title>

<style>
:root {
  color-scheme: light;
  --bg: #f5f7fb;
  --surface: rgba(255,255,255,0.94);
  --surface-strong: #ffffff;
  --surface-soft: #f8fafc;
  --text: #172033;
  --muted: #667085;
  --border: #e3e8ef;
  --border-strong: #d3dae5;
  --primary: #4f46e5;
  --primary-2: #7c3aed;
  --primary-soft: #eef2ff;
  --success: #087a5b;
  --shadow-sm: 0 2px 8px rgba(23, 32, 51, 0.05);
  --shadow-md: 0 14px 34px rgba(23, 32, 51, 0.10);
  --hero-a: #18243a;
  --hero-b: #344b75;
  --hero-c: #6555d8;
}

html[data-theme="dark"] {
  color-scheme: dark;
  --bg: #0d1118;
  --surface: rgba(20,27,39,0.96);
  --surface-strong: #151c28;
  --surface-soft: #101722;
  --text: #eef2f7;
  --muted: #a4afbf;
  --border: #293345;
  --border-strong: #38445a;
  --primary: #8b8cf8;
  --primary-2: #aa7af5;
  --primary-soft: #222745;
  --success: #62d9b1;
  --shadow-sm: 0 2px 8px rgba(0,0,0,0.18);
  --shadow-md: 0 14px 34px rgba(0,0,0,0.30);
  --hero-a: #131c2c;
  --hero-b: #243758;
  --hero-c: #5545b6;
}

* { box-sizing: border-box; }

html { scroll-behavior: smooth; }

body {
  margin: 0;
  background:
    radial-gradient(circle at 10% 0%, rgba(99,102,241,0.08), transparent 28rem),
    var(--bg);
  color: var(--text);
  font-family: Inter, ui-sans-serif, -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;
  line-height: 1.68;
  -webkit-font-smoothing: antialiased;
}

button { font: inherit; }

.app-shell {
  width: min(1180px, calc(100% - 32px));
  margin: 0 auto;
  padding: 30px 0 54px;
}

.hero {
  position: relative;
  overflow: hidden;
  color: #fff;
  background: linear-gradient(135deg, var(--hero-a) 0%, var(--hero-b) 54%, var(--hero-c) 100%);
  border-radius: 24px;
  padding: 34px 36px 30px;
  box-shadow: var(--shadow-md);
}

.hero::after {
  content: "";
  position: absolute;
  width: 310px;
  height: 310px;
  right: -110px;
  top: -150px;
  border-radius: 50%;
  background: rgba(255,255,255,0.10);
  filter: blur(2px);
}

.hero-top,
.hero-main,
.action-row,
.meta-row { position: relative; z-index: 1; }

.hero-top {
  display: flex;
  justify-content: space-between;
  gap: 16px;
  align-items: center;
  margin-bottom: 18px;
}

.brand-lockup {
  display: inline-flex;
  align-items: center;
  gap: 11px;
  min-width: 0;
}

.brand-logo {
  width: 42px;
  height: 42px;
  flex: 0 0 42px;
  object-fit: contain;
  display: block;
  filter: drop-shadow(0 3px 8px rgba(0,0,0,0.18));
}

.brand-copy {
  display: flex;
  flex-direction: column;
  justify-content: center;
  min-width: 0;
}

.brand {
  font-size: 13px;
  font-weight: 850;
  letter-spacing: 0.14em;
  text-transform: uppercase;
  line-height: 1.15;
  color: #fff;
}

.brand-subtitle {
  margin-top: 4px;
  font-size: 11px;
  font-weight: 650;
  letter-spacing: 0.06em;
  line-height: 1.15;
  color: rgba(255,255,255,0.72);
}

.icon-button {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  min-width: 38px;
  height: 38px;
  padding: 0 11px;
  border: 1px solid rgba(255,255,255,0.22);
  border-radius: 11px;
  color: #fff;
  background: rgba(255,255,255,0.10);
  cursor: pointer;
  transition: 160ms ease;
}

.icon-button:hover { background: rgba(255,255,255,0.18); }

.hero h1 {
  max-width: 780px;
  margin: 0;
  font-size: clamp(27px, 4vw, 39px);
  line-height: 1.13;
  letter-spacing: -0.025em;
}

.hero-description {
  max-width: 760px;
  margin: 13px 0 0;
  font-size: 15px;
  color: rgba(255,255,255,0.86);
}

.meta-row {
  display: flex;
  flex-wrap: wrap;
  gap: 8px;
  margin-top: 20px;
}

.meta-chip {
  padding: 6px 10px;
  border: 1px solid rgba(255,255,255,0.16);
  border-radius: 999px;
  background: rgba(255,255,255,0.09);
  color: rgba(255,255,255,0.90);
  font-size: 12px;
  font-weight: 650;
}

.action-row {
  display: flex;
  flex-wrap: wrap;
  gap: 9px;
  margin-top: 24px;
}

.btn {
  border: 1px solid transparent;
  border-radius: 11px;
  padding: 10px 14px;
  font-size: 13px;
  font-weight: 750;
  cursor: pointer;
  transition: transform 140ms ease, background 140ms ease, border-color 140ms ease;
}

.btn:hover { transform: translateY(-1px); }
.btn:active { transform: translateY(0); }

.btn-primary {
  color: #20283a;
  background: #fff;
}

.btn-primary:hover { background: #f7f8fb; }

.btn-ghost {
  color: #fff;
  background: rgba(255,255,255,0.10);
  border-color: rgba(255,255,255,0.19);
}

.btn-ghost:hover { background: rgba(255,255,255,0.17); }

.workspace {
  margin-top: 22px;
}

.toolbar {
  position: sticky;
  top: 10px;
  z-index: 20;
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 12px;
  padding: 10px;
  margin-bottom: 16px;
  border: 1px solid var(--border);
  border-radius: 15px;
  background: var(--surface);
  backdrop-filter: blur(12px);
  box-shadow: var(--shadow-sm);
}

.tabs {
  display: inline-flex;
  gap: 4px;
  padding: 4px;
  background: var(--surface-soft);
  border-radius: 10px;
}

.tab {
  border: 0;
  border-radius: 8px;
  padding: 7px 11px;
  background: transparent;
  color: var(--muted);
  font-size: 12px;
  font-weight: 750;
  cursor: pointer;
}

.tab.active {
  color: var(--text);
  background: var(--surface-strong);
  box-shadow: 0 1px 5px rgba(0,0,0,0.08);
}

.toolbar-actions {
  display: flex;
  flex-wrap: wrap;
  gap: 6px;
}

.small-btn {
  border: 1px solid var(--border);
  border-radius: 9px;
  padding: 7px 10px;
  color: var(--muted);
  background: var(--surface-strong);
  font-size: 12px;
  font-weight: 700;
  cursor: pointer;
}

.small-btn:hover {
  color: var(--text);
  border-color: var(--border-strong);
}

.prompt-section {
  margin-bottom: 14px;
  border: 1px solid var(--border);
  border-radius: 16px;
  background: var(--surface);
  box-shadow: var(--shadow-sm);
  overflow: hidden;
}

.prompt-section > summary {
  list-style: none;
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 12px;
  padding: 18px 21px;
  cursor: pointer;
  user-select: none;
}

.prompt-section > summary::-webkit-details-marker { display: none; }

.section-title {
  font-size: 18px;
  line-height: 1.3;
  font-weight: 800;
  letter-spacing: -0.012em;
}

.chevron {
  width: 9px;
  height: 9px;
  flex: 0 0 9px;
  border-right: 2px solid var(--muted);
  border-bottom: 2px solid var(--muted);
  transform: rotate(45deg);
  transition: transform 160ms ease;
}

.prompt-section[open] .chevron { transform: rotate(225deg); }

.section-body {
  padding: 0 21px 21px;
  border-top: 1px solid var(--border);
}

h3 {
  margin: 24px 0 9px;
  color: var(--primary);
  font-size: 15px;
  letter-spacing: -0.005em;
}

p { margin: 10px 0; }

ul, ol {
  margin: 10px 0;
  padding-left: 24px;
}

li { margin: 5px 0; }

.table-wrapper {
  overflow-x: auto;
  margin: 15px 0 7px;
  border: 1px solid var(--border);
  border-radius: 12px;
}

table {
  width: 100%;
  border-collapse: collapse;
  background: var(--surface-strong);
  font-size: 13px;
}

th {
  padding: 11px 13px;
  text-align: left;
  color: var(--text);
  background: var(--surface-soft);
  border-bottom: 1px solid var(--border);
  font-weight: 800;
}

td {
  padding: 10px 13px;
  vertical-align: top;
  border-bottom: 1px solid var(--border);
}

tr:last-child td { border-bottom: 0; }

.raw-card {
  display: none;
  border: 1px solid var(--border);
  border-radius: 16px;
  overflow: hidden;
  background: var(--surface-strong);
  box-shadow: var(--shadow-sm);
}

.raw-card-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 12px;
  padding: 13px 16px;
  border-bottom: 1px solid var(--border);
  color: var(--muted);
  font-size: 12px;
  font-weight: 700;
}

pre {
  margin: 0;
  max-height: 72vh;
  overflow: auto;
  padding: 20px;
  white-space: pre-wrap;
  overflow-wrap: anywhere;
  tab-size: 2;
  color: var(--text);
  background: var(--surface-soft);
  font-family: "SFMono-Regular", Consolas, "Liberation Mono", Menlo, monospace;
  font-size: 12.5px;
  line-height: 1.65;
}

.footer {
  padding: 26px 6px 4px;
  text-align: center;
  color: var(--muted);
  font-size: 12px;
}

.toast {
  position: fixed;
  left: 50%;
  bottom: 24px;
  z-index: 100;
  transform: translate(-50%, 14px);
  padding: 10px 15px;
  border-radius: 10px;
  color: #fff;
  background: #172033;
  box-shadow: 0 10px 28px rgba(0,0,0,0.20);
  font-size: 12px;
  font-weight: 700;
  opacity: 0;
  pointer-events: none;
  transition: 180ms ease;
}

.toast.show {
  opacity: 1;
  transform: translate(-50%, 0);
}

@media (max-width: 720px) {
  .app-shell { width: min(100% - 20px, 1180px); padding-top: 10px; }
  .hero { border-radius: 18px; padding: 25px 21px 23px; }
  .brand-lockup { gap: 9px; }
  .brand-logo { width: 36px; height: 36px; flex-basis: 36px; }
  .brand { font-size: 12px; }
  .brand-subtitle { font-size: 10px; }
  .toolbar { position: static; align-items: flex-start; flex-direction: column; }
  .toolbar-actions { width: 100%; }
  .small-btn { flex: 1 1 auto; }
  .prompt-section > summary { padding: 16px; }
  .section-body { padding: 0 16px 17px; }
}

@media print {
  body { background: #fff !important; color: #000 !important; }
  .app-shell { width: 100%; padding: 0; }
  .hero { color: #000; background: #fff; box-shadow: none; border: 1px solid #ddd; }
  .hero-description, .meta-chip { color: #333; }
  .meta-chip { border-color: #ddd; background: #fff; }
  .hero-top .icon-button, .action-row, .toolbar, .footer, .toast { display: none !important; }
  .prompt-section { break-inside: avoid; box-shadow: none; border-color: #ddd; }
  .raw-card { display: none !important; }
  #formatted-view { display: block !important; }
}
</style>

<script>
function rawPrompt() {
  return document.getElementById("raw-prompt").value;
}

function showToast(message) {
  const toast = document.getElementById("toast");
  toast.textContent = message;
  toast.classList.add("show");
  window.clearTimeout(window.__cvToastTimer);
  window.__cvToastTimer = window.setTimeout(function() {
    toast.classList.remove("show");
  }, 1800);
}

function fallbackCopy(text) {
  const area = document.createElement("textarea");
  area.value = text;
  area.style.position = "fixed";
  area.style.left = "-9999px";
  area.setAttribute("readonly", "");
  document.body.appendChild(area);
  area.select();
  let ok = false;
  try { ok = document.execCommand("copy"); } catch (e) { ok = false; }
  document.body.removeChild(area);
  return ok;
}

function copyPrompt() {
  const text = rawPrompt();

  if (navigator.clipboard && window.isSecureContext) {
    navigator.clipboard.writeText(text)
      .then(function() { showToast("Prompt copied to clipboard"); })
      .catch(function() {
        showToast(fallbackCopy(text) ? "Prompt copied to clipboard" : "Copy failed - use the Raw view");
      });
  } else {
    showToast(fallbackCopy(text) ? "Prompt copied to clipboard" : "Copy failed - use the Raw view");
  }
}

function downloadBlob(content, type, filename) {
  const blob = new Blob([content], { type: type });
  const url = URL.createObjectURL(blob);
  const a = document.createElement("a");
  a.href = url;
  a.download = filename;
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
  window.setTimeout(function() { URL.revokeObjectURL(url); }, 100);
}

function downloadPrompt() {
  downloadBlob(rawPrompt(), "text/plain;charset=utf-8", "celliverse_typoPrompt.txt");
  showToast("Text prompt saved");
}

function downloadHtml() {
  const clone = document.documentElement.cloneNode(true);
  const formatted = clone.querySelector("#formatted-view");
  const raw = clone.querySelector("#raw-view");
  const formattedTab = clone.querySelector("#tab-formatted");
  const rawTab = clone.querySelector("#tab-raw");
  const sectionActions = clone.querySelector("#section-actions");

  if (formatted) formatted.style.display = "block";
  if (raw) raw.style.display = "none";
  if (formattedTab) formattedTab.classList.add("active");
  if (rawTab) rawTab.classList.remove("active");
  if (sectionActions) sectionActions.style.display = "flex";
  clone.querySelectorAll("details.prompt-section").forEach(function(section) {
    section.open = true;
  });

  const html = "<!DOCTYPE html>\\n" + clone.outerHTML;
  downloadBlob(html, "text/html;charset=utf-8", "celliverse_typoPrompt.html");
  showToast("HTML prompt saved");
}

function setView(view) {
  const formatted = document.getElementById("formatted-view");
  const raw = document.getElementById("raw-view");
  const formattedTab = document.getElementById("tab-formatted");
  const rawTab = document.getElementById("tab-raw");
  const sectionActions = document.getElementById("section-actions");

  const rawMode = view === "raw";
  formatted.style.display = rawMode ? "none" : "block";
  raw.style.display = rawMode ? "block" : "none";
  formattedTab.classList.toggle("active", !rawMode);
  rawTab.classList.toggle("active", rawMode);
  sectionActions.style.display = rawMode ? "none" : "flex";
}

function setAllSections(open) {
  document.querySelectorAll("details.prompt-section").forEach(function(section) {
    section.open = open;
  });
}

function toggleTheme() {
  const root = document.documentElement;
  const current = root.getAttribute("data-theme") || "light";
  const next = current === "dark" ? "light" : "dark";
  root.setAttribute("data-theme", next);
  document.getElementById("theme-label").textContent = next === "dark" ? "Light" : "Dark";
  try { localStorage.setItem("celliverse-typoprompt-theme", next); } catch (e) {}
}

let __cvPrintSectionState = null;
let __cvPrintWasRaw = false;

window.addEventListener("beforeprint", function() {
  const sections = Array.from(document.querySelectorAll("details.prompt-section"));
  __cvPrintSectionState = sections.map(function(section) { return section.open; });
  __cvPrintWasRaw = document.getElementById("raw-view").style.display === "block";
  sections.forEach(function(section) { section.open = true; });
  setView("formatted");
});

window.addEventListener("afterprint", function() {
  if (!__cvPrintSectionState) return;
  document.querySelectorAll("details.prompt-section").forEach(function(section, i) {
    section.open = __cvPrintSectionState[i];
  });
  if (__cvPrintWasRaw) setView("raw");
  __cvPrintWasRaw = false;
  __cvPrintSectionState = null;
});

(function initTheme() {
  let theme = "light";
  try {
    const saved = localStorage.getItem("celliverse-typoprompt-theme");
    if (saved === "dark" || saved === "light") {
      theme = saved;
    } else if (window.matchMedia && window.matchMedia("(prefers-color-scheme: dark)").matches) {
      theme = "dark";
    }
  } catch (e) {}

  document.documentElement.setAttribute("data-theme", theme);
  window.addEventListener("DOMContentLoaded", function() {
    document.getElementById("theme-label").textContent = theme === "dark" ? "Light" : "Dark";
  });
})();
</script>
</head>

<body>
<div class="app-shell">

  <header class="hero">
    <div class="hero-top">
      <div class="brand-lockup">
        ', logo_html, '
        <div class="brand-copy">
          <div class="brand">CelliVerse</div>
          <div class="brand-subtitle">TypoPrompt | Cell annotation</div>
        </div>
      </div>
      <button class="icon-button" type="button" onclick="toggleTheme()" title="Switch appearance">
        <span id="theme-label">Dark</span>
      </button>
    </div>

    <div class="hero-main">
      <h1>', escape_html(title), '</h1>
      <p class="hero-description">
        LLM-ready cell annotation prompt generated from your marker results. Review it here, copy it directly to your preferred AI model, or save a portable copy.
      </p>
    </div>

    <div class="meta-row">
      <span class="meta-chip">', format(set_count, big.mark = ","), ' marker sets</span>
      <span class="meta-chip">', format(word_count, big.mark = ","), ' words</span>
      <span class="meta-chip">', format(char_count, big.mark = ","), ' characters</span>
    </div>

    <div class="action-row">
      <button class="btn btn-primary" type="button" onclick="copyPrompt()">Copy prompt</button>
      <button class="btn btn-ghost" type="button" onclick="downloadPrompt()">Save TXT</button>
      <button class="btn btn-ghost" type="button" onclick="downloadHtml()">Save HTML</button>
      <button class="btn btn-ghost" type="button" onclick="window.print()">Print / PDF</button>
    </div>
  </header>

  <main class="workspace">
    <div class="toolbar">
      <div class="tabs" role="tablist" aria-label="Prompt view">
        <button id="tab-formatted" class="tab active" type="button" onclick="setView(\'formatted\')">Formatted</button>
        <button id="tab-raw" class="tab" type="button" onclick="setView(\'raw\')">Raw prompt</button>
      </div>

      <div id="section-actions" class="toolbar-actions">
        <button class="small-btn" type="button" onclick="setAllSections(true)">Expand all</button>
        <button class="small-btn" type="button" onclick="setAllSections(false)">Collapse all</button>
      </div>
    </div>

    <div id="formatted-view">',
formatted_prompt,
'</div>

    <div id="raw-view" class="raw-card">
      <div class="raw-card-header">
        <span>Exact text sent to your LLM</span>
        <button class="small-btn" type="button" onclick="copyPrompt()">Copy</button>
      </div>
      <pre>', escape_html(raw_prompt), '</pre>
    </div>
  </main>

  <footer class="footer">
    Generated locally by CelliVerse | TypoPrompt
  </footer>

  <textarea id="raw-prompt" style="display:none;">', escape_html(raw_prompt), '</textarea>
  <div id="toast" class="toast" role="status" aria-live="polite"></div>

</div>
</body>
</html>'
  )
  
  html
}


# ___________________________________________________________________
# Print method
# ___________________________________________________________________

#' @export
#' @noRd
print.TypoPrompt <- function(x, ...) {
  
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    warning(
      "Install 'htmltools' to display TypoPrompt as an interactive HTML page.",
      call. = FALSE
    )
    cat(as.character(x))
    return(invisible(x))
  }
  
  html_file <- tempfile(
    pattern = "celliverse_typoPrompt_",
    fileext = ".html"
  )
  
  writeLines(
    .cv_typoPrompt_html(x),
    html_file,
    useBytes = TRUE
  )
  
  if (
    interactive() &&
    requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()
  ) {
    rstudioapi::viewer(html_file)
  } else if (interactive()) {
    path <- normalizePath(
      html_file,
      winslash = "/",
      mustWork = FALSE
    )
    
    if (.Platform$OS.type == "windows") {
      path <- paste0("file:///", path)
    } else {
      path <- paste0("file://", path)
    }
    
    utils::browseURL(path)
  } else {
    cat(as.character(x))
  }
  
  invisible(x)
}


# ___________________________________________________________________
# Programmatic save helper
# ___________________________________________________________________

#' Save a TypoPrompt to a file
#'
#' @description
#' Saves a \code{TypoPrompt} object as either its exact plain-text prompt or a
#' self-contained interactive HTML page.
#'
#' @param x A \code{TypoPrompt} object returned by \code{\link{typoPrompt}}.
#' @param file Character string giving the output file path.
#' @param format Output format: \code{"txt"} for the raw prompt or \code{"html"}
#'   for the interactive HTML viewer.
#'
#' @return Invisibly returns the normalized output file path.
#'
#' @examples
#' utils::data("pbmc_small", package = "SeuratObject")
#'
#' cc <- clustoCell(
#'   data = pbmc_small,
#'   identify_subclusters = FALSE,
#'   num_threads = 1,
#'   verbose = FALSE
#' )
#'
#' desired_set <- utils::head(
#'   sort(unique(as.character(cc$clusters$major_clusters))),
#'   1
#' )
#'
#' prompt <- typoPrompt(
#'   object = cc,
#'   desired_sets = desired_set,
#'   sample_source = "human peripheral blood",
#'   tissue = "Blood",
#'   condition = "Healthy",
#'   species = "human",
#'   use_neg_markers = FALSE,
#'   thresh = 10,
#'   verbose = FALSE
#' )
#'
#' txt_file <- tempfile(fileext = ".txt")
#'
#' saveTypoPrompt(
#'   prompt,
#'   file = txt_file,
#'   format = "txt"
#' )
#'
#' \dontshow{
#' unlink(txt_file)
#' }
#'
#' @seealso \code{\link{typoPrompt}}
#' @export
saveTypoPrompt <- function(
    x,
    file,
    format = c("txt", "html")
) {
  
  if (!inherits(x, "TypoPrompt")) {
    stop("'x' must be a TypoPrompt object.")
  }
  
  if (missing(file) || length(file) != 1L || is.na(file) ||
      !is.character(file) || !nzchar(file)) {
    stop("'file' must be a single non-empty character string.")
  }
  
  format <- match.arg(format)
  
  if (format == "txt") {
    writeLines(
      as.character(x),
      con = file,
      useBytes = TRUE
    )
  } else {
    if (!requireNamespace("htmltools", quietly = TRUE)) {
      stop(
        "The 'htmltools' package is required to save TypoPrompt as HTML.",
        call. = FALSE
      )
    }
    
    writeLines(
      .cv_typoPrompt_html(x),
      con = file,
      useBytes = TRUE
    )
  }
  
  invisible(normalizePath(file, winslash = "/", mustWork = FALSE))
}


# ___________________________________________________________________
# as.character method
# ___________________________________________________________________

#' @export
#' @noRd
as.character.TypoPrompt <- function(x, ...) {
  unclass(x)
}
