#' Cell type annotation of clusters, sub-clusters, or cell subsets
#'
#' @description
#' Annotates clusters, sub-clusters, or arbitrary cell subsets using curated
#' cell-type marker databases or large language model (LLM)–based annotation.
#' Annotation can be performed either from marker results stored in
#' \code{ClustoCell} or \code{MarkoCell} objects, or directly from user-specified
#' positive and/or negative marker panels.
#'
#' @details
#' \code{typoClust()} identifies candidate cell types for each target set by
#' comparing positive and/or negative marker genes against tissue- and
#' condition-aware cell-type marker databases. Users may restrict annotation
#' to specific tissues or conditions, control the number of markers used per
#' cluster, and choose between rank-based or fixed-size marker selection.
#'
#' Exactly one of the following inputs must be provided:
#' \itemize{
#'   \item One or more \code{ClustoCell} or \code{MarkoCell} objects via
#'   \code{objects}
#'   \item User-defined positive and/or negative marker panels via
#'   \code{desired_pos_markers} and/or \code{desired_neg_markers}
#' }
#'
#' @param objects
#' A list of one or more objects of class \code{ClustoCell} or \code{MarkoCell}
#' (e.g. \code{list(obj1, obj2)}). Mandatory if \code{desired_pos_markers} and/or
#' \code{desired_neg_markers} are not specified.
#'
#' @param desired_sets
#' Optional character vector specifying the names of clusters, sub-clusters,
#' and/or cell subsets to annotate. These names must exist in the supplied
#' \code{objects}. If \code{NULL}, all available sets are annotated.
#'
#' @param tissue
#' Optional character vector specifying one or more tissue contexts used for
#' annotation. If \code{NULL}, all available tissues are examined. Available
#' tissue types can be accessed via
#' \code{data("tissueCondition_types", package = "celliverse")}.
#'
#' @param condition
#' Optional character vector specifying one or more conditions (e.g. Healthy,
#' disease states) used for annotation. If \code{NULL}, all available conditions
#' are examined. Available condition types can be accessed via
#' \code{data("tissueCondition_types", package = "celliverse")}.
#'
#' @param use_pos_markers
#' Logical; whether to use positive markers for cell-type annotation.
#' Default is \code{TRUE}.
#'
#' @param use_neg_markers
#' Logical; whether to use negative markers for cell-type annotation.
#' Default is \code{TRUE}.
#'
#' @param desired_pos_markers
#' Optional named list of character vectors specifying positive marker panels.
#' Each list element corresponds to one cluster or cell subset
#' (e.g. \code{list(cluster1 = c("GeneA", "GeneB"))}). Mandatory if
#' \code{objects} and \code{desired_neg_markers} are not specified. List names
#' must match those of \code{desired_neg_markers}, if provided.
#'
#' @param desired_neg_markers
#' Optional named list of character vectors specifying negative marker panels.
#' Each list element corresponds to one cluster or cell subset. Mandatory if
#' \code{objects} and \code{desired_pos_markers} are not specified. List names
#' must match those of \code{desired_pos_markers}, if provided.
#'
#' @param thresh_mode
#' Character string specifying how to select top markers. One of:
#' \itemize{
#'   \item \code{"rank"}: Selects all markers with ranks up to the threshold.
#'   If multiple markers share the cutoff rank, all are included.
#'   \item \code{"n"}: Selects strictly the top \code{n} markers in rank order,
#'   even if additional markers share the same rank.
#' }
#'
#' @param thresh
#' Integer specifying the marker selection threshold. Interpreted according to
#' \code{thresh_mode}. Only used when \code{objects} is specified.
#'
#' @param mode
#' Character string specifying the annotation strategy. One of:
#' \itemize{
#'   \item \code{"markerDB"}: Annotates cell sets using curated cell-type
#'   marker databases.
#'   \item \code{"ceLLMarkup"}: Annotates cell sets using large language models
#'   (LLM) via \code{\link{ceLLMarkup}}; configure the LLM with
#'   \code{llm_provider}, \code{llm_model}, \code{llm_api_key}, \code{llm_host}.
#' }
#'
#' @param inherit_major_clusters
#' Logical; whether a sub-cluster should be annotated \emph{within the identity
#' of its own major cluster}. Default \code{TRUE}.
#'
#' When \code{FALSE}, every set is annotated independently and the function
#' behaves exactly as it did before this argument existed.
#'
#' When \code{TRUE}, and the input contains both major clusters and
#' sub-clusters, annotation becomes two-stage. Each requested sub-cluster's
#' parent major cluster is annotated first, from the \emph{major cluster's own}
#' top markers; the sub-cluster is then annotated from \emph{its own} top
#' markers, constrained by that parent identity:
#' \itemize{
#'   \item \code{mode = "markerDB"}: the database is restricted to every cell
#'   type whose name contains an admitted parent identity as a whole phrase —
#'   so a parent called \code{"CD8+ T Cell"} leaves \code{"CD8+ T Cell"} itself
#'   plus \code{"Exhausted CD8+ T Cell"}, \code{"Memory CD8+ T Cell"},
#'   \code{"GZMK+ CD8+ T Cell"} and so on — and the sub-cluster is scored
#'   against that restricted database only. Which identities count as admitted
#'   is set by \code{inherit_score_ratio}.
#'   \item \code{mode = "ceLLMarkup"}: the model is told the parent's identity
#'   and asked which specific subtype or state of \emph{that} cell type the
#'   sub-cluster represents, given the sub-cluster's own markers.
#' }
#'
#' A parent major cluster needed for this is annotated and returned even when it
#' was not itself named in \code{desired_sets}; \code{metadata$inheritance}
#' records, per sub-cluster, which parent was used and what it was called.
#' The restriction is applied per sub-cluster, so one major cluster's identity
#' can never constrain a different major cluster's sub-clusters.
#'
#' @param inherit_score_ratio
#' Numeric in \code{(0, 1]}, default \code{0.5}. \strong{\code{mode = "markerDB"}
#' only}, and only when \code{inherit_major_clusters = TRUE}.
#'
#' How close to the parent's rank-1 score a runner-up candidate must come before
#' it \emph{also} constrains that parent's sub-clusters. At \code{1} only the
#' rank-1 label is used; at \code{0.6} any candidate scoring at least 60\% of
#' the rank-1 score is admitted alongside it, and the database is restricted to
#' the union of their named varieties.
#'
#' This exists because a major cluster's own label is often not certain, and
#' treating it as certain propagates the doubt into every sub-cluster beneath
#' it. Measured on the bundled pbmc3k \code{ClustoCell} (\code{thresh = 10},
#' Blood/Healthy), each parent's rank-2 candidate as a fraction of its rank-1
#' score:
#'
#' \tabular{lll}{
#'   \strong{Parent} \tab \strong{rank-1 vs rank-2} \tab \strong{ratio} \cr
#'   C1 \tab NK Cell vs CD8+ Alpha-Beta T Cell \tab 0.653 \cr
#'   C2 \tab B Cell vs MS4A1+ B Cell \tab 0.248 \cr
#'   C3 \tab T Cell vs CD4+ Alpha-Beta T Cell \tab 0.965 \cr
#'   C4 \tab Mononuclear Phagocyte vs Monocyte \tab 0.837 \cr
#'   C5 \tab Platelet vs Megakaryocyte \tab 0.846
#' }
#'
#' C1 is a mixed cytotoxic compartment whose sub-clustering separates CD8+ T
#' cells from NK cells; on rank-1 alone its T-cell sub-cluster is folded back
#' into NK. C4 is the other end of the same problem: exactly one database cell
#' type is named as a variety of \code{"Mononuclear Phagocyte"}, so on rank-1
#' alone its sub-clusters can only repeat the parent's label, while admitting
#' \code{"Monocyte"} restores a vocabulary of 32. At the default, no parent on
#' that dataset admits more than three identities.
#'
#' @param species
#' Character string specifying the species (either \code{"human"} or
#' \code{"mouse"}). Other species names may also be supplied when the mode is set to \code{"ceLLMarkup"}. Default is
#' \code{"human"}.
#' 
#' @param sample_source Free-text description of the sample origin shown to the
#'   model (e.g. \code{"human peripheral blood"}). Improves accuracy. Only used when mode is set to \code{"ceLLMarkup"}.
#' @param feature_type Feature type of the markers (e.g. \code{"gene"},
#'   \code{"protein"}). Default \code{"gene"}. Only used when mode is set to \code{"ceLLMarkup"}.
#'
#' @param llm_provider
#' (\code{mode = "ceLLMarkup"} only) LLM provider. One of \code{"ollama"},
#' \code{"lmstudio"}, \code{"openai"}, \code{"anthropic"}, \code{"gemini"},
#' \code{"deepseek"}, \code{"groq"}, \code{"openrouter"}, \code{"cerebras"}.
#' Default \code{"ollama"}.
#'
#' @param llm_model
#' (\code{mode = "ceLLMarkup"} only) Model id for \code{llm_provider}
#' (e.g. \code{"qwen3:8b"} for Ollama, \code{"qwen/qwen3-8b"} for LM Studio,
#' \code{"gpt-4o-mini"} for OpenAI, \code{"anthropic/claude-3-haiku"} for
#' OpenRouter). Mandatory when \code{mode = "ceLLMarkup"}.
#'
#' @param llm_api_key
#' (\code{mode = "ceLLMarkup"} only) API key for cloud providers (not needed
#' for Ollama/LM Studio). If \code{NULL}, falls back to the provider's standard
#' environment variable (e.g. \code{OPENAI_API_KEY}, \code{OPENROUTER_API_KEY}).
#'
#' @param llm_host
#' (\code{mode = "ceLLMarkup"} only) Base URL for local providers. Defaults to
#' \code{http://localhost:11434} (Ollama) or \code{http://localhost:1234/v1}
#' (LM Studio).
#'
#' @param llm_top_k
#' (\code{mode = "ceLLMarkup"} only) Number of ranked candidate cell types
#' returned per set. Default 3.
#'
#' @param verbose
#' Logical; whether to display progress messages.
#'
#' @return
#' An object of class \code{TypoClust} containing ranked cell-type annotations,
#' supporting marker evidence, and summary statistics for each annotated set.
#'
#' @seealso
#' \code{\link{typoClustVis}}, \code{\link{markoCell}}, \code{\link{markoClust}},
#' \code{\link{clustoCell}}
#'
#' @examples
#' \dontrun{
#' tc <- typoClust(
#'   objects = list(clust_obj),
#'   tissue = "Blood",
#'   condition = "Healthy",
#'   thresh = 20,
#'   species = "human"
#' )
#' }
#' 
#' @export

typoClust <- function(objects = NULL, # A list of one or more objects of class ClustoCell or MarkoCell (e.g. list(obj1, obj2)). This argument is mandatory if desired_pos_markers and desired_neg_markers are not specified.
                      desired_sets = NULL, # Optional. A character vector of the names of desired clusters, sub-clusters, and/or cell-subsets present in the specified `objects`. If not specified, all clusters and cell-subsets in the objects will be annotated.
                      tissue = NULL, # A character vector specifying one or more tissue contexts to be used for identifying specific cell types. If not specified, all available tissues will be examined. The available tissue types can be accessed from the tissueCondition_types object, which can be loaded using data("tissueCondition_types", package = "celliverse").
                      condition = NULL, # A character vector specifying one or more conditions to be used for identifying specific cell types. If not specified, all conditions including 'Healthy' as well as all available diseased conditions will be examined. The available condition types can be accessed from the tissueCondition_types object, which can be loaded using data("tissueCondition_types", package = "celliverse").
                      use_pos_markers = TRUE, # logical, whether to use positive markers for cell type annotation (default if TRUE).
                      use_neg_markers = TRUE, # logical, whether to use negative markers for cell type annotation.
                      desired_pos_markers = NULL, # Optional. Mandatory if `objects` and `desired_neg_markers` are not specified. A named list of character vectors of the desired positive marker panels (e.g. list(cluster1 = c(gene1, gene 2), cluster2 = c(gene5, gene7))). The names of panels in the list should match those of desired_neg_markers, if specified.
                      desired_neg_markers = NULL, # Optional. Mandatory if `objects` and `desired_pos_markers` are not specified. A named list of character vectors of the desired negative marker panels (e.g. list(cluster1 = c(gene1, gene 2), cluster2 = c(gene5, gene7))). The names of panels in the list should match those of desired_pos_markers, if specified.
                      thresh_mode = c("n", "rank"), # Specifies how to select top markers for each cluster or subcluster. Options are:
                      ## "rank": selects all markers with ranks up to the threshold. If multiple markers share the same rank as the cutoff, they are all included.
                      ## "n": selects strictly the top n rows in rank order. Only the first n rows are kept, even if additional rows share the same rank as the n-th row.
                      thresh = 20, # Integer, threshold for choosing the top N rows of the marker tables or top N ranked markers of each cluster, sub-cluster, and cell-subset. Only used if `objects` is specified.
                      mode = c("markerDB", "ceLLMarkup"), # Either `markerDB` (which annotates the cell clusters and cell subsets based on the prepared database of cell-type markers) or ceLLMarkup (which annotates the cell clusters and cell subsets using large language models (LLM))
                      inherit_major_clusters = TRUE, # Logical. When TRUE (default) and the input holds both major clusters and sub-clusters, each sub-cluster is annotated WITHIN the identity of its own major cluster: the parent is annotated first from the parent's own top markers, then the sub-cluster is annotated from its own top markers, restricted to that parent's cell type (markerDB: the database is narrowed to every cell type whose name contains an admitted parent identity as a whole phrase; ceLLMarkup: the model is told the parent identity and asked for the subtype/state). FALSE reproduces the pre-argument behaviour exactly.
                      inherit_score_ratio = 0.5, # Numeric in (0, 1]. markerDB mode only. How close to the parent's rank-1 score a runner-up must come to ALSO constrain that parent's sub-clusters. 1 means rank-1 only; lower admits more near-ties. Ignored unless mode = "markerDB" and inherit_major_clusters = TRUE.
                      species = c("human", "mouse"), # Character vector, either 'human' or 'mouse'.
                      sample_source = NULL,
                      feature_type = "gene",
                      llm_provider = "ollama", # (mode = "ceLLMarkup" only) LLM provider: one of 'ollama', 'lmstudio', 'openai', 'anthropic', 'gemini', 'deepseek', 'groq', 'openrouter', 'cerebras'.
                      llm_model = NULL, # (mode = "ceLLMarkup" only) model id for `llm_provider` (e.g. 'qwen3:8b' for ollama, 'qwen/qwen3-8b' for lmstudio, 'gpt-4o-mini' for openai, 'anthropic/claude-3-haiku' for openrouter). Mandatory when mode = "ceLLMarkup".
                      llm_api_key = NULL, # (mode = "ceLLMarkup" only) API key for cloud providers (not needed for ollama/lmstudio). If NULL, falls back to the provider's standard environment variable (e.g. OPENAI_API_KEY, OPENROUTER_API_KEY).
                      llm_host = NULL, # (mode = "ceLLMarkup" only) base URL for local providers (default http://localhost:11434 for ollama, http://localhost:1234/v1 for lmstudio).
                      llm_top_k = 3, # (mode = "ceLLMarkup" only) number of ranked candidate cell types returned per set.
                      verbose = TRUE # Logical, whether to show progress messages
                      ) {
  
  #________________________________________
  # Dealing with warnings
  ## Save current warning setting and disable warnings
  old_warn <- getOption("warn")
  options(warn = -1)   # -1 = suppress all warnings
  
  on.exit(options(warn = old_warn), add = TRUE)  # restore when function exits
  
  #________________________________________
  
  # Defining the default logs for info messages
  log_message <- function(...) {
    if (verbose) cli::cli_alert_info(...)
  }
  
  #_____________
  
  log_warning <- function(...) {
    if (verbose) cli::cli_alert_warning(...)
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
  
  log_h2 <- function(...) {
    if(verbose) cli::cli_h2(...)
  }
  
  #_____________
  
  log_space <- function() {
    if(verbose) cli::cli_text("")
  }
  
  #________________________________________
  
  # Setting the args
  
  thresh_mode <- match.arg(thresh_mode)
  mode <- match.arg(mode)
  
  species <- species[1]
  if (!is.character(species) || length(species) == 0 || is.na(species)) {
    cli::cli_abort("{.arg species} must be a non-empty character string.")
  }
  
  # Normalize to lowercase
  species <- tolower(species)
  

  if (length(inherit_major_clusters) != 1L || is.na(inherit_major_clusters) ||
      !is.logical(inherit_major_clusters)) {
    cli::cli_abort("The `inherit_major_clusters` argument must be TRUE or FALSE!")
  }

  if (length(inherit_score_ratio) != 1L || is.na(inherit_score_ratio) ||
      !is.numeric(inherit_score_ratio) || !is.finite(inherit_score_ratio) ||
      inherit_score_ratio <= 0 || inherit_score_ratio > 1) {
    cli::cli_abort("The `inherit_score_ratio` argument must be a single number greater than 0 and at most 1!")
  }

  if (mode == "ceLLMarkup" && (is.null(llm_model) || !nzchar(llm_model))) {
    cli::cli_abort(c(
      "{.arg llm_model} is required when {.code mode = 'ceLLMarkup'}.",
      i = "e.g. llm_provider = 'ollama', llm_model = 'qwen3:8b' (local, offline); or llm_provider = 'openrouter', llm_model = 'anthropic/claude-3-haiku', llm_api_key = <key>."
    ))
  }

  #________________________________________
  
  # Helper Functions ----
  
  ## special_multiply ----
  # Multiplies two numbers with a custom sign rule:
  #   - Both positive  → positive product (as usual)
  #   - One positive, one negative → negative product (as usual)
  #   - Both negative  → negative product (different from standard multiplication)
  # Works element-wise on vectors using ifelse().
  
  special_multiply <- function(x, y) {
    xy <- x * y
    ifelse(x < 0 & y < 0, -xy, xy)
  }

  ## celltype_contains ----
  # TRUE for every database cell type whose NAME contains `phrase` as a whole
  # phrase. Used by `inherit_major_clusters` to narrow the database to a parent
  # cluster's identity and everything named as a variety of it, e.g. a parent of
  # "CD8+ T Cell" keeps "CD8+ T Cell", "Exhausted CD8+ T Cell",
  # "GZMK+ CD8+ T Cell", "Tissue-Resident Memory CD8+ T Cell", ...
  #
  # Two details here are load-bearing, and both were verified against the real
  # database rather than assumed:
  #
  #  1. The phrase MUST be escaped before it is used as a pattern. Cell-type
  #     names routinely contain regex metacharacters -- `grepl("CD8+ T Cell")`
  #     unescaped matches NOTHING at all, because `8+` is a quantifier, so the
  #     restriction would silently discard the entire database and report that
  #     no cell type could be identified.
  #  2. The match must not start or end mid-word. A bare substring test for
  #     "T Cell" also matches "Malignan(t Cell)", "Goble(t Cell)", "Tuf(t Cell)"
  #     and "NK(T Cell)" -- 64 spurious types out of 270 on the human database.
  #     The look-around below removes exactly those.
  #
  # This is a NAME rule, not an ontology: it cannot know that an "NKT Cell" is
  # arguably a T cell, or that a "B Cell Zone Reticular Cell" is not a B cell.
  # That is the intended, stated behaviour -- restriction follows the naming of
  # the curated database.
  # `phrases` may hold MORE than one identity -- see parent_identities() below.
  # The result is their union: a sub-cluster of a parent that is genuinely
  # ambiguous between two lineages may be named as a variety of either.
  celltype_contains <- function(x, phrases) {
    phrases <- trimws(as.character(phrases))
    phrases <- phrases[!is.na(phrases) & nzchar(phrases)]
    if (!length(phrases)) return(rep(TRUE, length(x)))
    x <- as.character(x)
    hit <- rep(FALSE, length(x))
    for (phrase in phrases) {
      esc <- gsub("([.\\\\|()\\[\\]{}^$*+?])", "\\\\\\1", phrase, perl = TRUE)
      hit <- hit | grepl(paste0("(?<![A-Za-z0-9])", esc, "(?![A-Za-z0-9])"),
                         x, ignore.case = TRUE, perl = TRUE)
    }
    hit
  }

  ## parent_identities ----
  # Which of a parent major cluster's candidate cell types should constrain its
  # sub-clusters: the rank-1 label, plus any NEAR-TIE scoring at least
  # `inherit_score_ratio` of it.
  #
  # Rank-1 alone is the obvious rule and it is wrong, because it treats the
  # parent's own label as certain when the score often says otherwise. Measured
  # on the bundled pbmc3k ClustoCell (thresh=10, Blood/Healthy), rank-2 as a
  # fraction of rank-1:
  #
  #   C1  NK Cell               vs CD8+ Alpha-Beta T Cell   0.653
  #   C2  B Cell                vs MS4A1+ B Cell            0.248
  #   C3  T Cell                vs CD4+ Alpha-Beta T Cell   0.965
  #   C4  Mononuclear Phagocyte vs Monocyte                 0.837
  #   C5  Platelet              vs Megakaryocyte            0.846
  #
  # C1 is the case that matters most: it is a MIXED cytotoxic compartment, and
  # its sub-clustering separates a CD8+ T population from an NK one. Constraining
  # to rank-1 alone folds the T cells back into NK and discards exactly the
  # distinction the sub-clustering found (CHANGES.md Round XXIII, where the user
  # established that call by hand). C4 is the second case: on rank-1 alone the
  # database holds ONE type named as a variety of "Mononuclear Phagocyte", so its
  # sub-clusters could only ever echo the parent; admitting "Monocyte" at 0.837
  # restores a real vocabulary of 32.
  #
  # A non-positive top score means the parent has no usable evidence at all, so
  # there is no scale to take a fraction of; fall back to the rank-1 label.
  parent_identities <- function(p_res, ratio) {
    if (!is.data.frame(p_res) || !nrow(p_res) || !("CellType" %in% colnames(p_res)))
      return(character(0))
    d <- p_res[order(p_res$Rank), , drop = FALSE]
    sc <- suppressWarnings(as.numeric(d$Combined_Score))
    top <- sc[1]
    if (!is.finite(top) || top <= 0) return(unique(as.character(d$CellType[1])))
    keep <- is.finite(sc) & sc > 0 & sc >= (ratio * top)
    keep[1] <- TRUE
    unique(as.character(d$CellType[keep]))
  }
  
  #________________________________________
  
  # Start of function ----
  if(verbose) {
    cli::cli_rule(left = cli::style_italic(cli::style_bold("Starting TypoClust!")), right = cli::col_silver(Sys.time()))
  }
  
  if(!use_pos_markers & !use_neg_markers) {
    cli::cli_abort("Either `use_pos_markers`, `use_neg_markers` or both should be set to TRUE!")
  }
  
  log_h1("Preparing the Input Data")
  
  if(!is.null(objects) & is.null(desired_sets)) {
    log_message("Since `objects` is specified but `desired_sets` is not specified, all clusters and cell subsets of `objects` will be annotated!")
  }
  
  log_progress_step("Inspecting the input data")
  
  if((is.null(objects) & is.null(desired_pos_markers) & is.null(desired_neg_markers))) {
    cli::cli_abort("At least one of `objects`, `desired_pos_markers`, or `desired_neg_markers` should be specified!")
  }
  
  if(!is.null(objects)) {
    if(!inherits(objects, "ClustoCell") & !inherits(objects, "MarkoCell") & !inherits(objects, "list")) {
      cli::cli_abort("The `objects` argument should be a list of one or more objects of class ClustoCell or MarkoCell (e.g. list(obj1, obj2))!")
    }
  }
  
  if(!is.null(objects) & !inherits(objects, "list")) {
    if(inherits(objects, c("ClustoCell", "MarkoCell"))) {
      objects <- list(objects)
    }
  }
  
  if(!is.null(desired_pos_markers)) {
    if(!inherits(desired_pos_markers, "list")) {
      cli::cli_abort("The `desired_pos_markers` argument should be a named list of character vectors of the desired positive marker panels (e.g. list(cluster1 = c(gene1, gene 2), cluster2 = c(gene5, gene7)))!")
    }
  }
  
  if(!is.null(desired_neg_markers)) {
    if(!inherits(desired_neg_markers, "list")) {
      cli::cli_abort("The `desired_neg_markers` argument should be a named list of character vectors of the desired positive marker panels (e.g. list(cluster1 = c(gene1, gene 2), cluster2 = c(gene5, gene7)))!")
    }
  }
  
  if(!is.null(desired_sets)) {
    if(!inherits(desired_sets, "character")) {
      cli::cli_abort("The `desired_sets` argument should be a character vector of the names of desired clusters, sub-clusters, and/or cell-subsets present in the specified `objects`!")
    }
  }
  
  log_progress_done()
  
  log_progress_step("Preparing the clusters, cell-subsets, and marker panels.")
  
  # Setting the names of all clusters, sub-clusters, cell-subsets, and desired panels ----
  clusters <- NULL
  sub_clusters <- NULL
  cell_subsets <- NULL
  panels <- NULL
  all_clusters_pos <- NULL
  all_clusters_neg <- NULL
  combined_cell_set_names <- NULL
  combined_panels_list <- list(pos_panels = NULL,
                               neg_panels = NULL)
  
  ## Getting the names of all clusters, sub-clusters and cell-subsets ----
  if(!is.null(objects)) {
    
    ### Getting all major_clusters ----
    # Extract names separately
    all_clusters <- unlist(lapply(objects, function(i) {
      if(any(grepl("major_clusters", names(i$markers)))) {
        i$markers$major_clusters$cluster_specific$positive_markers %>% names()
      }
    }), use.names = FALSE)
    all_clusters <- unique(all_clusters)
    
    # Extract pos and neg separately
    all_clusters_pos <- unlist(lapply(objects, function(i) {
      if(any(grepl("major_clusters", names(i$markers)))) {
        tmp_pos <- i$markers$major_clusters$cluster_specific$positive_markers
        names(tmp_pos) <- names(tmp_pos)
        tmp_pos
      } else {
        list()
      }
    }), recursive = FALSE)
    
    all_clusters_neg <- unlist(lapply(objects, function(i) {
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
    all_sub_clusters <- unlist(lapply(objects, function(j) {
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
    all_sub_clusters_pos <- unlist(lapply(objects, function(j) {
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
    all_sub_clusters_neg <- unlist(lapply(objects, function(j) {
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
    
    MarkoCell_clusters <- unlist(lapply(objects, function(i) {
      if(any(grepl("cluster_markers", names(i)))) {
        i$cluster_markers$positive_markers %>% names()
      }
    }), use.names = FALSE)
    MarkoCell_clusters <- unique(MarkoCell_clusters)
    
    MarkoCell_pos <- unlist(lapply(objects, function(i) {
      if(any(grepl("cluster_markers", names(i)))) {
        tmp_pos <- i$cluster_markers$positive_markers
        names(tmp_pos) <- names(tmp_pos)
        tmp_pos
      } else {
        list()
      }
    }), recursive = FALSE)
    
    MarkoCell_neg <- unlist(lapply(objects, function(i) {
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
    
    MarkoCell_cell_subset <- unlist(lapply(objects, function(i) {
      if(any(grepl("cell_subset_markers", names(i)))) {
        i$cell_subset_markers$positive_markers %>% names()
      }
    }), use.names = FALSE)
    MarkoCell_cell_subset <- unique(MarkoCell_cell_subset)
    
    MarkoCell_cell_subset_pos <- unlist(lapply(objects, function(i) {
      if(any(grepl("cell_subset_markers", names(i)))) {
        tmp_pos <- i$cell_subset_markers$positive_markers
        names(tmp_pos) <- names(tmp_pos)
        tmp_pos
      } else {
        list()
      }
    }), recursive = FALSE)
    
    MarkoCell_cell_subset_neg <- unlist(lapply(objects, function(i) {
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
        cli::cli_abort(paste0("All cluster, sub-cluster, and cell-subset names specified in the `desired_sets` argument must be present in the `objects`!\n\n",
                              "The following set name(s) are not present in the `objects`: ", paste0(wrong_desired_sets, collapse = ", "),
                              "\n\n", "See below for all clusters, sub-clusters, and cell-subsets present in the `objects`:\n\n",
                              paste0(combined_cell_set_names, collapse = ", ")))
      } else {
        combined_cell_set_names <- desired_sets
      }
    }

    #____________

    ## Working out which sets are sub-clusters of which major cluster ----
    #
    # Parentage is derived by NAME against the real major-cluster list rather
    # than by a "-Sub[0-9]+$" pattern: sub-cluster ids are built above as
    # paste0(<major>, "-", <sub name>), and the sub name is whatever the
    # clustering produced. Matching against the actual major names is therefore
    # exact where a pattern would only be a guess, and the LONGEST match wins so
    # that a major cluster called "C1" cannot claim "C10-Sub1".
    #
    # This has to happen BEFORE the desired_sets panel filter below, because a
    # user who asks only for "C1-Sub1" still needs C1's own marker panel kept in
    # order to establish the parent identity.
    parent_of <- rep(NA_character_, length(combined_cell_set_names))
    names(parent_of) <- combined_cell_set_names
    if (isTRUE(inherit_major_clusters) && length(all_clusters)) {
      for (s in combined_cell_set_names) {
        cand <- all_clusters[startsWith(s, paste0(all_clusters, "-"))]
        cand <- cand[cand != s]
        if (length(cand)) parent_of[[s]] <- cand[which.max(nchar(cand))]
      }
    }
    inherit_active <- any(!is.na(parent_of))
    needed_parents <- unique(stats::na.omit(unname(parent_of)))
    # Parents are annotated and returned even when they were not requested: the
    # sub-cluster's answer is conditioned on the parent's label, so returning
    # the second without the first would be an answer nobody could check.
    added_parents <- setdiff(needed_parents, combined_cell_set_names)
    if (length(added_parents)) {
      combined_cell_set_names <- c(combined_cell_set_names, added_parents)
      log_message(paste0(
        "Because `inherit_major_clusters` is TRUE, the parent major cluster(s) ",
        paste0(added_parents, collapse = ", "),
        " will also be annotated, so the requested sub-cluster(s) can be interpreted within them."))
    }

    #____________

    ## Preparing final markers tables based on objects ----

    all_clusters_pos <- all_clusters_pos[vapply(all_clusters_pos, function(i) is.data.frame(i) && nrow(i) > 0, logical(1))]
    all_clusters_neg <- all_clusters_neg[vapply(all_clusters_neg, function(i) is.data.frame(i) && nrow(i) > 0, logical(1))]

    if(!is.null(desired_sets)) {
      keep_sets <- unique(c(desired_sets, needed_parents))
      all_clusters_pos <- all_clusters_pos[names(all_clusters_pos) %in% keep_sets]
      all_clusters_neg <- all_clusters_neg[names(all_clusters_neg) %in% keep_sets]
    }
    
    combined_panels_list <- list(pos_panels = all_clusters_pos, neg_panels = all_clusters_neg)
    
    # Process purity and panels in one nested lapply, combining operations
    combined_panels_purity_list <- lapply(combined_panels_list, function(i) {
      lapply(i, function(j) {
        if (thresh_mode == "n") {
          j <- j[seq_len(min(thresh, nrow(j))), , drop = FALSE]  # Use base slicing for speed
        } else if (thresh_mode == "rank") {
          j <- j[j$Rank < thresh, , drop = FALSE]
        }
        if ("Purity" %in% colnames(j)) {
          j[, c("Feature", "Purity"), drop = FALSE]
        } else if ("EWCSR" %in% colnames(j)) {
          j <- j[, c("Feature", "EWCSR"), drop = FALSE]
          j$EWCSR <- abs(j$EWCSR)
          colnames(j)[colnames(j) == "EWCSR"] <- "Purity"
          j
        } else {
          j  # Fallback
        }
      })
    })
    
    combined_panels_list <- lapply(combined_panels_list, function(i) {
      lapply(i, function(j) {
        if (thresh_mode == "n") {
          j$Feature[seq_len(min(thresh, nrow(j)))]  # Base extraction
        } else if (thresh_mode == "rank") {
          j$Feature[j$Rank < thresh]
        }
      })
    })
    
  } else {
    all_clusters_pos <- NULL
    all_clusters_neg <- NULL
    combined_panels_list <- list(pos_panels = NULL,
                                 neg_panels = NULL)
    combined_panels_purity_list <- list(pos_panels = NULL,
                                        neg_panels = NULL)
    # No `objects` means no major/sub-cluster structure to inherit from: the
    # user supplied bare marker panels, which carry no hierarchy.
    parent_of <- character(0)
    inherit_active <- FALSE
    needed_parents <- character(0)
    added_parents <- character(0)
  }
  
  #____________
  
  # Preparing the desired markers ----
  
  ## Preparing the desired_pos_markers ----
  
  if(!is.null(desired_pos_markers)) {
    combined_panels_list$pos_panels <- append(combined_panels_list$pos_panels, desired_pos_markers)
    combined_cell_set_names <- append(combined_cell_set_names, names(desired_pos_markers))
  }
  
  ## Preparing the desired_neg_markers ----
  
  if(!is.null(desired_neg_markers)) {
    combined_panels_list$neg_panels <- append(combined_panels_list$neg_panels, desired_neg_markers)
    combined_cell_set_names <- append(combined_cell_set_names, names(desired_neg_markers))
  }
  
  #____________
  
  # Finalizing cluster/subset names and markers ----
  
  combined_cell_set_names <- unique(combined_cell_set_names)
  combined_panels_list <- combined_panels_list[!is.null(combined_panels_list)]
  combined_panels_purity_list <- combined_panels_purity_list[!is.null(combined_panels_purity_list)]
  
  #____________________
  
  # Generating the standard marker names dataframe ----
  
  # Check if 'markerDictionary' exists and is of the correct class
  if (!exists("markerDictionary", inherits = FALSE)) {
    utils::data("markerDictionary", package = "celliverse", envir = environment())
  }
  
  markerDictionary <- get("markerDictionary", envir = environment())
  
  #____________________
  
  # Set the curr_markerDictionary ----
  if(species == "human") {
    curr_markerDictionary <- markerDictionary$human
  } else if(species == "mouse") {
    curr_markerDictionary <- markerDictionary$mouse
  } else {
    cli::cli_abort("The 'species' argument should be any of 'human' or 'mouse'!")
  }
  
  # Preapre all markers
  combined_panels_all_markers <- combined_panels_list %>% unlist() %>% unname() %>% unique()
  
  # Split aliases by "|"
  alias_list <- strsplit(as.character(curr_markerDictionary$Alias), "\\|")
  
  # Create lookup vector: names are aliases, values are Symbols
  alias_to_symbol <- setNames(rep(curr_markerDictionary$Symbol, lengths(alias_list)),
                              unlist(alias_list))
  
  # Lookup
  combined_markers_symbol <- alias_to_symbol[combined_panels_all_markers]
  
  marker_symbol_df <- data.frame(Input_Marker = combined_panels_all_markers,
                                 Std_Symbol = combined_markers_symbol,
                                 row.names = combined_panels_all_markers)
  marker_symbol_df$Available <- TRUE
  marker_symbol_df$Available[is.na(marker_symbol_df$Std_Symbol)] <- FALSE
  
  marker_symbol_df_flt <- marker_symbol_df[!is.na(marker_symbol_df$Std_Symbol),]
  
  log_progress_done()

  #________________________________________________
  
  log_h1("Performing Cell Type Annotation!")

  if(mode == "markerDB") {

    log_message("Setting the cell type annotation mode to markerDB!")

    # Check if 'markerDB' exists and is of the correct class
    if (!exists("markerDB", inherits = FALSE)) {
      utils::data("markerDB", package = "celliverse", envir = environment())
    }
    
    markerDB <- get("markerDB", envir = environment())
    
    #____________________
    
    # Check if 'tissueCondition_types' exists and is of the correct class ----
    if(!is.null(tissue)) {
      
      if (!exists("tissueCondition_types", inherits = FALSE)) {
        utils::data("tissueCondition_types", package = "celliverse", envir = environment())
      }
      
      tissueCondition_types <- get("tissueCondition_types", envir = environment())
    }
    
    #____________________

    # Import the occurrence sparse matrices ----
    if(species == "human") {
      pos_sparse_mat <- markerDB$human$positive_db
      neg_sparse_mat <- markerDB$human$negative_db
    } else if(species == "mouse") {
      pos_sparse_mat <- markerDB$mouse$positive_db
      neg_sparse_mat <- markerDB$mouse$negative_db
    } else {
      cli::cli_abort("The 'species' argument should be any of 'human' or 'mouse'!")
    }
    
    if(!use_pos_markers) {
      pos_sparse_mat <- NULL
    } else if(!use_neg_markers) {
      neg_sparse_mat <- NULL
    }
    
    #____________________
    
    # Checking the tissue type
    if(!is.null(tissue)) {
      if(species == "human") {
        curr_tissue_types <- tissueCondition_types$human$all_tissues
        if(all(tissue %in% curr_tissue_types)) {
          log_message(paste0("Tissue type is set to '", paste0(tissue, collapse = ", "), "'."))
        } else {
          cli::cli_abort("The `tissue` argument is incorrect. Please use the available tissue types from the tissueCondition_types object, which can be loaded using data('tissueCondition_types', package = 'celliverse').")
        }
      } else if(species == "mouse") {
        curr_tissue_types <- tissueCondition_types$mouse$all_tissues
        if(all(tissue %in% curr_tissue_types)) {
          log_message(paste0("Tissue type is set to '", paste0(tissue, collapse = ", "), "'."))
        } else {
          cli::cli_abort("The `tissue` argument is incorrect. Please use the available tissue types from the tissueCondition_types object, which can be loaded using data('tissueCondition_types', package = 'celliverse').")
        }
      }
    }
    
    #____________________
    
    # Checking the condition type
    if(!is.null(condition)) {
      if(species == "human") {
        curr_condition_types <- tissueCondition_types$human$all_conditions
        if(all(condition %in% curr_condition_types)) {
          log_message(paste0("Condition is set to '", paste0(condition, collapse = ", "), "'."))
        } else {
          cli::cli_abort("The `condition` argument is incorrect. Please use the available condition types from the tissueCondition_types object, which can be loaded using data('tissueCondition_types', package = 'celliverse').")
        }
      } else if(species == "mouse") {
        curr_condition_types <- tissueCondition_types$mouse$all_conditions
        if(all(condition %in% curr_condition_types)) {
          log_message(paste0("Condition is set to '", paste0(condition, collapse = ", "), "'."))
        } else {
          cli::cli_abort("The `condition` argument is incorrect. Please use the available condition types from the tissueCondition_types object, which can be loaded using data('tissueCondition_types', package = 'celliverse').")
        }
      }
    }

    #_______________
    
    ### Helper: process a sparse matrix and marker panel
    process_matrix <- function(sparse_mat, marker_panel) {
      # Intersect to get valid markers
      marker_panel <- intersect(marker_panel, rownames(sparse_mat))
      
      # Initialize outputs to match original behavior
      n_cols <- ncol(sparse_mat)
      occurrence <- numeric(n_cols)
      matched_markers <- character(n_cols)
      overlaps <- integer(n_cols)
      names(occurrence) <- names(matched_markers) <- names(overlaps) <- colnames(sparse_mat)
      
      # Early return for empty marker_panel
      if (length(marker_panel) == 0) {
        return(list(overlap_counts = overlaps, 
                    overlap_markers = matched_markers, 
                    overlap_occurrence = occurrence))
      }
      
      # Subset sparse matrix
      sub_mat <- sparse_mat[marker_panel, , drop = FALSE]
      
      # Use Matrix::summary for non-zero elements
      summ <- Matrix::summary(sub_mat)
      if (nrow(summ) == 0) {
        return(list(overlap_counts = overlaps, 
                    overlap_markers = matched_markers, 
                    overlap_occurrence = occurrence))
      }
      
      # Group by column index (j)
      col_indices <- summ$j
      row_indices <- summ$i
      values <- summ$x
      
      # Compute matched_markers: paste row names for each column
      marker_names <- rownames(sub_mat)[row_indices]
      markers_by_col <- split(marker_names, col_indices)
      matched_markers[as.integer(names(markers_by_col))] <- vapply(
        markers_by_col, 
        paste, collapse = "|", 
        FUN.VALUE = character(1)
      )
      
      # Compute overlaps: count markers per column
      overlaps[as.integer(names(markers_by_col))] <- lengths(markers_by_col)
      
      # Compute occurrence: sum values per column
      occurrence[as.integer(names(markers_by_col))] <- tapply(values, col_indices, sum, simplify = TRUE)
      
      return(list(overlap_counts = overlaps, 
                  overlap_markers = matched_markers, 
                  overlap_occurrence = occurrence))
    }
    
    #_______________
    
    # Scoring one set against the database. Extracted from the lapply() it used
    # to be so that `inherit_major_clusters` can call it TWICE for the same
    # object -- once for a parent major cluster against the whole database, and
    # once for that parent's sub-clusters against the narrowed one. The body is
    # unchanged apart from the `celltype_keep` filter, which sits with the
    # tissue and condition filters because it is the same kind of restriction:
    # it removes candidate columns before any score is computed.
    score_set <- function(curr_set, celltype_keep = NULL) {

      # Setting the marker panels, converting them to standard names, and setting their purity
      
      ## For Positive Markers
      
      ### Finding the marker panel
      pos_marker_panel <- combined_panels_list$pos_panels[[curr_set]]
      
      #_____________
      
      ### Setting the purities
      if(curr_set %in% names(combined_panels_purity_list$pos_panels) & !is.null(pos_marker_panel)) {
        pos_marker_purity_df <- data.frame(
          Input_Marker = marker_symbol_df_flt$Input_Marker[which(marker_symbol_df_flt$Input_Marker %in% pos_marker_panel)],
          Std_Symbol = marker_symbol_df_flt$Std_Symbol[which(marker_symbol_df_flt$Input_Marker %in% pos_marker_panel)]
          )
        pos_marker_purity_df <- pos_marker_purity_df %>% dplyr::mutate(
          Purity = combined_panels_purity_list$pos_panels[[curr_set]][match(Input_Marker, combined_panels_purity_list$pos_panels[[curr_set]][,"Feature"]),
                                                                      "Purity"]
        )
      } else if(!is.null(pos_marker_panel)) {
        pos_marker_purity_df <- data.frame(
          Input_Marker = marker_symbol_df_flt$Input_Marker[which(marker_symbol_df_flt$Input_Marker %in% pos_marker_panel)],
          Std_Symbol = marker_symbol_df_flt$Std_Symbol[which(marker_symbol_df_flt$Input_Marker %in% pos_marker_panel)],
          Purity = NA
          )
      } else {
        pos_marker_purity_df <- NULL
      }
      
      #_____________

      ### Converting names to standard names
      pos_marker_panel <- marker_symbol_df_flt$Std_Symbol[which(marker_symbol_df_flt$Input_Marker %in% pos_marker_panel)]
      
      if(length(pos_marker_panel) == 0) {
        pos_marker_panel <- NULL
      }
      
      #__________________________
      
      ## For Negative Markers
      
      ### Finding the marker panel
      neg_marker_panel <- combined_panels_list$neg_panels[[curr_set]]

      #_____________
      
      ### Setting the purities
      if(curr_set %in% names(combined_panels_purity_list$neg_panels) & !is.null(neg_marker_panel)) {
        neg_marker_purity_df <- data.frame(
          Input_Marker = marker_symbol_df_flt$Input_Marker[which(marker_symbol_df_flt$Input_Marker %in% neg_marker_panel)],
          Std_Symbol = marker_symbol_df_flt$Std_Symbol[which(marker_symbol_df_flt$Input_Marker %in% neg_marker_panel)]
        )
        
        neg_marker_purity_df <- neg_marker_purity_df %>% dplyr::mutate(
          Purity = combined_panels_purity_list$neg_panels[[curr_set]][match(Input_Marker, combined_panels_purity_list$neg_panels[[curr_set]][,"Feature"]),
                                                                      "Purity"]
        )
      } else if(!is.null(neg_marker_panel)) {
        neg_marker_purity_df <- data.frame(
          Input_Marker = marker_symbol_df_flt$Input_Marker[which(marker_symbol_df_flt$Input_Marker %in% neg_marker_panel)],
          Std_Symbol = marker_symbol_df_flt$Std_Symbol[which(marker_symbol_df_flt$Input_Marker %in% neg_marker_panel)],
          Purity = NA
        )
      } else {
        neg_marker_purity_df <- NULL
      }
      
      #_____________
      
      ### Converting names to standard names
      neg_marker_panel <- marker_symbol_df_flt$Std_Symbol[which(marker_symbol_df_flt$Input_Marker %in% neg_marker_panel)]
      
      if(length(neg_marker_panel) == 0) {
        neg_marker_panel <- NULL
      }
      
      #_____________
      
      # Check that at least one marker panel is specified
      if (is.null(pos_marker_panel) && is.null(neg_marker_panel)) {
        cli::cli_abort("At least one of pos_marker_panel or neg_marker_panel must be specified. There is something wrong with one of the desired sets!")
      }
      
      results_list <- list()
      
      #_______
      
      # Initialize column names
      reference_colnames <- NULL
      if (!is.null(pos_sparse_mat) & !is.null(neg_sparse_mat)) { 
        reference_colnames <- c(colnames(pos_sparse_mat), colnames(neg_sparse_mat)) %>% unique()
      } else if (!is.null(pos_sparse_mat)) {
        reference_colnames <- colnames(pos_sparse_mat)
      } else if (!is.null(neg_sparse_mat)) {
        reference_colnames <- colnames(neg_sparse_mat)
      } else cli::cli_abort("At least one of pos_sparse_mat or neg_sparse_mat must be specified!")
      
      df <- data.frame(
        Combo = reference_colnames,
        stringsAsFactors = FALSE
      )
      
      split_combo <- do.call(rbind, strsplit(as.character(df$Combo), "_"))
      df$Tissue <- split_combo[, 1]
      df$Condition <- split_combo[, 2]
      df$CellType <- split_combo[, 3]
      
      # Filter the df based on the user specified tissue type
      if(!is.null(tissue)) {
        df <- df %>% dplyr::filter(.data$Tissue %in% tissue)
      }
      
      # Filter the df based on the user specified condition type
      if(!is.null(condition)) {
        df <- df %>% dplyr::filter(.data$Condition %in% condition)
      }

      # Restrict to the parent major cluster's identity (inherit_major_clusters).
      # Applied here, before any overlap is computed, so the sub-cluster is
      # scored against the narrowed database rather than scored broadly and
      # trimmed afterwards.
      if (length(celltype_keep) && any(nzchar(celltype_keep))) {
        df <- df[celltype_contains(df$CellType, celltype_keep), , drop = FALSE]
      }

      ### Positive overlap
      if (!is.null(pos_marker_panel) && !is.null(pos_sparse_mat) && use_pos_markers) {
        pos_results <- process_matrix(sparse_mat = pos_sparse_mat, marker_panel = pos_marker_panel)
        df$Pos_Markers <- pos_results$overlap_markers[df$Combo]
        df$Pos_Markers[is.na(df$Pos_Markers)] <- ""
        # Pre-create named purity vector for fast lookup
        pos_purity_lookup <- setNames(pos_marker_purity_df$Purity, pos_marker_purity_df$Std_Symbol)
        
        df$Avg_Pos_Purity <- vapply(df$Pos_Markers, function(tmp_marker_set) {
          if (tmp_marker_set == "") return(NA_real_)
          markers <- strsplit(as.character(tmp_marker_set), "|", fixed = TRUE)[[1]]  # Drop unique if no dups expected
          mean(pos_purity_lookup[markers], na.rm = TRUE)
        }, FUN.VALUE = numeric(1))  
        df$Pos_Count <- pos_results$overlap_counts[df$Combo]
        df$Pos_Occurrence <- pos_results$overlap_occurrence[df$Combo]
      } else {
        df$Pos_Markers <- ""
        df$Avg_Pos_Purity <- NA
        df$Pos_Count <- 0
        df$Pos_Occurrence <- 0
      }
      
      ### Negative overlap
      if (!is.null(neg_marker_panel) && !is.null(neg_sparse_mat) && use_neg_markers) {
        neg_results <- process_matrix(neg_sparse_mat, neg_marker_panel)
        df$Neg_Markers <- neg_results$overlap_markers[df$Combo]
        df$Neg_Markers[is.na(df$Neg_Markers)] <- ""
        # Pre-create named purity vector for fast lookup
        neg_purity_lookup <- setNames(neg_marker_purity_df$Purity, neg_marker_purity_df$Std_Symbol)
        
        df$Avg_Neg_Purity <- vapply(df$Neg_Markers, function(tmp_marker_set) {
          if (tmp_marker_set == "") return(NA_real_)
          markers <- strsplit(as.character(tmp_marker_set), "|", fixed = TRUE)[[1]]  # Drop unique if no dups expected
          mean(neg_purity_lookup[markers], na.rm = TRUE)
        }, FUN.VALUE = numeric(1))
        df$Neg_Count <- neg_results$overlap_counts[df$Combo]
        df$Neg_Occurrence <- neg_results$overlap_occurrence[df$Combo]
      } else {
        df$Neg_Markers <- ""
        df$Avg_Neg_Purity <- NA
        df$Neg_Count <- 0
        df$Neg_Occurrence <- 0
      }
      
      # Wrong markers
      ## Positive markers wrongly found in neg matrix
      if (!is.null(pos_marker_panel) && !is.null(neg_sparse_mat) && use_pos_markers) {
        wrong_pos <- process_matrix(neg_sparse_mat, pos_marker_panel)
        df$Wrong_Positive_Markers <- wrong_pos$overlap_markers[df$Combo]
        df$Wrong_Positive_Markers[is.na(df$Wrong_Positive_Markers)] <- ""
        df$Avg_Wrong_Pos_Purity <- sapply(df$Wrong_Positive_Markers, function(tmp_marker_set) {
          if(tmp_marker_set == "") {
            return(NA)
          } else {
            pos_marker_purity_df$Purity[match(c(strsplit(as.character(tmp_marker_set), split = "\\|") %>% unlist() %>% unique()),
                                              pos_marker_purity_df$Std_Symbol)] %>% mean()
          }
        })  
        df$Wrong_Pos_Count <- wrong_pos$overlap_counts[df$Combo]
        df$Wrong_Pos_Occurrence <- wrong_pos$overlap_occurrence[df$Combo]
      } else {
        df$Wrong_Positive_Markers <- ""
        df$Avg_Wrong_Pos_Purity <- NA
        df$Wrong_Pos_Count <- 0
        df$Wrong_Pos_Occurrence <- 0
      }
      
      ## Negative markers wrongly found in pos matrix
      if (!is.null(neg_marker_panel) && !is.null(pos_sparse_mat) && use_neg_markers) {
        wrong_neg <- process_matrix(pos_sparse_mat, neg_marker_panel)
        df$Wrong_Negative_Markers <- wrong_neg$overlap_markers[df$Combo]
        df$Wrong_Negative_Markers[is.na(df$Wrong_Negative_Markers)] <- ""
        df$Avg_Wrong_Neg_Purity <- sapply(df$Wrong_Negative_Markers, function(tmp_marker_set) {
          if(tmp_marker_set == "") {
            return(NA)
          } else {
            neg_marker_purity_df$Purity[match(c(strsplit(as.character(tmp_marker_set), split = "\\|") %>% unlist() %>% unique()),
                                              neg_marker_purity_df$Std_Symbol)] %>% mean()
          }
        })  
        df$Wrong_Neg_Count <- wrong_neg$overlap_counts[df$Combo]
        df$Wrong_Neg_Occurrence <- wrong_neg$overlap_occurrence[df$Combo]
      } else {
        df$Wrong_Negative_Markers <- ""
        df$Avg_Wrong_Neg_Purity <- NA
        df$Wrong_Neg_Count <- 0
        df$Wrong_Neg_Occurrence <- 0
      }
      
      # Convert NAs to 0 (for non-purity columns)
      df[,-(grep("Purity", colnames(df)))][is.na(df[,-(grep("Purity", colnames(df)))])] <- 0
      
      # Final columns
      df <- df %>%
        dplyr::mutate(
          Combined_Markers = ifelse(Pos_Markers == "" & Neg_Markers == "", "",
                                    ifelse(Pos_Markers == "", Neg_Markers,
                                           ifelse(Neg_Markers == "", Pos_Markers,
                                                  paste(Pos_Markers, Neg_Markers, sep = "|")))),
          
          Combined_Count = Pos_Count + Neg_Count,
          
          Adjusted_Count = (Pos_Count * 4) * ifelse(is.na(Avg_Pos_Purity), 1, Avg_Pos_Purity) +
            Neg_Count * ifelse(is.na(Avg_Neg_Purity), 1, Avg_Neg_Purity) -
            (Wrong_Pos_Count * 4) * ifelse(is.na(Avg_Wrong_Pos_Purity), 1, Avg_Wrong_Pos_Purity) -
            (Wrong_Neg_Count * 2) * ifelse(is.na(Avg_Wrong_Neg_Purity), 1, Avg_Wrong_Neg_Purity),
          
          Adjusted_Occurrence = (Pos_Occurrence * 4) * ifelse(is.na(Avg_Pos_Purity), 1, Avg_Pos_Purity) +
            Neg_Occurrence * ifelse(is.na(Avg_Neg_Purity), 1, Avg_Neg_Purity) -
            (Wrong_Pos_Occurrence * 4) * ifelse(is.na(Avg_Wrong_Pos_Purity), 1, Avg_Wrong_Pos_Purity) -
            (Wrong_Neg_Occurrence * 2) * ifelse(is.na(Avg_Wrong_Neg_Purity), 1, Avg_Wrong_Neg_Purity),
          
          Combined_Score = special_multiply((Adjusted_Count^2) * sign(Adjusted_Count), log2(Adjusted_Occurrence + 1))
        ) %>%
        dplyr::filter(.data$Combined_Score != 0 & !is.na(.data$Combined_Score)) %>%
        dplyr::select(Tissue, Condition, CellType, 
                      Pos_Markers, Neg_Markers, Combined_Markers, 
                      Pos_Count, Neg_Count, Combined_Count,
                      Pos_Occurrence, Neg_Occurrence,
                      Avg_Pos_Purity, Avg_Neg_Purity, 
                      Wrong_Positive_Markers, Wrong_Negative_Markers,
                      Wrong_Pos_Count, Wrong_Neg_Count,
                      Wrong_Pos_Occurrence, Wrong_Neg_Occurrence,
                      Avg_Wrong_Pos_Purity, Avg_Wrong_Neg_Purity,
                      Adjusted_Count, Adjusted_Occurrence, Combined_Score) %>%
        dplyr::arrange(desc(Combined_Score), desc(Adjusted_Count), desc(Adjusted_Occurrence), desc(Pos_Count), desc(Neg_Count)) %>%
        dplyr::mutate(Rank = dplyr::row_number())
      
      if(nrow(df) > 0) {
        return(df)
      } else {
        # When the run was restricted to a parent's identity, say so and say
        # which -- otherwise the advice below ("relax tissue/condition") points
        # at the wrong knob, and the restriction is the one thing the user did
        # not choose explicitly.
        restricted_note <- if (length(celltype_keep) && any(nzchar(celltype_keep))) paste0(
          " Note that this set was annotated within its major cluster's identity ('",
          paste(celltype_keep, collapse = "' / '"),
          "'), so only cell types named as a variety of that were considered. ",
          "Lower `inherit_score_ratio` to admit more of the parent's candidates, or set ",
          "`inherit_major_clusters = FALSE` to annotate against the whole database instead.") else ""
        no_hit_msg <- paste0(
          "No specific cell type was identified for ", curr_set, " cell subset! ",
          "This may be because the cell subset is of low quality (e.g. it contains a mixture of cell types), ",
          "or because its corresponding cell type is not present in the specified `tissue` and/or `condition`. ",
          "If you specified the `tissue` and/or `condition` argument, try changing them or relaxing one or both ",
          "by setting them to NULL so that the cell subset is evaluated against the entire database. ",
          "To perform this relaxed analysis only for this cell subset, rerun `typoClust` with ",
          "`desired_sets = ", curr_set, "`.", restricted_note)
        log_warning(no_hit_msg)
        return(base::structure(no_hit_msg, class = "logMessage"))
      }
    }

    # Phase 1: every set that is NOT inheriting -- i.e. the major clusters, the
    # MarkoCell subsets, and (when inherit_major_clusters is FALSE) everything.
    # Sub-clusters are held back because their restriction is not known yet.
    inheriting <- combined_cell_set_names[
      combined_cell_set_names %in% names(parent_of)[!is.na(parent_of)]]
    first_pass <- setdiff(combined_cell_set_names, inheriting)

    set_celltype_list <- stats::setNames(
      lapply(first_pass, function(s) score_set(s, NULL)), first_pass)

    # Phase 2: each sub-cluster, against a database narrowed to ITS OWN parent's
    # rank-1 cell type. Per sub-cluster, never per run: one major cluster's
    # identity must not be able to constrain another major cluster's
    # sub-clusters, and doing the lookup inside this loop is what guarantees it.
    inheritance_rows <- list()
    if (length(inheriting)) {
      log_h2("Annotating sub-clusters within their major clusters")
      # The database's cell-type vocabulary, for reporting how much of it a
      # given parent restriction leaves.
      db_celltypes <- unique(do.call(rbind, strsplit(
        unique(c(colnames(pos_sparse_mat), colnames(neg_sparse_mat))), "_"))[, 3])
      for (s in inheriting) {
        p <- parent_of[[s]]
        p_res <- set_celltype_list[[p]]
        p_types <- parent_identities(p_res, inherit_score_ratio)
        p_type <- if (length(p_types)) p_types[1] else NA_character_

        if (is.na(p_type) || !nzchar(p_type)) {
          # The parent produced no confident label, so there is nothing to
          # inherit. Annotate normally rather than against an empty database,
          # and record that the inheritance did not happen.
          log_warning(paste0(
            "No cell type could be established for the major cluster '", p, "', so its sub-cluster '", s,
            "' was annotated against the whole database instead of within its parent's identity."))
          set_celltype_list[[s]] <- score_set(s, NULL)
          inheritance_rows[[length(inheritance_rows) + 1L]] <- data.frame(
            Set = s, Parent = p, Parent_CellType = NA_character_,
            Parent_CellTypes = NA_character_, N_Parent_CellTypes = 0L,
            Restricted = FALSE, stringsAsFactors = FALSE)
        } else {
          # State how much database the restriction actually leaves. This is
          # not cosmetic: the restriction is a NAME rule, so how much survives
          # depends entirely on whether the database names subtypes as
          # "<modifier> <parent>". A parent of "T Cell" leaves 206 cell types to
          # choose between; a parent of "Mononuclear Phagocyte" leaves 1, and
          # the sub-cluster can then only be given its parent's own label. Told
          # the count, a user can see immediately which of those happened.
          n_keep <- sum(celltype_contains(db_celltypes, p_types))
          extra <- if (length(p_types) > 1L)
            paste0(" (with near-tie", if (length(p_types) > 2L) "s" else "", " ",
                   paste0("'", p_types[-1], "'", collapse = ", "),
                   " at >= ", format(inherit_score_ratio), " of its score)") else ""
          log_message(paste0(
            "Annotating '", s, "' within its major cluster '", p, "' (", p_type, ")", extra,
            " - ", n_keep, " database cell type(s) named as a variety of ",
            if (length(p_types) > 1L) "those" else "it", "."))
          if (n_keep <= 1L) log_warning(paste0(
            "The database holds no named subtype of '", paste(p_types, collapse = "' / '"),
            "', so '", s, "' can only be labelled as '", p_type,
            "' itself. Lower `inherit_score_ratio` to admit more of the parent's candidates, ",
            "or set `inherit_major_clusters = FALSE` to use the whole database."))
          set_celltype_list[[s]] <- score_set(s, p_types)
          inheritance_rows[[length(inheritance_rows) + 1L]] <- data.frame(
            Set = s, Parent = p, Parent_CellType = p_type,
            Parent_CellTypes = paste(p_types, collapse = " | "),
            N_Parent_CellTypes = length(p_types),
            Restricted = TRUE, stringsAsFactors = FALSE)
        }
      }
    }

    # Restore the caller's ordering: phase 1 and phase 2 ran in a different
    # order than the sets were requested in, and the returned list should not
    # expose that as a reordering.
    set_celltype_list <- set_celltype_list[combined_cell_set_names]
    names(set_celltype_list) <- combined_cell_set_names

    #___________________________________
    
    # Preparing the Results Lists
    
    final_results_list <- list(
      cell_types = set_celltype_list,
      metadata = list(desired_sets = combined_cell_set_names,
                      marker_panels = combined_panels_list,
                      marker_symbol_df = marker_symbol_df,
                      # The provenance of every restricted annotation, so a
                      # reader can check WHICH parent constrained WHICH
                      # sub-cluster and what it was called. Kept out of the
                      # per-set data.frames deliberately: those are consumed by
                      # addTypoData() and typoClustVis(), which row-bind sets
                      # together, and adding columns to only some sets would
                      # break that bind.
                      inherit_major_clusters = isTRUE(inherit_major_clusters),
                      inheritance = if (length(inheritance_rows))
                        do.call(rbind, inheritance_rows) else NULL,
                      auto_added_parents = if (length(added_parents)) added_parents else NULL)
    )

  } else if(mode == "ceLLMarkup") {

    log_message("Setting the cell type annotation mode to ceLLMarkup!")

    # LLM-based annotation: hand the already-assembled marker panels to
    # ceLLMarkup(), which asks the configured LLM for ranked cell types per
    # set. The LLM connection is configured via the `llm_provider`, `llm_model`,
    # `llm_api_key`, and `llm_host` arguments (explicit, not inherited from the
    # agent session). The result is reshaped to the same final_results_list
    # structure as the markerDB branch so downstream functions work unchanged.
    if(is.null(sample_source)) {
      sample_source <- paste(
        c(if (!is.null(tissue)) paste(tissue, collapse = ", ") else NULL,
          if (!is.null(condition)) paste(condition, collapse = ", ") else NULL),
        collapse = " ")
    }

    llm_res <- ceLLMarkup(
      sample_source = if (nzchar(sample_source)) sample_source else NULL,
      feature_type = feature_type,
      panels = list(pos_panels = combined_panels_list$pos_panels %||% list(),
                    neg_panels = if (isTRUE(use_neg_markers)) combined_panels_list$neg_panels %||% list() else list()),
      tissue = if (!is.null(tissue)) paste(tissue, collapse = ", ") else NULL,
      condition = if (!is.null(condition)) paste(condition, collapse = ", ") else NULL,
      species = species,
      provider = llm_provider,
      model = llm_model,
      api_key = llm_api_key,
      host = llm_host,
      top_k = llm_top_k,
      n_markers = thresh,
      # The hierarchy travels as the panel NAMES ("C1" and "C1-Sub1"), which
      # ceLLMarkup re-derives for itself; the parent panels were retained above
      # so they are present for it to find.
      inherit_major_clusters = inherit_major_clusters,
      verbose = verbose
    )

    # ceLLMarkup already returns TypoClust-shaped cell_types; keep them and
    # attach the same metadata the markerDB branch produces.
    final_results_list <- list(
      cell_types = llm_res$cell_types,
      metadata = list(desired_sets = combined_cell_set_names,
                      marker_panels = combined_panels_list,
                      marker_symbol_df = marker_symbol_df,
                      ann_method = "ceLLMarkup",
                      inherit_major_clusters = isTRUE(inherit_major_clusters),
                      inheritance = llm_res$metadata$inheritance,
                      auto_added_parents = if (length(added_parents)) added_parents else NULL)
    )

  } else {
    cli::cli_abort("The `mode` argument should be either 'markerDB', or 'ceLLMarkup'!")
  }
  
  #___________________________________
  
  if(verbose) {
    log_space()
    cli::cli_rule(left = cli::col_green("SUCCESS"), right = cli::col_silver(Sys.time()))
    cli::cli_alert_success(cli::style_italic(cli::style_bold("TypoClust finished successfully!")))
  }
  
  # Return results
  structure(final_results_list,
            class = "TypoClust")
}
