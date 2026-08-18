# =============================================================================
# CelliVerse Agent - CORE tool definitions (12 first-class tools)
#
# Each tool is pinned to the REAL formals() of the corresponding CelliVerse
# function (verified from the package source). For each tool we expose a curated
# set of "primary" arguments richly to the LLM (the ones that change biology /
# behaviour); the long tail of styling/tuning args keeps its real defaults and
# is passed through (settable via Tool Inspector) so the LLM prompt stays lean.
#
# Handlers receive (store, args) where `args` already has handles resolved to
# strings; the handler pulls the actual object from the store, calls the
# CelliVerse function, stores any object output, and returns a result record:
#   list(kind=, handle=, descriptor=, table=, plot_object=, text=, ...)
# =============================================================================

# Helper: pull resolved handle args into real objects, keep scalars as-is.
.cv_materialize <- function(store, args, handle_args) {
  for (nm in handle_args) args[[nm]] <- cv_object_get(store, args[[nm]])
  args
}

# Helper: capture the input handle(s) of a tool call BEFORE .cv_materialize
# replaces them with the objects themselves. Covers single-handle params
# (named in `handle_args`) and array-of-handle params (e.g. typoClust's
# `objects`, which stay character vectors in args). Used to give the result
# object a handle that inherits the input's base name.
.cv_input_handles <- function(tool, args, handle_args) {
  hs <- as.character(unlist(args[intersect(names(args), handle_args)], use.names = FALSE))
  spec <- tool$parameters %||% list()
  for (nm in names(spec)) {
    p <- spec[[nm]]
    if (identical(p$type, "array") && length(p$handle_types) && !is.null(args[[nm]])) {
      hs <- c(hs, as.character(unlist(args[[nm]], use.names = FALSE)))
    }
  }
  hs[nzchar(hs)]
}

# Helper: materialize ARRAY-of-handle params (e.g. typoClust's `objects`) from a
# character vector of handle strings into a LIST of the actual objects, driven
# by the tool's parameter spec. cv_resolve_args validates the handles and leaves
# them as strings; this turns them into objects just before the CelliVerse call.
# Used by BOTH the heavy worker (cv_launch_heavy) and the light handlers so the
# two execution paths stay consistent.
.cv_materialize_array_handles <- function(store, tool, args) {
  spec <- tool$parameters
  for (nm in names(spec)) {
    p <- spec[[nm]]
    if (identical(p$type, "array") && length(p$handle_types) && !is.null(args[[nm]])) {
      args[[nm]] <- lapply(as.character(args[[nm]]),
                           function(h) cv_object_get(store, h))
    }
  }
  args
}

# Normalise typoClust's `tissue` / `condition` args against the Marker DB
# vocabulary. The DB stores these in TITLE CASE (e.g. "Blood", "Healthy",
# "Lung"), and celliverse::typoClust() validates them CASE-SENSITIVELY. The
# reported bug: the model passed a lowercase value ("blood") and typoClust
# aborted with the opaque "The `tissue` argument is incorrect" on EVERY retry,
# then the repeat-call guard trapped it. This shim makes the agent path
# forgiving: trim, then match each value CASE-INSENSITIVELY against the real
# vocab and rewrite it to the vocab's exact (correctly-cased) string, so
# "blood"/"BLOOD"/" blood " all resolve to "Blood". On a genuine no-match it
# aborts listing the VALID values so the model can self-correct instead of
# looping. Returns the (possibly modified) args; leaves NULL/empty
# tissue/condition untouched (typoClust default = all).
.cv_normalize_tissue_condition <- function(args) {
  sp <- args$species %||% "human"
  tc <- tryCatch({
    e <- new.env(); utils::data("tissueCondition_types", package = "celliverse", envir = e)
    get("tissueCondition_types", envir = e)
  }, error = function(err) NULL)
  norm_one <- function(vals, valid, label) {
    if (is.null(vals)) return(NULL)
    vals <- trimws(as.character(vals))
    vals <- vals[nzchar(vals)]
    if (!length(vals)) return(NULL)
    if (is.null(valid) || !length(valid)) return(vals)  # vocab unavailable: pass through
    # Case-insensitive match -> rewrite to the vocab's exact casing.
    li <- match(tolower(vals), tolower(valid))
    bad <- vals[is.na(li)]
    if (length(bad)) {
      # Lead with the most common query terms (Blood/Brain/Lung/Skin/...) so the
      # model can self-correct the frequent case, then a short alpha sample.
      # Kept compact so cli does not truncate away the common values.
      common <- intersect(c("Blood", "Brain", "Lung", "Skin", "Heart", "Liver",
                            "Kidney", "Bone Marrow", "Healthy"), valid)
      sample_vals <- unique(c(common, utils::head(valid, 12)))
      cli::cli_abort(c(
        "Invalid {label} value{?s}: {.val {bad}}.",
        "i" = "Valid {label} values for {sp} include: {.val {sample_vals}}, ... ({length(valid)} total).",
        "i" = "Call the 'tissue_condition_vocab' tool to see them all, or omit '{label}' to use all."
      ))
    }
    valid[li]
  }
  valid_tissue    <- if (!is.null(tc) && !is.null(tc[[sp]])) tc[[sp]]$all_tissues    else NULL
  valid_condition <- if (!is.null(tc) && !is.null(tc[[sp]])) tc[[sp]]$all_conditions else NULL
  if (!is.null(args$tissue))    args$tissue    <- norm_one(args$tissue,    valid_tissue,    "tissue")
  if (!is.null(args$condition)) args$condition <- norm_one(args$condition, valid_condition, "condition")
  args
}

#' Raise a standing note ONCE per session, not once per call.
#'
#' Round LXXX (audit #89). `result_note` fired on every successful run of the
#' tool that declares it, and exactly one tool does: typoClust's note saying the
#' Marker DB was used and that an LLM alternative exists. Annotating six
#' sub-clusters in a row printed that same paragraph six times, which is how a
#' reader learns to skip the notes panel entirely -- the same failure Round LXIX
#' invoked when it capped the severity scale at two levels.
#'
#' The flag lives as an attribute on the STORE, which is the session's own
#' environment, using the pattern `cv_object_next_seq()` already established for
#' the monotonic counter. A NEW session gets a fresh store and therefore hears
#' the note again, which is right: it is orientation for someone who has just
#' arrived, not a caveat about a particular result.
#'
#' Keyed on the note's CODE, deliberately, not on its text -- so re-wording the
#' note does not silently make it start repeating again.
#'
#' Returns `list(cv_warn(...))` the first time and `NULL` afterwards, so callers
#' splice it straight into cv_result_add_warnings(). With no store (unit tests,
#' programmatic callers) it behaves exactly as it always did.
#' @noRd
.cv_session_note_once <- function(store, code, text) {
  if (is.null(text) || !nzchar(text)) return(NULL)
  if (!is.environment(store)) return(list(cv_warn("info", text, code)))
  seen <- attr(store, "cv_notes_said") %||% character(0)
  if (code %in% seen) return(NULL)
  attr(store, "cv_notes_said") <- c(seen, code)
  list(cv_warn("info", text, code))
}

# Standing note appended to a typoClust result: it states that the curated
# CelliVerse Marker DB was used AND advertises the LLM-based alternative
# (annotateCellsLLM / ceLLMarkup) with a concrete how-to-prompt. Single source
# of truth shared by the tool's result_note and the inline handler.
.cv_typoclust_llm_note <- function() {
  paste(
    "Annotation used the curated CelliVerse Marker DB (mode='markerDB', the",
    "default).",
    "Alternative: for an LLM-based annotation of the same set(s), call the",
    "'annotateCellsLLM' tool (the ceLLMarkup method) on the same object -",
    "e.g. ask 'annotate that cluster with the LLM' or 'use ceLLMarkup to label",
    "C1-Sub1'. Use the LLM route only when you explicitly ask for it.")
}

#' Advisory cross-tissue warning for an UNFILTERED markerDB typoClust run.
#'
#' Round XIX: with tissue=NULL, typoClust searches every tissue in the Marker DB
#' and a spurious high-overlap entry from an irrelevant tissue can out-score the
#' biologically correct one (pbmc3k C3 -> "NK Cell (Pronephros/Healthy)" instead
#' of "T Cell (Blood/Healthy)"). When the run was NOT tissue-filtered and one or
#' more sets' rank-1 tissue differs from the majority tissue across all sets,
#' append an advisory naming the outlier set(s), their tissue, the majority
#' tissue, and the fix (re-run with an explicit tissue=). Advisory only - it
#' never overrides the top hit. Returns "" when there is nothing to warn about
#' (single tissue, or a tissue-filtered run).
#' Advisory for a run where the user ASSERTED a tissue (audit #15).
#'
#' Round LXV Batch 2b. When `tissue` is set, every markerDB hit comes from that
#' tissue by construction, so the cross-tissue outlier check is structurally
#' unable to fire -- which is why the advisory was previously switched off
#' entirely for this case. That left the most likely user error completely
#' unflagged: asserting "Brain" for a PBMC dataset.
#'
#' What is checkable here is whether the asserted tissue was PRODUCTIVE. A
#' tissue with no useful entries for these markers yields Unknown/empty labels
#' for most sets, and the asserted tissue is then the first thing to doubt --
#' well before the biology.
#'
#' Advisory only. It never overrides the result, and it stays silent whenever
#' the run produced real labels, because a warning that fires on good runs is
#' how users learn to ignore warnings.
#' @noRd
.cv_typoclust_explicit_tissue_warning <- function(res, tissue) {
  ct <- tryCatch(res$cell_types, error = function(e) NULL)
  if (is.null(ct) || !length(ct)) return("")
  lab <- vapply(names(ct), function(s) {
    df <- ct[[s]]
    if (is.null(df) || !is.data.frame(df) || !nrow(df) || !("CellType" %in% names(df)))
      return(NA_character_)
    as.character(df$CellType[1])
  }, character(1))
  useless <- is.na(lab) | !nzchar(lab) | tolower(lab) %in% c("unknown", "unassigned", "na")
  n <- length(lab)
  if (n < 1L) return("")
  # Only speak up when MOST sets came back empty: one Unknown among several is
  # ordinary, and flagging it would be noise.
  if (sum(useless) < ceiling(n * 0.6)) return("")
  paste0(
    "NOTE (tissue may not fit): this run was restricted to tissue='", tissue, "', ",
    "and ", sum(useless), " of ", n, " set(s) came back without a confident label. ",
    "If these cells are not ", tissue, ", that restriction is the likely cause - ",
    "re-run with a different tissue, or with 'All (no filter)' to search the whole ",
    "database.")
}

#' @noRd
.cv_typoclust_tissue_warning <- function(res, tissue_arg) {
  # Round LXV Batch 2b (audit #15): this used to return early whenever a tissue
  # WAS given -- i.e. the advisory was switched off in exactly the case the user
  # is most likely to be wrong.
  #
  # The two situations need different messages, so they are handled separately
  # rather than by widening one:
  #   tissue = NULL  -> the search ranged over all tissues and the top hit for
  #                     some set came from a minority tissue. Advisory below.
  #   tissue = "X"   -> the user asserted the tissue. Every hit is from X by
  #                     construction, so the majority-outlier logic below cannot
  #                     fire at all. What CAN be checked is whether X was a
  #                     productive choice: if the labels came back empty or
  #                     Unknown for most sets, the asserted tissue is the first
  #                     thing to doubt.
  explicit <- !is.null(tissue_arg) && length(tissue_arg) &&
              nzchar(as.character(tissue_arg)[1])
  if (explicit) return(.cv_typoclust_explicit_tissue_warning(res, as.character(tissue_arg)[1]))
  ct <- tryCatch(res$cell_types, error = function(e) NULL)
  if (is.null(ct) || !length(ct)) return("")
  # rank-1 tissue per set
  r1_tissue <- vapply(names(ct), function(s) {
    df <- ct[[s]]
    if (is.null(df) || !is.data.frame(df) || !nrow(df) || !("Tissue" %in% names(df)))
      return(NA_character_)
    # Round LV (Batch 5a): shared with cv_typoclust_top_labels()
    # (agent_object_store.R), which held a byte-identical copy of this idiom.
    # The guard above differs between the two on purpose - that one needs a
    # CellType column, this one a Tissue column - but the rank-1 extraction is
    # the same thing and now has exactly one definition.
    r1 <- .cv_typoclust_rank1_row(df)
    as.character(r1$Tissue[1] %||% NA_character_)
  }, character(1))
  r1_tissue <- r1_tissue[!is.na(r1_tissue) & nzchar(r1_tissue)]
  if (length(r1_tissue) < 2L) return("")            # need >=2 sets to compare
  uniq <- unique(r1_tissue)
  if (length(uniq) < 2L) return("")                 # all same tissue -> fine
  # majority (modal) tissue
  tab <- sort(table(r1_tissue), decreasing = TRUE)
  majority <- names(tab)[1]
  outliers <- names(r1_tissue)[r1_tissue != majority]
  if (!length(outliers)) return("")
  # For each outlier, find the best cell type under the majority tissue (rank>1
  # rows of the same set) to offer a concrete alternative.
  alt <- vapply(outliers, function(s) {
    df <- ct[[s]]
    alt_rows <- df[!is.na(df$Tissue) & df$Tissue == majority, , drop = FALSE]
    if (nrow(alt_rows) && "CellType" %in% names(alt_rows)) {
      as.character(alt_rows$CellType[1])
    } else NA_character_
  }, character(1))
  frag <- vapply(seq_along(outliers), function(i) {
    s <- outliers[i]
    a <- alt[i]
    if (!is.na(a) && nzchar(a)) sprintf("%s (%s; under %s: %s)", s, r1_tissue[s], majority, a)
    else sprintf("%s (%s)", s, r1_tissue[s])
  }, character(1))
  paste0(
    "NOTE (cross-tissue match): this run searched ALL tissues (tissue=NULL), and ",
    "the top hit for ", paste(frag, collapse = "; "), " came from a different tissue ",
    "than the majority (", majority, "). A cross-tissue match can be spurious. ",
    "If these cells are expected to be ", majority, ", re-run with an explicit tissue ",
    "(e.g. tissue='", majority, "') to restrict the search.")
}

# Helper: colorblind-safe discrete palette of length n.
# Uses the Okabe-Ito palette (8 distinguishable colors) and cycles / falls back
# to scales::hue_pal() for larger n so cluster UMAPs stay readable for CVD users.
cv_discrete_palette <- function(n) {
  n <- max(as.integer(n), 1L)
  okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
                 "#0072B2", "#D55E00", "#CC79A7", "#000000")
  if (n <= length(okabe_ito)) return(okabe_ito[seq_len(n)])
  # For many groups, generate a broader evenly-spaced hue palette.
  scales::hue_pal()(n)
}

# Helper: build a standard "object produced" result record (NEW handle).
# `inherit_from` = input handle(s) the new object was derived from; the result
# handle then inherits the input's base name (obj_pbmc3k -> clusto_pbmc3k),
# with _2/_3 suffixes on collision. Empty -> historical random id.
.cv_result_object <- function(store, value, source, produces = "object", inherit_from = NULL) {
  handle <- cv_object_put(store, value,
                          handle = cv_derived_handle(store, value, inherit_from),
                          source = source)
  list(
    kind = produces,
    handle = handle,
    descriptor = cv_object_descriptor(store, handle),
    text = sprintf("Created %s (handle: %s).",
                   cv_object_descriptor(store, handle)$summary, handle)
  )
}

# Helper: build a result record for an IN-PLACE update (SAME handle, no
# duplicate). Used by addClustoData / addTypoData, which add metadata columns to
# an object the user already loaded. `handle` is the existing handle of the
# object that was updated (captured from args BEFORE .cv_materialize replaces it
# with the object itself).
.cv_result_object_inplace <- function(store, handle, value, source, produces = "object") {
  cv_object_update(store, handle, value, source = source)
  list(
    kind = produces,
    handle = handle,
    descriptor = cv_object_descriptor(store, handle),
    text = sprintf("Updated %s in place (handle: %s; no duplicate created).",
                   cv_object_descriptor(store, handle)$summary, handle)
  )
}

# Normalise the `desired_cells` argument of markoCell / markerPurity so it also
# accepts a CellSet produced by get_cluster_cells. A CellSet may arrive as:
#   * a bare handle string (e.g. "cellset_ab12cd") -> look it up in the store
#   * the materialised CellSet object itself (if the caller passed a handle in a
#     handle-typed slot) -> use directly
# In both cases it becomes list(<cellset name or auto>=<barcode vector>), which
# is exactly the named-list shape celliverse::markoCell/markerPurity expect. Any
# other value (already a named list of barcodes) is returned unchanged. This lets
# a user say "run markoCell on that subset" and just reference the CellSet.
#' Pre-dispatch preparation shared by markoCell and markerPurity.
#'
#' Round LXIV (D1). Everything here used to live inside each tool's `handler`,
#' which cv_make_dispatcher() invokes ONLY for a light tool. Both of these tools
#' are `cost = "heavy"`, so in production none of it ran: a CellSet handle
#' reached `celliverse::markoCell` as the raw string `"cellset_ab12cd"` instead
#' of the named barcode list it requires, and aborted -- breaking the very
#' hand-off the system prompt instructs the model to use ("pass the CellSet
#' handle as desired_cells - do NOT re-list the barcodes"). The subset guard,
#' which is the most useful mis-routing correction in the layer because it names
#' `getClusterMarkers` as the right tool, was dead for the same reason.
#'
#' It went unnoticed because every test in test-markocell-guard.R calls
#' `tool$handler(store, args)` directly, so the suite exercised a path
#' production never takes. Tests that reach through cv_make_dispatcher() are in
#' test-round64-batch1b-safe.R.
#'
#' MUST run in the parent process: the object store lives there, and a CellSet
#' handle cannot be resolved anywhere else.
#'
#' @param store Session object store.
#' @param tool The `cv_tool` being dispatched.
#' @param args Resolved call arguments.
#' @param handle_args Names of the handle-typed arguments.
#' @param require_subset Abort when no cell subset is given (markoCell only;
#'   markerPurity can legitimately run on whole clusters).
#' @return `list(args = <args with desired_cells expanded>, inherit_from = <chr>)`.
#' @noRd
.cv_prepare_cell_subset <- function(store, tool, args, handle_args,
                                    require_subset = FALSE) {
  dc_raw <- args$desired_cells
  args$desired_cells <- .cv_expand_desired_cells(store, args$desired_cells)

  if (isTRUE(require_subset)) {
    has_cells <- !is.null(args$desired_cells) && length(args$desired_cells) > 0L
    has_clusters <- !is.null(args$cluster_labels) && nzchar(args$cluster_labels %||% "") &&
                    !is.null(args$desired_clusters) && length(args$desired_clusters) > 0L
    if (!has_cells && !has_clusters) {
      cli::cli_abort(c(
        "markoCell needs a cell subset to analyse, but none was given.",
        i = "Provide {.arg desired_cells} (a named list of cell-barcode vectors, or a CellSet handle from get_cluster_cells), OR both {.arg cluster_labels} and {.arg desired_clusters}.",
        i = "If you just want the markers of an EXISTING cluster (e.g. the top markers of C1), use {.fn getClusterMarkers} on the ClustoCell object instead - markoCell is only for a NEW user-defined subset."
      ))
    }
  }

  inh <- .cv_input_handles(tool, args, handle_args)
  inh <- .cv_subset_inherit(store, dc_raw, args$desired_cells, inh)
  list(args = args, inherit_from = inh)
}

# ---- Degenerate clustering, said out loud (Round LXXIV, audit #16) ----------
#
# Two shapes a clustering can take that look completely ordinary in the summary
# line and mean the run is not usable as it stands. Both were approved by the
# user with his own wording attached, which is preserved here because it changes
# the severity of one of them:
#
#   * ONE major cluster. Flag it -- but the user's note is that "a multi-cell
#     dataset may genuinely have only a single cell type", so this is INFO, not
#     amber. It is a fact worth surfacing, not a verdict on the run.
#   * MOST cells isolated. clustoCell sets aside cells it cannot connect, plus
#     any major cluster below `isolated_cluster_thresh` (5 by default). When
#     that is more than half the data, the clusters that remain describe a
#     minority of the cells -- so the numbers on screen answer a much smaller
#     question than they appear to. MAY_INVALIDATE, and the message says what
#     the user said it should: the data may be low quality, or genuinely very
#     heterogeneous.
#' @noRd
.cv_clustering_warnings <- function(value) {
  d <- tryCatch(cv_describe_object(value), error = function(e) NULL)
  if (!is.list(d) || !identical(d$type, "ClustoCell")) return(list())
  ws <- list()
  n_major <- suppressWarnings(as.integer(d$n_major_clusters %||% NA))
  n_cells <- suppressWarnings(as.integer(d$n_cells %||% NA))
  n_iso   <- suppressWarnings(as.integer(d$n_isolated %||% NA))

  if (!is.na(n_major) && n_major == 1L && !is.na(n_cells) && n_cells > 1L)
    ws <- c(ws, list(cv_warn("info", sprintf(paste0(
      "Every one of the %s cells fell into a single major cluster. That can be real -- a ",
      "dataset may genuinely hold one cell type -- but it is also what an over-coarse ",
      "clustering looks like. If you expected more populations, try a higher ",
      "leiden_resolution."), format(n_cells, big.mark = ",")), "clustering_single_cluster")))

  # Denominator is the cells that went IN: n_cells counts the clustered ones, so
  # the isolated cells have to be added back or the fraction is understated
  # exactly when it matters most.
  if (!is.na(n_iso) && n_iso > 0L && !is.na(n_cells)) {
    total <- n_cells + n_iso
    if (total > 0L && (n_iso / total) > 0.5)
      ws <- c(ws, list(cv_warn("may_invalidate", sprintf(paste0(
        "%s of %s cells (%.0f%%) were set aside as isolated rather than placed in a cluster ",
        "-- by default any cell that could not be connected, plus any cluster smaller than 5 ",
        "cells. The clusters below therefore describe a minority of the data. This usually ",
        "means the data is of low quality, or that it is genuinely very heterogeneous."),
        format(n_iso, big.mark = ","), format(total, big.mark = ","),
        100 * n_iso / total), "clustering_mostly_isolated")))
  }
  ws
}

# ---- Cell-count floor (Round LXXIV, audit #14) ------------------------------
#
# markoCell and markerPurity on a very small cell subset return markers with the
# maximum confidence the metric can express, on data that contains nothing.
# Measured on a 300x300 matrix of pure Poisson noise -- no structure at all, so
# there is no true marker to find:
#
#   subset size   rank-1 "markers"   max Purity
#        2              20             1.000
#        3              11             1.000
#        5              11             0.800
#       10               3             0.700
#       20               2             0.600
#       50               2             0.480
#
# At n <= 3 the metric reports its maximum possible value on noise. The user
# chose WARN-and-proceed over refusing: a 3-cell subset is sometimes exactly
# what somebody wants to look at, and a hard gate here would be the
# "are you sure?" the audit itself warns against (3b#4).
#
# MAY_INVALIDATE, and the Round LXIX question answers itself here: the numbers on
# screen are a Purity of 1.00 and a confident ranked list. A reader who skips
# this draws precisely the wrong conclusion from them.
CV_MIN_SUBSET_CELLS <- 10L

#' @noRd
.cv_warn_small_subset <- function(store, args, warnings, tool_name) {
  dc <- tryCatch(.cv_expand_desired_cells(store, args$desired_cells),
                 error = function(e) NULL)
  if (is.null(dc)) return(invisible(NULL))
  sizes <- if (is.list(dc)) vapply(dc, function(v) length(unique(as.character(v))), integer(1))
           else length(unique(as.character(dc)))
  sizes <- sizes[!is.na(sizes) & sizes > 0L]
  if (!length(sizes)) return(invisible(NULL))
  small <- sizes[sizes < CV_MIN_SUBSET_CELLS]
  if (!length(small)) return(invisible(NULL))
  nm <- names(small); if (is.null(nm) || !any(nzchar(nm))) nm <- rep("", length(small))
  who <- paste(ifelse(nzchar(nm), sprintf("%s (%d cell%s)", nm, small, ifelse(small == 1L, "", "s")),
                      sprintf("%d cell%s", small, ifelse(small == 1L, "", "s"))),
               collapse = "; ")
  cv_warn_add(warnings, "may_invalidate", sprintf(paste0(
    "%s was run on a very small cell subset: %s. Below about %d cells the purity and Gini ",
    "scores reach their maximum on data with no real structure in it, so a confident-looking ",
    "score here is not evidence of one. Treat these results as exploratory, or widen the ",
    "subset."),
    tool_name, who, CV_MIN_SUBSET_CELLS), code = "subset_too_small")
}

# ---- Pre-dispatch validation (Round LXX, audit #12 / #13) -------------------
#
# Three checks that share one shape: a condition the callee ALREADY refuses,
# moved in front of the worker spawn and given a message the model can act on.
# None of them refuses anything the product accepts today -- each refuses a
# strict subset of what already fails, which is why they ship in the safe half.
#
# They read the store's DESCRIPTOR rather than the object itself. The
# descriptor is computed once, at cv_object_put(), and already carries
# `metadata_cols` and `n_cells` for every Seurat/SCE; going through it means
# there is no second derivation of "what columns does this object have" to
# drift from cv_describe_seurat(), and it costs nothing to re-run on every poll
# of a suspended heavy job.

#' The descriptor behind a handle-typed argument, or NULL when there isn't one.
#'
#' Returns NULL rather than aborting for every "cannot tell" case -- an absent
#' argument, a handle that is not in the store, a type with no descriptor. A
#' validator that cannot see the object must stay silent: refusing on missing
#' information would turn a diagnostic into a new failure mode.
#' @noRd
.cv_arg_descriptor <- function(store, args, data_arg) {
  h <- args[[data_arg]]
  if (!is.character(h) || length(h) != 1L || is.na(h) || !nzchar(h)) return(NULL)
  tryCatch(cv_object_descriptor(store, h), error = function(e) NULL)
}

#' Refuse a metadata column that is not on the object (audit #12).
#'
#' `cluster_labels` was carried into a heavy job unchecked by markoClust,
#' markoCell and markerPurity alike. All three abort eventually -- via Seurat's
#' own `[[`, which says the column is "not found in this Seurat object" and
#' does NOT list the ones that are. So the model saw a failure it could not
#' correct without a second round-trip through get_metadata_columns, and the
#' user waited for a worker to start and die to get there.
#'
#' Silent when there is nothing to check against: a matrix input has no
#' metadata (markoCell then reads `cluster_labels` as a per-cell vector, a shape
#' the agent schema cannot even express), and an empty `metadata_cols` means the
#' descriptor failed rather than that the object has no columns.
#' @noRd
.cv_assert_metadata_column <- function(store, args, col_arg, data_arg) {
  col <- args[[col_arg]]
  if (!is.character(col) || length(col) != 1L || is.na(col) || !nzchar(col)) return(invisible(NULL))
  d <- .cv_arg_descriptor(store, args, data_arg)
  if (is.null(d)) return(invisible(NULL))
  cols <- d$metadata_cols
  if (is.null(cols) || !length(cols)) return(invisible(NULL))
  if (col %in% cols) return(invisible(NULL))
  cli::cli_abort(c(
    "{.arg {col_arg}} names {.val {col}}, which is not a metadata column of this object.",
    i = "Available columns: {.val {cols}}.",
    i = paste0("If you expected cluster labels here, run {.fn addClustoData} ",
               "(or {.fn addTypoData} for cell types) to write them onto the object first.")
  ))
}

#' Column names `addClustoData` writes by default.
#' @noRd
CV_CLUSTO_DEFAULT_COLS <- c("ClustoCell_Clusters", "ClustoCell_SubClusters")

#' Write the missing ClustoCell labels rather than refusing the plot.
#'
#' Round LXXXI (D2), from live use. The user asked for "a umap of the object
#' coloured by subclusters". `umapPlot` refused, correctly, because the Seurat
#' object did not carry `ClustoCell_SubClusters` yet -- and then the turn spent
#' six steps and two further tool calls recovering from a prerequisite that was
#' one unambiguous call away.
#'
#' The refusal was RIGHT and the outcome was still bad. When the missing column
#' is one of the two names `addClustoData` writes by default, and EXACTLY ONE
#' ClustoCell is loaded, there is nothing to decide: run it and say so.
#'
#' WHAT IT WILL NOT DO, and why each boundary is where it is:
#'   * Only the two DEFAULT column names. A user who wrote their labels under a
#'     custom name has made a choice, and guessing which object it came from
#'     would be a guess.
#'   * Only when exactly one ClustoCell is loaded. Two means a decision, and
#'     ask-when-ambiguous is this project's standing rule.
#'   * Only when the object does not already have the column -- this never
#'     overwrites anything.
#'   * It writes ONLY the column that was asked for, not both.
#'
#' It is a NOTICE, never a gate: audit category 3b item 4 ruled that a
#' confirmation prompt on addClustoData is the wrong answer and a notice is the
#' right one. The user is told, in the same card, that the labels were added.
#'
#' Best effort by construction. Any failure returns quietly and leaves
#' `.cv_assert_metadata_column()` to raise its own actionable error, so this can
#' only ever turn a failure into a success, never a success into a failure.
#' @return invisibly TRUE when labels were written.
#' @noRd
.cv_autofill_cluster_labels <- function(store, args, warnings,
                                        col_arg = "group_by", data_arg = "seurat_obj") {
  tryCatch({
    col <- args[[col_arg]]
    if (!is.character(col) || length(col) != 1L || is.na(col) || !nzchar(col))
      return(invisible(FALSE))
    if (!(col %in% CV_CLUSTO_DEFAULT_COLS)) return(invisible(FALSE))
    h <- args[[data_arg]]
    if (!is.character(h) || length(h) != 1L || is.na(h) || !nzchar(h))
      return(invisible(FALSE))
    d <- tryCatch(cv_object_descriptor(store, h), error = function(e) NULL)
    if (is.null(d) || !length(d$metadata_cols)) return(invisible(FALSE))
    if (col %in% d$metadata_cols) return(invisible(FALSE))
    ccs <- tryCatch(cv_objects_of_type(store, "ClustoCell"), error = function(e) character(0))
    if (length(ccs) != 1L) return(invisible(FALSE))

    obj <- cv_object_get(store, h)
    cc  <- cv_object_get(store, ccs[[1]])
    if (is.null(obj) || is.null(cc)) return(invisible(FALSE))
    want_sub <- identical(col, "ClustoCell_SubClusters")
    res <- celliverse::addClustoData(
      obj = obj, clustoCell = cc,
      add_major_clusters = !want_sub, add_sub_clusters = want_sub,
      major_cluster_name = "ClustoCell_Clusters",
      sub_cluster_name   = "ClustoCell_SubClusters")
    cv_object_update(store, h, res, source = "addClustoData()")

    # CONFIRM IT ACTUALLY LANDED, AND THAT IT LANDED AS DATA. The
    # anti-fabrication rule applies to the R-authored strings too, not only to
    # the model's prose.
    #
    # The existence check alone is not enough, and break-verification is what
    # showed it. `addClustoData()` creates the column FIRST and then fills it by
    # matching barcodes, so a ClustoCell with no sub-clusters at all -- which is
    # exactly what `clustoCell(identify_subclusters = FALSE)` produces -- yields
    # a column that is present and entirely NA. The autofill then reported
    # success, and the UMAP the user asked for would have been drawn coloured by
    # nothing: strictly worse than the honest refusal they got before this
    # function existed.
    #
    # So a column of all-NA is treated as NOT landing. The object is put back
    # the way it was and FALSE is returned, which leaves
    # `.cv_assert_metadata_column()` to raise its own actionable error listing
    # the columns that do exist.
    landed <- tryCatch({
      obj2 <- cv_object_get(store, h)
      v <- obj2@meta.data[[col]]
      !is.null(v) && any(!is.na(v))
    }, error = function(e) FALSE)
    d2 <- tryCatch(cv_object_descriptor(store, h), error = function(e) NULL)
    if (!isTRUE(landed) || is.null(d2) ||
        !(col %in% (d2$metadata_cols %||% character(0)))) {
      tryCatch(cv_object_update(store, h, obj, source = "addClustoData() (rolled back)"),
               error = function(e) NULL)
      return(invisible(FALSE))
    }
    cv_warn_add(warnings, "info", sprintf(paste0(
      "'%s' was not on %s yet, so I added it from the one loaded ClustoCell ",
      "object (%s) before plotting. The object was updated in place - same ",
      "handle, no duplicate."), col, h, ccs[[1]]), "autofilled_cluster_labels")
    invisible(TRUE)
  }, error = function(e) invisible(FALSE))
}

#' Refuse a sketch that cannot be smaller than the data (audit #13).
#'
#' clustoCell refuses this itself, correctly and in 0.1 s -- but from inside a
#' callr child, so the agent pays a full worker spawn to deliver it wrapped in
#' worker-failure framing. The wording below deliberately echoes clustoCell's
#' own guidance ("at least half the total" under 10,000 cells) so the early
#' message and the late one cannot give different advice.
#' @noRd
.cv_assert_sketch_fits <- function(store, args, data_arg = "data") {
  if (!isTRUE(args$sketch)) return(invisible(NULL))
  n <- suppressWarnings(as.integer(args$sketch_ncells %||% NA))
  if (is.na(n)) return(invisible(NULL))
  d <- .cv_arg_descriptor(store, args, data_arg)
  if (is.null(d)) return(invisible(NULL))
  nc <- suppressWarnings(as.integer(d$n_cells %||% NA))
  if (is.na(nc) || nc <= 0L) return(invisible(NULL))
  if (n < nc) return(invisible(NULL))
  cli::cli_abort(c(
    "{.arg sketch_ncells} is {.val {n}}, which is not fewer than the {.val {nc}} cell(s) in this object.",
    i = "Sketching clusters a representative subset of the cells, so it needs a number below the cell count.",
    i = paste0("For a dataset this size, either set {.code sketch = FALSE}, or lower {.arg sketch_ncells} ",
               "-- under 10,000 cells, at least half the total works well.")
  ))
}

#' The `createDataSketch` counterpart of `.cv_assert_sketch_fits()` above --
#' same shape of mistake (a sketch that is not actually smaller than the
#' object), different tool: `createDataSketch` always sketches (there is no
#' `sketch=` flag to gate on), so this checks `ncells` unconditionally rather
#' than only when sketching is enabled.
#' @noRd
.cv_assert_createsketch_fits <- function(store, args, data_arg = "data") {
  n <- suppressWarnings(as.integer(args$ncells %||% NA))
  if (is.na(n)) return(invisible(NULL))
  d <- .cv_arg_descriptor(store, args, data_arg)
  if (is.null(d)) return(invisible(NULL))
  nc <- suppressWarnings(as.integer(d$n_cells %||% NA))
  if (is.na(nc) || nc <= 0L) return(invisible(NULL))
  if (n < nc) return(invisible(NULL))
  cli::cli_abort(c(
    "{.arg ncells} is {.val {n}}, which is not fewer than the {.val {nc}} cell(s) in this object.",
    i = "A sketch is a representative SUBSET of the cells, so it needs a number below the cell count.",
    i = "Lower {.arg ncells}, or skip sketching if this object is already small enough to use directly."
  ))
}

#' Refuse to sketch an assay that has no normalized "data" layer yet (Round
#' LXXXVI, found while diagnosing a live test failure -- not something the
#' user reported directly, but a real correctness gap underneath item 5b of
#' their brief).
#'
#' `Seurat::SketchData()` reads `Layers(object[[assay]], search = "data")`
#' UNCONDITIONALLY, before it even looks at `method` -- so this is not a
#' `LeverageScore`-only precondition. When no `"data"`-pattern layer exists
#' (a freshly-created or raw-counts-only Seurat object), that lookup returns
#' `character(0)`, the internal per-layer sampling loop runs zero times, and
#' `subset(..., cells = NULL)` is interpreted as "keep every cell" -- so
#' `SketchData()` returns SILENTLY, with a `sketched_assay` that is the same
#' size as the input. No error, no size mismatch either -- it looks like it
#' worked. Confirmed live against Seurat 5.0.1: an unnormalized 80-cell
#' object asked for a 20-cell sketch and got 80 back; the identical call
#' after `Seurat::NormalizeData()` correctly returned 20.
#'
#' This matters more for `createDataSketch` than it would elsewhere: the tool
#' exists specifically so a user can skip clustoCell's/markoClust's own
#' (slower) internal preprocessing, which means the object handed to it is
#' *expected* to often still be raw. Silently "succeeding" with a same-size,
#' non-sketch would waste exactly the time this tool exists to save, and
#' would do so without any signal that anything was wrong.
#'
#' Deliberately an abort, not an auto-normalize-and-continue (contrast with
#' `.cv_umap_plot_compute()`'s auto-preprocessing in this same file): the
#' vignette's own decoupled-workflow note requires the sketch and the full
#' dataset to share the same normalization strategy for
#' `clustoCell_TransferLabel`'s expression-based methods
#' (`seurat-project`/`seurat-knn`). umapPlot's auto-preprocessing is safe
#' because a UMAP is a terminal visualization with no downstream consistency
#' requirement; guessing a normalization here could silently create a
#' sketch/full-dataset mismatch the user never asked for. Asking is the
#' honest option when the "right" default depends on a choice only the user
#' can make.
#'
#' Only checks for the PRESENCE of at least one `"data"`-pattern layer, not
#' completeness across every split layer of a multi-sample/merged Assay5
#' object -- a real but narrower edge case, named in CHANGES.md rather than
#' handled here.
#' @noRd
.cv_assert_createsketch_normalized <- function(store, args, data_arg = "data") {
  handle <- args[[data_arg]]
  if (!is.character(handle) || length(handle) != 1L || is.na(handle) || !nzchar(handle) ||
      !tryCatch(cv_object_exists(store, handle), error = function(e) FALSE)) {
    return(invisible(NULL))
  }
  obj <- tryCatch(cv_object_get(store, handle), error = function(e) NULL)
  if (is.null(obj) || !inherits(obj, "Seurat")) return(invisible(NULL))
  assay <- args$assay
  assays <- tryCatch(SeuratObject::Assays(obj), error = function(e) character(0))
  if (is.null(assay) || !is.character(assay) || !nzchar(assay)) {
    assay <- tryCatch(SeuratObject::DefaultAssay(obj), error = function(e) NA_character_)
  }
  if (is.na(assay) || !nzchar(assay) || !assay %in% assays) return(invisible(NULL))
  # suppressWarnings(): Seurat's own Layers(search=) emits "No layers found
  # matching search pattern provided" precisely when zero layers match --
  # exactly the case this check exists to detect, so it is expected here,
  # not a sign anything went wrong. Left uncaught, it would otherwise
  # surface as a stray, unrelated warning alongside this function's own
  # cli_abort() below.
  layers <- tryCatch(suppressWarnings(SeuratObject::Layers(obj[[assay]], search = "data")),
                      error = function(e) NULL)
  if (length(layers) > 0L) return(invisible(NULL))
  cli::cli_abort(c(
    "The {.val {assay}} assay has not been normalized yet -- it has no {.val data} layer.",
    i = paste0("SketchData reads normalized values from this layer to decide which cells belong in ",
               "the sketch, even with {.code method = \"Uniform\"} -- without it, no cells are actually ",
               "dropped and the sketch comes back the same size as the input, silently."),
    i = paste0("Normalize this object first (for example {.code Seurat::NormalizeData()}, or your own ",
               "SCTransform pipeline) using whatever strategy you also plan to use on the full dataset -- ",
               "clustoCell_TransferLabel's seurat-project/seurat-knn methods expect the sketch and the ",
               "full dataset to share the same normalization."),
    i = "If you don't need control over preprocessing, clustoCell's/markoClust's own sketch=TRUE handles this step for you."
  ))
}

#' Refuse an oversized heavy dispatch by asking, not by crashing (Round
#' LXXXV, from the brief: "in no case does the machine go down").
#'
#' clustoCell's callr child always deserialises its OWN full copy of `data`
#' (Round XXXVIII) and runs its initial filtration on that full copy BEFORE
#' any sketch is taken, so a small `sketch_ncells` does not, by itself,
#' guarantee the call fits -- see `cv_heavy_dispatch_route()`
#' (agent_bigdata.R) for the two-term estimate this reads.
#'
#' TWO outcomes when the call as given does not fit, handled differently on
#' purpose:
#'
#'   * Sketching CAN bring it under budget -- raises `cv_needs_clarification`
#'     (not `cli::cli_abort()`), which `cv_run_tool_call()` re-signals rather
#'     than converting, ending the turn in an interactive card offering a
#'     size expected to fit. Unlike every other advisory in this codebase,
#'     this has NO "proceed anyway" bypass: this check exists specifically to
#'     prevent the one outcome the brief rules out, so a bypassable gate
#'     would defeat the property it is for.
#'   * Sketching CANNOT help -- the floor alone already exceeds budget, so no
#'     `sketch_ncells` value changes the answer. There is nothing interactive
#'     left to collect, only an explanation, so this stays an ordinary
#'     `cli::cli_abort()` -- the ESTABLISHED contract every other validate
#'     hook here already uses.
#'
#' Silent whenever the route cannot be assessed (no measurement -> no
#' opinion, proceed), and silent when it fits.
#' @noRd
.cv_assert_heavy_object_fits <- function(store, args, tool, data_arg = "data") {
  route <- cv_heavy_dispatch_route(store, args, data_arg = data_arg)
  if (!identical(route$fits, FALSE)) return(invisible(NULL))

  needs  <- cv_bytes_human(route$needs_mb * 2^20)
  budget <- cv_bytes_human(route$budget_mb * 2^20)
  nc     <- route$n_cells_total

  if (identical(route$sketch_can_help, FALSE)) {
    tool_name   <- tool$name %||% "This step"
    floor_human <- cv_bytes_human(cv_heavy_dispatch_floor_mb(route$bytes) * 2^20)
    cli::cli_abort(c(
      paste0("{.val {tool_name}} needs about {needs} to run on this {.val {nc}}-cell object, ",
             "and this machine can offer about {budget}."),
      i = paste0("Sketching cannot bring this under budget here: the preparation this step ",
                 "does before any sketch is taken already needs about {floor_human} on its own, ",
                 "regardless of {.arg sketch_ncells}."),
      i = "Loading a smaller subset of this dataset and clustering that directly would fit."
    ))
  }
  stop(cv_needs_clarification_condition(
    cv_sketch_size_clarification_payload(store, args, tool, route, data_arg = data_arg)))
}

#' Offer sketching on a large dataset for SPEED, not memory (Round LXXXVI,
#' from live use).
#'
#' `.cv_assert_heavy_object_fits()` above answers "will this fit on this
#' machine" and is silent whenever the answer is yes. It has nothing to say
#' about a call that fits comfortably in memory but is still slow: on live
#' use, 208,506 cells cleared the memory check without complaint and still ran
#' long enough at clustoCell's cell-cell similarity/network stage that the
#' user wanted the choice up front. This function is that separate question,
#' gated on `CV_LARGE_DATASET_SKETCH_HINT_NCELLS` (agent_bigdata.R) rather than
#' on the machine's budget, and is only reached when the mandatory check above
#' has already returned silently (wired last in each tool's `validate=`).
#'
#' Silent whenever: `sketch=TRUE` already (nothing to offer -- the resolved
#' value TRUE can only be explicit, the schema default is FALSE, so this
#' direction is unambiguous); the object cannot be assessed; the cell count is
#' under the threshold; or `identify_subclusters` is FALSE for a
#' fraction-style caller (markoClust's own `sketch`/`sketch_fraction` do
#' nothing in that mode, so offering them would be a fabricated choice -- the
#' Round LXXXII mistake this project does not repeat).
#'
#' The harder case is the OTHER direction: a resolved `sketch=FALSE` is
#' ambiguous, since `cv_resolve_args()` (agent_registry.R) merges the schema
#' default into a call that never mentioned sketching at all, and the two are
#' indistinguishable by value alone. So "already asked" is tracked on the
#' SESSION, not inferred from the argument: the first time this fires for a
#' given (tool, handle) it raises the clarification AND records the key before
#' raising it; the "run on the full dataset" bypass resends the plain original
#' request, which reaches this function a second time, finds the key already
#' recorded, and returns silently -- regardless of how the model phrases "no
#' sketch" on that second pass. A session that never existed (or could not be
#' reached) degrades to "ask every time" rather than erroring.
#' @noRd
.cv_offer_speed_sketch <- function(store, args, tool, data_arg = "data",
                                    sketch_kind = c("ncells", "fraction")) {
  sketch_kind <- match.arg(sketch_kind)
  if (isTRUE(args$sketch)) return(invisible(NULL))
  if (identical(sketch_kind, "fraction") && !isTRUE(args$identify_subclusters)) {
    return(invisible(NULL))
  }
  handle <- args[[data_arg]]
  if (!is.character(handle) || length(handle) != 1L || is.na(handle) || !nzchar(handle) ||
      !tryCatch(cv_object_exists(store, handle), error = function(e) FALSE)) {
    return(invisible(NULL))
  }
  desc <- .cv_arg_descriptor(store, args, data_arg)
  if (is.null(desc)) return(invisible(NULL))
  ncell <- suppressWarnings(as.integer(desc$n_cells %||% NA))
  if (is.na(ncell) || ncell < CV_LARGE_DATASET_SKETCH_HINT_NCELLS) return(invisible(NULL))

  key <- paste0(tool$name, "::", handle)
  sid <- attr(store, "cv_session_id")
  sess <- if (!is.null(sid)) tryCatch(cv_session_get(sid), error = function(e) NULL) else NULL
  already <- !is.null(sess) && key %in% (sess$cv_sketch_prompted %||% character(0))
  if (already) return(invisible(NULL))
  if (!is.null(sess)) {
    sess$cv_sketch_prompted <- union(sess$cv_sketch_prompted %||% character(0), key)
    cv_session_set(sess)
  }

  stop(cv_needs_clarification_condition(
    cv_speed_sketch_clarification_payload(handle, tool, ncell, sketch_kind = sketch_kind)))
}

#' Say when a setting had no effect (audit #13, second half).
#'
#' `refine_transferred_subClusters` only does anything when labels are being
#' TRANSFERRED, i.e. when sketching is on and sub-clusters are being detected.
#' With either precondition off it is silently inert -- measured: clustoCell
#' completes normally and says nothing.
#'
#' INFO rather than amber, and the Round LXIX rule decides it rather than taste:
#' the run is correct and complete. Without sketching there is no transfer to
#' refine, and sub-clusters are detected on the full data directly, which is the
#' thing refinement approximates. Nobody reading the numbers draws a wrong
#' conclusion from skipping this note.
#' @noRd
.cv_note_inert_refine <- function(args, warnings) {
  if (!isTRUE(args$refine_transferred_subClusters)) return(invisible(NULL))
  missing_pre <- c(
    if (!isTRUE(args$sketch)) "sketch=TRUE",
    if (!isTRUE(args$identify_subclusters)) "identify_subclusters=TRUE")
  if (!length(missing_pre)) return(invisible(NULL))
  cv_warn_add(warnings, "info", sprintf(paste0(
    "'refine_transferred_subClusters' was on, but it only applies with %s, so it had no effect ",
    "on this run. Sub-clusters were handled the ordinary way instead."),
    paste(missing_pre, collapse = " and ")),
    code = "refine_without_sketch")
}

#' @noRd
.cv_expand_desired_cells <- function(store, dc) {
  if (is.null(dc)) return(NULL)
  cs <- NULL
  if (is.character(dc) && length(dc) == 1L && cv_object_exists(store, dc)) {
    cand <- cv_object_get(store, dc)
    if (inherits(cand, "CellSet")) cs <- cand
  } else if (inherits(dc, "CellSet")) {
    cs <- dc
  }
  if (is.null(cs)) return(dc)  # already a named list of barcodes, or unrelated
  nm <- cs$name %||% sprintf("cellset_%s", cs$cluster %||% "subset")
  stats::setNames(list(cs$cells), nm)
}

# Build the `inherit_from` handle(s) for markoCell / markerPurity so the OUTPUT
# handle names the SUBSET when one is identifiable, not just the source data.
# The user asked for "markocell_pbmc_c1_sub2", not "markocell_pbmc".
# Precedence:
#   1. desired_cells came from a CellSet (handle "cellset_<data>_<cluster>") ->
#      inherit "cellset_<data>_<cluster>" so cv_derived_handle strips the
#      "cellset_" prefix and yields "<prefix>_<data>_<cluster>".
#   2. desired_cells is a named list with exactly ONE subset name ->
#      append the sanitized subset name to the data handle's base.
#   3. otherwise (multi-subset / cluster_labels path / no subset) -> the data
#      handle(s) unchanged (current behaviour).
#' @noRd
.cv_subset_inherit <- function(store, dc_raw, dc_expanded, inh) {
  # Case 1: a CellSet handle string or CellSet object.
  cs_handle <- NULL
  if (is.character(dc_raw) && length(dc_raw) == 1L && cv_object_exists(store, dc_raw)) {
    cand <- cv_object_get(store, dc_raw)
    if (inherits(cand, "CellSet")) cs_handle <- dc_raw
  }
  if (!is.null(cs_handle)) return(cs_handle)
  # Case 2: single named subset in the expanded list. Append the sanitized
  # subset name to the data handle; cv_derived_handle strips the data handle's
  # type prefix and re-sanitizes, so "obj_pbmc_C1-Sub2" -> base "pbmc_c1_sub2".
  nm <- names(dc_expanded)
  if (!is.null(nm) && length(nm) == 1L && nzchar(nm[1]) && length(inh) >= 1L) {
    sub_tag <- gsub("[^a-z0-9]+", "_", tolower(nm[1]), perl = TRUE)
    sub_tag <- gsub("^_+|_+$", "", sub_tag, perl = TRUE)
    if (nzchar(sub_tag)) return(paste0(inh[1], "_", sub_tag))
  }
  inh
}

# ---- Marker-table filtering engine (Round IV, Req 1) ------------------------
# Deterministic, case-insensitive, intent-aware filtering of a stored marker
# table. The user's VERBATIM phrase (request_text) is parsed SERVER-SIDE into a
# small spec and the applied interpretation is ALWAYS reported back. Intents:
#   * rows      : the first N rows by rank (EXACTLY N)              "top 10"
#   * rank      : every marker with Rank <= N, ties kept (>= N)     "top 10 ranked"
#   * predicate : arbitrary column comparisons, AND-combined        "purity > 0.5"
# Precedence: a confident parse of request_text > explicit `filter` > mode/top_n.
# Unknown / absent columns are dropped gracefully (never an error) and reported.

# Canonical marker columns and the case-insensitive words that map onto them.
#' @noRd
.cv_marker_concept_map <- function() {
  list(
    Purity     = c("purity"),
    Gini_Score = c("gini_score", "gini score", "gini-score", "giniscore", "gini", "score"),
    Rank       = c("rank"),
    Feature    = c("feature", "features", "gene", "genes", "marker", "markers", "symbol", "symbols")
  )
}

# Resolve a canonical concept to the ACTUAL column name present (case-insensitive).
#' @noRd
.cv_resolve_col <- function(concept, columns) {
  hit <- columns[tolower(columns) == tolower(concept)]
  if (length(hit)) hit[1] else NA_character_
}

# Pretty-print a filter value: integer if whole, else the numeric as-is.
#' @noRd
.cv_fmt_num <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1L || is.na(x)) return("NA")
  if (x == round(x)) format(as.integer(round(x))) else format(x)
}

# Normalise an operator phrase (words or symbols) to one canonical operator.
#' @noRd
.cv_norm_op <- function(s) {
  s <- tolower(trimws(s))
  if (s %in% c(">=", "\u2265", "greater than or equal to", "at least", "no less than", "no fewer than")) return(">=")
  if (s %in% c("<=", "\u2264", "less than or equal to", "at most", "no more than", "no greater than")) return("<=")
  if (s %in% c(">", "greater than", "greater", "above", "over", "more than", "exceeds", "exceeding")) return(">")
  if (s %in% c("<", "less than", "less", "below", "under", "fewer than")) return("<")
  if (s %in% c("!=", "\u2260")) return("!=")
  if (s %in% c("=", "==", "equal to", "equals", "equal", "is")) return("==")
  NA_character_
}

# All non-overlapping PCRE matches of `pat`, each as its capture-group vector
# (version-safe stand-in for gregexec).
#' @noRd
.cv_all_matches <- function(pat, text) {
  out <- list(); rest <- text
  repeat {
    if (!nzchar(rest)) break
    m <- regexec(pat, rest, perl = TRUE)
    st <- m[[1]][1]
    if (is.na(st) || st < 0) break
    out[[length(out) + 1L]] <- regmatches(rest, m)[[1]]
    adv <- st - 1L + attr(m[[1]], "match.length")[1]
    if (is.na(adv) || adv < 1L) adv <- 1L
    rest <- substring(rest, adv + 1L)
  }
  out
}

# Case-insensitive synonym alternation tolerant of space / underscore / hyphen.
#' @noRd
.cv_syn_alt <- function(syns) {
  syns <- gsub("[ _-]", "[ _-]?", syns)
  syns <- unique(syns[order(nchar(syns), decreasing = TRUE)])
  paste0("(?:", paste(syns, collapse = "|"), ")")
}

# Extract column predicates from a lowercased phrase. Predicates on columns not
# present in `columns` are dropped and recorded in `unknown` (concept names).
#' @noRd
.cv_extract_marker_predicates <- function(lt, columns) {
  cmap <- .cv_marker_concept_map()
  preds <- list(); unknown <- character(0)
  add_pred <- function(concept, op, v1, v2 = NA) {
    actual <- .cv_resolve_col(concept, columns)
    if (is.na(actual)) { unknown <<- c(unknown, concept); return(invisible()) }
    if (identical(concept, "Feature")) {
      vals <- toupper(trimws(strsplit(as.character(v1), ",")[[1]])); vals <- vals[nzchar(vals)]
      preds[[length(preds) + 1L]] <<- list(col = actual, op = op, value = vals, value2 = NA)
    } else {
      preds[[length(preds) + 1L]] <<- list(col = actual, op = op,
        value = suppressWarnings(as.numeric(v1)),
        value2 = suppressWarnings(as.numeric(v2)))
    }
  }
  numval <- "(-?\\d+(?:\\.\\d+)?)"
  op_pat <- paste0("(greater than or equal to|less than or equal to|no less than|",
                   "no fewer than|no greater than|no more than|greater than|less than|",
                   "more than|fewer than|at least|at most|equal to|<=|>=|==|!=|\u2264|\u2265|",
                   "\u2260|=|<|>|greater|above|over|exceeds|exceeding|below|under|less|equals|equal|is)")
  for (concept in c("Purity", "Gini_Score", "Rank")) {
    S <- .cv_syn_alt(cmap[[concept]])
    for (gm in .cv_all_matches(paste0(S, "\\s*(?:is\\s+|of\\s+)?between\\s+", numval, "\\s+and\\s+", numval), lt))
      if (length(gm) >= 3) add_pred(concept, "between", gm[2], gm[3])
    for (gm in .cv_all_matches(paste0(S, "\\s*(?:is\\s+|of\\s+)?", numval,
                                      "\\s+or\\s+(above|higher|greater|larger|more|below|lower|less|smaller|fewer)"), lt))
      if (length(gm) >= 3) add_pred(concept, if (gm[3] %in% c("above","higher","greater","larger","more")) ">=" else "<=", gm[2])
    # Round LXXIV (audit #11, rider): the optional "is"/"of" was present on the
    # `between` and `N or above` branches and MISSING here, so
    # "purity of at least 0.3" -- where "at least" IS a recognised operator --
    # failed to parse over one filler word and silently returned the default.
    for (gm in .cv_all_matches(paste0(S, "\\s*(?:is\\s+|of\\s+|that\\s+is\\s+)?", op_pat, "\\s*", numval), lt)) {
      if (length(gm) >= 3) { op <- .cv_norm_op(gm[2]); if (!is.na(op)) add_pred(concept, op, gm[3]) }
    }
  }
  SF <- .cv_syn_alt(cmap$Feature)
  for (gm in .cv_all_matches(paste0(SF,
      "\\s+(is not|isn't|not equal to|not|named|called|equals|equal to|equal|is|==|!=|=)\\s+([a-z0-9][a-z0-9._,-]*)"), lt)) {
    if (length(gm) >= 3) add_pred("Feature", if (grepl("not|!=|isn't", gm[2])) "!=" else "==", gm[3])
  }
  list(preds = preds, unknown = unique(unknown))
}

# Detect the "ranked" intent (Rank <= N with ties) and its N, if present.
#' @noRd
.cv_detect_rank_kind <- function(lt) {
  trig <- FALSE; n <- NA_integer_
  m <- regmatches(lt, regexec("rank\\s*(?:<=|\u2264)\\s*(\\d+)", lt, perl = TRUE))[[1]]
  if (length(m) >= 2) { trig <- TRUE; n <- as.integer(m[2]) }
  if (!trig) {
    m <- regmatches(lt, regexec("rank\\s*(?:of\\s+)?(\\d+)\\s+or\\s+(?:below|less|lower|fewer|better|higher)", lt, perl = TRUE))[[1]]
    if (length(m) >= 2) { trig <- TRUE; n <- as.integer(m[2]) }
  }
  if (!trig) {
    m <- regmatches(lt, regexec("rank\\s+(?:at most|no more than)\\s+(\\d+)", lt, perl = TRUE))[[1]]
    if (length(m) >= 2) { trig <- TRUE; n <- as.integer(m[2]) }
  }
  if (grepl("\\branked\\b", lt, perl = TRUE)) {
    trig <- TRUE
    if (is.na(n)) for (p in c("top\\s+(\\d+)", "first\\s+(\\d+)", "(\\d+)\\s+ranked",
                              "ranked\\D{0,20}?(\\d+)", "rank\\D{0,8}?(\\d+)")) {
      mm <- regmatches(lt, regexec(p, lt, perl = TRUE))[[1]]
      if (length(mm) >= 2) { n <- as.integer(mm[2]); break }
    }
  }
  list(trigger = trig, n = n)
}

# Detect a positional "top N" / "first N" / "N markers" count, if present.
#' @noRd
.cv_detect_rows_number <- function(lt) {
  for (p in c("top\\s+(\\d+)", "first\\s+(\\d+)",
              "(\\d+)\\s+(?:markers?|genes?|features?|rows?|symbols?|results?)")) {
    m <- regmatches(lt, regexec(p, lt, perl = TRUE))[[1]]
    if (length(m) >= 2) return(as.integer(m[2]))
  }
  NA_integer_
}

#' @noRd
.cv_detect_rows_signal <- function(lt) {
  grepl("\\btop\\s+\\d+|\\bfirst\\s+\\d+|\\d+\\s+(?:markers?|genes?|features?|rows?|symbols?|results?)",
        lt, perl = TRUE)
}

# Parse one free-text expression into a spec, or NULL when it carries no
# confident filtering signal (so the caller can fall through to the next source).
#' @noRd
.cv_parse_marker_expr <- function(text, columns, top_n) {
  if (is.null(text)) return(NULL)
  text <- cv_strip_ansi(as.character(text)[1])
  if (!nzchar(trimws(text))) return(NULL)
  lt <- tolower(text)

  ext <- .cv_extract_marker_predicates(lt, columns)
  preds <- ext$preds; unknown <- ext$unknown
  rk <- .cv_detect_rank_kind(lt)
  rows_n <- .cv_detect_rows_number(lt)
  rows_sig <- .cv_detect_rows_signal(lt)

  is_rank <- vapply(preds, function(p) identical(tolower(p$col), "rank"), logical(1))
  nr <- preds[!is_rank]; rp <- preds[is_rank]

  # Any NON-rank column predicate -> predicate intent (rank preds fold in too).
  if (length(nr) > 0)
    return(list(kind = "predicate", n = NA_integer_, predicates = preds,
                limit = if (!is.na(rows_n)) rows_n else NA_integer_, unknown_cols = unknown))
  # "ranked" / "rank <= N" style with no other predicate -> the ties-kept intent.
  if (isTRUE(rk$trigger)) {
    n <- rk$n; if (is.na(n)) n <- rows_n; if (is.na(n)) n <- top_n
    return(list(kind = "rank", n = as.integer(n), predicates = list(),
                limit = NA_integer_, unknown_cols = unknown))
  }
  # A rank comparison in another form (>, between, =, ...) -> predicate intent.
  if (length(rp) > 0)
    return(list(kind = "predicate", n = NA_integer_, predicates = rp,
                limit = if (!is.na(rows_n)) rows_n else NA_integer_, unknown_cols = unknown))
  # Positional "top N" / "N markers".
  if (isTRUE(rows_sig)) {
    n <- rows_n; if (is.na(n)) n <- top_n
    return(list(kind = "rows", n = as.integer(n), predicates = list(),
                limit = NA_integer_, unknown_cols = unknown))
  }
  # A recognised column word whose column is absent here still counts as intent:
  # fall back to top_n rows but report what we could not apply.
  if (length(unknown) > 0)
    return(list(kind = "rows", n = as.integer(top_n), predicates = list(),
                limit = NA_integer_, unknown_cols = unknown))
  NULL
}

# Human-readable description of exactly what a spec will do.
#' @noRd
.cv_marker_spec_interpretation <- function(spec, columns) {
  base <- switch(spec$kind,
    rows = sprintf("return the first %d marker(s) by rank (exactly %d row(s))", spec$n, spec$n),
    rank = sprintf("keep every marker with Rank <= %d (ties at the same rank are all included)", spec$n),
    predicate = {
      parts <- vapply(spec$predicates, function(p) {
        if (identical(p$op, "between"))
          sprintf("%s between %s and %s", p$col, .cv_fmt_num(p$value), .cv_fmt_num(p$value2))
        else if (is.character(p$value))
          sprintf("%s %s %s", p$col, if (identical(p$op, "!=")) "is not" else "is",
                  paste(p$value, collapse = "/"))
        else sprintf("%s %s %s", p$col, p$op, .cv_fmt_num(p$value))
      }, character(1))
      lim <- if (!is.na(spec$limit)) sprintf(", then keep the first %d row(s)", spec$limit) else ""
      sprintf("keep markers where %s%s", paste(parts, collapse = " and "), lim)
    },
    "return markers")
  if (length(spec$unknown_cols))
    base <- sprintf("%s (ignored column(s) not present here: %s; available columns: %s)",
                    base, paste(spec$unknown_cols, collapse = ", "),
                    paste(columns, collapse = ", "))
  base
}

#' @noRd
.cv_finalize_marker_spec <- function(spec, columns) {
  if (is.null(spec$unknown_cols)) spec$unknown_cols <- character(0)
  if (is.null(spec$limit)) spec$limit <- NA_integer_
  if (is.null(spec$predicates)) spec$predicates <- list()
  if (identical(spec$kind, "predicate") && !length(spec$predicates)) {
    spec$kind <- "rows"
    if (is.null(spec$n) || is.na(spec$n)) spec$n <- 10L
  }
  spec$interpretation <- .cv_marker_spec_interpretation(spec, columns)
  spec
}

#' Parse a marker-table filter request into a deterministic spec.
#'
#' Returns a list with `kind` ("rows" | "rank" | "predicate"), `n`, `predicates`
#' (each `list(col, op, value, value2)`), `limit`, `unknown_cols`, `source`
#' ("request_text" | "filter" | "mode") and a human-readable `interpretation`.
#' @noRd
cv_parse_marker_filter <- function(request_text = NULL, filter = NULL,
                                   mode = c("rows", "rank"),
                                   top_n = 10L, columns = character()) {
  mode <- match.arg(mode)
  top_n <- as.integer(top_n %||% 10L)
  columns <- as.character(columns)

  spec <- .cv_parse_marker_expr(request_text, columns, top_n)
  if (!is.null(spec)) { spec$source <- "request_text"; return(.cv_finalize_marker_spec(spec, columns)) }
  spec <- .cv_parse_marker_expr(filter, columns, top_n)
  if (!is.null(spec)) { spec$source <- "filter"; return(.cv_finalize_marker_spec(spec, columns)) }
  spec <- list(kind = mode, n = top_n, predicates = list(), limit = NA_integer_,
               unknown_cols = character(0), source = "mode")
  spec$unparsed <- .cv_marker_filter_looked_like_one(request_text, filter)
  .cv_finalize_marker_spec(spec, columns)
}

# Round LXXIV (audit #11). A phrase that could not be parsed used to fall through
# to the mode/top_n default and be reported with total confidence:
# "lower the purity cutoff to 0.2" came back as
# "Top 10 positive marker(s) (by row)" with no hint that the request had been
# ignored. Measured, before the fix, all silently defaulting:
#
#   "lower the purity cutoff to 0.2"
#   "raise the gini threshold to 0.8"
#   "markers with purity of at least 0.3"    (the rider above now parses this)
#
# The detector must be NARROW. `request_text` is the user's verbatim phrase and
# arrives on nearly every getClusterMarkers call, so a broad test would raise a
# warning on ordinary requests like "show me the markers of C1" -- which is how
# users learn to ignore warnings, the failure audit 3b#5 exists to prevent.
#
# So it fires only when the phrase carries an explicit FILTERING signal: a
# number, a marker-table column word, or one of the words people use to move a
# threshold. Anything vaguer stays silent.
#' @noRd
.cv_marker_filter_looked_like_one <- function(request_text, filter) {
  txt <- c(as.character(request_text %||% ""), as.character(filter %||% ""))
  txt <- txt[nzchar(trimws(txt))]
  if (!length(txt)) return(NA_character_)
  lt <- tolower(paste(txt, collapse = " "))
  # A bare digit is NOT a signal: cluster ids carry them, so "show me the
  # markers of C1" tripped the first version of this test and would have warned
  # on one of the most ordinary requests there is. A DECIMAL is a signal --
  # nobody names a cluster "0.2" -- and so is an explicit threshold word.
  #
  # Round LXXVI: a bare COLUMN NAME was a signal too, and that was wrong. It
  # fired on "what is the purity of marker CD8A in C1" -- naming the quantity
  # you are asking about, with no comparison anywhere -- and told the user their
  # request could not be turned into a filter. Found while reproducing this
  # round's own report, on the user's literal question. Naming a column is not
  # asking to filter by it; the threshold and verb words below already cover
  # every phrasing that is.
  signal <-
    grepl("[0-9]*\\.[0-9]+", lt, perl = TRUE) ||
    grepl("\\b(cutoff|cut-off|threshold|thresholds|filter|filtered|above|below|over|under|at least|at most)\\b",
          lt, perl = TRUE) ||
    grepl("\\b(lower|raise|increase|decrease|reduce|tighten|loosen|relax)\\b", lt, perl = TRUE)
  if (!signal) return(NA_character_)
  trimws(txt[1])
}

#' Apply a parsed marker-filter spec to a single per-set marker data.frame.
#' Clamp a caller-supplied count to a non-negative integer for use with
#' utils::head(). BATCH2 FIX: utils::head(x, n) with a NEGATIVE n means
#' "drop the last |n| rows" in base R, not "invalid/empty" -- none of the
#' call sites below previously clamped a model-supplied count before passing
#' it straight to head(), so e.g. `top_n = -5` silently returned rows with a
#' directly self-contradictory "Top -5 marker(s)" label instead of an error.
#' @noRd
.cv_safe_head_n <- function(n, default = 10L) {
  n <- suppressWarnings(as.integer(n %||% default))
  if (is.na(n) || n < 0L) 0L else n
}

cv_apply_marker_filter <- function(df, spec) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(df)
  if ("Rank" %in% names(df)) df <- df[order(df$Rank), , drop = FALSE]
  kind <- spec$kind %||% "rows"
  if (identical(kind, "rows")) return(utils::head(df, .cv_safe_head_n(spec$n)))
  if (identical(kind, "rank")) {
    if ("Rank" %in% names(df))
      return(df[!is.na(df$Rank) & df$Rank <= (spec$n %||% 10L), , drop = FALSE])
    return(utils::head(df, .cv_safe_head_n(spec$n)))
  }
  keep <- rep(TRUE, nrow(df))
  for (p in spec$predicates) {
    if (is.null(p$col) || !(p$col %in% names(df))) next
    x <- df[[p$col]]
    cmp <- switch(p$op,
      ">"  = suppressWarnings(as.numeric(x) >  p$value),
      ">=" = suppressWarnings(as.numeric(x) >= p$value),
      "<"  = suppressWarnings(as.numeric(x) <  p$value),
      "<=" = suppressWarnings(as.numeric(x) <= p$value),
      "==" = if (is.character(p$value)) toupper(as.character(x)) %in% toupper(p$value) else suppressWarnings(as.numeric(x) == p$value),
      "!=" = if (is.character(p$value)) !(toupper(as.character(x)) %in% toupper(p$value)) else suppressWarnings(as.numeric(x) != p$value),
      "between" = suppressWarnings(as.numeric(x) >= p$value & as.numeric(x) <= p$value2),
      rep(TRUE, nrow(df)))
    cmp[is.na(cmp)] <- FALSE
    keep <- keep & cmp
  }
  out <- df[keep, , drop = FALSE]
  if (!is.na(spec$limit)) out <- utils::head(out, .cv_safe_head_n(spec$limit))
  out
}

# Read the top-N ranked markers of one or more clusters/sub-clusters straight
# out of a ClustoCell object's stored $markers, returning a tidy result record
# (kind="table"). No recomputation, no new object. Handles the real CelliVerse
# marker structure verified against the package's own results:
#   major_clusters$cluster_specific$<positive|negative|medium>_markers$<Cx>  (df)
#   major_clusters$cross_cluster$positive_features                            (df, alt key/cols)
#   sub_clusters[["Cx-Subclusters"]]$<...>_markers$<Sub_i>                    (df)
# Some leaves are a 'logMessage' string ("No specific marker was identified!")
# rather than a data.frame; those degrade to an honest per-set note.
# Resolve user/model-supplied set ids to the CANONICAL stored ids, tolerating
# case and common separator differences. The LLM often writes "C1-sub2",
# "c1 sub2", or "C1Sub2" when the stored id is "C1-Sub2"; an exact match used to
# abort the whole call. We normalise by lowercasing and stripping all non-
# alphanumeric characters, so "C1-sub2" == "C1-Sub2" == "c1 sub2" == "C1Sub2".
# Returns the canonical stored id for an unambiguous match. Aborts (listing the
# available ids) only when a request matches nothing, or matches >1 stored id
# (ambiguous). This is the single resolver used by every agent-layer set-id
# lookup so the agent "understands" the id regardless of case/separator.
#' @noRd
cv_resolve_set_ids <- function(requested, available, level = NULL) {
  requested <- as.character(requested)
  available <- as.character(available)
  norm <- function(x) gsub("[^[:alnum:]]", "", tolower(trimws(x)))
  norm_avail <- norm(available)
  resolved <- character(length(requested))
  for (i in seq_along(requested)) {
    key <- norm(requested[i])
    if (!nzchar(key)) {
      cli::cli_abort(c(
        "Empty set id supplied{?s}.",
        i = "Available: {.val {available}}."
      ))
    }
    hit <- available[norm_avail == key]
    if (length(hit) == 1L) {
      resolved[i] <- hit
    } else if (length(hit) > 1L) {
      cli::cli_abort(c(
        "Set id {.val {requested[i]}} is ambiguous - it matches {length(hit)} sets.",
        i = "Matches: {.val {hit}}.",
        i = "Please use the exact id."
      ))
    } else {
      # NB: build the level suffix with a plain quoted value - {.val {%s}}
      # would inject the raw level text into cli's inline-code parser and fail
      # on values with spaces (e.g. "Major cluster").
      lvl <- if (!is.null(level) && nzchar(level)) sprintf(' for level {.val "%s"}', level) else ""
      # Steer a self-correction: a sub-cluster id (contains "sub") queried at
      # the Major-cluster level (or vice-versa) fails here. Tell the caller to
      # RETRY the SAME id at the OTHER level rather than guessing a fragment.
      other_lvl <- if (!is.null(level) && grepl("sub", level, ignore.case = TRUE))
        "Major cluster" else "Sub cluster"
      cli::cli_abort(c(
        paste0("Unknown set id(s) {.val {requested[i]}}", lvl, "."),
        i = "Available at this level: {.val {available}}.",
        i = paste0("If {.val {requested[i]}} is a valid id, it likely lives at the OTHER level ",
                   "('", other_lvl, "') - RETRY the same id with level='", other_lvl,
                   "' instead of guessing a different id.")
      ))
    }
  }
  unname(resolved)
}

# ---- Where a marker actually lives (Round LXXVI, from live use) --------------
#
# THE REPORT. The user asked "what is the purity of marker CD8A in C1". The
# agent ran markerPurity, then ALSO ran getClusterMarkers for C1's top 10
# POSITIVE markers, did not find CD8A among them, and answered:
#
#     "The purity of CD8A in C1 is 0.0000 (0% purity) ... CD8A is not expressed
#      in C1 ... C1 is not a CD8+ T cell cluster"
#
# Every part of that is wrong, and reproduced here against the bundled object:
#
#   featureInspect(cc, "CD8A")  -> CD8A IS a POSITIVE marker of C1,
#                                  Gini 0.8639, Purity 0.1361, RANK 47
#   positive_markers$C1         -> 601 rows; CD8A is one of them
#   markerPurity(...)$within_clusters
#        $positive_markers$C1   -> 0 rows
#        $negative_markers$C1   -> 0 rows
#        $medium_markers$C1     -> CD8A, Gini 0.75, Purity 0.25, Rank 1
#
# So the gene was at rank 47 of 601, the agent looked at a top-TEN slice, and
# reported its absence from that slice as absence from the data.
#
# A PROMPT RULE DID NOT PREVENT THIS, AND ONE WAS ALREADY THERE. Round LXVII
# wrote rule 2h, which says in as many words: "getClusterMarkers ... lists the
# top-ranked markers OF a cluster, so it cannot report the purity of a gene the
# user named, and if that gene is outside the top N it will appear to be absent
# when it is not." The model did it anyway. That is the standing rule of this
# project demonstrated rather than restated: a prompt rule is not an enforcement
# mechanism. The fix below is code.
#
# THREE MARKER CLASSES, NOT ONE. Both object types carry positive, negative AND
# medium markers, and every read path in the agent defaulted to positive alone:
#
#   ClustoCell  $markers$major_clusters$cluster_specific$<type>$<set>
#               $markers$sub_clusters$<Cx-Subclusters>$<type>$<sub>
#   MarkoCell   $cell_subset_markers$<type>$<subset>
#
# ONE LOCATOR. The navigation above was inline in cv_cluster_markers_table();
# the lookup below needs exactly the same walk over all three types and both
# levels. Two copies of this walk is the shape that has drifted four times in
# this codebase, so it is extracted once here and both callers use it. It
# returns NULL rather than aborting -- a SEARCH across every type must be able
# to find nothing in one collection and keep going, while the table tool still
# raises its own (unchanged) errors at its own call site.

#' Locate the named list of per-set marker data.frames for one type and level.
#'
#' @param cc a ClustoCell or MarkoCell.
#' @param type_key one of "positive_markers", "negative_markers", "medium_markers".
#' @param level "Major cluster" or "Sub cluster" (ignored for a MarkoCell).
#' @return list(set_list=, level=) with `set_list` NULL when there is nothing.
#' @noRd
.cv_marker_set_list <- function(cc, type_key, level = "Major cluster") {
  if (inherits(cc, "MarkoCell")) {
    set_list <- cc$cell_subset_markers[[type_key]]
    if (!is.null(set_list) && length(set_list)) {
      # keep only real data.frames (skip logMessage sentinels)
      is_df <- vapply(set_list, function(x) is.data.frame(x), logical(1))
      set_list <- set_list[is_df]
    }
    if (!length(set_list %||% list())) set_list <- NULL
    # `level` is ClustoCell-specific; a MarkoCell is always a cell subset.
    return(list(set_list = set_list, level = "Cell subset"))
  }
  if (is.null(cc$markers)) return(list(set_list = NULL, level = level))

  if (identical(level, "Major cluster")) {
    coll <- cc$markers$major_clusters$cluster_specific
    # cross_cluster stores positives under 'positive_features' (different cols);
    # cluster_specific is the primary per-cluster ranking and is what the user
    # means by "markers of C1", so prefer it and fall back only if absent.
    set_list <- if (!is.null(coll) && !is.null(coll[[type_key]])) coll[[type_key]]
                else if (identical(type_key, "positive_markers"))
                     cc$markers$major_clusters$cross_cluster[["positive_features"]]
                else NULL
  } else {
    # Sub-cluster: sets are named "Cx-Subclusters"; each has per-subcluster dfs.
    sc <- cc$markers$sub_clusters
    set_list <- list()
    for (grp in names(sc %||% list())) {
      inner <- sc[[grp]][[type_key]]
      if (is.null(inner)) next
      for (sub in names(inner)) {
        # Prefix with the major cluster so ids are unambiguous, e.g. "C1-Sub1".
        pref <- sub("-Subclusters$", "", grp)
        set_list[[paste0(pref, "-", sub)]] <- inner[[sub]]
      }
    }
    if (!length(set_list)) set_list <- NULL
  }
  list(set_list = set_list, level = level)
}

CV_MARKER_TYPE_KEYS <- c(Positive = "positive_markers",
                         Negative = "negative_markers",
                         Medium   = "medium_markers")

# ---- What a heavy run will cost you (Round LXXVIII, audit #44) --------------
#
# THE GAP, as the audit puts it: "the only cost signal today arrives 90 s after
# the user is committed". A heavy tool is dispatched to a worker and the user
# learns what they let themselves in for from the progress ticks.
#
# STATED, NOT GATED. Audit category 3b item 4 is explicit that confirmation
# gates on this product are "precisely the unnecessary 'are you sure?'" it
# objects to, and Round LXXVII's #43 already applied that ruling. So this
# announces and proceeds. Nothing is blocked and nothing is asked.
#
# NO INVENTED SECONDS. It would be easy to multiply cells by a constant and
# print an ETA; it would also be a fabricated number, and this project's error
# voice does not do those. What it states is what is actually known before the
# run: which tool, how big the input is, that it runs in the background, and
# that it can be stopped. Size is the thing that varies by three orders of
# magnitude between users and is the only honest predictor available here.
#
# INFO. It fires on every heavy dispatch, which by Round LXIX's rule settles the
# severity: a note on ordinary correct work is not a signal.

#' Announce the scale of a heavy run before it starts.
#' @noRd
.cv_note_heavy_cost <- function(store, args, tool, warnings) {
  h <- NULL
  for (k in c("data", "obj", "clustoCell", "query_expr_mat")) {
    v <- args[[k]]
    if (is.character(v) && length(v) == 1L && nzchar(v) &&
        tryCatch(cv_object_exists(store, v), error = function(e) FALSE)) { h <- v; break }
  }
  d <- if (is.null(h)) NULL else tryCatch(cv_object_descriptor(store, h), error = function(e) NULL)
  n <- suppressWarnings(as.integer(d$n_cells %||% NA))
  size <- if (!is.na(n)) sprintf(" on %s cells", format(n, big.mark = ",")) else ""
  cv_warn_add(warnings, "info", sprintf(paste0(
    "%s is a heavy step and runs%s in a background worker. It does not block the ",
    "chat, progress appears on its card, and you can stop it there if it is taking ",
    "longer than you want."), tool$name %||% "This step", size), code = "heavy_cost")
}

# ---- Curated next steps (Round LXXVII, audit #35) ---------------------------
#
# `cv_tool(next_suggestions =)` has existed since the registry was written.
# Measured before building this:
#
#   9 tools carry suggestions
#   0 of the named tools are unregistered (every name is real)
#   0 consumers anywhere -- the only other references in R/ are
#     agent_registry.R's constructor assigning the field to itself, and there
#     are 0 in the frontend
#
# So the workflow knowledge was written down by the person who has it, checked
# out correct, and was shown to nobody. The audit calls this the cheapest win
# in the list and that is right.
#
# INFO, not may_invalidate. It fires on every successful run of those nine
# tools, and Round LXIX's rule is that a note appearing on ordinary correct work
# is by definition not a signal. `result_note` is the same shape and precedent.
#
# VALIDATED AGAINST THE REGISTRY at render time, not trusted. Zero are invalid
# today; this stops a future rename turning a helpful pointer into advice to
# call a tool that no longer exists.

#' Render a tool's `next_suggestions` as an `info` warning, or nothing.
#'
#' @param tool the tool record just run.
#' @param reg the registry, used to drop any suggestion that is not a real tool.
#' @return a list of cv_warn() entries (empty when there is nothing to say).
#' @noRd
.cv_next_steps_note <- function(tool, reg = NULL) {
  ns <- tryCatch(as.character(tool$next_suggestions %||% character(0)),
                 error = function(e) character(0))
  ns <- unique(ns[nzchar(ns)])
  if (!length(ns)) return(list())
  if (!is.null(reg)) ns <- ns[ns %in% names(reg)]
  if (!length(ns)) return(list())
  list(cv_warn("info", sprintf(
    "Next steps usually taken after %s: %s. Suggest one of these if it matches what the user wants; ignore them if it does not.",
    tool$name %||% "this tool", paste(ns, collapse = ", ")), "next_steps"))
}

# ---- clustoCell will CREATE clusters, not use yours (Round LXXVII, #36) -----
#
# The audit's wording: "'find markers for my Seurat clusters' can silently
# re-cluster the data". clustoCell computes a NEW clustering and labels it
# C1..Cn; it never reads an existing metadata column. markoClust is the tool
# that takes `cluster_labels` and works on clusters that already exist.
#
# So a user whose object carries `seurat_clusters` and who asks for "markers of
# my clusters" can get markers of a DIFFERENT partition, with no indication
# that a substitution happened.
#
# INFO, deliberately and after checking what it would fire on: most Seurat
# objects that reach this agent already carry `seurat_clusters`, so an amber
# here would amber nearly every clustoCell run -- the exact failure Round LXIX's
# severity rule exists to prevent. It is a fact worth putting next to the
# result, not a verdict on it.
#
# The pattern is deliberately narrow: only names that ARE cluster labels, never
# a guess from column type, because "a factor with few levels" is also what
# sample, donor, condition and batch look like.

#' @noRd
.cv_cluster_like_columns <- function(cols) {
  cols <- as.character(cols %||% character(0))
  if (!length(cols)) return(character(0))
  cols[grepl("(^|_)clusters?$|^seurat_clusters$|_snn_res\\.|_res\\.[0-9]|louvain|leiden",
             cols, ignore.case = TRUE, perl = TRUE)]
}

#' @noRd
.cv_note_existing_clusters <- function(store, args, warnings) {
  d <- tryCatch(cv_object_descriptor(store, args$data), error = function(e) NULL)
  hits <- .cv_cluster_like_columns(d$metadata_cols)
  if (!length(hits)) return(invisible(NULL))
  cv_warn_add(warnings, "info", sprintf(paste0(
    "This object already carries cluster labels (%s). clustoCell computes a NEW ",
    "clustering and labels it C1, C2, ... -- it does not read those columns, so the ",
    "clusters below are not the same partition. If you wanted the markers of the ",
    "EXISTING labels, use markoClust with cluster_labels=\"%s\" instead."),
    paste(hits, collapse = ", "), hits[1]), code = "reclustering_notice")
}

# ---- A metadata column is about to be replaced (Round LXXVII, audit #43) ----
#
# `addClustoData` writes its labels into the Seurat under `major_cluster_name` /
# `sub_cluster_name` (defaults ClustoCell_Clusters / ClustoCell_SubClusters). If
# the column is already there it is overwritten -- and the tool's own success
# text says "no duplicate created", which the audit rightly calls reassurance
# about the wrong thing: it answers "did you make a mess of my columns?" while
# the actual event is that a column's contents were replaced.
#
# A NOTICE, NOT A GATE. Audit category 3b item 4 rules explicitly on this:
# confirmation gates on addClustoData/addTypoData are "precisely the unnecessary
# 'are you sure?'" the audit objects to, and "the right answer is a NOTICE that
# a column was replaced (item 43), not a gate". So this warns and proceeds, and
# it is INFO -- re-running addClustoData after re-clustering is the ordinary
# workflow, and ambering it would train the user past it.
#
# addClustoData.R itself is a core scientific function and is NOT touched: the
# check reads the store's descriptor before dispatch, through the Round LXX
# validate hook.

#' @noRd
.cv_note_replaced_columns <- function(store, args, warnings) {
  d <- tryCatch(cv_object_descriptor(store, args$obj), error = function(e) NULL)
  cols <- as.character(d$metadata_cols %||% character(0))
  if (!length(cols)) return(invisible(NULL))
  want <- character(0)
  if (!identical(args$add_major_clusters, FALSE))
    want <- c(want, as.character(args$major_cluster_name %||% "ClustoCell_Clusters"))
  if (!identical(args$add_sub_clusters, FALSE))
    want <- c(want, as.character(args$sub_cluster_name %||% "ClustoCell_SubClusters"))
  hit <- intersect(unique(want[nzchar(want)]), cols)
  if (!length(hit)) return(invisible(NULL))
  cv_warn_add(warnings, "info", sprintf(paste0(
    "%s already exist%s on this object and %s be REPLACED with the labels from this ",
    "ClustoCell. The previous contents are not kept. Pass a different ",
    "major_cluster_name / sub_cluster_name if you want to keep both."),
    paste(hit, collapse = " and "), if (length(hit) == 1L) "s" else "",
    if (length(hit) == 1L) "will" else "will"), code = "metadata_column_replaced")
}

#' Gene symbols named in a request, VALIDATED against the object itself.
#'
#' Round LXXVI. Deliberately not a clever gene-symbol regex: a loose shape test
#' produces candidates, and the object decides which of them are real. A token
#' that is not a feature of this object simply yields no rows in
#' cv_find_marker_in_object(), so a false positive is not merely unlikely, it is
#' structurally impossible. Round LXXIV's #11 spent a whole iteration on a
#' false-positive detector; this one cannot have that failure mode.
#' @noRd
.cv_candidate_genes <- function(request_text) {
  txt <- as.character(request_text %||% "")
  if (!length(txt) || !nzchar(trimws(txt[1]))) return(character(0))
  toks <- unlist(strsplit(txt[1], "[^A-Za-z0-9.\\-]+", perl = TRUE))
  toks <- toks[nzchar(toks)]
  # A symbol starts with a letter, is at least two characters, and is not a
  # cluster id (C1, C1-Sub2). Everything else the object will reject anyway.
  keep <- grepl("^[A-Za-z][A-Za-z0-9]*([.-][A-Za-z0-9]+)*$", toks, perl = TRUE) &
    nchar(toks) >= 2L &
    !grepl("^C[0-9]+(-Sub[0-9]+)?$", toks, ignore.case = TRUE, perl = TRUE)
  unique(toupper(toks[keep]))
}

#' Genes named in the request that this slice does not show but the object has.
#'
#' @return list(append = <rows to add, same type+level>, message = <warning or "">).
#' @noRd
.cv_out_of_slice_hits <- function(cc, request_text, shown, sets, type, level) {
  none <- list(append = NULL, message = "")
  cand <- .cv_candidate_genes(request_text)
  if (!length(cand)) return(none)
  hits <- tryCatch(cv_find_marker_in_object(cc, cand, sets), error = function(e) NULL)
  if (is.null(hits) || !nrow(hits)) return(none)
  seen <- toupper(as.character(shown$Feature %||% character(0)))
  hits <- hits[!(toupper(hits$Feature) %in% seen), , drop = FALSE]
  if (!nrow(hits)) return(none)

  same <- hits[hits$Type == type & hits$Level == level, , drop = FALSE]
  other <- hits[!(hits$Type == type & hits$Level == level), , drop = FALSE]

  parts <- character(0)
  if (nrow(same))
    parts <- c(parts, sprintf(
      "%s in this same list, below the rows shown (%s)",
      paste(unique(same$Feature), collapse = ", "),
      paste(sprintf("rank %s of %s, purity %.4f", same$Rank, same$Membership, same$Purity),
            collapse = "; ")))
  if (nrow(other))
    parts <- c(parts, sprintf(
      "%s under a DIFFERENT marker class (%s)",
      paste(unique(other$Feature), collapse = ", "),
      paste(sprintf("%s marker of %s at %s, rank %s, purity %.4f",
                    tolower(other$Type), other$Membership, tolower(other$Level),
                    other$Rank, other$Purity), collapse = "; ")))

  # The table's set column is called `Set`; the lookup calls it `Membership`.
  # Without this the appended row arrives with Set = NA, which reads as "no
  # cluster" on a per-cluster table.
  if (nrow(same)) { same$Set <- same$Membership }

  msg <- sprintf(paste0(
    "This table is the top-ranked %s markers only, so it does NOT show whether a ",
    "particular gene is a marker. You named %s, and this object DOES record it: %s. ",
    "Do not read absence from this table as absence from the data."),
    tolower(type), paste(unique(hits$Feature), collapse = ", "),
    paste(parts, collapse = "; and "))
  list(append = if (nrow(same)) same else NULL, message = msg)
}

#' Find named gene(s) anywhere in a ClustoCell's / MarkoCell's stored markers.
#'
#' Searches ALL THREE marker classes at BOTH levels -- the whole point of the
#' item -- and reports where each gene really is, with the numbers already
#' computed and stored on the object. Free: no recomputation, no worker.
#'
#' @param obj a ClustoCell or MarkoCell.
#' @param features gene symbol(s) to find.
#' @param sets optional set ids to restrict to (e.g. "C1"); NULL searches all.
#' @return a data.frame with Feature, Level, Membership, Type, Gini_Score,
#'   Purity, Rank -- the same shape celliverse::featureInspect() returns, so the
#'   two are directly comparable. Zero rows when the gene is in none of them.
#' @noRd
cv_find_marker_in_object <- function(obj, features, sets = NULL) {
  empty <- data.frame(Feature = character(0), Level = character(0),
                      Membership = character(0), Type = character(0),
                      Gini_Score = numeric(0), Purity = numeric(0),
                      Rank = integer(0), stringsAsFactors = FALSE)
  features <- toupper(as.character(features %||% character(0)))
  features <- features[nzchar(features)]
  if (!length(features) || is.null(obj)) return(empty)
  levels_to_search <- if (inherits(obj, "MarkoCell")) "Cell subset"
                      else c("Major cluster", "Sub cluster")
  out <- list()
  for (lv in levels_to_search) {
    for (ty in names(CV_MARKER_TYPE_KEYS)) {
      loc <- tryCatch(.cv_marker_set_list(obj, CV_MARKER_TYPE_KEYS[[ty]], lv),
                      error = function(e) NULL)
      sl <- loc$set_list
      if (is.null(sl) || !length(sl)) next
      ids <- names(sl)
      if (!is.null(sets) && length(sets)) ids <- intersect(ids, as.character(sets))
      for (id in ids) {
        df <- sl[[id]]
        if (!is.data.frame(df) || !nrow(df) || !("Feature" %in% names(df))) next
        hit <- df[toupper(as.character(df$Feature)) %in% features, , drop = FALSE]
        if (!nrow(hit)) next
        num <- function(col) if (col %in% names(hit))
          suppressWarnings(as.numeric(hit[[col]])) else rep(NA_real_, nrow(hit))
        out[[length(out) + 1L]] <- data.frame(
          Feature = as.character(hit$Feature), Level = loc$level %||% lv,
          Membership = id, Type = ty,
          Gini_Score = num("Gini_Score"), Purity = num("Purity"),
          Rank = suppressWarnings(as.integer(num("Rank"))),
          stringsAsFactors = FALSE)
      }
    }
  }
  if (!length(out)) return(empty)
  res <- do.call(rbind, out)
  res[order(res$Feature, res$Level, res$Membership, res$Type), , drop = FALSE]
}

cv_cluster_markers_table <- function(cc, desired_sets = NULL,
                                     level = "Major cluster", type = "Positive",
                                     top_n = 10L, mode = c("rows", "rank"),
                                     request_text = NULL, filter = NULL) {
  mode <- match.arg(mode)
  type_key <- switch(type,
    "Positive" = "positive_markers",
    "Negative" = "negative_markers",
    "Medium"   = "medium_markers",
    cli::cli_abort("Unknown marker type {.val {type}}; use Positive, Negative, or Medium.")
  )

  # Round LXXVI: the collection walk moved to .cv_marker_set_list() so the
  # stored-marker LOOKUP uses the identical navigation. The error messages below
  # are unchanged and stay here, because a search wants NULL where a table tool
  # wants an abort.
  loc <- .cv_marker_set_list(cc, type_key, level)
  set_list <- loc$set_list
  level <- loc$level

  if (is.null(set_list) || !length(set_list)) {
    if (inherits(cc, "MarkoCell")) {
      cli::cli_abort(c(
        "This MarkoCell object has no {tolower(type)} cell-subset markers to read.",
        i = "Run markoCell on a cell subset first (it creates a MarkoCell with the subset's markers)."
      ))
    }
    if (is.null(cc$markers)) {
      cli::cli_abort("This ClustoCell object has no stored markers ($markers is empty).")
    }
    cli::cli_abort(c(
      "No {tolower(type)} markers at level {.val {level}} in this ClustoCell object.",
      i = "Try level='Major cluster' with type='Positive', or check the object with describe_object."
    ))
  }

  available <- names(set_list)
  sets <- desired_sets
  if (is.null(sets) || !length(sets)) {
    sets <- available
  } else {
    # Tolerate case/separator differences ("C1-sub2" -> "C1-Sub2"); aborts with
    # the available list only when a request is truly unknown or ambiguous.
    sets <- cv_resolve_set_ids(sets, available, level = level)
  }

  # Parse the user's intent ONCE against the columns actually present across the
  # requested sets, then apply the SAME spec to every set. request_text (the
  # verbatim user phrase) wins over an explicit `filter`, which wins over the
  # legacy mode/top_n path (whose exact wording is preserved below).
  cols_union <- unique(unlist(lapply(sets, function(s) {
    d <- set_list[[s]]; if (is.data.frame(d)) names(d) else character(0)
  })))
  spec <- cv_parse_marker_filter(request_text = request_text, filter = filter,
                                 mode = mode, top_n = top_n, columns = cols_union)

  rows <- list()
  notes <- character(0)
  for (s in sets) {
    df <- set_list[[s]]
    # logMessage sentinel or non-data.frame -> honest note, not an error.
    if (is.null(df) || !is.data.frame(df) || !nrow(df)) {
      msg <- if (inherits(df, "logMessage")) as.character(df)[1] else "No markers identified"
      notes <- c(notes, sprintf("%s: %s", s, msg))
      next
    }
    take <- cv_apply_marker_filter(df, spec)
    if (is.null(take) || !nrow(take)) {
      notes <- c(notes, sprintf("%s: no markers matched the filter", s))
      next
    }
    take$Set <- s
    rows[[length(rows) + 1L]] <- take
  }

  if (!length(rows)) {
    # Every requested set was empty / sentinel / matched nothing.
    extra <- if (!identical(spec$source, "mode"))
      sprintf(" Interpretation (from your request): %s.", spec$interpretation) else ""
    empty_res <- list(kind = "table",
                table = data.frame(Set = character(0), Feature = character(0),
                                   Rank = integer(0), stringsAsFactors = FALSE),
                text = sprintf("No %s markers found. %s%s", tolower(type),
                               paste(notes, collapse = "; "), extra))
    if (!is.na(spec$unparsed %||% NA_character_))
      empty_res <- cv_result_add_warnings(empty_res, list(cv_warn(
        "may_invalidate",
        sprintf(paste0(
          "I could not turn \"%s\" into a filter, so the default selection was used ",
          "instead of what you asked for."), spec$unparsed),
        "marker_filter_unparsed")))
    return(empty_res)
  }

  out <- do.call(rbind, lapply(rows, function(d) {
    # Reorder columns: Set, Feature, Rank, then any remaining score columns.
    front <- intersect(c("Set", "Feature", "Rank"), names(d))
    rest  <- setdiff(names(d), front)
    d[, c(front, rest), drop = FALSE]
  }))
  rownames(out) <- NULL

  # Message states EXACTLY which interpretation was applied. The legacy
  # mode/top_n path keeps its precise historical wording; the request_text /
  # filter path appends an explicit "Interpretation (from your request): ..."
  # clause so the user always sees how their phrase was understood.
  if (identical(spec$source, "mode")) {
    if (identical(spec$kind, "rank")) {
      txt <- sprintf(
        "%s marker(s) with Rank <= %d at level '%s' for %d set(s): %s (%d row(s); ties at the same rank are all included).",
        type, spec$n, level, length(sets), paste(sets, collapse = ", "), nrow(out))
    } else {
      txt <- sprintf("Top %d %s marker(s) (by row) at level '%s' for %d set(s): %s.",
                     spec$n, tolower(type), level, length(sets),
                     paste(sets, collapse = ", "))
    }
  } else {
    head_txt <- switch(spec$kind,
      rank = sprintf("%s marker(s) with Rank <= %d", type, spec$n),
      rows = sprintf("Top %d %s marker(s) (by row)", spec$n, tolower(type)),
      predicate = sprintf("%s marker(s) matching your filter", type))
    txt <- sprintf("%s at level '%s' for %d set(s): %s (%d row(s)). Interpretation (from your request): %s.",
                   head_txt, level, length(sets), paste(sets, collapse = ", "),
                   nrow(out), spec$interpretation)
  }
  # Round LXIX (audit #24): these used to be pasted onto `txt` behind a bare
  # " Note: ", which is the exact string the audit names as the problem -- "one
  # cluster had no hits" reading identically to a caveat that invalidates the
  # answer.
  #
  # INFO, deliberately, and this is the audit's own worked example. The table
  # is correct and complete for the sets that produced rows; a set with nothing
  # matching the filter is a fact about the data, reported accurately. Making
  # it amber would put a warning on ordinary, correct runs.
  # Round LXXVI: a gene the user NAMED, missing from this slice but present
  # deeper in the object. This is the reported defect, closed in code.
  oos <- .cv_out_of_slice_hits(cc, request_text, out, sets, type, level)
  if (!is.null(oos$append) && nrow(oos$append)) {
    # Appended ONLY when the hit is the same type and level as this table --
    # deeper down the same ranking, which is genuinely the same table. A hit in
    # a DIFFERENT class is reported in the warning and NOT appended, because
    # dropping a medium marker into a positive-marker table would corrupt the
    # one thing the table is captioned as.
    add <- oos$append[, intersect(names(out), names(oos$append)), drop = FALSE]
    for (cn in setdiff(names(out), names(add))) add[[cn]] <- NA
    out <- rbind(out, add[, names(out), drop = FALSE])
    txt <- paste0(txt, sprintf(
      " Plus %d row(s) for gene(s) you named that rank below the top %d: %s.",
      nrow(oos$append), spec$n %||% top_n,
      paste(sprintf("%s (rank %s)", oos$append$Feature, oos$append$Rank), collapse = ", ")))
  }

  res <- list(kind = "table", table = out, text = txt)
  if (nzchar(oos$message %||% "")) {
    # MAY_INVALIDATE. The whole defect is that a reader took absence from a
    # top-N slice for absence from the data, and then wrote "not expressed" and
    # "not a CD8+ T cell cluster". Nothing on screen contradicted them.
    res <- cv_result_add_warnings(res, list(cv_warn(
      "may_invalidate", oos$message, "marker_outside_slice")))
  }
  # Round LXXIV (audit #11). MAY_INVALIDATE: the table on screen is NOT the one
  # that was asked for, and every visible thing about it -- the row count, the
  # confident "Top 10 ... (by row)" caption -- is accurate for a request the
  # user did not make. A reader who skips this reads a filtered table.
  if (!is.na(spec$unparsed %||% NA_character_))
    res <- cv_result_add_warnings(res, list(cv_warn(
      "may_invalidate",
      sprintf(paste0(
        "I could not turn \"%s\" into a filter, so these are the default %s instead of ",
        "what you asked for. A filter needs the COLUMN as well as the number - \"%s\" ",
        "does not say which quantity to compare. Restate it in full, e.g. ",
        "\"purity > 0.2\" or \"rank <= 5\"."),
        spec$unparsed, tolower(spec$interpretation %||% "results"), spec$unparsed),
      "marker_filter_unparsed")))
  if (length(notes))
    res <- cv_result_add_warnings(res, list(cv_warn(
      "info",
      sprintf("Some requested sets returned no rows: %s.", paste(notes, collapse = "; ")),
      "markers_empty_sets")))
  res
}

# ---- Cluster-cell retrieval helpers -----------------------------------------

# Extract the ALL cell barcodes belonging to one cluster id from any supported
# object, plus the vector of available cluster ids (for error messages).
#
# ClustoCell: reads the appropriate $clusters slot for the requested `level`
#   ("major" -> major_clusters, "sub"/"merged_sub" -> merged_sub_clusters), each
#   a named character vector (names = barcodes, values = cluster id). Cells for
#   cluster cid = names(v)[v == cid], mirroring the marker-access pattern used
#   throughout the package.
# Seurat / SCE: reads a metadata column (`cluster_column`, defaulting to
#   "ClustoCell_Clusters", auto-detected if that is absent) and returns the
#   cell names whose value == cid.
#
# Returns list(cells=<chr>, available=<chr sorted unique ids>, column=<used col
# name or NA>). Aborts (cli) only for genuinely unusable input; an unknown
# cluster is reported by the CALLER using `available`.
#' @noRd
cv_cluster_cell_names <- function(obj, cluster, level = "major",
                                  cluster_column = NULL) {
  cluster <- as.character(cluster)[1]

  if (inherits(obj, "ClustoCell")) {
    cl <- obj$clusters
    if (is.null(cl) || !is.list(cl)) {
      cli::cli_abort("This ClustoCell object has no cluster assignments ($clusters is empty).")
    }
    vec <- switch(level,
      "major"      = cl$major_clusters,
      "sub"        = cl$merged_sub_clusters %||% cl$sub_clusters,
      "merged_sub" = cl$merged_sub_clusters %||% cl$sub_clusters,
      cl$major_clusters)
    if (is.null(vec) || !length(vec)) {
      cli::cli_abort(c(
        "No cluster assignments found at level {.val {level}} in this ClustoCell object.",
        i = "Use level='major' (or run clustoCell with identify_subclusters=TRUE for sub-clusters)."
      ))
    }
    vec <- stats::setNames(as.character(vec), names(vec))
    available <- sort(unique(vec))
    cells <- names(vec)[vec == cluster]
    return(list(cells = cells, available = available, column = NA_character_))
  }

  if (inherits(obj, c("Seurat", "SingleCellExperiment", "SpatialExperiment"))) {
    md <- if (inherits(obj, "Seurat")) {
      as.data.frame(obj@meta.data)
    } else {
      as.data.frame(SummarizedExperiment::colData(obj))
    }
    if (!nrow(md) || !ncol(md)) {
      cli::cli_abort("This object has no cell metadata to read cluster ids from.")
    }
    col <- cluster_column %||% NA_character_
    if (is.na(col) || !nzchar(col) || !(col %in% names(md))) {
      # Auto-detect: prefer the CelliVerse default, then any column that
      # actually contains the requested cluster id, then the first column whose
      # name looks cluster-like.
      cand <- c("ClustoCell_Clusters", "ClustoCell_SubClusters")
      hit <- cand[cand %in% names(md)]
      contains_id <- names(md)[vapply(md, function(x)
        cluster %in% as.character(x), logical(1))]
      like <- grep("cluster|clust|ident|seurat_clusters", names(md),
                   ignore.case = TRUE, value = TRUE)
      col <- (c(intersect(hit, contains_id), contains_id, hit, like))[1]
      if (is.na(col) || is.null(col)) {
        cli::cli_abort(c(
          "Could not find a cluster-id metadata column.",
          i = "Pass {.arg cluster_column} explicitly (see {.fn get_metadata_columns}). Columns: {.val {names(md)}}."
        ))
      }
    }
    vals <- as.character(md[[col]])
    barcodes <- rownames(md)
    available <- sort(unique(vals))
    cells <- barcodes[vals == cluster]
    return(list(cells = cells, available = available, column = col))
  }

  cli::cli_abort("get_cluster_cells needs a ClustoCell, Seurat, or SingleCellExperiment/SpatialExperiment object.")
}

# Build a CellSet from an extracted barcode vector, applying optional
# reproducible random sampling. `n` NULL/NA/>=length -> all cells (no sampling).
# Returns list(cellset=<CellSet>, sampled=<bool>, n_total=<int>, seed=<int|NA>).
#' @noRd
cv_make_cellset <- function(cells, cluster, level, source_handle,
                            n = NULL, seed = 9999L) {
  n_total <- length(cells)
  sampled <- FALSE
  used_seed <- NA_integer_
  take <- cells
  if (!is.null(n) && !is.na(n)) {
    n <- as.integer(n)
    # BATCH2 FIX: n <= 0 (a model/caller passing n=0 or a negative count)
    # used to fall through this guard entirely and silently return ALL
    # cells, unsampled -- with no error or indication that the requested
    # count was not honored. n <= 0 unambiguously means "take none".
    if (!is.na(n) && n <= 0L) {
      take <- cells[0]
      sampled <- TRUE
      used_seed <- NA_integer_
    } else if (!is.na(n) && n > 0L && n < n_total) {
      seed <- as.integer(seed %||% 9999L)
      # Local, reproducible sampling that does NOT disturb the global RNG state.
      old <- if (exists(".Random.seed", envir = .GlobalEnv))
               get(".Random.seed", envir = .GlobalEnv) else NULL
      set.seed(seed)
      take <- sample(cells, n)
      if (!is.null(old)) assign(".Random.seed", old, envir = .GlobalEnv)
      sampled <- TRUE
      used_seed <- seed
    }
  }
  cs <- structure(
    list(cells = take, cluster = as.character(cluster)[1], level = level,
         source_handle = source_handle %||% NA_character_,
         sampled = sampled, seed = used_seed, n_total = n_total),
    class = "CellSet")
  list(cellset = cs, sampled = sampled, n_total = n_total, seed = used_seed)
}

#' Register the core CelliVerse tools
#' @noRd
cv_register_core_tools <- function() {
  list(

    # ---- 1. clustoCell [HEAVY] : main entry point -------------------------
    cv_tool(
      name = "clustoCell",
      description = paste(
        "Run CelliVerse ClustoCell: unsupervised EWCSR-based clustering of",
        "single-cell data with automatic marker discovery and optional",
        "sub-clustering. This is the main entry point. Input is a Seurat/SCE",
        "object or a (sparse) count matrix; output is a ClustoCell object.",
        "Works directly on raw counts (log1p=TRUE by default). For very large",
        "datasets enable sketch=TRUE."),
      parameters = list(
        data = cv_param("handle", "The single-cell object or matrix to cluster.",
                        required = TRUE, handle_types = c("Seurat", "SingleCellExperiment", "SpatialExperiment", "dgCMatrix", "matrix")),
        assay = cv_param("string", "Assay to use.", default = "RNA"),
        layer = cv_param("string", "Layer/slot to use.", default = "counts"),
        log1p = cv_param("boolean", "Log1p-transform the data. Set FALSE if the layer is already normalized/log.", default = TRUE),
        gini_thresh = cv_param("number", "Gini threshold for detecting globally uninformative features (0-1).", default = 0.5, min = 0, max = 1),
        identify_subclusters = cv_param("boolean", "Also detect sub-clusters within each major cluster.", default = TRUE),
        sketch = cv_param("boolean", "Enable sketching for large datasets (cluster a representative subset, then transfer labels).", default = FALSE),
        sketch_ncells = cv_param("integer", "Number of cells in the sketch when sketch=TRUE.", default = 5000L, min = 1),
        refine_transferred_subClusters = cv_param("boolean", paste(
          "Only relevant when sketch=TRUE and identify_subclusters=TRUE. If FALSE",
          "(default) sub-cluster labels are transferred directly from the sketch;",
          "if TRUE, major-cluster labels are transferred first and sub-clusters are",
          "then re-detected within each major cluster on the full data (slower but",
          "often higher quality)."), default = FALSE),
        leiden_resolution = cv_param("number", "Leiden clustering resolution (higher = more clusters).", default = 1, min = 0),
        num_threads = cv_param("integer", "Threads (-1 = all available). The agent overrides this per worker.", default = -1L),
        seed = cv_param("integer", "Random seed.", default = 121L)
      ),
      input_object_types = c("Seurat", "SingleCellExperiment", "SpatialExperiment", "dgCMatrix", "matrix"),
      output_object_type = "ClustoCell",
      cost = "heavy", produces = "object", tier = "core",
      next_suggestions = c("addClustoData", "getDatasetMarkers", "markoClustVis", "typoClust", "featureInspect"),
      # Round LXX (audit #13): both halves of the parameter-combination guard.
      # Round LXXVII (audit #36): and the re-clustering notice.
      # Round LXXXV: the memory-budget check, LAST and deliberately so -- a
      # sketch_ncells that is not even smaller than the data is refused by
      # .cv_assert_sketch_fits() above before a memory estimate is ever built
      # from it.
      validate = function(store, args, tool, warnings) {
        .cv_assert_sketch_fits(store, args)
        .cv_note_inert_refine(args, warnings)
        .cv_note_existing_clusters(store, args, warnings)
        .cv_assert_heavy_object_fits(store, args, tool)
        # Round LXXXVI: LAST and deliberately so -- only reached once the
        # mandatory memory check above has already returned silently.
        .cv_offer_speed_sketch(store, args, tool, sketch_kind = "ncells")
      },
      handler = function(store, args) {
        inh <- .cv_input_handles(attr(args, "cv_tool"), args, attr(args, "handle_args"))
        a <- .cv_materialize(store, args, attr(args, "handle_args"))
        res <- do.call(celliverse::clustoCell, a)
        rec <- .cv_result_object(store, res, source = "clustoCell()", inherit_from = inh)
        # Round LXXIV (audit #16): same helper as the worker path.
        cv_result_add_warnings(rec, .cv_clustering_warnings(res))
      }
    ),

    # ---- 2. markoClust [HEAVY] : markers for predefined clusters -----------
    cv_tool(
      name = "markoClust",
      description = paste(
        "Identify and rank positive/negative markers for clusters that already",
        "exist in a Seurat/SCE object (e.g. from Seurat's FindClusters). Point",
        "cluster_labels at the metadata column holding the cluster ids.",
        "Optionally also detect sub-clusters. For very large datasets, enable",
        "sketch=TRUE together with identify_subclusters=TRUE to speed up",
        "sub-cluster detection. Output is a MarkoClust object."),
      parameters = list(
        data = cv_param("handle", "Seurat/SCE object with existing cluster labels.",
                        required = TRUE, handle_types = c("Seurat", "SingleCellExperiment", "SpatialExperiment")),
        cluster_labels = cv_param("string", "Name of the metadata column holding cluster ids.", required = TRUE),
        assay = cv_param("string", "Assay to use.", default = "RNA"),
        layer = cv_param("string", "Layer/slot to use.", default = "counts"),
        log1p = cv_param("boolean", "Log1p-transform the data.", default = TRUE),
        gini_thresh = cv_param("number", "Gini threshold for globally uninformative features.", default = 0.5, min = 0, max = 1),
        identify_subclusters = cv_param("boolean", "Also detect sub-clusters within each cluster.", default = FALSE),
        sketch = cv_param("boolean", paste(
          "Enable sketching for large datasets. Only takes effect together with",
          "identify_subclusters=TRUE -- sketching applies to sub-cluster detection."),
          default = FALSE),
        sketch_fraction = cv_param("number", paste(
          "Fraction of cells per cluster to keep in the sketch when sketch=TRUE",
          "(only takes effect with identify_subclusters=TRUE). For small datasets",
          "(under 10,000 cells), at least 0.5 is recommended."),
          default = 0.5, min = 0, max = 0.99),
        num_threads = cv_param("integer", "Threads (-1 = all).", default = -1L),
        seed = cv_param("integer", "Random seed.", default = 121L)
      ),
      input_object_types = c("Seurat", "SingleCellExperiment", "SpatialExperiment"),
      output_object_type = "MarkoClust",
      cost = "heavy", produces = "object", tier = "core",
      next_suggestions = c("markoClustVis", "typoClust"),
      # Round LXX (audit #12). Round LXXXVI: the speed-only sketch offer, gated
      # on identify_subclusters=TRUE since markoClust's own sketch/sketch_fraction
      # do nothing otherwise (see markoClust.R's documented precondition) --
      # offering it in that mode would be a fabricated choice, not a real one.
      validate = function(store, args, tool, warnings) {
        .cv_assert_metadata_column(store, args, "cluster_labels", "data")
        .cv_offer_speed_sketch(store, args, tool, sketch_kind = "fraction")
      },
      handler = function(store, args) {
        inh <- .cv_input_handles(attr(args, "cv_tool"), args, attr(args, "handle_args"))
        a <- .cv_materialize(store, args, attr(args, "handle_args"))
        res <- do.call(celliverse::markoClust, a)
        .cv_result_object(store, res, source = "markoClust()", inherit_from = inh)
      }
    ),

    # ---- 3. markoCell [HEAVY] : markers for arbitrary cell subsets ---------
    cv_tool(
      name = "markoCell",
      description = paste(
        "Identify and rank markers for a specific SUBSET of cells (which may be",
        "a cluster, sub-cluster, an arbitrary set of cell barcodes, or even a",
        "single cell). Provide desired_cells as a named list of barcode vectors,",
        "OR cluster_labels + desired_clusters. Output is a MarkoCell object."),
      parameters = list(
        data = cv_param("handle", "Seurat/SCE object containing the cells.",
                        required = TRUE, handle_types = c("Seurat", "SingleCellExperiment", "SpatialExperiment")),
        desired_cells = cv_param("object", "Named list of character vectors of cell barcodes, e.g. {\"subsetA\": [\"cell1\",\"cell2\"]}. You may also pass a CellSet handle string produced by get_cluster_cells (e.g. \"cellset_ab12cd\") to analyse exactly that stored subset."),
        cluster_labels = cv_param("string", "Metadata column with cluster ids (alternative to desired_cells)."),
        desired_clusters = cv_param("array", "Cluster ids to analyze (used with cluster_labels).", items = "string"),
        assay = cv_param("string", "Assay to use.", default = "RNA"),
        layer = cv_param("string", "Layer/slot to use.", default = "counts"),
        log1p = cv_param("boolean", "Log1p-transform the data.", default = TRUE),
        num_threads = cv_param("integer", "Threads (-1 = all).", default = -1L),
        seed = cv_param("integer", "Random seed.", default = 9999L)
      ),
      input_object_types = c("Seurat", "SingleCellExperiment", "SpatialExperiment"),
      output_object_type = "MarkoCell",
      cost = "heavy", produces = "object", tier = "core",
      next_suggestions = c("markoClustVis", "typoClust"),
      # Round LXIV (D1): the CellSet expansion and the subset guard MUST run on
      # the heavy path too. They live in `prepare` now; the handler below simply
      # calls the same helper, so the two paths cannot diverge.
      # Round LXX (audit #12); Round LXXIV (audit #14).
      validate = function(store, args, tool, warnings) {
        .cv_assert_metadata_column(store, args, "cluster_labels", "data")
        .cv_warn_small_subset(store, args, warnings, "markoCell")
      },
      prepare = function(store, args, tool, handle_args)
        .cv_prepare_cell_subset(store, tool, args, handle_args, require_subset = TRUE),
      handler = function(store, args) {
        p <- .cv_prepare_cell_subset(store, attr(args, "cv_tool"), args,
                                     attr(args, "handle_args"), require_subset = TRUE)
        args <- p$args
        a <- .cv_materialize(store, args, attr(args, "handle_args"))
        res <- do.call(celliverse::markoCell, a)
        .cv_result_object(store, res, source = "markoCell()", inherit_from = p$inherit_from)
      }
    ),

    # ---- 4. markerPurity [HEAVY] : purity of a marker in a group -----------
    cv_tool(
      name = "markerPurity",
      description = paste(
        "Quantify how purely one or more marker genes are expressed within a",
        "cluster or cell subset, using the EWCSR framework and Gini coefficient.",
        "Returns a MarkerPurity object with per-group purity values."),
      parameters = list(
        data = cv_param("handle", "Seurat/SCE object.", required = TRUE,
                        handle_types = c("Seurat", "SingleCellExperiment", "SpatialExperiment")),
        desired_markers = cv_param("array", "Marker gene(s) to assess.", items = "string", required = TRUE),
        cluster_labels = cv_param("string", "Metadata column with cluster ids."),
        desired_clusters = cv_param("array", "Cluster ids to evaluate.", items = "string"),
        desired_cells = cv_param("object", "Named list of cell-barcode vectors (alternative to clusters). May also be a CellSet handle string from get_cluster_cells."),
        assay = cv_param("string", "Assay to use.", default = "RNA"),
        layer = cv_param("string", "Layer/slot to use.", default = "counts"),
        log1p = cv_param("boolean", "Log1p-transform the data.", default = TRUE),
        num_threads = cv_param("integer", "Threads (-1 = all).", default = -1L),
        seed = cv_param("integer", "Random seed.", default = 121L)
      ),
      input_object_types = c("Seurat", "SingleCellExperiment", "SpatialExperiment"),
      output_object_type = "MarkerPurity",
      cost = "heavy", produces = "object", tier = "core",
      # Round LXIV (D1): as markoCell. markerPurity has no subset GUARD (it can
      # legitimately run on whole clusters), but its CellSet expansion was dead
      # on the heavy path in exactly the same way.
      # Round LXX (audit #12); Round LXXIV (audit #14).
      validate = function(store, args, tool, warnings) {
        .cv_assert_metadata_column(store, args, "cluster_labels", "data")
        .cv_warn_small_subset(store, args, warnings, "markerPurity")
      },
      prepare = function(store, args, tool, handle_args)
        .cv_prepare_cell_subset(store, tool, args, handle_args, require_subset = FALSE),
      handler = function(store, args) {
        p <- .cv_prepare_cell_subset(store, attr(args, "cv_tool"), args,
                                     attr(args, "handle_args"), require_subset = FALSE)
        args <- p$args
        a <- .cv_materialize(store, args, attr(args, "handle_args"))
        res <- do.call(celliverse::markerPurity, a)
        .cv_result_object(store, res, source = "markerPurity()", inherit_from = p$inherit_from)
      }
    ),

    # ---- 5. getDatasetMarkers [LIGHT] : feature selection ------------------
    cv_tool(
      name = "getDatasetMarkers",
      description = paste(
        "Extract dataset-level features (markers) from a ClustoCell object for",
        "downstream dimensionality reduction / ML - a CelliVerse alternative to",
        "highly variable genes. Choose major/sub clusters and positive/negative/",
        "medium markers. Returns a DatasetMarkers object with $combined_markers."),
      parameters = list(
        obj = cv_param("handle", "A ClustoCell object.", required = TRUE, handle_types = "ClustoCell"),
        clusters = cv_param("boolean", "Include markers from major clusters.", default = TRUE),
        sub_clusters = cv_param("boolean", "Include markers from sub-clusters.", default = TRUE),
        positive_markers = cv_param("boolean", "Include positive markers.", default = TRUE),
        negative_markers = cv_param("boolean", "Include negative markers.", default = FALSE),
        medium_markers = cv_param("boolean", "Include medium markers.", default = FALSE),
        pos_thresh = cv_param("integer", paste(
          "How many positive markers to keep per set. Read according to",
          "thresh_mode: with 'n' it is exactly that many rows; with 'rank' it is",
          "every marker ranked at or better than this number, so ties can return",
          "more rows than the number given."), default = 25L, min = 0),
        neg_thresh = cv_param("integer", paste(
          "How many negative markers to keep per set. Read according to",
          "thresh_mode, exactly as pos_thresh is."), default = 20L, min = 0)
      ),
      input_object_types = "ClustoCell",
      output_object_type = "DatasetMarkers",
      cost = "light", produces = "object", tier = "core",
      handler = function(store, args) {
        inh <- .cv_input_handles(attr(args, "cv_tool"), args, attr(args, "handle_args"))
        a <- .cv_materialize(store, args, attr(args, "handle_args"))
        res <- do.call(celliverse::getDatasetMarkers, a)
        rec <- .cv_result_object(store, res, source = "getDatasetMarkers()", inherit_from = inh)
        # also surface the feature list as a small table
        cm <- tryCatch(res$combined_markers, error = function(e) NULL)
        if (!is.null(cm)) rec$table <- data.frame(feature = as.character(cm), stringsAsFactors = FALSE)
        rec
      }
    ),

    # ---- 5b. getClusterMarkers [LIGHT] : ranked marker TABLE from ClustoCell
    # Read-only. The markers are ALREADY stored in the ClustoCell object
    # ($markers); this tool simply returns the top-N ranked markers of the
    # requested cluster(s)/sub-cluster(s) as a table. It does NOT recompute
    # anything and creates NO new object. This is the correct way to answer
    # "give me the top 10 markers of cluster C1" - do NOT use markoCell for that
    # (markoCell is only for a NEW user-defined cell subset).
    cv_tool(
      name = "getClusterMarkers",
      description = paste(
        "Return the TOP-N RANKED marker genes of an existing cluster,",
        "sub-cluster, OR user-defined cell subset as a TABLE, read directly from",
        "the object's stored markers. Works on a ClustoCell (cluster/sub-cluster",
        "markers in $markers) OR a MarkoCell (the cell-subset markers it just",
        "computed, e.g. after running markoCell on a set of barcodes). Use this",
        "to answer 'top N markers of cluster Cx' OR 'top N markers of my subset",
        "/ of markocell_<x>'. Columns include Feature and Rank (plus",
        "Gini_Score/Purity). For a ClustoCell this is the correct tool for",
        "markers of an EXISTING cluster (do NOT use markoCell for that). To PLOT",
        "the markers instead of listing them, use markoClustVis."),
      parameters = list(
        clustoCell = cv_param("handle", paste(
          "A ClustoCell object whose stored markers to read, OR a MarkoCell",
          "object (reads its cell-subset marker table)."),
          required = TRUE, handle_types = c("ClustoCell", "MarkoCell")),
        desired_sets = cv_param("array", paste(
          "Cluster/sub-cluster id(s) to return, e.g. ['C1'] or ['C1','C3'].",
          "REQUIRED whenever the user names specific cluster(s): pass exactly",
          "those ids. If omitted, the tool returns EVERY cluster - only do that",
          "when the user explicitly asks for all clusters."), items = "string"),
        level = cv_param("string", paste(
          "Which marker collection to read. IMPORTANT: a cluster id only exists",
          "at ONE level. Major-cluster ids look like 'C1','C2',...; sub-cluster",
          "ids look like 'C1-Sub1','C2-Sub3',... (they contain '-Sub'). If the",
          "user names a sub-cluster (id contains '-Sub', e.g. 'C1-Sub1'), you",
          "MUST set level='Sub cluster'; for a plain major cluster ('C1') use",
          "level='Major cluster'. If a call fails with 'Unknown set id' at one",
          "level, RETRY the SAME id at the OTHER level instead of guessing a",
          "different id."), default = "Major cluster",
                         enum = c("Major cluster", "Sub cluster")),
        type = cv_param("string", "Marker type to return.", default = "Positive",
                        enum = c("Positive", "Negative", "Medium")),
        top_n = cv_param("integer", "The N in 'top N'. Number of rows (mode='rows') or the maximum Rank value (mode='rank').", default = 10L),
        mode = cv_param("string", paste(
          "How to interpret top_n. 'rows' (default) = return EXACTLY top_n rows",
          "(first N by rank); pick this for 'top N', 'top N rows', 'first N',",
          "'N markers'. 'rank' = return ALL markers whose Rank <= top_n (ties at",
          "the same rank are ALL kept, so this can return MORE than top_n rows);",
          "pick this for 'top N ranked', 'rank N or below', 'ranked N or better',",
          "'markers ranked <= N'. If the wording is ambiguous, use 'rows'."),
          default = "rows", enum = c("rows", "rank")),
        request_text = cv_param("string", paste(
          "The user's VERBATIM natural-language phrase describing WHICH markers",
          "they want, copied unchanged (e.g. 'top 10 ranked markers of C1-Sub1',",
          "'markers with purity over 0.5', 'rank between 2 and 5', 'feature is",
          "CD3E'). ALWAYS pass this whenever the user constrains the markers: the",
          "server parses it deterministically (case-insensitive, ties-aware) and",
          "reports how it was interpreted. It takes precedence over mode/top_n/",
          "filter when it expresses a clear intent, so relaying the phrase is the",
          "most reliable way to honour exactly what the user asked.")),
        filter = cv_param("string", paste(
          "Optional compact, explicit filter expression, used only if",
          "request_text does not already express a filter, e.g. 'Purity>0.5',",
          "'Rank<=5', 'Gini_Score between 0.3 and 0.6', 'Feature=CD3E'. Supported",
          "columns: Feature, Gini_Score, Purity, Rank. Operators: > >= < <= = !=",
          "and 'between A and B'."))
      ),
      input_object_types = c("ClustoCell", "MarkoCell"),
      output_object_type = NA_character_,
      cost = "light", produces = "table", tier = "core",
      next_suggestions = c("markoClustVis", "featureInspect", "typoClust"),
      handler = function(store, args) {
        a <- .cv_materialize(store, args, attr(args, "handle_args"))
        cc <- a$clustoCell
        level <- a$level %||% "Major cluster"
        type  <- a$type  %||% "Positive"
        top_n <- as.integer(a$top_n %||% 10L)
        mode  <- match.arg(as.character(a$mode %||% "rows"), c("rows", "rank"))
        cv_cluster_markers_table(cc, desired_sets = a$desired_sets,
                                 level = level, type = type, top_n = top_n,
                                 mode = mode,
                                 request_text = a$request_text,
                                 filter = a$filter)
      }
    ),

    # ---- 5c. get_cluster_cells [LIGHT] : cell barcodes of a cluster --------
    # Read-only extraction of the cell barcodes that belong to one cluster, from
    # a ClustoCell ($clusters) OR a Seurat/SCE metadata column. Returns the
    # barcodes as a downloadable table AND stores them as a reusable CellSet
    # object handle that downstream tools (markoCell, markerPurity) accept as
    # `desired_cells`, so "run markoCell on that subset" works in a follow-up.
    # Optional reproducible random sampling (seed default 9999); omit n for all.
    cv_tool(
      name = "get_cluster_cells",
      description = paste(
        "Return the CELL BARCODES (cell names) that belong to a given cluster,",
        "read from a ClustoCell object's cluster assignments OR a Seurat/SCE",
        "metadata column. Use this whenever the user asks for the 'names',",
        "'barcodes', 'cell ids' or 'which cells' of a cluster, or for 'N random",
        "cells from cluster Cx'. The result is a downloadable table of barcodes",
        "AND a reusable CellSet handle. That handle can be passed straight to",
        "markoCell or markerPurity as the cell subset (desired_cells), so a",
        "follow-up like 'run markoCell on that subset' just references the",
        "CellSet handle. To draw markers as a table/plot for an EXISTING cluster",
        "use getClusterMarkers/markoClustVis instead - this tool returns cells,",
        "not markers."),
      parameters = list(
        object = cv_param("handle", paste(
          "The object to read cluster membership from: a ClustoCell",
          "('clusto_...') handle, or a Seurat/SCE ('obj_...'/'sce_...') handle",
          "that carries cluster ids in a metadata column."),
          required = TRUE,
          handle_types = c("ClustoCell", "Seurat", "SingleCellExperiment", "SpatialExperiment")),
        cluster = cv_param("string", paste(
          "The cluster id whose cells to return, e.g. 'C2'. Must be one of the",
          "object's existing cluster ids."), required = TRUE),
        level = cv_param("string", paste(
          "For a ClustoCell object, which assignment to read: 'major' (default)",
          "or 'sub'/'merged_sub' for sub-clusters. Ignored for Seurat/SCE."),
          default = "major", enum = c("major", "sub", "merged_sub")),
        cluster_column = cv_param("string", paste(
          "For a Seurat/SCE object, the metadata column holding cluster ids.",
          "Optional: defaults to 'ClustoCell_Clusters' and otherwise auto-detects",
          "a cluster-like column. Ignored for a ClustoCell object.")),
        n = cv_param("integer", paste(
          "Optional number of cells to RANDOMLY sample from the cluster. Omit",
          "(or set >= cluster size) to return ALL cells of the cluster. If n",
          "exceeds the cluster size, all cells are returned with a note.")),
        seed = cv_param("integer", paste(
          "Random seed for reproducible sampling (only used when n triggers",
          "sampling)."), default = 9999L),
        name = cv_param("string", paste(
          "Optional name for the stored CellSet (used as the subset name when",
          "fed to markoCell). If omitted, an automatic name",
          "'cellset_<cluster>_<k>' is used."))
      ),
      input_object_types = c("ClustoCell", "Seurat", "SingleCellExperiment", "SpatialExperiment"),
      output_object_type = "CellSet",
      cost = "light", produces = "object", tier = "core",
      next_suggestions = c("markoCell", "markerPurity"),
      handler = function(store, args) {
        src_handle <- args$object
        a <- .cv_materialize(store, args, attr(args, "handle_args"))
        obj     <- a$object
        cluster <- as.character(a$cluster %||% "")[1]
        level   <- a$level %||% "major"
        if (!nzchar(cluster)) {
          cli::cli_abort(c("No cluster id was given.",
                           i = "Pass {.arg cluster}, e.g. cluster='C2'."))
        }
        ex <- cv_cluster_cell_names(obj, cluster, level = level,
                                    cluster_column = a$cluster_column)
        if (!length(ex$cells)) {
          cli::cli_abort(c(
            "Cluster {.val {cluster}} has no cells (or is not a valid cluster id).",
            i = "Available cluster id(s): {.val {ex$available}}."
          ))
        }
        # Build CellSet (+ optional reproducible sampling).
        n_req <- if (is.null(a$n)) NULL else as.integer(a$n)
        mk <- cv_make_cellset(ex$cells, cluster = cluster, level = level,
                              source_handle = src_handle, n = n_req,
                              seed = as.integer(a$seed %||% 9999L))
        cs <- mk$cellset

        # Honor a user-supplied name; else auto-name cellset_<cluster>_<k>.
        set_name <- a$name %||% NA_character_
        if (is.na(set_name) || !nzchar(set_name)) {
          set_name <- sprintf("cellset_%s_%d", cluster, length(cs$cells))
        }
        cs$name <- set_name

        # Handle inherits the SOURCE object's base name + the cluster id, e.g.
        # cellset_pbmc3k_C2 from obj_pbmc3k (collisions get _2/_3 suffixes).
        src_base <- sub("^(obj|sce|spe|clusto|markoclust|markocell|purity|features|typo|cellset|mat|df)_", "",
                        src_handle %||% "", perl = TRUE)
        src_base <- gsub("[^a-z0-9]+", "_", tolower(src_base), perl = TRUE)
        src_base <- gsub("^_+|_+$", "", src_base, perl = TRUE)
        cl_tag <- gsub("[^a-z0-9]+", "_", tolower(cluster), perl = TRUE)
        cs_handle <- if (nzchar(src_base)) {
          cand <- paste0("cellset_", src_base, "_", cl_tag)
          if (!cv_object_exists(store, cand)) cand else {
            k <- 2L
            while (cv_object_exists(store, paste0(cand, "_", k))) k <- k + 1L
            paste0(cand, "_", k)
          }
        } else NULL
        handle <- cv_object_put(store, cs, handle = cs_handle, source = "get_cluster_cells()")

        # Downloadable barcode table (render layer adds CSV + paging).
        tbl <- data.frame(Cell = cs$cells, Cluster = cluster,
                          stringsAsFactors = FALSE)

        note <- ""
        if (!is.null(n_req) && !is.na(n_req) && n_req >= mk$n_total) {
          note <- sprintf(" Requested n=%d >= cluster size %d, so ALL cells were returned.",
                          n_req, mk$n_total)
        }
        samp_txt <- if (mk$sampled)
          sprintf("Randomly sampled %d of %d cell(s) from cluster %s (seed %d).",
                  length(cs$cells), mk$n_total, cluster, mk$seed)
        else
          sprintf("All %d cell(s) of cluster %s.", length(cs$cells), cluster)
        txt <- sprintf(
          "%s Stored as CellSet '%s' (handle: %s). Pass this handle to markoCell or markerPurity to analyse just this subset.%s",
          samp_txt, set_name, handle, note)

        # Round LXIX: INFO. The CellSet is exactly what was asked for and the
        # text says so; what the user needs to know is that "a random n" turned
        # out to be the whole cluster, so a later "does this hold in a subsample"
        # reading of it would be wrong. Informational rather than invalidating
        # because nothing computed here is affected -- only how a downstream
        # result should be described.
        cv_result_add_warnings(
          list(kind = "object", handle = handle,
               descriptor = cv_object_descriptor(store, handle),
               table = tbl, text = txt),
          if (nzchar(note))
            list(cv_warn("info", sprintf(
              paste0("You asked for %d cells and cluster %s has %d, so this set is the ",
                     "WHOLE cluster, not a random subsample of it."),
              n_req, cluster, mk$n_total), "cellset_not_sampled")) else NULL)
      }
    ),

    # ---- 6. featureInspect [LIGHT] : inspect features in marker sets -------
    cv_tool(
      name = "featureInspect",
      description = paste(
        "Search one or more genes across all marker collections in a ClustoCell",
        "object (global collections, cross-cluster, major-cluster, sub-cluster).",
        "Tells you whether/where a gene is a marker and its rank. Leave level &",
        "type NULL to search everything. Returns a table (and optionally a plot)."),
      parameters = list(
        clustoCell = cv_param("handle", "A ClustoCell object.", required = TRUE, handle_types = "ClustoCell"),
        features = cv_param("array", "Gene(s) to inspect.", items = "string", required = TRUE),
        level = cv_param("string", "Restrict to a level, e.g. 'Major cluster' or 'Sub cluster'. NULL searches all."),
        type = cv_param("string", "Restrict to a marker type, e.g. 'Positive' or 'Negative'. NULL searches all."),
        plot = cv_param("boolean", paste(
          "Return a figure alongside the table. FALSE gives the numbers only,",
          "which is faster when inspecting many features."), default = FALSE)
      ),
      input_object_types = "ClustoCell",
      output_object_type = NA_character_,
      cost = "light", produces = "table", tier = "core",
      handler = function(store, args) {
        a <- .cv_materialize(store, args, attr(args, "handle_args"))
        res <- do.call(celliverse::featureInspect, a)
        if (is.list(res) && !is.data.frame(res) && !is.null(res$plot)) {
          tbl <- if (!is.null(res$table)) res$table else NULL
          list(kind = "plot", plot_object = res$plot, table = tbl,
               text = "Feature inspection finished: a plot and a table of the per-group values.")
        } else {
          list(kind = "table", table = as.data.frame(res),
               text = sprintf("featureInspect returned %d row(s).", nrow(as.data.frame(res))))
        }
      }
    ),

    # ---- 7. typoClust [HEAVY] : cell type annotation -----------------------
    cv_tool(
      name = "typoClust",
      description = paste(
        "Infer / annotate the CELL TYPE of clusters, sub-clusters, or cell",
        "subsets using the curated CelliVerse Marker DB (mode='markerDB', the",
        "DEFAULT). This is the correct tool for 'what cell type is C1-Sub1',",
        "'annotate cluster C2', 'label the sub-clusters', etc. Provide the",
        "ClustoCell/MarkoClust/MarkoCell object handle(s) via 'objects' (if you",
        "omit it and exactly one such object is loaded, that one is used",
        "automatically) and, when the user names specific sets, 'desired_sets'",
        "(e.g. ['C1-Sub1']). Output is a TypoClust object. Do NOT route cell-type",
        "questions through get_cluster_cells/markoCell. For an LLM-based",
        "alternative use the tool 'annotateCellsLLM' (ceLLMarkup)."),
      parameters = list(
        objects = cv_param("array", paste(
          "Handles of ClustoCell/MarkoClust/MarkoCell objects to annotate.",
          "Optional: if omitted and exactly one such object is loaded, it is",
          "used automatically."),
          required = TRUE, items = "string",
          handle_types = c("ClustoCell", "MarkoClust", "MarkoCell")),
        desired_sets = cv_param("array", "Names of clusters/sets to annotate, e.g. ['C1-Sub1'] (default: all).", items = "string"),
        use_pos_markers = cv_param("boolean", "Use positive markers when matching against the Marker DB.", default = TRUE),
        use_neg_markers = cv_param("boolean", "Use negative markers when matching against the Marker DB.", default = TRUE),
        desired_pos_markers = cv_param("object", "Named list of positive marker panels, e.g. {\"panelA\": [\"CD3E\",\"CD8A\"]}."),
        desired_neg_markers = cv_param("object", "Named list of negative marker panels."),
        tissue = cv_param("array", "Tissue context(s), e.g. ['Blood']. Case-insensitive (matched against the Marker DB vocab). Call 'tissue_condition_vocab' first to see valid values. Default: all.", items = "string"),
        condition = cv_param("array", "Condition(s), e.g. ['Healthy']. Case-insensitive (matched against the Marker DB vocab). Call 'tissue_condition_vocab' first to see valid values. Default: all.", items = "string"),
        species = cv_param("string", "Species.", default = "human", enum = c("human", "mouse")),
        thresh_mode = cv_param("string", "How 'thresh' selects markers per set: 'n' = top-N markers, 'rank' = markers up to that rank.", default = "n", enum = c("n", "rank")),
        thresh = cv_param("integer", "Number of top markers used per set.", default = 20L, min = 0),
        # Round LXXII: these were live from the day typoClust gained them --
        # the function defaults to TRUE/0.5 and the agent calls it directly --
        # but they were not DECLARED, so the model could neither turn the
        # behaviour off for a user who wanted a flat annotation nor loosen the
        # restriction when a parent's identity is doubtful. Working by default
        # is not the same as being controllable.
        inherit_major_clusters = cv_param("boolean", paste(
          "Annotate each requested sub-cluster WITHIN its own major cluster's identity:",
          "the parent is annotated first from the parent's own top markers, then the",
          "sub-cluster from its own markers against a database narrowed to that parent's",
          "cell type and anything named as a variety of it. Parents are annotated and",
          "returned even when not asked for, which is intended. LEAVE THIS AT TRUE.",
          "Only set FALSE if the user explicitly asks for sub-clusters to be annotated",
          "on their own, independently of their parent."),
          default = TRUE),
        inherit_score_ratio = cv_param("number", paste(
          "How close to the parent's top score a runner-up must come to ALSO constrain",
          "that parent's sub-clusters (1 = top hit only; lower admits more near-ties).",
          "Only used when inherit_major_clusters is TRUE. Raise it toward 1 for a",
          "stricter reading of the parent, lower it when the parent's own label is doubtful."),
          default = 0.5, min = 0, max = 1)
      ),
      input_object_types = c("ClustoCell", "MarkoClust", "MarkoCell"),
      output_object_type = "TypoClust",
      cost = "heavy", produces = "object", tier = "core",
      next_suggestions = c("typoClustVis", "addTypoData"),
      # Always advertise markerDB-used + the LLM alternative on the result.
      result_note = .cv_typoclust_llm_note(),
      # Round LXXIII: a disabled default must be as visible as an applied one.
      validate = function(store, args, tool, warnings)
        .cv_warn_inheritance_off(store, args, warnings),
      handler = function(store, args) {
        # NOTE: typoClust is HEAVY, so in production it runs via the worker
        # (cv_launch_heavy -> cv_result_from_value), which materializes
        # array-of-handle params ('objects') via .cv_materialize_array_handles()
        # and tags source as paste0(tool$name, "()"). This inline handler
        # mirrors that path so a DIRECT call (tests / light dispatch) behaves
        # IDENTICALLY -- see CHANGES.md Round XXXIII (Batch 3b, item 2) for the
        # source=/array-handle drift this used to have vs. the worker path.
        inh <- .cv_input_handles(attr(args, "cv_tool"), args, attr(args, "handle_args"))
        # Array-of-handle params (e.g. 'objects') -> LIST of real objects, via
        # the SAME generic helper the worker path uses (driven by the tool's
        # parameter spec, not hardcoded to the 'objects' name).
        args <- .cv_materialize_array_handles(store, attr(args, "cv_tool"), args)
        args$mode <- "markerDB"  # ceLLMarkup handled by the dedicated LLM tool
        # Normalise tissue/condition (lowercase + validate) so a wrongly-cased
        # model value like 'Blood' works instead of aborting on every retry.
        args <- .cv_normalize_tissue_condition(args)
        tissue_arg <- args$tissue  # NULL when unfiltered -> cross-tissue warning may apply
        res <- do.call(celliverse::typoClust, args)
        # Round XXIV: descriptive handle -> typo_<base>_markerdb_<sets>.
        inh_tagged <- cv_tagged_inherit(inh, method = "markerdb",
                                        desired_sets = args$desired_sets)
        # source= now matches the worker path's convention (paste0(tool$name,
        # "()")) instead of the historical, hand-written "typoClust(markerDB)"
        # -- every other tool's inline handler already used this convention.
        rec <- .cv_result_object(store, res, source = "typoClust()", inherit_from = inh_tagged)
        # Round LXIX: the same two advisories as the heavy path in
        # cv_result_from_value() (agent_worker.R), with the same severities and
        # the same codes -- and a test now compares the two rather than trusting
        # this comment, because the light/heavy drift has bitten three times.
        warn <- .cv_typoclust_tissue_warning(res, tissue_arg)
        cv_result_add_warnings(
          rec,
          if (nzchar(warn)) list(cv_warn("may_invalidate", warn, "typoclust_tissue")) else NULL,
          local({ n <- .cv_inheritance_note(res)
                  if (!is.null(n)) list(cv_warn("info", n, "inherited_major_cluster")) else NULL }),
          # Round LXXX (audit #89): once per session, not once per call.
          # Raised through the SAME helper as the heavy path in
          # cv_result_from_value(), because a note that is once-per-session on
          # one dispatch path and once-per-call on the other is the light/heavy
          # drift this codebase has been bitten by four times.
          .cv_session_note_once(store, "result_note", .cv_typoclust_llm_note()))
      }
    ),

    # ---- 8. markoClustVis [LIGHT] : marker dot plot ------------------------
    cv_tool(
      name = "markoClustVis",
      description = paste(
        "Generate a faceted dot plot of top marker genes across clusters/",
        "sub-clusters/cell-subsets stored in a ClustoCell, MarkoClust, or",
        "MarkoCell object. Choose positive/negative/medium markers and how many.",
        "This PLOTS the markers; to get them as a ranked TABLE of numbers use",
        "getClusterMarkers instead."),
      parameters = list(
        obj = cv_param("handle", "A ClustoCell / MarkoClust / MarkoCell object.", required = TRUE,
                       handle_types = c("ClustoCell", "MarkoClust", "MarkoCell")),
        desired_sets = cv_param("array", "Which sets to show (default: all).", items = "string"),
        show_pos_markers = cv_param("boolean", "Show positive markers.", default = TRUE),
        show_neg_markers = cv_param("boolean", "Show negative markers.", default = FALSE),
        show_med_markers = cv_param("boolean", "Show medium markers.", default = FALSE),
        thresh = cv_param("integer", paste(
          "How many markers to draw per set. Read according to thresh_mode: with",
          "'n' exactly that many, with 'rank' every marker at or better than that",
          "rank. Large values make a tall figure - 30 markers over 12 sets is a",
          "readable plot; 200 is not."), default = 5L, min = 0)
      ),
      input_object_types = c("ClustoCell", "MarkoClust", "MarkoCell"),
      output_object_type = NA_character_,
      cost = "light", produces = "plot", tier = "core",
      handler = function(store, args) {
        a <- .cv_materialize(store, args, attr(args, "handle_args"))
        p <- do.call(celliverse::markoClustVis, a)
        # Round LXXIX (audit #58): a faceted marker dot plot's size is set by
        # the request, not by the default canvas -- `thresh` markers for each of
        # `desired_sets` sets, times up to three marker classes. 5 markers over
        # 3 sets is the 9x6 default and stays there; 30 markers over 12 sets is
        # the case the audit is about.
        n_set <- tryCatch(length(a$desired_sets %||% character(0)), error = function(e) 0L)
        n_cls <- sum(isTRUE(a$show_pos_markers %||% TRUE),
                     isTRUE(a$show_neg_markers %||% FALSE),
                     isTRUE(a$show_med_markers %||% FALSE))
        n_row <- tryCatch(as.numeric(a$thresh %||% 5L) * max(1L, n_cls), error = function(e) NA_real_)
        list(kind = "plot", plot_object = p,
             text = "Drew the marker dot plot. Each dot is one marker in one set, sized by how many cells express it.",
             plot_size = if (n_set > 0L) cv_grid_plot_size(n_set, n_row) else NULL)
      }
    ),

    # ---- 9. typoClustVis [LIGHT] : annotation visualization ----------------
    cv_tool(
      name = "typoClustVis",
      description = paste(
        "Visualize cell-type annotation results from a TypoClust object as a composite",
        "label+tile+dot plot (one row per cluster/rank). DEFAULTS MATCH THE VIGNETTE:",
        "rank_thresh=1 + refine=TRUE + refine_thresh=1 gives ONE row per cluster",
        "(e.g. 'C1_L1', or bare 'C5' when no deeper refinement was found). Only raise",
        "rank_thresh when the user explicitly wants multiple ranked predictions per",
        "cluster. Row-label semantics: '_R<k>' = the rank-k prediction (suffix only",
        "appears when rank_thresh>1); '_L<n>' = refined n levels down the cell-type",
        "hierarchy (n = number of '->' steps); '_L0' means no refinement and is shown",
        "as the bare cluster id. So 'C1_L1' = cluster C1 top prediction refined 1",
        "level; 'C2_R2_L1' = cluster C2 rank-2 prediction refined 1 level."),
      parameters = list(
        typoClust = cv_param("handle", "A TypoClust object.", required = TRUE, handle_types = "TypoClust"),
        desired_sets = cv_param("array", "Which sets to show (default: all).", items = "string"),
        order_by = cv_param("string", "Ordering of the plot.", default = "Cell Type",
                            enum = c("Cell Type", "Cluster", "Combined Score", "Combined Count", "Purity")),
        rank_thresh = cv_param("integer", "Top N ranked cell types to display per set. Default 1 (one row per cluster, the vignette view). Only increase when the user asks for multiple ranked predictions.", default = 1L, min = 0),
        refine = cv_param("boolean", "Refine each prediction down the cell-type hierarchy (default TRUE, matches vignette).", default = TRUE),
        refine_thresh = cv_param("integer", "How many hierarchy levels to traverse when refining (default 1). Ignored if refine=FALSE.", default = 1L, min = 0)
      ),
      input_object_types = "TypoClust",
      output_object_type = NA_character_,
      cost = "light", produces = "plot", tier = "core",
      handler = function(store, args) {
        a <- .cv_materialize(store, args, attr(args, "handle_args"))
        # Validate desired_sets against the sets actually annotated in this
        # TypoClust object. Without this, a hallucinated/empty selection reaches
        # typoClustVis and surfaces as the cryptic base-R "replacement has length
        # zero". Abort cleanly with the available ids instead.
        tc <- a$typoClust
        avail <- tryCatch(names(tc$cell_types), error = function(e) character(0))
        ds <- a$desired_sets
        if (!is.null(ds) && length(ds)) {
          ds <- as.character(ds)
          # Tolerate case/separator differences, then keep the not-annotated
          # guard for ids that still match nothing.
          ds <- tryCatch(
            cv_resolve_set_ids(ds, avail),
            error = function(e) {
              cli::cli_abort(c(
                "This TypoClust object has no annotation for the requested set(s) {.val {ds}}.",
                i = "Annotated set(s): {.val {avail}}.",
                i = "Re-run annotateCellsLLM/typoClust for the missing set(s), or plot without `desired_sets` to show all."
              ))
            }
          )
          a$desired_sets <- ds
        }
        p <- do.call(celliverse::typoClustVis, a)
        list(kind = "plot", plot_object = p,
             text = "Drew the annotation plot for the requested sets.")
      }
    ),

    # ---- 10. signatureDotHeatmap [LIGHT] : signature expression ------------
    cv_tool(
      name = "signatureDotHeatmap",
      description = paste(
        "Dot heatmap of gene-signature expression across clusters of a Seurat",
        "object. row_data is a data.frame mapping features to signatures."),
      parameters = list(
        seurat_obj = cv_param("handle", "A Seurat object.", required = TRUE, handle_types = "Seurat"),
        cluster_col = cv_param("string", "Metadata column with cluster ids.", required = TRUE),
        row_data = cv_param("object", "Data.frame (as records) mapping features to signatures.", required = TRUE),
        # Round LXXX (audit #72). These declared `default = "Features"` /
        # `"Signature"` and were therefore optional. The function's formals give
        # them NO default (they are NULL) and its very first check is
        #   if (is.null(features_col) || is.null(signature_col)) stop(...)
        # -- so the schema described two MANDATORY arguments as optional ones
        # with a helpful default, and a model that omitted them either got a
        # bare "must be provided" abort or, worse, ran against invented column
        # names that happened to be filled in on its behalf. The audit found
        # this by inspection; the measurement in this round confirms it is the
        # ONLY real default divergence across all 27 tools (the other four
        # apparent ones are match.arg formals, where the effective default is
        # element 1 and the schema already matches).
        #
        # Marked required, with the conventional names kept in the DESCRIPTION
        # where they belong: a hint the model can use, not a value the server
        # supplies for it.
        features_col = cv_param("string", paste(
          "Column in row_data holding feature/gene names - commonly 'Features'.",
          "Required: this function has no default for it."), required = TRUE),
        signature_col = cv_param("string", paste(
          "Column in row_data holding signature labels - commonly 'Signature'.",
          "Required: this function has no default for it."), required = TRUE)
      ),
      input_object_types = "Seurat",
      output_object_type = NA_character_,
      cost = "light", produces = "plot", tier = "core",
      handler = function(store, args) {
        a <- .cv_materialize(store, args, attr(args, "handle_args"))
        # row_data may arrive as a list-of-records from JSON; coerce to data.frame
        if (!is.null(a$row_data) && !is.data.frame(a$row_data)) {
          a$row_data <- do.call(rbind, lapply(a$row_data, function(r) as.data.frame(r, stringsAsFactors = FALSE)))
        }
        p <- do.call(celliverse::signatureDotHeatmap, a)
        # Round LXXIX (audit #58): this is the figure the audit named by name --
        # "one global 9x6 landscape is applied to a UMAP and a 40x12 dot-heatmap
        # alike". The handler already holds both dimensions before it draws:
        # clusters along x, features along y. cv_grid_plot_size() returns the
        # 9x6 default for anything small, so an ordinary signature panel is
        # completely unchanged and only a genuinely large grid grows.
        #
        # Both counts are tryCatch'd to NA rather than guarded field by field:
        # a size hint must never be the reason a figure that rendered fine fails
        # to come back, and cv_plot_geometry() treats NA as "use the default".
        n_col <- tryCatch(length(unique(as.character(
          a$seurat_obj[[a$cluster_col]][[1]]))), error = function(e) NA_real_)
        n_row <- tryCatch(length(unique(as.character(
          a$row_data[[a$features_col %||% "Features"]]))), error = function(e) NA_real_)
        list(kind = "plot", plot_object = p,
             text = "Drew the signature dot heatmap: signatures down the rows, clusters across the columns.",
             plot_size = cv_grid_plot_size(n_col, n_row))
      }
    ),

    # ---- 11. addClustoData [LIGHT] : write cluster labels to object --------
    cv_tool(
      name = "addClustoData",
      description = paste(
        "Add ClustoCell major-cluster and/or sub-cluster labels back into a",
        "Seurat/SCE object's metadata. Updates the object IN PLACE (same handle;",
        "no duplicate object is created)."),
      parameters = list(
        obj = cv_param("handle", "The Seurat/SCE object to annotate.", required = TRUE,
                       handle_types = c("Seurat", "SingleCellExperiment", "SpatialExperiment")),
        clustoCell = cv_param("handle", "The ClustoCell object with the labels.", required = TRUE, handle_types = "ClustoCell"),
        add_major_clusters = cv_param("boolean", "Add major cluster labels.", default = TRUE),
        add_sub_clusters = cv_param("boolean", "Add sub-cluster labels.", default = TRUE),
        major_cluster_name = cv_param("string", "Metadata column name for major clusters.", default = "ClustoCell_Clusters"),
        sub_cluster_name = cv_param("string", "Metadata column name for sub-clusters.", default = "ClustoCell_SubClusters")
      ),
      input_object_types = c("Seurat", "SingleCellExperiment", "SpatialExperiment"),
      output_object_type = "Seurat",
      cost = "light", produces = "object", tier = "core",
      next_suggestions = c("getDatasetMarkers", "typoClust", "signatureDotHeatmap"),
      # Round LXXVII (audit #43): a notice, never a gate -- audit category 3b
      # item 4 rules on that explicitly. Reads the store's descriptor before
      # dispatch; addClustoData.R itself is a core function and is untouched.
      validate = function(store, args, tool, warnings) {
        .cv_note_replaced_columns(store, args, warnings)
      },
      handler = function(store, args) {
        obj_handle <- args$obj  # capture the SAME handle before materialization
        a <- .cv_materialize(store, args, attr(args, "handle_args"))
        res <- do.call(celliverse::addClustoData, a)
        # Update in place: addClustoData only adds metadata columns to the
        # existing Seurat/SCE, so reuse the same handle rather than duplicating.
        .cv_result_object_inplace(store, obj_handle, res, source = "addClustoData()")
      }
    ),

    # ---- 12. addTypoData [LIGHT] : write cell types to object --------------
    cv_tool(
      name = "addTypoData",
      description = paste(
        "Add inferred cell-type annotations from a TypoClust object back into a",
        "Seurat/SCE object's metadata. Updates the object IN PLACE (same handle;",
        "no duplicate object is created)."),
      parameters = list(
        obj = cv_param("handle", "The Seurat/SCE object to annotate.", required = TRUE,
                       handle_types = c("Seurat", "SingleCellExperiment", "SpatialExperiment")),
        typoClust = cv_param("handle", "The TypoClust object with annotations.", required = TRUE, handle_types = "TypoClust"),
        clusters = cv_param("array", "Metadata column(s) whose sets were annotated, e.g. ['ClustoCell_Clusters'].", items = "string", required = TRUE),
        rank_thresh = cv_param("integer", "How many ranked predictions to add.", default = 1L, min = 0),
        refine_thresh = cv_param("integer", paste(
          "How many ranked predictions below the top one to consider when looking",
          "for a more specific label (e.g. 'CD8+ T Cell' in place of 'T Cell').",
          "0 disables refinement and shows the rank-1 label as it stands."),
          default = 1L, min = 0)
      ),
      input_object_types = c("Seurat", "SingleCellExperiment", "SpatialExperiment"),
      output_object_type = "Seurat",
      cost = "light", produces = "object", tier = "core",
      handler = function(store, args) {
        obj_handle <- args$obj  # capture the SAME handle before materialization
        a <- .cv_materialize(store, args, attr(args, "handle_args"))
        res <- do.call(celliverse::addTypoData, a)
        # Update in place: addTypoData only adds metadata columns to the existing
        # Seurat/SCE, so reuse the same handle rather than duplicating.
        .cv_result_object_inplace(store, obj_handle, res, source = "addTypoData()")
      }
    ),

    # ---- 13. umapPlot [VARIABLE COST] : UMAP colored by a metadata column --
    # This is the ONLY tool that produces a UMAP. It wraps STANDARD Seurat
    # (RunUMAP + DimPlot) - no CelliVerse analysis function is called or
    # modified. Use it after addClustoData/addTypoData have written cluster or
    # cell-type labels into the Seurat object's metadata.
    #
    # Round XXXIV (Batch 3b item 3): this was hardcoded `cost = "light"` even
    # though re-drawing an EXISTING embedding (near-instant) and computing a
    # FRESH one (NormalizeData+FindVariableFeatures+ScaleData+RunPCA+RunUMAP,
    # can run for a long time on a large object) are wildly different costs.
    # Being always "light" meant the slow path ran inline with no timeout, no
    # progress streaming, and no cancel button -- the exact protections heavy
    # tools get. See .cv_umap_plot_dispatch_cost() below for the fix: cost is
    # now decided PER CALL from the object's actual reduction state, so the
    # common fast case stays inline (no worker-pool/child-process overhead)
    # and the slow case gets routed through the heavy worker pool.
    cv_tool(
      name = "umapPlot",
      description = paste(
        "Generate a UMAP scatter plot of a Seurat object, coloring cells by a",
        "metadata column (group_by), e.g. 'ClustoCell_Clusters' (major clusters)",
        "or 'ClustoCell_SubClusters' (sub-clusters). Reuses an existing UMAP",
        "embedding when present; otherwise computes one (preferring an existing",
        "PCA such as 'clustoCell_pca'). This is the tool to use whenever the user",
        "asks for a UMAP / DimPlot / to visualize clusters on a UMAP."),
      parameters = list(
        seurat_obj = cv_param("handle", "A Seurat object (typically the one produced by addClustoData).",
                              required = TRUE, handle_types = "Seurat"),
        group_by = cv_param("string", paste(
          "Metadata column used to color cells, e.g. 'ClustoCell_Clusters'.",
          "Use get_metadata_columns to see available columns."), required = TRUE),
        reduction = cv_param("string", paste(
          "Which dimensional reduction to plot. Leave NULL to auto-detect an",
          "existing UMAP (e.g. 'clustoCell_umap' or 'umap'), computing one only",
          "if none exists.")),
        dims = cv_param("integer", "Number of PCA dims to use if a UMAP must be computed.", default = 10L),
        seed = cv_param("integer", paste(
          "Random seed. UMAP is stochastic, so the same seed is what makes two",
          "runs of this plot comparable; changing it moves every point."),
          default = 121L),
        title = cv_param("string", "Optional plot title.")
      ),
      input_object_types = "Seurat",
      output_object_type = NA_character_,
      # Static display value only (cv_registry_metadata() ships this to the
      # client Tool Inspector as a plain string) -- the common case (an
      # embedding already exists) IS light. Real per-call routing is
      # dispatch_cost, below.
      cost = "light", produces = "plot", tier = "core",
      dispatch_cost = .cv_umap_plot_dispatch_cost,
      heavy_impl = ".cv_umap_plot_compute",
      # Round LXX (audit #12). The audit cited umapPlot as the precedent for
      # checking a metadata column, and it is -- but only half of one. That
      # check lives inside .cv_umap_plot_compute(), which IS this tool's
      # heavy_impl, so on the path where no UMAP exists yet (measured:
      # dispatch_cost returns "heavy") it runs in the child, after the spawn,
      # exactly like the three tools this round is fixing. Same validator here,
      # so the precedent now matches what it was cited for. The in-compute check
      # stays: that function is also called directly and must defend itself.
      # Round LXXXI (D2): try to SUPPLY the missing column before refusing it.
      # Runs in `validate` rather than `prepare` because validate is the hook
      # the funnel calls on BOTH dispatch paths (prepare is worker-only), and
      # umapPlot routes light or heavy per call.
      validate = function(store, args, tool, warnings) {
        .cv_autofill_cluster_labels(store, args, warnings, "group_by", "seurat_obj")
        .cv_assert_metadata_column(store, args, "group_by", "seurat_obj")
      },
      handler = function(store, args) {
        # Round XXXIX: capture the SOURCE handle before materialization, same
        # pattern addClustoData/addTypoData use, so a reduction computed here
        # can be cached back onto it. Normally a no-op on this path -- "light"
        # means .cv_umap_needs_compute() was FALSE, so nothing was computed --
        # but this handler is also reached whenever dispatch_cost errors and
        # falls back, and when called directly, so both paths persist
        # identically rather than depending on which one ran.
        obj_handle <- args$seurat_obj
        a <- .cv_materialize(store, args, attr(args, "handle_args"))
        res <- .cv_umap_plot_compute(a$seurat_obj, a$group_by, a$reduction,
                                     a$dims, a$seed, a$title)
        .cv_persist_returned_reductions(store, obj_handle, res)
      }
    ),

    # ---- toSeurat -------------------------------------------------------
    # Round LXXXIII. THE TOOL THE PRODUCT ALREADY TOLD USERS TO ASK FOR.
    #
    # Round LXXXII's decline message said: Say "convert <handle> to a Seurat
    # object". No such tool existed. The user said it, the model emitted a call
    # to a tool that is not in the registry, and the raw arguments blob --
    #   "arguments": {"matrix": "mat_...", "name": "seurat_from_mat_..."}
    # -- was printed into the transcript as if it were an answer.
    #
    # The parameter names are taken FROM that leaked call. The model reached for
    # `matrix` and `name` unprompted, which is the strongest evidence available
    # about what a model will send for this operation; naming them anything else
    # would be preferring our taste to the measurement.
    #
    # LIGHT on purpose, though the work is not small. Every other expensive tool
    # here runs in a callr child, but a child means serialising a multi-GB matrix
    # in and a multi-GB Seurat back out -- two full copies to avoid one in-place
    # allocation. For this one operation the worker is strictly worse.
    cv_tool(
      name = "toSeurat",
      description = paste(
        "Convert a loaded counts MATRIX (dgCMatrix / matrix / data.frame) into a",
        "Seurat object, so tools that require a Seurat can use it: markoClust,",
        "markoCell, markerPurity, addClustoData and umapPlot. Use this whenever",
        "the user asks to 'convert', 'turn into' or 'make a Seurat' from a matrix",
        "handle, and whenever a Seurat is needed and only a matrix is loaded.",
        "clustoCell does NOT need this - it reads a matrix directly. The original",
        "matrix stays loaded under its own handle."),
      parameters = list(
        matrix = cv_param("handle", "The matrix handle to convert (e.g. 'mat_ab12cd').",
                          required = TRUE,
                          handle_types = c("dgCMatrix", "matrix", "data.frame")),
        name = cv_param("string", "Optional name for the new Seurat object.")
      ),
      input_object_types = c("dgCMatrix", "matrix", "data.frame"),
      output_object_type = "Seurat",
      cost = "light", produces = "object", tier = "core",
      handler = function(store, args) {
        cv_build_seurat_from_handle(store, args$matrix, args$name)
      }
    )

  )
}

# ---- umapPlot: shared reduction-picking + compute logic --------------------
# Extracted (Round XXXIV, Batch 3b item 3) out of the inline handler above so
# the IDENTICAL logic backs both dispatch paths: the fast inline path (an
# embedding already exists -> just re-draw it) and the heavy worker-pool path
# (no embedding -> full RunPCA+RunUMAP pipeline in a background callr child),
# instead of duplicating (and risking drift between) two copies.

#' Pick an existing UMAP-like reduction name from a Seurat object's reduction
#' list, or NA if none exists.
#' @noRd
.cv_umap_pick_existing <- function(reds) {
  for (nm in c("clustoCell_umap", "umap")) if (nm %in% reds) return(nm)
  hit <- grep("umap", reds, ignore.case = TRUE, value = TRUE)
  if (length(hit)) hit[1] else NA_character_
}

#' Does umapPlot need to COMPUTE a fresh UMAP for this call, or can it just
#' re-draw an existing embedding? Shared by the dispatch-cost classifier and
#' the actual compute function so the two decisions can never disagree.
#' @noRd
.cv_umap_needs_compute <- function(so, reduction_arg) {
  reds <- SeuratObject::Reductions(so)
  reduction <- reduction_arg %||% NA_character_
  if (is.na(reduction) || !nzchar(reduction)) reduction <- .cv_umap_pick_existing(reds)
  is.na(reduction)
}

#' Per-call dispatch-cost classifier for the umapPlot tool (cv_tool()'s
#' `dispatch_cost` field, consulted by cv_make_dispatcher()). Resolves the
#' target Seurat object via a cheap store lookup (no computation) and checks
#' whether a usable reduction already exists. Falls back to "heavy" -- the
#' safe default of timeout + progress + cancel -- if the handle can't be
#' resolved or isn't a Seurat object; the real, specific error still surfaces
#' from .cv_umap_plot_compute() a moment later either way, so this
#' classifier's only job is choosing a dispatch path, never validation.
#' @noRd
.cv_umap_plot_dispatch_cost <- function(store, call_args) {
  so <- tryCatch(cv_object_get(store, call_args$seurat_obj), error = function(e) NULL)
  if (is.null(so) || !methods::is(so, "Seurat")) return("heavy")
  if (.cv_umap_needs_compute(so, call_args$reduction)) "heavy" else "light"
}

#' The actual umapPlot computation: pick or compute a reduction, draw the
#' DimPlot. Store-independent (a plain Seurat object + scalar args) so it
#' runs identically whether called inline (light path, via the tool's
#' `handler`) or in a heavy worker's child process (`heavy_impl`, invoked by
#' name via getFromNamespace -- see cv_launch_heavy()'s child_fun). Returns
#' the SAME list(kind=, plot_object=, text=) shape either way, which is what
#' cv_render_result()/cv_result_from_value() both expect for a "plot" result.
#' @noRd
.cv_umap_plot_compute <- function(seurat_obj, group_by, reduction = NULL,
                                  dims = 10L, seed = 121L, title = NULL) {
  so <- seurat_obj
  if (is.null(group_by) || !nzchar(group_by)) {
    cli::cli_abort("`group_by` is required (a metadata column to color by).")
  }
  md_cols <- colnames(so@meta.data)
  if (!group_by %in% md_cols) {
    cli::cli_abort(c(
      "`group_by` column {.val {group_by}} is not in the object's metadata.",
      "i" = "Available columns: {.val {md_cols}}. Run addClustoData/addTypoData first if you expect cluster labels."))
  }
  # 1) Pick a reduction to plot: explicit > existing umap-like > compute.
  reds <- SeuratObject::Reductions(so)
  reduction <- reduction %||% NA_character_
  if (is.na(reduction) || !nzchar(reduction)) reduction <- .cv_umap_pick_existing(reds)
  if (!is.na(reduction) && !reduction %in% reds) {
    cli::cli_abort(c("Reduction {.val {reduction}} not found.",
                     "i" = "Available reductions: {.val {reds}}."))
  }
  # 2) If no UMAP exists, compute one (prefer an existing PCA).
  #
  # INCIDENT FIX (Round XXXIX): this whole branch used to run on a LOCAL copy
  # of the object and then throw the result away -- only the plot was
  # returned, and the stored object never gained the embedding. Measured
  # consequence on the user's own 2,700-cell dataset: every single umapPlot
  # call re-ran NormalizeData + FindVariableFeatures + ScaleData + RunPCA +
  # RunUMAP from scratch, at ~650 MB peak child RSS and ~27s, and discarded
  # it. Two consecutive identical calls both took ~27s and both left the
  # stored object with zero reductions, so .cv_umap_plot_dispatch_cost() below
  # kept returning "heavy" forever. The cheap inline path Round XXXIV built
  # (an embedding already exists -> just re-draw it) was therefore UNREACHABLE
  # through the agent: nothing in the system ever created the precondition it
  # tests for.
  #
  # Fix: the computed reduction is returned alongside the plot in
  # `reductions_added` and written back onto the SAME object handle by
  # .cv_persist_returned_reductions() (agent_worker.R) -- in place, no
  # duplicate object, mirroring what addClustoData/addTypoData already do.
  # Only the DimReduc is shipped back, never the whole modified Seurat: the
  # embedding is well under a megabyte, whereas the object carrying a dense
  # scale.data layer is ~100 MB, and shipping that across the callr boundary
  # would trade one memory problem for another.
  computed_reductions <- NULL
  if (is.na(reduction)) {
    dims <- as.integer(dims %||% 10L); seed <- as.integer(seed %||% 121L)
    pca_name <- if ("clustoCell_pca" %in% reds) "clustoCell_pca" else if ("pca" %in% reds) "pca" else NA_character_
    if (is.na(pca_name)) {
      # Minimal standard Seurat prep to get a PCA on a fresh object.
      if ("counts" %in% SeuratObject::Layers(so)) {
        so <- Seurat::NormalizeData(so, verbose = FALSE)
      }
      so <- Seurat::FindVariableFeatures(so, verbose = FALSE)
      so <- Seurat::ScaleData(so, verbose = FALSE)
      so <- Seurat::RunPCA(so, verbose = FALSE, npcs = max(dims, 30L))
      pca_name <- "pca"
    }
    npc <- ncol(SeuratObject::Embeddings(so, pca_name))
    dims <- seq_len(min(dims, npc))
    so <- Seurat::RunUMAP(so, dims = dims, reduction = pca_name,
                          reduction.name = "umap", seed.use = seed, verbose = FALSE)
    reduction <- "umap"
    computed_reductions <- tryCatch(list(umap = so[["umap"]]),
                                    error = function(e) NULL)
  }
  # 3) DimPlot grouped by the requested column, colorblind-safe palette.
  n_grp <- length(unique(as.character(so@meta.data[[group_by]])))
  pal <- cv_discrete_palette(n_grp)
  # Axis titles from the embedding's own column names (fall back to UMAP_1/2).
  emb_cols <- tryCatch(colnames(SeuratObject::Embeddings(so, reduction)),
                       error = function(e) NULL)
  x_lab <- if (length(emb_cols) >= 1) emb_cols[[1]] else "UMAP_1"
  y_lab <- if (length(emb_cols) >= 2) emb_cols[[2]] else "UMAP_2"
  p <- Seurat::DimPlot(so, reduction = reduction, group.by = group_by, cols = pal) +
    ggplot2::labs(x = x_lab, y = y_lab, colour = group_by,
                  title = title %||% sprintf("UMAP colored by %s", group_by),
                  subtitle = sprintf("%d cells - %d groups - reduction: %s",
                                     ncol(so), n_grp, reduction)) +
    ggplot2::guides(colour = ggplot2::guide_legend(title = group_by,
                                                   override.aes = list(size = 3)))
  out <- list(kind = "plot", plot_object = p,
              text = sprintf("UMAP of %d cells colored by '%s' (%d groups), reduction '%s'.",
                             ncol(so), group_by, n_grp, reduction))
  # Round XXXIX: carried out to the caller, which writes it back onto the
  # source handle and then STRIPS this field, so it never reaches the model,
  # the client, or the object store's result record. NULL (the common case:
  # an embedding already existed and nothing was computed) means the whole
  # write-back path is a no-op.
  if (!is.null(computed_reductions)) out$reductions_added <- computed_reductions
  out
}

# ---- Hierarchical annotation, said out loud (Round LXXII) -------------------
#
# Both annotation tools may silently do three things on the user's behalf when a
# sub-cluster is annotated: pull in and annotate a parent that was never asked
# for, restrict the sub-cluster to that parent's identity, and pick which of the
# parent's candidates count. Live testing found the markerDB card listing four
# sets where two were requested, with nothing on screen saying why -- and the
# model's own prose dropping the parents entirely. That is audit #23's
# complaint exactly: the decision was made, recorded in metadata, and never
# reached the person reading the numbers.
#
# INFO, not amber, and the Round LXIX rule decides it rather than taste: the run
# is correct and complete, and this fires on every ordinary hierarchical
# annotation. Ambering it would amber the normal case.
#' Warn when the model switched sub-cluster inheritance off (typoClust side).
#'
#' Round LXXIII. The LLM tool records this in its own metadata; typoClust cannot,
#' because the core function is out of scope for the agent to change. So it is
#' detected here instead, from the store's DESCRIPTOR -- which already carries
#' `major_labels` and `sub_labels`, so no object is touched and the check costs
#' nothing.
#'
#' Fires only when the request actually contains a sub-cluster whose parent
#' exists, which is the only case where turning inheritance off changes an
#' answer. Annotating major clusters flat is the ordinary case and stays silent.
#' @noRd
.cv_warn_inheritance_off <- function(store, args, warnings) {
  if (is.null(args$inherit_major_clusters) || isTRUE(args$inherit_major_clusters)) return(invisible(NULL))
  want <- as.character(unlist(args$desired_sets %||% character(0)))
  if (!length(want)) return(invisible(NULL))
  handles <- as.character(unlist(args$objects %||% character(0)))
  ids <- unique(unlist(lapply(handles, function(h) {
    d <- tryCatch(cv_object_descriptor(store, h), error = function(e) NULL)
    if (is.null(d)) return(character(0))
    c(as.character(d$major_labels %||% character(0)),
      as.character(d$sub_labels %||% character(0)))
  })))
  if (!length(ids)) return(invisible(NULL))
  po <- .cv_cellmarkup_parentage(ids, enabled = TRUE)
  skipped <- intersect(want, names(po)[!is.na(po)])
  if (!length(skipped)) return(invisible(NULL))
  cv_warn_add(warnings, "may_invalidate", sprintf(paste0(
    "Sub-cluster(s) %s were annotated on their own rather than within their major ",
    "cluster\'s identity, because inheritance was turned off for this run. Their labels ",
    "are not guaranteed to agree with the population they belong to. Re-run without ",
    "setting \'inherit_major_clusters\' to read them within their parents."),
    paste(skipped, collapse = ", ")),
    code = "inheritance_skipped")
}

#' The other half of the disclosure: inheritance that was TURNED OFF.
#'
#' Round LXXIII. Live testing produced a card reading "2 annotated set(s)" for a
#' two-sub-cluster request -- byte-identical to what the build without this
#' feature produced, and with nothing on screen to tell them apart. The model had
#' set `inherit_major_clusters = FALSE` (the schema's own wording invited it by
#' advertising the saved model call), and a disabled default is exactly as
#' consequential as an applied one: it is what lets a sub-cluster of an NK-cell
#' population come back labelled a T cell.
#'
#' MAY_INVALIDATE, unlike its applied counterpart, and the Round LXIX question
#' decides it: would a reader who skipped this draw a wrong conclusion from the
#' labels on screen? Yes -- these labels may contradict their own parents, and
#' nothing else says so. It fires only when parents were actually AVAILABLE, so
#' an ordinary flat annotation of major clusters stays silent.
#' @noRd
.cv_inheritance_skipped_note <- function(value) {
  md <- tryCatch(value$metadata, error = function(e) NULL)
  if (!is.list(md)) return(NULL)
  sk <- md$inherit_skipped_sets
  if (!length(sk)) return(NULL)
  sprintf(paste0(
    "Sub-cluster(s) %s were annotated on their own rather than within their major ",
    "cluster's identity, because inheritance was turned off for this run. Their labels ",
    "are not guaranteed to agree with the population they belong to. Re-run without ",
    "setting 'inherit_major_clusters' to read them within their parents."),
    paste(sk, collapse = ", "))
}

#' @noRd
.cv_inheritance_note <- function(value) {
  md <- tryCatch(value$metadata, error = function(e) NULL)
  if (!is.list(md)) return(NULL)
  inh <- md$inheritance
  if (!is.data.frame(inh) || !nrow(inh)) return(NULL)
  pieces <- vapply(seq_len(nrow(inh)), function(i) {
    if (isTRUE(inh$Restricted[i]))
      sprintf("%s was read within %s (%s)", inh$Set[i], inh$Parent[i], inh$Parent_CellType[i])
    else
      sprintf("%s could not be read within %s (no confident label for it), so it was annotated on its own",
              inh$Set[i], inh$Parent[i])
  }, character(1))
  extra <- if (length(md$auto_added_parents))
    sprintf(" %s %s annotated as well so this was possible.",
            paste(md$auto_added_parents, collapse = ", "),
            if (length(md$auto_added_parents) > 1L) "were" else "was") else ""
  paste0(paste(pieces, collapse = "; "), ".", extra)
}
