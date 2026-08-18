# =============================================================================
# CelliVerse Agent — results rendering (plots -> SVG/PNG, tables -> CSV/JSON)
#
# Tool handlers stay PURE: a plot tool returns list(kind="plot", plot_object=),
# a table tool returns a data.frame (or list(kind="table", table=)). They do NOT
# know about sessions or disk. This module is the single place that turns those
# in-memory results into ARTIFACT FILES under the session's artifacts dir and
# produces compact descriptors (paths + a browser URL) for the API/LLM.
#
# Rendering is invoked from the agent loop right after a tool returns, because
# only there do we know the session's artifacts_dir.
#
# Format policy (user preference): SVG is the default vector format; a PNG is
# also written for universal display + the media-quality check. SVG keeps text
# as text (svglite) so it stays editable.
# =============================================================================

#' Default raster size / dpi for rendered plots.
#'
#' Round LXXIX (audit #57): dpi was 150. At the 9 x 6 inch default that is a
#' 1350 x 900 px PNG, which is a screenshot, not a figure -- a journal asking
#' for a 9-inch-wide panel at 300 dpi wants 2700 px, twice what this produced.
#' The SVG has always been vector and print-safe; the raster beside it was the
#' one asset a reader would reach for and could not use.
#' @noRd
.cv_plot_dims <- list(width = 9, height = 6, dpi = 300)

#' Formats written for every plot.
#'
#' Round LXXIX (audit #57): `pdf` joins the two that were already here. The
#' device branch for it has existed in cv_open_device() since this file was
#' written and nothing ever asked for it.
#'
#' It is not redundant with SVG, which is the objection worth answering: both
#' are vector, but the journals this package's users submit to accept PDF and
#' mostly do not accept SVG, and Illustrator/Inkscape open both. The cost is one
#' extra device pass per figure and a file that is typically smaller than the
#' PNG.
#'
#' SVG stays FIRST, and stays the primary display asset -- see cv_render_plot(),
#' which still selects it by name.
#' @noRd
CV_PLOT_FORMATS <- c("svg", "png", "pdf")

#' Bounds on a per-figure canvas, in inches.
#'
#' A dot heatmap of 60 genes across 20 clusters genuinely needs a wide canvas;
#' nothing needs 400 inches. The clamp exists so a computed or supplied size can
#' never turn a rendering call into a multi-gigabyte allocation -- at 300 dpi a
#' 40 x 40 inch PNG is already 12000 x 12000 px.
#' @noRd
CV_PLOT_MIN_IN <- 3
#' @rdname CV_PLOT_MIN_IN
#' @noRd
CV_PLOT_MAX_IN <- 40

#' Resolve the geometry one figure should be drawn at.
#'
#' Round LXXIX (audit #58). One global 9 x 6 landscape was applied to a UMAP and
#' to a 40-gene x 12-cluster dot heatmap alike, and there was no way to say
#' otherwise: `cv_render_plot()` has always taken width/height/dpi and every one
#' of its call sites used the defaults.
#'
#' WHY THE OVERRIDE TRAVELS ON THE RESULT rather than as a tool parameter. A
#' parameter would have to be stripped out of the arguments before
#' `do.call(celliverse::<fn>, a)` sees it, and that stripping would have to
#' happen on BOTH the light handler path and the heavy worker path -- the
#' light/heavy drift that has bitten this codebase four separate times. A field
#' on the returned result flows through `cv_render_result()`, which is the one
#' function both paths call, so there is no second place for it to go missing.
#'
#' Anything unusable is IGNORED rather than corrected into a different figure:
#' a non-finite or non-numeric size falls back to the default, and a finite one
#' outside the bounds is clamped. Silently drawing at some third size the caller
#' did not ask for would be worse than either.
#' @param plot_size optional list(width=, height=, dpi=), any subset.
#' @return list(width=, height=, dpi=)
#' @noRd
cv_plot_geometry <- function(plot_size = NULL) {
  out <- .cv_plot_dims
  if (!is.list(plot_size)) return(out)
  num1 <- function(v) {
    if (is.null(v) || !length(v)) return(NULL)
    v <- suppressWarnings(as.numeric(v[[1]]))
    if (length(v) != 1L || is.na(v) || !is.finite(v) || v <= 0) return(NULL)
    v
  }
  w <- num1(plot_size[["width"]])
  h <- num1(plot_size[["height"]])
  d <- num1(plot_size[["dpi"]])
  if (!is.null(w)) out$width  <- min(max(w, CV_PLOT_MIN_IN), CV_PLOT_MAX_IN)
  if (!is.null(h)) out$height <- min(max(h, CV_PLOT_MIN_IN), CV_PLOT_MAX_IN)
  if (!is.null(d)) out$dpi    <- min(max(d, 72), 600)
  out
}

#' A canvas sized to a grid of `n_col` columns by `n_row` rows.
#'
#' Used by the two dot-plot tools, which know their own dimensions before they
#' draw: a marker dot plot's width is set by how many clusters are on the x axis
#' and its height by how many genes are on the y. Returns the default geometry
#' for a small grid, so an ordinary 5-marker plot is completely unchanged.
#' @noRd
cv_grid_plot_size <- function(n_col, n_row, per_col = 0.55, per_row = 0.22) {
  n_col <- suppressWarnings(as.numeric(n_col)[1]); n_row <- suppressWarnings(as.numeric(n_row)[1])
  if (is.na(n_col) || is.na(n_row) || !is.finite(n_col) || !is.finite(n_row)) return(NULL)
  list(width  = max(.cv_plot_dims$width,  3 + n_col * per_col),
       height = max(.cv_plot_dims$height, 2 + n_row * per_row))
}

#' Is x a ggplot object?
#' @noRd
cv_is_ggplot <- function(x) inherits(x, c("ggplot", "gg", "patchwork"))

# Round LV (Batch 5a): cv_is_grid_like() removed — zero call sites. The grid /
# ComplexHeatmap path in cv_save_plot() below is reached by the `else` branch of
# `if (cv_is_ggplot(plot_object))`, so no predicate was ever needed. It was also
# subtly wrong: its class list included "gg" and "ggplot", so a plain ggplot
# satisfied BOTH predicates — further evidence it was abandoned rather than
# reserved for future use.

#' Save one plot object to <artifacts_dir>/<basename>.{svg,png}.
#'
#' @param plot_object a ggplot (preferred), patchwork, or ComplexHeatmap/grid obj.
#' @param artifacts_dir session artifacts directory (already exists).
#' @param basename file stem (no extension); a short unique stem if NULL.
#' @param session_id used to build the browser URL.
#' @param formats which formats to write; SVG first (default).
#' @param width,height,dpi raster geometry.
#' @return an artifact descriptor list (kind="plot").
#' @noRd
cv_render_plot <- function(plot_object, artifacts_dir, basename = NULL,
                           session_id = NULL, formats = CV_PLOT_FORMATS,
                           width = .cv_plot_dims$width,
                           height = .cv_plot_dims$height,
                           dpi = .cv_plot_dims$dpi) {
  if (is.null(basename)) basename <- cv_new_id("plot")
  dir.create(artifacts_dir, recursive = TRUE, showWarnings = FALSE)
  written <- character(0)

  for (fmt in formats) {
    path <- file.path(artifacts_dir, paste0(basename, ".", fmt))
    okw <- tryCatch({
      cv_save_one_plot(plot_object, path, fmt, width, height, dpi)
      TRUE
    }, error = function(e) {
      cli::cli_warn("Failed to write {.file {path}}: {conditionMessage(e)}")
      FALSE
    })
    if (okw && file.exists(path) && file.info(path)$size > 0) written[[fmt]] <- path
  }

  if (!length(written)) {
    return(list(kind = "plot", error = "Plot could not be rendered to any format."))
  }

  files <- lapply(names(written), function(fmt) {
    fn <- basename(written[[fmt]])
    list(
      format = fmt,
      filename = fn,
      path = unname(written[[fmt]]),
      url = cv_artifact_url(session_id, fn)
    )
  })
  # Prefer SVG as the primary display asset; fall back to first written.
  primary <- files[[which(vapply(files, function(f) f$format, character(1)) == "svg")[1]]]
  if (is.null(primary) || is.na(primary$format)) primary <- files[[1]]

  list(
    kind = "plot",
    files = files,
    primary = primary,
    # a PNG (if present) is what the media-quality check / thumbnail uses
    png = tryCatch(files[[which(vapply(files, function(f) f$format, character(1)) == "png")[1]]],
                   error = function(e) NULL),
    # Round LXXIX (audit #58): the geometry this figure was ACTUALLY drawn at.
    # Reported rather than assumed -- "expose per-figure width/height" is not
    # served by a size the reader cannot see, and a heatmap that came out 25
    # inches wide because it had 40 clusters should say so.
    width = width, height = height, dpi = dpi
  )
}

#' Write a single plot file with the right device for the object + format.
#' @noRd
cv_save_one_plot <- function(plot_object, path, fmt, width, height, dpi) {
  # Round LXXX (audit #74): svglite is a Suggests, and both SVG paths below call
  # it BARE. Without it installed, every figure in the app failed with R's
  # generic "there is no package called 'svglite'" -- at the moment a plot was
  # requested, which is the least useful time to learn a package is missing.
  # svglite is now also in .cv_agent_runtime_pkgs so the startup gate catches
  # it first; this is the second line of defence, and it says what the user
  # loses and what to do rather than naming a namespace.
  if (identical(fmt, "svg") && !requireNamespace("svglite", quietly = TRUE)) {
    cli::cli_abort(c(
      "Cannot write an SVG: the {.pkg svglite} package is not installed.",
      i = "Install it with {.code install.packages(\"svglite\")} - it is what keeps SVG text editable.",
      i = "PNG and PDF versions of this figure are unaffected."
    ))
  }
  if (cv_is_ggplot(plot_object)) {
    # ggplot / patchwork: use ggsave. svglite keeps SVG text editable.
    if (identical(fmt, "svg")) {
      ggplot2::ggsave(path, plot = plot_object, device = svglite::svglite,
                      width = width, height = height, units = "in")
    } else {
      ggplot2::ggsave(path, plot = plot_object, device = fmt,
                      width = width, height = height, units = "in", dpi = dpi)
    }
    return(invisible(path))
  }
  # Non-ggplot (ComplexHeatmap / grid): open a device, draw, close.
  cv_open_device(path, fmt, width, height, dpi)
  on.exit(grDevices::dev.off(), add = TRUE)
  if (inherits(plot_object, c("Heatmap", "HeatmapList"))) {
    if (requireNamespace("ComplexHeatmap", quietly = TRUE)) {
      ComplexHeatmap::draw(plot_object)
    } else {
      methods::show(plot_object)
    }
  } else if (inherits(plot_object, c("grob", "gTree"))) {
    grid::grid.newpage(); grid::grid.draw(plot_object)
  } else {
    print(plot_object)
  }
  invisible(path)
}

#' Open a graphics device for the given format (svg via svglite).
#' @noRd
cv_open_device <- function(path, fmt, width, height, dpi) {
  switch(fmt,
    svg = svglite::svglite(path, width = width, height = height),
    png = grDevices::png(path, width = width * dpi, height = height * dpi, res = dpi),
    pdf = grDevices::pdf(path, width = width, height = height),
    cli::cli_abort("Unsupported plot format {.val {fmt}}.")
  )
}

#' Render a data.frame to CSV (full) + a paged JSON preview.
#'
#' @param df a data.frame.
#' @param artifacts_dir session artifacts dir.
#' @param basename file stem.
#' @param session_id for the URL.
#' @param page 1-based page for the preview.
#' @param page_size rows per page in the preview.
#' @return a table artifact descriptor (kind="table").
#' @noRd
cv_render_table <- function(df, artifacts_dir, basename = NULL,
                            session_id = NULL, page = 1L, page_size = 50L) {
  if (is.null(basename)) basename <- cv_new_id("table")
  dir.create(artifacts_dir, recursive = TRUE, showWarnings = FALSE)
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)

  csv_path <- file.path(artifacts_dir, paste0(basename, ".csv"))
  utils::write.csv(df, csv_path, row.names = FALSE)

  nrow_tot <- nrow(df)
  # BATCH1 FIX (rebuilt from scratch, confirmed empirically against this exact
  # code): `page` is already safely clamped into [1, n_pages] below, but a
  # zero/negative/NA `page_size` was not guarded at all -- these are tool
  # parameters a model can pass when calling a table-viewing tool, so a
  # hallucinated value from a weaker/local model can reach here directly.
  # page_size=0 -> division produces Inf/NaN -> `df[from:to, ]` throws
  # "NA/NaN argument"; a negative page_size throws "only 0's may be mixed
  # with negative subscripts"; page_size=NA throws the same NA/NaN error.
  # Clamp to a sane minimum of 1 instead of crashing.
  # Round LXXV (D5): the clamping moved into .cv_page_slice() so the new
  # /api/table paging route cannot drift from it. Two implementations of "which
  # rows are page N" is exactly the shape that has bitten this codebase four
  # times elsewhere.
  sl <- .cv_page_slice(nrow_tot, page, page_size)
  page_rows <- if (nrow_tot == 0L) df else df[sl$from:sl$to, , drop = FALSE]

  list(
    kind = "table",
    csv = list(filename = basename(csv_path), path = csv_path,
               url = cv_artifact_url(session_id, basename(csv_path))),
    columns = colnames(df),
    nrow = nrow_tot, ncol = ncol(df),
    page = sl$page, page_size = sl$page_size, n_pages = sl$n_pages,
    rows = page_rows
  )
}

#' Which rows are page N? The single definition of this arithmetic.
#'
#' Round LXXV (D5). Extracted from cv_render_table() unchanged so the paging
#' API route re-slices with the SAME clamping, rather than a second copy that
#' agrees today. The BATCH1 comment above cv_render_table's old inline version
#' explains why every one of these guards is load-bearing: page_size 0 gives
#' Inf/NaN and `df[from:to, ]` throws; a negative page_size throws "only 0's may
#' be mixed with negative subscripts"; NA throws the same NA/NaN error. These
#' are model-supplied tool parameters, so a hallucinated value reaches here.
#'
#' Out-of-range pages CLAMP rather than error, which is deliberate: a user
#' clicking "next" past the end should land on the last page, not a 400.
#' @noRd
.cv_page_slice <- function(nrow_tot, page = 1L, page_size = 50L) {
  page_size <- suppressWarnings(as.integer(page_size))
  if (is.na(page_size) || page_size < 1L) page_size <- 50L
  nrow_tot <- suppressWarnings(as.integer(nrow_tot))
  if (is.na(nrow_tot) || nrow_tot < 0L) nrow_tot <- 0L
  n_pages <- max(1L, as.integer(ceiling(nrow_tot / page_size)))
  page <- suppressWarnings(as.integer(page))
  if (is.na(page)) page <- 1L
  page <- max(1L, min(page, n_pages))
  list(page = page, page_size = page_size, n_pages = n_pages,
       from = (page - 1L) * page_size + 1L,
       to = min(page * page_size, nrow_tot), nrow = nrow_tot)
}

#' Build the browser URL for an artifact file served by the plumber API.
#' @noRd
cv_artifact_url <- function(session_id, filename) {
  if (is.null(session_id)) return(NA_character_)
  sprintf("/api/artifacts/%s/%s", utils::URLencode(session_id, reserved = TRUE),
          utils::URLencode(filename, reserved = TRUE))
}

#' Raise a may_invalidate warning when a plot rendered to no format at all.
#'
#' Round LXIX (audit #24/#25), closing the other half of D9. Round LXIV made a
#' failed render visible to the MODEL (`cv_tool_result_for_model()` emits
#' `plot.rendered = FALSE`). It stayed invisible on the CARD: `Artifacts.tsx`
#' returns `NULL` without `artifact$primary`, so the card showed a green tick,
#' the tool's own summary ("UMAP of 2,638 cells colored by ...") and no figure —
#' and the only thing telling the user the figure does not exist was whatever
#' the model chose to write underneath it.
#'
#' `may_invalidate` without hesitation: the analysis may well have succeeded,
#' and the deliverable the user asked for is not there.
#' @param res a handler result that may carry a plot artifact.
#' @return `res`, with a warning attached only when a render truly failed.
#' @noRd
.cv_warn_render_failure <- function(res) {
  a <- res$artifact
  if (is.null(a) || !identical(a$kind, "plot")) return(res)
  if (!is.null(a$primary) || is.null(a$error)) return(res)
  cv_result_add_warnings(res, list(cv_warn("may_invalidate", paste0(
    "The figure could not be written to any image format, so there is no plot to ",
    "show. Anything described about it below is not based on a rendered figure. ",
    "The underlying analysis may still have succeeded."), "plot_render_failed")))
}


#' Render a tool result record into artifacts, given the session.
#'
#' Dispatches on the result shape produced by handlers:
#'   - list(kind="plot", plot_object=)   -> cv_render_plot -> $artifact
#'   - a bare data.frame                  -> cv_render_table -> $table + $artifact
#'   - list(kind="table", table=<df>)     -> cv_render_table
#'   - list(table=<df>, plot=<gg>)        -> both (featureInspect(plot=TRUE))
#'   - anything else (object results etc.)-> returned unchanged
#'
#' The returned record keeps the original fields and ADDS $artifact (plots) and
#' /or $table (tables) so cv_tool_result_for_model + the API can expose them.
#' @param res a handler result.
#' @param artifacts_dir session artifacts dir.
#' @param session_id session id (for URLs).
#' @param basename optional file stem (tool name + id recommended).
#' @noRd
cv_render_result <- function(res, artifacts_dir, session_id = NULL, basename = NULL) {
  if (is.data.frame(res)) {
    tb <- cv_render_table(res, artifacts_dir, basename = basename, session_id = session_id)
    return(list(kind = "table", table = res, table_artifact = tb,
                text = sprintf("Table with %d rows x %d cols.", nrow(res), ncol(res))))
  }
  if (!is.list(res)) return(res)

  kind <- res$kind %||% NA_character_
  # NOTE: use [["..."]] exact indexing throughout. `res$plot` would PARTIAL-MATCH
  # `res$plot_object` in R and pick up the wrong element.

  # Round LXXIX (audit #58): the per-figure geometry a handler asked for.
  #
  # THIS IS THE ONLY PLACE IT IS READ, deliberately. Every dispatch path lands
  # here -- the light handler path via the agent loop, the heavy worker path via
  # cv_job_finalize() -- so a figure cannot come out 9x6 on one and 25x9 on the
  # other. That is the light/heavy drift this codebase has been bitten by four
  # times, fixed the way Round LXIV and Round LXXII fixed it: one implementation,
  # not two branches asked to stay in step.
  #
  # `[["plot_size"]]` exact indexing, because `res$plot_size` would partial-match
  # nothing today and something tomorrow -- the trap two rounds have already hit.
  geom <- cv_plot_geometry(res[["plot_size"]])
  res[["plot_size"]] <- NULL   # a directive to the renderer, not part of the result

  # A pure plot result: list(kind="plot", plot_object=<gg>).
  if (identical(kind, "plot") && !is.null(res[["plot_object"]])) {
    res$artifact <- cv_render_plot(res[["plot_object"]], artifacts_dir,
                                   basename = basename, session_id = session_id,
                                   width = geom$width, height = geom$height, dpi = geom$dpi)
    res[["plot_object"]] <- NULL  # don't keep the heavy grob in the result / model view
    return(.cv_warn_render_failure(res))
  }

  # featureInspect(plot=TRUE): list(table=<df>, plot=<gg>)
  has_tbl_field  <- !is.null(res[["table"]]) && is.data.frame(res[["table"]])
  has_plot_field <- !is.null(res[["plot"]]) && cv_is_ggplot(res[["plot"]])
  if (has_tbl_field || has_plot_field) {
    if (has_tbl_field) {
      res$table_artifact <- cv_render_table(res[["table"]], artifacts_dir,
                                            basename = cv_basename_suffix(basename, "table"),
                                            session_id = session_id)
    }
    if (has_plot_field) {
      res$artifact <- cv_render_plot(res[["plot"]], artifacts_dir,
                                     basename = cv_basename_suffix(basename, "plot"),
                                     session_id = session_id,
                                     width = geom$width, height = geom$height, dpi = geom$dpi)
      res[["plot"]] <- NULL
    }
    if (is.null(res$text)) res$text <- "Generated a table and a plot."
    return(.cv_warn_render_failure(res))
  }

  # A pure table result carried as list(kind="table", table=<df>).
  if (identical(kind, "table") && is.data.frame(res[["table"]])) {
    res$table_artifact <- cv_render_table(res[["table"]], artifacts_dir,
                                          basename = basename, session_id = session_id)
    return(res)
  }

  res
}

# ---- What the BROWSER gets (Round LXXV, D5) ---------------------------------
#
# `res$table` is the full frame. It exists for the MODEL:
# cv_tool_result_for_model() prefers it over the artifact's page rows so a
# 60-row result reaches the model complete instead of silently losing rows 51+.
# That is correct and this round does not touch it.
#
# The BROWSER never reads it. Chat.tsx:567 takes `res.table_artifact` and
# nothing else; a repo-wide search for `res.table` outside that line returns
# only `item.table`, which is the artifact under a different name. So every
# table result has been shipping its whole frame to a client that discards it.
#
# Measured on a 5,000-row table: 307,280 bytes emitted, 3,380 without the field
# -- 91x. With D5's paging route the browser can now ask for any page from the
# CSV on disk, so the field is not merely unread, it is superseded.
#
# THIS IS NOT "SHRINKING AN OUTPUT TO SAVE TOKENS", the thing this project has a
# standing rule against. Nothing the user can see gets smaller: every row is
# reachable by paging, the full CSV download is unchanged, and the model's view
# is built from the untouched result and keeps its own honest `truncated` flag.
# What goes away is a copy nobody ever read.
#
# ONE HELPER, because the light path emits `rr$result` (agent_loop.R) and the
# heavy path emits `rr` (agent_worker.R). That pair has drifted four times.

#' Strip result fields that exist for the model and are dead weight in the UI.
#' @param res a rendered result (or the whole record, for the heavy path).
#' @noRd
cv_result_for_browser <- function(res) {
  if (!is.list(res)) return(res)
  # Only drop the raw frame when the rendered artifact is there to replace it.
  # Without that guard a handler that returned a table and failed to render one
  # would send the browser nothing at all.
  if (!is.null(res[["table_artifact"]]) && is.data.frame(res[["table"]]))
    res[["table"]] <- NULL
  res
}

#' Small helper: append a suffix to a basename stem (keeps files grouped).
#' @noRd
cv_basename_suffix <- function(basename, suffix) {
  if (is.null(basename)) return(cv_new_id(suffix))
  paste0(basename, "_", suffix)
}
