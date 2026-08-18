# =============================================================================
# CelliVerse Agent — results artifacts (object RDS / CSV / TXT + manifest + zip)
#
# The Results tab needs MORE than the plots/tables a turn happens to render. The
# user wants every server-side object downloadable on its own (as a portable
# .rds they can readRDS() back into R), plus convenience text/table exports and
# a single "download everything" zip.
#
# This module is the one place that turns the per-session object store into
# durable, downloadable files under the session artifacts dir:
#
#   <artifacts_dir>/
#     object_<Type>__<handle>.rds              # ALWAYS, one per store object
#     object_CellSet__<handle>__barcodes.txt   # CellSet cell barcodes
#     object_TypoClust__<handle>__celltypes.txt# top cell-type call per set
#     object_DatasetMarkers__<handle>__markers.txt
#     object_<Type>__<handle>.csv              # when the object is a data.frame
#     <plots/tables already written by agent_render.R: .svg/.png/.csv>
#     manifest.json                            # single source of truth for /api
#     .artifacts_state.json                    # internal incremental-sync state
#
# saveRDS() writes straight to the artifacts dir because that dir lives on the
# user's local disk (~/.celliverse/sessions/...), never on a FUSE/S3 mount.
#
# Two entry points:
#   cv_sync_object_artifacts(store, dir, session)  # end-of-turn: WRITE objects
#                                                   #   (incremental) + manifest
#   cv_build_manifest(store, dir, session)          # read-only: scan dir -> list
#
# Splitting them matters: the turn hook OWNS the store and may serialize new
# objects; the list endpoint (and a restored session whose store is empty) must
# NEVER delete or rewrite object files, only enumerate what is on disk.
# =============================================================================

# ---- Filenames --------------------------------------------------------------

#' Filesystem-safe token (letters, digits, dot, underscore, hyphen).
#' @noRd
.cv_fs_safe <- function(x) {
  x <- as.character(x)[1]
  if (is.na(x) || !nzchar(x)) return("x")
  gsub("[^A-Za-z0-9._-]+", "_", x)
}

#' @noRd
.cv_object_rds_name <- function(handle, type) {
  sprintf("object_%s__%s.rds", .cv_fs_safe(type), .cv_fs_safe(handle))
}
#' @noRd
.cv_object_txt_name <- function(handle, type, suffix) {
  sprintf("object_%s__%s__%s.txt", .cv_fs_safe(type), .cv_fs_safe(handle), .cv_fs_safe(suffix))
}
#' @noRd
.cv_object_csv_name <- function(handle, type) {
  sprintf("object_%s__%s.csv", .cv_fs_safe(type), .cv_fs_safe(handle))
}

# ---- Object -> files --------------------------------------------------------

#' Write the celltype-annotation TXT for a TypoClust object.
#'
#' TypoClust$cell_types is a named list (one entry per annotated set, e.g. "C1")
#' whose value is a data.frame of ranked candidate matches with a `CellType`
#' column (row 1 = top call). We emit a small tab-separated summary: the top
#' cell type for each set. Returns TRUE if a file was written.
#' @noRd
.cv_write_typoclust_txt <- function(val, path) {
  ct <- tryCatch(val$cell_types, error = function(e) NULL)
  if (is.null(ct) || !length(ct)) return(FALSE)
  lines <- "set\tcell_type"
  for (nm in names(ct)) {
    df <- ct[[nm]]
    lab <- NA_character_
    if (is.data.frame(df) && "CellType" %in% names(df) && nrow(df) > 0) {
      lab <- as.character(df$CellType[1])
    } else if (is.character(df) && length(df)) {
      lab <- df[1]
    }
    lines <- c(lines, paste(nm, if (is.na(lab)) "NA" else lab, sep = "\t"))
  }
  writeLines(lines, path)
  TRUE
}

#' Write all downloadable files for ONE object; return the relative filenames.
#'
#' Always writes the portable .rds. Adds a convenience export when the object is
#' a kind the user typically wants as plain text/table:
#'   CellSet         -> barcodes .txt
#'   TypoClust       -> celltypes .txt
#'   DatasetMarkers  -> combined markers .txt
#'   data.frame      -> .csv
#'   character vector-> values .txt
#' @noRd
.cv_write_object_artifacts <- function(val, type, handle, artifacts_dir) {
  written <- character(0)

  rds <- .cv_object_rds_name(handle, type)
  ok <- tryCatch({ saveRDS(val, file.path(artifacts_dir, rds)); TRUE },
                 error = function(e) {
                   cli::cli_warn("Could not save {.val {handle}} as .rds: {conditionMessage(e)}")
                   FALSE
                 })
  if (ok) written <- c(written, rds)

  if (identical(type, "CellSet")) {
    cells <- tryCatch(as.character(val$cells %||% character(0)), error = function(e) character(0))
    if (length(cells)) {
      fn <- .cv_object_txt_name(handle, type, "barcodes")
      tryCatch({ writeLines(cells, file.path(artifacts_dir, fn)); written <- c(written, fn) },
               error = function(e) NULL)
    }
  } else if (identical(type, "TypoClust")) {
    fn <- .cv_object_txt_name(handle, type, "celltypes")
    if (isTRUE(tryCatch(.cv_write_typoclust_txt(val, file.path(artifacts_dir, fn)),
                        error = function(e) FALSE))) {
      written <- c(written, fn)
    }
  } else if (identical(type, "DatasetMarkers")) {
    mk <- tryCatch(as.character(val$combined_markers %||% character(0)), error = function(e) character(0))
    if (length(mk)) {
      fn <- .cv_object_txt_name(handle, type, "markers")
      tryCatch({ writeLines(mk, file.path(artifacts_dir, fn)); written <- c(written, fn) },
               error = function(e) NULL)
    }
  } else if (is.data.frame(val)) {
    fn <- .cv_object_csv_name(handle, type)
    tryCatch({ utils::write.csv(val, file.path(artifacts_dir, fn), row.names = FALSE); written <- c(written, fn) },
             error = function(e) NULL)
  } else if (is.character(val) && length(val)) {
    fn <- .cv_object_txt_name(handle, type, "values")
    tryCatch({ writeLines(val, file.path(artifacts_dir, fn)); written <- c(written, fn) },
             error = function(e) NULL)
  }

  written
}

#' The filenames `.cv_write_object_artifacts()` WOULD produce, without producing
#' them.
#'
#' Round LIV: the end-of-turn hook now records what each object can be exported
#' as, and the actual serialization is deferred until a download asks for it.
#' That needs the names up front, so the Results tab can list a row for a file
#' that does not exist yet.
#'
#' This mirrors `.cv_write_object_artifacts()`'s branching exactly and must be
#' kept in step with it -- a divergence would either show the user a row whose
#' download produces nothing, or hide one that is genuinely available. Every
#' condition here is a cheap field read (does this TypoClust have any cell types
#' at all?), never a serialization, which is the entire point.
#' `test-round54-...` asserts the two functions agree for every supported type.
#' @noRd
.cv_planned_object_files <- function(val, type, handle) {
  planned <- .cv_object_rds_name(handle, type)   # ALWAYS, same as the writer

  if (identical(type, "CellSet")) {
    cells <- tryCatch(as.character(val$cells %||% character(0)), error = function(e) character(0))
    if (length(cells)) planned <- c(planned, .cv_object_txt_name(handle, type, "barcodes"))
  } else if (identical(type, "TypoClust")) {
    # The writer emits this only when .cv_write_typoclust_txt() returns TRUE,
    # and that returns FALSE for an absent or empty cell_types.
    ct <- tryCatch(val$cell_types, error = function(e) NULL)
    if (!is.null(ct) && length(ct)) planned <- c(planned, .cv_object_txt_name(handle, type, "celltypes"))
  } else if (identical(type, "DatasetMarkers")) {
    mk <- tryCatch(as.character(val$combined_markers %||% character(0)), error = function(e) character(0))
    if (length(mk)) planned <- c(planned, .cv_object_txt_name(handle, type, "markers"))
  } else if (is.data.frame(val)) {
    planned <- c(planned, .cv_object_csv_name(handle, type))
  } else if (is.character(val) && length(val)) {
    planned <- c(planned, .cv_object_txt_name(handle, type, "values"))
  }

  planned
}

#' The change-detection signature for one stored object.
#'
#' Pulled out of cv_sync_object_artifacts() in Round LIV so the index hook and
#' the materialization path cannot drift apart on what "changed" means.
#' @noRd
.cv_object_artifact_sig <- function(rec) {
  # BATCH2B FIX: cv_now() (agent_utils.R) has only whole-second resolution, so
  # two in-place updates to the SAME handle within the same wall-clock second
  # used to stamp an IDENTICAL created/updated pair, making this signature
  # falsely equal to the previous sync's and silently skipping the re-write of a
  # genuinely changed object (stale .rds/.csv download). `rec$rev`
  # (agent_object_store.R) is a per-handle counter incremented on every
  # cv_object_put()/cv_object_update() call, so it always changes when the
  # object actually does, independent of clock resolution. Keep the timestamps
  # in the signature too (harmless, and they still change on any update whose
  # timestamp resolution DOES happen to differ).
  paste0(rec$created %||% "", "|", rec$updated %||% "", "|", rec$rev %||% 0L)
}

#' Read `.artifacts_state.json`, degrading to an empty list on anything unusable.
#'
#' Round LV (Batch 5a): this five-line read-with-fallback appeared verbatim in
#' three places after Round LIV added the index (cv_index_object_artifacts,
#' cv_sync_object_artifacts, cv_build_manifest) plus a fourth variant in
#' cv_materialize_pending_artifact. Own duplication, introduced one round ago
#' and collapsed here — the state file is read on every turn and every download,
#' so "what counts as an unreadable state file" is exactly the decision that
#' should not have four independent answers.
#' @param artifacts_dir session artifacts dir.
#' @return the parsed state list, or an empty list.
#' @noRd
.cv_read_artifacts_state <- function(artifacts_dir) {
  p <- file.path(artifacts_dir, ".artifacts_state.json")
  if (!file.exists(p)) return(list())
  tryCatch(jsonlite::read_json(p, simplifyVector = FALSE), error = function(e) list())
}

#' Is this state entry still waiting to be written?
#'
#' Entries written before Round LIV carry no `pending` field at all, and absent
#' means FALSE (already materialized) -- so an existing session's state file
#' keeps working untouched.
#' @noRd
.cv_state_is_pending <- function(entry) isTRUE(entry$pending)

# ---- Round LXXV (audit #33): the terminal paths index too ---------------------
#
# Until this round `cv_index_object_artifacts()` had exactly ONE call site --
# `finish()` in agent_loop.R, reached only when a turn completed normally. A turn
# that produced an object and then errored, or that the user stopped, left the
# object live in the store and absent from `.artifacts_state.json`, so
# cv_build_manifest()'s pending rows (which iterate `names(state)`) produced no
# row at all and Results showed nothing.
#
# Reproduced before building, on one store holding one finished ClustoCell:
#
#   indexer NOT called (error / cancel path) -> 0 artifacts, obj_clusto absent
#   indexer called     (success path)        -> 1 artifact, obj_clusto pending
#
# The object was never lost -- only invisible, which is the worse failure of the
# two because nothing says so.
#
# Figures and tables are unaffected: cv_render_result() writes those files at
# tool-success time and the manifest finds them by directory scan. The gap is
# objects only, exactly as the audit says.
#
# WHY A HELPER RATHER THAN THREE CALLS. The three terminal paths live in two
# files (agent_loop.R's finish(); agent_turns.R's fail() and cv_turn_cancel()),
# and this is the same shape as the light/heavy pair that has drifted four
# times. One helper, one tryCatch, one warning voice -- and a test that asserts
# every terminal path reaches it.
#
# BEST EFFORT, ALWAYS. Indexing must never turn a turn that merely failed into a
# turn that failed twice, and must never mask the user's real error with an
# artifact-bookkeeping one. Hence the tryCatch and the invisible(NULL).

#' Index the session's objects, swallowing anything that goes wrong.
#'
#' @param session_id session id; looked up for its store and artifacts dir.
#' @param when short phrase naming the path that called this, for the warning.
#' @noRd
cv_index_artifacts_safe <- function(session_id, when = "turn end") {
  tryCatch({
    sess <- cv_session_get(session_id)
    if (is.null(sess)) return(invisible(NULL))
    cv_index_object_artifacts(sess$object_store, sess$artifacts_dir, session_id)
  }, error = function(e) {
    cli::cli_warn("Artifact index failed at {when}: {conditionMessage(e)}")
    invisible(NULL)
  })
}

#' One-line description of a job log file, for the Results manifest.
#'
#' Round LXXV (audit #31). Deliberately says what the file IS and where it came
#' from, because the alternative the user sees is a bare `job_a1b2c3.log`.
#' @noRd
.cv_job_log_summary <- function(filename) {
  jid <- sub("\\.log$", "", filename)
  sprintf(paste0("Run log for job %s -- the analysis process's own output, ",
                 "kept so a failure can be read in full. Not one of your results."), jid)
}

# ---- Index (end of turn) ----------------------------------------------------

#' Record what each session object COULD be downloaded as, without writing it.
#'
#' Round LIV replaces the end-of-turn serialization with this. The old hook
#' called cv_sync_object_artifacts() after EVERY turn, which gzip-serialized
#' every changed object on the single thread that also serves HTTP:
#'
#'     16 MB object  617 ms      64 MB  1,519 ms      193 MB  4,649 ms
#'
#' -- a multi-second freeze at the end of an ordinary turn, spent on an object
#' the user had not asked to download. Round XXXIX's reduction cache made it
#' worse rather than better: persisting a computed embedding back onto the
#' source object bumps that handle's `rev`, which is exactly what the signature
#' keys on, so the whole Seurat was re-compressed.
#'
#' This writes only metadata the store already holds -- type, summary, source,
#' revision, and the filenames the object would produce -- so its cost does not
#' depend on object size at all. The bytes are written later, by
#' cv_sync_object_artifacts() below, from the two download paths in
#' agent_api.R.
#'
#' THE TRADE, put to the user and accepted before this was implemented: a
#' restored session comes back with an EMPTY object store by design (history and
#' descriptors survive a restart, the objects themselves do not), so an object
#' that was never downloaded cannot be produced after a restart.
#' cv_build_manifest() therefore offers a pending row only while its handle is
#' still live in the store -- it never advertises a download it cannot deliver.
#' @param store the session object store (may be NULL/empty -> no-op).
#' @param artifacts_dir session artifacts dir.
#' @param session_id for building URLs.
#' @return the freshly built manifest (invisibly via cv_build_manifest).
#' @noRd
#'
cv_index_object_artifacts <- function(store, artifacts_dir, session_id = NULL) {
  if (is.null(artifacts_dir)) return(invisible(NULL))
  dir.create(artifacts_dir, recursive = TRUE, showWarnings = FALSE)
  state_path <- file.path(artifacts_dir, ".artifacts_state.json")
  state <- .cv_read_artifacts_state(artifacts_dir)

  handles <- if (is.null(store)) character(0) else cv_object_handles(store)
  changed <- FALSE

  for (h in handles) {
    rec <- tryCatch(get(h, envir = store), error = function(e) NULL)
    if (is.null(rec)) next
    val  <- rec$value
    type <- tryCatch(cv_object_type(val), error = function(e) class(val)[1])
    sig  <- .cv_object_artifact_sig(rec)

    prev <- state[[h]]
    prev_files <- unlist(prev$files %||% list(), use.names = FALSE)
    # Nothing to do when the signature matches AND the entry is either already
    # recorded as pending, or already written with its files still on disk.
    if (!is.null(prev) && identical(prev$sig %||% "", sig)) {
      if (.cv_state_is_pending(prev)) next
      if (length(prev_files) > 0 && all(file.exists(file.path(artifacts_dir, prev_files)))) next
    }

    desc <- rec$descriptor
    state[[h]] <- list(
      sig     = sig,
      type    = type,
      handle  = h,
      summary = tryCatch(desc$summary %||% NA_character_, error = function(e) NA_character_),
      source  = tryCatch(desc$source  %||% NA_character_, error = function(e) NA_character_),
      # Round LXXV (audit #29): the arguments and versions this object was
      # produced with. Read from the RECORD, not the descriptor -- provenance is
      # deliberately kept off the descriptor so it never rides into the model
      # payload or the system prompt (see cv_object_set_provenance()). This is
      # the surface it is actually for: a Results row, and a .rds someone opens
      # months later with no memory of what produced it.
      provenance = tryCatch(rec$provenance, error = function(e) NULL),
      created = rec$created %||% cv_now(),
      files   = as.list(.cv_planned_object_files(val, type, h)),
      pending = TRUE
    )
    changed <- TRUE
  }

  if (changed) {
    tryCatch(
      jsonlite::write_json(state, state_path, auto_unbox = TRUE, null = "null", force = TRUE),
      error = function(e) NULL)
  }

  invisible(cv_build_manifest(store, artifacts_dir, session_id))
}

# ---- Incremental sync (on download) -----------------------------------------

#' Persist session objects to downloadable files, then rebuild the manifest.
#'
#' Round LIV: this is no longer the end-of-turn hook (that is
#' cv_index_object_artifacts() above). It is the MATERIALIZATION step, called
#' only when a download actually asks for the bytes -- one handle at a time from
#' cv_api_serve_artifact(), or all of them from cv_api_artifacts_zip(). The work
#' is identical to what it always did; only when it runs has changed, and that
#' is deliberately the moment the user is waiting for a file and being told so.
#'
#' Incremental: an object is (re)serialized only when it is new, has changed
#' since the last write (tracked by the signature in .artifacts_state.json), or
#' is still marked `pending` by the index. We do NOT delete files for handles
#' that vanish from the store: a session's produced objects remain downloadable
#' (in-place updates overwrite the same handle's file, so there is no
#' duplication for addClustoData/addTypoData).
#' @param store the session object store (may be NULL/empty -> no writes).
#' @param artifacts_dir session artifacts dir.
#' @param session_id for building URLs.
#' @param handles optional character vector: materialize only these handles.
#'   NULL (default) means every handle in the store, which is what the
#'   "Download all" path wants. A single-file download passes exactly one, so
#'   clicking one row never pays for every other object in the session.
#' @return the freshly built manifest (invisibly via cv_build_manifest).
#' @noRd
cv_sync_object_artifacts <- function(store, artifacts_dir, session_id = NULL,
                                     handles = NULL) {
  if (is.null(artifacts_dir)) return(invisible(NULL))
  dir.create(artifacts_dir, recursive = TRUE, showWarnings = FALSE)
  state_path <- file.path(artifacts_dir, ".artifacts_state.json")
  state <- .cv_read_artifacts_state(artifacts_dir)

  all_handles <- if (is.null(store)) character(0) else cv_object_handles(store)
  # Round LIV: restrict to the requested subset, ignoring anything the caller
  # names that is no longer in the store (a stale filename in a manifest the
  # browser is still holding, say) rather than erroring on it.
  target <- if (is.null(handles)) all_handles else intersect(all_handles, handles)

  for (h in target) {
    rec  <- tryCatch(get(h, envir = store), error = function(e) NULL)
    if (is.null(rec)) next
    val  <- rec$value
    type <- tryCatch(cv_object_type(val), error = function(e) class(val)[1])
    desc <- rec$descriptor
    sig  <- .cv_object_artifact_sig(rec)

    prev <- state[[h]]
    prev_files <- unlist(prev$files %||% list(), use.names = FALSE)
    # Round LIV: `!.cv_state_is_pending(prev)` is the new clause. An entry the
    # index recorded carries the right signature and the right filenames but no
    # bytes, so the old test alone would have declared it unchanged and skipped
    # the write -- and the download would have 404'd on a file the Results tab
    # had just offered.
    unchanged <- !is.null(prev) && identical(prev$sig %||% "", sig) &&
      !.cv_state_is_pending(prev) &&
      length(prev_files) > 0 && all(file.exists(file.path(artifacts_dir, prev_files)))
    if (unchanged) next

    files <- .cv_write_object_artifacts(val, type, h, artifacts_dir)
    state[[h]] <- list(
      sig     = sig,
      type    = type,
      handle  = h,
      summary = tryCatch(desc$summary %||% NA_character_, error = function(e) NA_character_),
      source  = tryCatch(desc$source  %||% NA_character_, error = function(e) NA_character_),
      created = rec$created %||% cv_now(),
      # The ACTUAL filenames written, which replace the index's planned list.
      # If the two ever disagree the truth wins here, so a planning bug can
      # produce a missing row but never a permanently broken download link.
      files   = as.list(files),
      pending = FALSE
    )
  }

  tryCatch(
    jsonlite::write_json(state, state_path, auto_unbox = TRUE, null = "null", force = TRUE),
    error = function(e) NULL)

  invisible(cv_build_manifest(store, artifacts_dir, session_id))
}

#' Write the ONE pending object that would produce `filename`, if any.
#'
#' Round LIV: the single-file download path. `cv_api_serve_artifact()` calls
#' this when a requested artifact is missing from disk; it looks the filename up
#' in the index, and materializes only the owning handle.
#'
#' Returns FALSE (silently, no error) for every case where nothing can be done:
#' an unknown session, a filename no index entry claims, an entry that is not
#' pending, or -- the important one -- a handle that is no longer in the store,
#' which is what a restored session looks like. The caller then 404s exactly as
#' it did before this round existed.
#' @param session_id session id.
#' @param filename bare artifact filename (already path-guarded by the caller).
#' @return invisibly TRUE if something was written.
#' @noRd
cv_materialize_pending_artifact <- function(session_id, filename) {
  if (!cv_session_exists(session_id)) return(invisible(FALSE))
  sess <- cv_session_get(session_id)
  adir <- sess$artifacts_dir
  if (is.null(adir)) return(invisible(FALSE))

  state <- .cv_read_artifacts_state(adir)
  if (!length(state)) return(invisible(FALSE))

  owner <- NULL
  for (h in names(state)) {
    if (filename %in% unlist(state[[h]]$files %||% list(), use.names = FALSE)) {
      if (.cv_state_is_pending(state[[h]])) owner <- h
      break
    }
  }
  if (is.null(owner)) return(invisible(FALSE))
  if (!cv_object_exists(sess$object_store, owner)) return(invisible(FALSE))

  cv_sync_object_artifacts(sess$object_store, adir, session_id, handles = owner)
  invisible(TRUE)
}

# ---- Manifest (read-only enumeration) ---------------------------------------

#' Attach object provenance (handle/type/summary/source) to a file entry.
#' @noRd
.cv_manifest_attach_provenance <- function(entry, filename, fname_to_handle, state, desc_by_handle) {
  h <- fname_to_handle[[filename]]
  if (is.null(h)) return(entry)
  entry$handle <- h
  st <- state[[h]]
  entry$type    <- (st$type    %||% NA_character_)
  entry$summary <- (st$summary %||% NA_character_)
  entry$source  <- (st$source  %||% NA_character_)
  # Round LXXV (audit #29). Survives a restart because it lives in the state
  # file, not only in the live store -- which is the case that matters, since a
  # downloaded .rds outlives the session that made it.
  if (!is.null(st$provenance)) entry$provenance <- st$provenance
  d <- desc_by_handle[[h]]
  if (!is.null(d)) {
    # Prefer the live descriptor when the object is still in the store.
    if (!is.null(d$summary)) entry$summary <- d$summary
    if (!is.null(d$source) && nzchar(d$source)) entry$source <- d$source
    if (!is.null(d$type)) entry$type <- d$type
  }
  entry
}

#' Scan the artifacts dir and build the results manifest (single source of truth).
#'
#' Read-only with respect to objects: it never writes or deletes object files
#' (safe for a restored session whose store is empty). It DOES (over)write
#' manifest.json so the on-disk manifest matches what /api returns and what the
#' zip bundles.
#'
#' Kinds: "figure" (svg/png/pdf, grouped by stem), "table" (csv), "rds", "text"
#' (txt), "other". Object-derived files also carry handle/type/summary/source.
#' @noRd
cv_build_manifest <- function(store, artifacts_dir, session_id = NULL) {
  if (is.null(artifacts_dir)) return(list(session = session_id %||% NA_character_,
                                           generated = cv_now(), artifacts = list()))
  dir.create(artifacts_dir, recursive = TRUE, showWarnings = FALSE)

  # Live descriptors (if the store is available) for freshest provenance.
  desc_by_handle <- list()
  if (!is.null(store)) {
    for (h in cv_object_handles(store)) desc_by_handle[[h]] <- cv_object_descriptor(store, h)
  }
  # Persisted state gives filename -> handle + provenance that survives restart.
  # (Round LV: the local `state_path` here became unused when the read moved
  # into .cv_read_artifacts_state() — this function only ever READS the state.)
  state <- .cv_read_artifacts_state(artifacts_dir)
  fname_to_handle <- list()
  for (h in names(state)) {
    for (f in unlist(state[[h]]$files %||% list(), use.names = FALSE)) fname_to_handle[[f]] <- h
  }

  files <- list.files(artifacts_dir, all.files = FALSE, no.. = TRUE)  # hidden (.state) excluded
  files <- setdiff(files, "manifest.json")

  arts <- list()
  fig_groups <- list()  # stem -> list(format entries)

  finfo <- function(f) {
    fp <- file.path(artifacts_dir, f)
    list(size = tryCatch(as.numeric(file.info(fp)$size), error = function(e) NA_real_),
         url  = cv_artifact_url(session_id, f))
  }

  for (f in files) {
    ext <- tolower(tools::file_ext(f))
    inf <- finfo(f)
    if (ext %in% c("svg", "png", "pdf")) {
      stem <- sub("\\.[^.]+$", "", f)
      fig_groups[[stem]] <- c(fig_groups[[stem]],
                              list(list(format = ext, filename = f, url = inf$url, size = inf$size)))
    } else if (ext == "csv") {
      arts[[length(arts) + 1L]] <- .cv_manifest_attach_provenance(
        list(kind = "table", filename = f, url = inf$url, size = inf$size),
        f, fname_to_handle, state, desc_by_handle)
    } else if (ext == "rds") {
      arts[[length(arts) + 1L]] <- .cv_manifest_attach_provenance(
        list(kind = "rds", filename = f, url = inf$url, size = inf$size),
        f, fname_to_handle, state, desc_by_handle)
    } else if (ext == "txt") {
      arts[[length(arts) + 1L]] <- .cv_manifest_attach_provenance(
        list(kind = "text", filename = f, url = inf$url, size = inf$size),
        f, fname_to_handle, state, desc_by_handle)
    } else if (ext == "log") {
      # Round LXXV (audit #31). The audit says the job log is "written to the
      # artifacts dir and never linked". Measured, that is not quite right: a
      # `job_x.log` is neither hidden nor `manifest.json`, so it already fell
      # into the `else` below and DID reach Results -- as a bare filename, with
      # no explanation and nothing tying it to the run that produced it. An
      # unlabelled `job_a1b2c3.log` sitting between a UMAP and a marker table is
      # arguably worse than absent, because the user cannot tell whether it is
      # one of their own results.
      #
      # It stays `kind = "other"` DELIBERATELY. Results.tsx groups strictly by
      # kind (`others = arts.filter(a => a.kind === "other")`), so inventing a
      # "log" kind would drop it out of every section and make the audit's
      # complaint literally true. What it gains is `summary`, which FileRow
      # already renders as its sub-line, so the label ships without the frontend
      # changing at all.
      arts[[length(arts) + 1L]] <- list(
        kind = "other", filename = f, url = inf$url, size = inf$size,
        summary = .cv_job_log_summary(f))
    } else {
      arts[[length(arts) + 1L]] <- list(kind = "other", filename = f, url = inf$url, size = inf$size)
    }
  }

  # Round LIV: rows for objects that have been INDEXED but not yet written.
  #
  # Two guards, both load-bearing:
  #   - only handles still live in the store. A restored session's store is
  #     empty by design, so a pending row there would offer a download nothing
  #     can produce. This is the agreed limit of the lazy scheme, enforced here
  #     rather than discovered by the user at download time.
  #   - only filenames not already on disk, so a partially materialized object
  #     (rds written, csv not) shows one real row and one pending row rather
  #     than duplicating the real one.
  on_disk <- files
  for (h in names(state)) {
    st <- state[[h]]
    if (!.cv_state_is_pending(st)) next
    if (is.null(desc_by_handle[[h]])) next
    for (f in unlist(st$files %||% list(), use.names = FALSE)) {
      if (f %in% on_disk) next
      ext <- tolower(tools::file_ext(f))
      kind <- switch(ext, csv = "table", rds = "rds", txt = "text", "other")
      # No `size` field at all -- knowing it would mean doing the very work
      # this defers, and an absent size is what the client already renders as
      # blank. `fname_to_handle` above was built from every state entry, so it
      # already maps this pending filename to its handle.
      arts[[length(arts) + 1L]] <- .cv_manifest_attach_provenance(
        list(kind = kind, filename = f, url = cv_artifact_url(session_id, f),
             pending = TRUE),
        f, fname_to_handle, state, desc_by_handle)
    }
  }

  fig_arts <- lapply(names(fig_groups), function(stem) {
    fmts <- fig_groups[[stem]]
    fmt_names <- vapply(fmts, function(x) x$format, character(1))
    prim_i <- if ("svg" %in% fmt_names) which(fmt_names == "svg")[1] else 1L
    png_i  <- which(fmt_names == "png")
    thumb  <- if (length(png_i)) fmts[[png_i[1]]]$url else fmts[[prim_i]]$url
    list(
      kind    = "figure",
      name    = stem,
      formats = fmts,
      primary = fmts[[prim_i]]$filename,
      url     = fmts[[prim_i]]$url,
      thumb   = thumb,
      size    = sum(vapply(fmts, function(x) x$size %||% 0, numeric(1)), na.rm = TRUE)
    )
  })

  all_arts <- c(fig_arts, arts)
  # Stable ordering: figures, rds, tables, text, other; then by name.
  rank_of <- c(figure = 1L, rds = 2L, table = 3L, text = 4L, other = 5L)
  keyk <- vapply(all_arts, function(a) rank_of[[a$kind]] %||% 9L, integer(1))
  keyn <- vapply(all_arts, function(a) a$name %||% a$filename %||% "", character(1))
  all_arts <- all_arts[order(keyk, keyn)]

  manifest <- list(
    session   = session_id %||% NA_character_,
    generated = cv_now(),
    n         = length(all_arts),
    artifacts = unname(all_arts)
  )
  tryCatch(
    jsonlite::write_json(manifest, file.path(artifacts_dir, "manifest.json"),
                         auto_unbox = TRUE, pretty = TRUE, null = "null", force = TRUE),
    error = function(e) NULL)
  manifest
}

# ---- Zip (download all) -----------------------------------------------------

#' Bundle all artifacts (incl. manifest.json) into a temp .zip; return its path.
#'
#' Prefers the cross-platform `zip` package (no system dependency); falls back to
#' utils::zip (system `zip`). Entries are stored with bare filenames (no nested
#' session path). Returns NULL when there is nothing to zip or zipping fails.
#' @noRd
cv_artifacts_zip_file <- function(artifacts_dir, session_id = NULL) {
  if (is.null(artifacts_dir) || !dir.exists(artifacts_dir)) return(NULL)
  files <- list.files(artifacts_dir, all.files = FALSE, no.. = TRUE)  # hidden (.state) excluded
  if (!length(files)) return(NULL)
  out <- tempfile(fileext = ".zip")

  ok <- FALSE
  if (requireNamespace("zip", quietly = TRUE)) {
    ok <- tryCatch({ zip::zip(out, files = files, root = artifacts_dir); file.exists(out) },
                   error = function(e) FALSE)
  }
  if (!ok) {
    old <- getwd(); on.exit(setwd(old), add = TRUE)
    ok <- tryCatch({
      setwd(artifacts_dir)
      utils::zip(out, files, flags = "-q -r9X")
      file.exists(out)
    }, error = function(e) FALSE)
  }
  if (!ok || !file.exists(out)) return(NULL)
  out
}
