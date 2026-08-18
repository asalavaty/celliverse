# =============================================================================
# CelliVerse Agent - object store & descriptors
#
# THE CORE DESIGN PRINCIPLE of this agent:
#   Large R objects (Seurat / SingleCellExperiment / matrices, and CelliVerse
#   result objects like ClustoCell / MarkoClust / TypoClust) NEVER enter the
#   LLM context. They live server-side in a per-session environment, keyed by a
#   short *handle* (e.g. "obj_pbmc3k", "clusto_1"). The LLM only ever sees a
#   compact *descriptor* (class, dimensions, metadata columns, result summary).
#
# This keeps the agent fast (no giant payloads to/from the model) and scalable
# (handles are passed between tools). Object-CREATING tools (clustoCell,
# markoClust, markoCell, typoClust, getDatasetMarkers, markerPurity,
# clustoCell_TransferLabel) each mint a NEW handle for their new result object.
# Object-UPDATING tools that merely write metadata back into an EXISTING object
# (addClustoData, addTypoData) mutate that object IN PLACE under the SAME handle
# via cv_object_update(), so a large Seurat/SCE is never silently duplicated.
# =============================================================================

# ---- Store construction -----------------------------------------------------

#' Create a new object store (an environment mapping handle -> record)
#'
#' Each record is a list: list(value = <the R object>, descriptor = <list>,
#' created = <time>, source = <chr>).
#' @param session_id the id of the session this store belongs to, stashed as
#'   an attribute so `cv_object_evict_stale()` can look up that session's
#'   config/jobs at eviction time without every `cv_object_put()`/
#'   `cv_object_update()` call site needing to pass session_id through
#'   explicitly (Batch 3b item 2). A store created with no session_id (as
#'   every existing standalone/test call site does) is simply never swept --
#'   eviction only ever applies to a store that's actually attached to a live
#'   session.
#' @keywords internal
cv_object_store_new <- function(session_id = NULL) {
  store <- new.env(parent = emptyenv())
  attr(store, "cv_session_id") <- session_id
  store
}

#' Get the next monotonic sequence number for a store (Batch 3b item 2)
#'
#' `cv_now()` only has whole-second resolution, so a burst of puts/updates/
#' reads within the same wall-clock second all tie on `last_accessed`.
#' `order()`'s tie-break then falls back to input-vector position, which for
#' `cv_object_handles()` (== `ls()`) is ALPHABETICAL - unrelated to real
#' recency. This was caught live: a test that created a handle, touched it
#' once, then added many alphabetically-earlier filler handles in the same
#' second found the touched handle evicted first anyway, because ties broke
#' alphabetically rather than chronologically.
#'
#' A per-store monotonic counter gives every put/update/read a unique,
#' strictly increasing value so `cv_object_evict_stale()`'s "least-recently-
#' used first" ordering is always correct regardless of clock resolution or
#' handle naming. Stored as an attribute on the store environment itself so
#' it survives across calls without a separate side-channel.
#' @keywords internal
cv_object_next_seq <- function(store) {
  n <- (attr(store, "cv_seq_counter") %||% 0L) + 1L
  attr(store, "cv_seq_counter") <- n
  n
}

# ---- Provenance (Round LXXV, audit #29) -------------------------------------
#
# WHAT WAS ACTUALLY MISSING. The audit says "no version is captured anywhere
# today, and the persisted tool_calls hold the model's *partial* args, not what
# ran". Both halves check out. Measured:
#
#   descriptor fields: handle, type, class, source, slots, n_cells, ... summary
#   any version / param / args field?  FALSE
#   source field: "clustoCell()"        <- a bare function name, nothing else
#
# `utils::packageVersion()` appears exactly twice in R/ (a print banner and the
# /api/status health field) and neither is attached to a result. `R.version`,
# `getRversion` and `sessionInfo` appear nowhere at all. No Seurat version is
# read anywhere. A result you download six months later cannot be traced to the
# code that produced it.
#
# WHERE THE REAL ARGUMENTS ARE. `cv_run_tool_call()` holds `call_args` -- the
# model's arguments AFTER cv_resolve_args() has typed and defaulted them and
# AFTER cv_adjust_log1p_layer() has rewritten the layer -- and then discards it:
# the return is list(ok=, tool=, result=), with no args field. That funnel is
# the one place both dispatch paths pass through, which is the same reason
# Round LXX put `validate` there.
#
# HONEST NAMING. This is NOT "the arguments the function saw" in every case: a
# heavy tool's `prepare` hook runs later, inside the worker, and markoCell's
# CellSet expansion happens there. So the field is called `dispatched_args` and
# is documented as what the agent sent, which is a strict and large improvement
# on the model's partial arguments and does not claim to be more.
#
# HANDLES ARE NOT RECORDED AS VALUES. `handle_args` names the parameters that
# carry an object handle; those are dropped, because a whole Seurat serialized
# into a provenance record would be absurd. The handle STRING is kept, since
# that is the provenance -- which object it ran on.

#' Runtime versions, computed once per session.
#'
#' Cached because these cannot change while the process lives, and this is
#' called on every object put.
#' @keywords internal
.cv_versions_cache <- new.env(parent = emptyenv())

#' @keywords internal
cv_runtime_versions <- function() {
  if (!is.null(.cv_versions_cache$v)) return(.cv_versions_cache$v)
  pv <- function(p) tryCatch(as.character(utils::packageVersion(p)),
                             error = function(e) NULL)
  v <- list(
    celliverse = pv("celliverse"),
    R          = tryCatch(as.character(getRversion()), error = function(e) NULL),
    Seurat     = pv("Seurat"),
    SeuratObject = pv("SeuratObject"),
    SingleCellExperiment = pv("SingleCellExperiment")
  )
  v <- v[!vapply(v, is.null, logical(1))]
  .cv_versions_cache$v <- v
  v
}

#' Build the provenance record for one tool call.
#'
#' @param tool_name the tool that ran.
#' @param args the resolved, dispatched arguments.
#' @param handle_args names of parameters carrying object handles (values kept
#'   as the handle STRING; anything unserializable is dropped).
#' @keywords internal
cv_call_provenance <- function(tool_name, args = list(), handle_args = character(0)) {
  keep <- list()
  nm <- names(args %||% list())
  for (k in nm) {
    if (!nzchar(k)) next
    v <- args[[k]]
    # Only scalars and short atomic vectors are provenance. A data.frame, a
    # Seurat, or a 40,000-element barcode vector is data, not a parameter, and
    # recording it would put the payload back where Round LIV took it out.
    if (is.null(v)) next
    if (is.atomic(v) && length(v) >= 1L && length(v) <= 25L) {
      keep[[k]] <- if (length(v) == 1L) v else as.list(v)
    } else if (is.atomic(v) && length(v) > 25L) {
      keep[[k]] <- sprintf("<%s[%d]>", class(v)[1], length(v))
    } else if (is.list(v) && length(v) <= 25L &&
               all(vapply(v, function(x) is.atomic(x) && length(x) == 1L, logical(1)))) {
      keep[[k]] <- unlist(v, use.names = FALSE)
    } else {
      keep[[k]] <- sprintf("<%s>", class(v)[1])
    }
  }
  list(tool = tool_name, dispatched_args = keep,
       handle_args = as.character(handle_args %||% character(0)),
       versions = cv_runtime_versions(), at = cv_now())
}

#' Attach a provenance record to a handle already in the store.
#'
#' Separate from cv_object_put() on purpose: the store is written by the handler
#' (which does not know the resolved arguments) and the provenance is known at
#' the funnel (which does not know the handle until the handler returns). This
#' is the join. Never errors -- provenance is a nicety and must not be able to
#' fail a successful call.
#'
#' DELIBERATELY NOT ON THE DESCRIPTOR. The first version of this set
#' `rec$descriptor$provenance` as well, which looked tidier and was wrong: the
#' descriptor is what `cv_tool_result_for_model()` sends as `payload$object` on
#' every object result, and `cv_summary_line()` re-injects into the SYSTEM
#' PROMPT on every single turn. That would spend tokens on five version strings
#' and a defaulted argument list, every turn, for information the model cannot
#' act on -- it already knows the arguments it sent. Provenance is for the human
#' reading a Results row or a downloaded file six months later, so it travels to
#' the artifacts state and to `describe_object` (an explicit, one-off request)
#' and nowhere else.
#' @keywords internal
cv_object_set_provenance <- function(store, handle, provenance) {
  tryCatch({
    if (is.null(store) || is.null(handle) || !nzchar(handle)) return(invisible(FALSE))
    if (!cv_object_exists(store, handle)) return(invisible(FALSE))
    rec <- get(handle, envir = store)
    rec$provenance <- provenance
    assign(handle, rec, envir = store)
    invisible(TRUE)
  }, error = function(e) invisible(FALSE))
}

#' Read a handle's provenance record, or NULL.
#' @keywords internal
cv_object_provenance <- function(store, handle) {
  tryCatch({
    if (is.null(store) || is.null(handle) || !nzchar(handle)) return(NULL)
    if (!cv_object_exists(store, handle)) return(NULL)
    get(handle, envir = store)$provenance
  }, error = function(e) NULL)
}

#' Render a provenance record as one readable block for `describe_object`.
#' @keywords internal
cv_provenance_text <- function(p) {
  if (is.null(p) || !is.list(p)) return("")
  bits <- character(0)
  a <- p$dispatched_args %||% list()
  if (length(a)) {
    kv <- vapply(names(a), function(k) {
      v <- a[[k]]
      paste0(k, "=", paste(as.character(v), collapse = ","))
    }, character(1))
    bits <- c(bits, paste0("Ran as: ", p$tool %||% "?", "(", paste(kv, collapse = ", "), ")"))
  }
  v <- p$versions %||% list()
  if (length(v))
    bits <- c(bits, paste0("Versions: ",
                           paste(sprintf("%s %s", names(v), unlist(v, use.names = FALSE)),
                                 collapse = ", ")))
  if (!is.null(p$at)) bits <- c(bits, paste0("Run at: ", p$at))
  paste(bits, collapse = "\n")
}

#' Put an object into the store, returning its handle
#' @param store object store environment.
#' @param value the R object to store.
#' @param handle optional explicit handle; auto-generated if NULL.
#' @param source short human string describing where it came from.
#' @return the handle (character).
#' @keywords internal
cv_object_put <- function(store, value, handle = NULL, source = "") {
  if (is.null(handle)) {
    handle <- cv_new_id(prefix = cv_handle_prefix(value))
  } else {
    # An explicitly-requested handle (e.g. from the upload "Optional name"
    # field) must be unique: silently overwriting a loaded object would confuse
    # the model/UI, which still reference the old handle. Block with a clear,
    # actionable error so the user picks a different name before the upload
    # succeeds. (cv_object_update is the legitimate in-place reuse path and is
    # unaffected.)
    if (cv_object_exists(store, handle)) {
      cli::cli_abort(c(
        "The name {.val {handle}} is already used by another loaded object.",
        i = "Choose a different name and try the upload again."
      ))
    }
  }
  desc <- cv_describe_object(value, handle = handle, source = source)
  assign(handle, list(
    value      = value,
    descriptor = desc,
    created    = cv_now(),
    # Batch 3b item 2: last time this handle's VALUE was read or written,
    # used by cv_object_evict_stale() below to pick the least-recently-used
    # objects once the store grows past its cap. A brand-new object is
    # "accessed" at creation time.
    last_accessed = cv_now(),
    # Monotonic per-store tie-break for the above -- see cv_object_next_seq()
    # for why last_accessed's whole-second resolution alone isn't enough.
    seq        = cv_object_next_seq(store),
    # Monotonic per-handle revision counter (Batch 2b fix) -- see
    # cv_object_update() below and cv_sync_object_artifacts() in
    # agent_artifacts.R for why the artifact-sync signature uses this
    # instead of (or alongside) created/updated timestamp strings.
    rev        = 1L,
    source     = source
  ), envir = store)
  # Batch 3b item 2: sweep least-recently-used objects out of the store once
  # it grows past its cap -- see cv_object_evict_stale() below. Wrapped so an
  # eviction-logic error never prevents the actual put from succeeding.
  tryCatch(cv_object_evict_stale(store), error = function(e) NULL)
  handle
}

#' Update an EXISTING object in place, reusing the same handle
#'
#' Used by update-style tools (addClustoData / addTypoData) that add metadata to
#' an object the user already loaded: instead of storing the modified object
#' under a fresh handle (which duplicates a potentially large Seurat/SCE in the
#' session environment), we overwrite the value at the existing handle and
#' refresh its descriptor. The object TYPE must not change (a Seurat stays a
#' Seurat) - this is a guardrail against a handler accidentally swapping the
#' object class under a stable handle the model/UI are still referencing.
#' @param store object store environment.
#' @param handle an existing handle.
#' @param value the updated R object (same canonical type as the current value).
#' @param source short human string describing the update.
#' @return the (unchanged) handle.
#' @keywords internal
cv_object_update <- function(store, handle, value, source = "") {
  if (!cv_object_exists(store, handle)) {
    cli::cli_abort(c(
      "Cannot update {.val {handle}}: no such object in this session.",
      i = "Available handles: {.val {cv_object_handles(store)}}."
    ))
  }
  old <- get(handle, envir = store)
  old_type <- tryCatch(old$descriptor$type, error = function(e) NA_character_)
  new_type <- cv_object_type(value)
  if (!is.na(old_type) && !identical(old_type, new_type)) {
    cli::cli_abort(c(
      "In-place update of {.val {handle}} would change its type from {.val {old_type}} to {.val {new_type}}.",
      i = "Update tools must preserve the object type; use a create-style tool for a new object type."
    ))
  }
  desc <- cv_describe_object(value, handle = handle, source = source)
  assign(handle, list(
    value      = value,
    descriptor = desc,
    created    = old$created %||% cv_now(),
    updated    = cv_now(),
    # Batch 3b item 2: an in-place update counts as a fresh access, same as a
    # cv_object_get() read -- see cv_object_evict_stale() below. This also
    # means an update on a handle that had been evicted (last_accessed +
    # value dropped, descriptor kept -- see cv_object_evict_stale()) simply
    # resurrects it with a fresh value, which is the correct, safe behavior:
    # the type check above already confirms it's a legitimate update of the
    # same handle, not a new object masquerading as an old one.
    last_accessed = cv_now(),
    # Monotonic per-store tie-break for the above -- see cv_object_next_seq().
    seq        = cv_object_next_seq(store),
    # Monotonic per-handle revision counter (Batch 2b fix): cv_now() has only
    # whole-second resolution, so two in-place updates to the SAME handle
    # within the same wall-clock second stamp an IDENTICAL `updated` string.
    # cv_sync_object_artifacts()'s change-detection signature used to be
    # built solely from created/updated, so the second update's artifact
    # write was silently skipped -- a user downloading that handle's .rds
    # got the FIRST update's stale content. `rev` increments unconditionally
    # on every real update, independent of clock resolution, so the
    # signature always changes when the object actually does.
    rev        = (old$rev %||% 0L) + 1L,
    source     = source
  ), envir = store)
  handle
}

#' Retrieve the raw object for a handle (server-side use only)
#' @keywords internal
cv_object_get <- function(store, handle) {
  if (!cv_object_exists(store, handle)) {
    cli::cli_abort("No object with handle {.val {handle}} exists in this session.")
  }
  rec <- get(handle, envir = store)
  # Batch 3b item 2: an evicted handle keeps its descriptor (so it still
  # shows up as "loaded" to the model/UI, and cv_object_update() can still
  # resurrect it) but its value has been dropped -- see
  # cv_object_evict_stale() below. Give a clear, actionable error here
  # instead of returning NULL, which would surface downstream as a confusing
  # type error deep inside whatever tool tried to use it.
  if (isTRUE(rec$evicted)) {
    cli::cli_abort(c(
      "{.val {handle}} was evicted from memory to save resources after a long time unused.",
      i = "Re-run {.val {rec$source %||% 'the tool that created it'}} to regenerate it."
    ))
  }
  # Batch 3b item 2: every real read counts as a fresh access, same as a put/
  # update -- see cv_object_evict_stale() below, which evicts the LEAST-
  # recently-accessed objects first once the store grows past its cap.
  rec$last_accessed <- cv_now()
  rec$seq <- cv_object_next_seq(store)
  assign(handle, rec, envir = store)
  rec$value
}

#' Does a handle exist?
#' @keywords internal
cv_object_exists <- function(store, handle) {
  is.character(handle) && length(handle) == 1L && exists(handle, envir = store, inherits = FALSE)
}

#' Remove an object
#' @keywords internal
cv_object_remove <- function(store, handle) {
  if (cv_object_exists(store, handle)) rm(list = handle, envir = store)
  invisible(NULL)
}

#' Handles of a session's job(s) still `queued`/`running`, i.e. handles that
#' must not be evicted right now regardless of how long they've sat unused.
#'
#' A heavy job's child process already received its own COPY of every input
#' object at launch time (via callr's serialization boundary - see
#' cv_launch_heavy() in agent_worker.R), so evicting the PARENT store's copy
#' mid-job can't corrupt the job itself. The real risk this guards against is
#' an update-style tool (addClustoData/addTypoData) finishing a long-running
#' heavy job and calling cv_object_update() on a handle that got evicted in
#' the meantime -- cv_object_update() already resurrects an evicted handle
#' safely (see above), so this protection is a belt-and-suspenders measure
#' against an unnecessary/confusing evict-then-immediately-resurrect cycle
#' for an object that was never actually idle, not a correctness requirement.
#' @keywords internal
.cv_object_inflight_handles <- function(sess) {
  if (is.null(sess) || is.null(sess$jobs)) return(character(0))
  jids <- ls(envir = sess$jobs)
  if (!length(jids)) return(character(0))
  out <- character(0)
  for (j in jids) {
    rec <- get(j, envir = sess$jobs)
    if (isTRUE(rec$status %in% c("queued", "running"))) {
      out <- c(out, rec$input_handles %||% character(0))
    }
  }
  unique(out)
}

#' Evict the LEAST-RECENTLY-ACCESSED objects from a store once it grows past
#' its cap, so a single long-lived session does not accumulate an unbounded
#' number of potentially large Seurat/SingleCellExperiment/CelliVerse-result
#' objects over its lifetime (this is the object-store sibling of the
#' job-registry leak fixed in Batch 2b item 6, and the session-registry /
#' history leaks fixed alongside this one -- see cv_sessions_evict_stale()
#' and cv_history_evict_stale() in agent_session.R).
#'
#' Unlike the job registry (where a "terminal" record can simply be deleted),
#' an evicted object's HANDLE is kept, so the model/UI don't lose track of it
#' entirely: only its (potentially large) `value` is dropped. The record
#' becomes a lightweight sentinel (`evicted = TRUE`, descriptor/created/
#' source kept) -- cv_object_get() gives a clear, actionable error naming the
#' handle and what regenerates it (see above) instead of the handle silently
#' vanishing, and cv_object_update() can resurrect it in place if the same
#' handle is legitimately written to again. `keep` counts only LIVE (non-
#' evicted) objects, since a sentinel is cheap and doesn't need its own cap.
#'
#' A store created with no session_id (every existing test call site) is not
#' attached to a real session and is never swept -- see cv_object_store_new().
#' @param store object store environment.
#' @param keep max LIVE objects to retain; defaults to the owning session's
#'   `object_store_limit` config (see cv_default_config()).
#' @return invisibly, the number of objects evicted.
#' @keywords internal
cv_object_evict_stale <- function(store, keep = NULL) {
  session_id <- attr(store, "cv_session_id")
  if (is.null(session_id)) return(invisible(0L))
  sess <- tryCatch(cv_session_get(session_id), error = function(e) NULL)
  if (is.null(sess)) return(invisible(0L))
  if (is.null(keep)) keep <- as.integer(sess$config$object_store_limit %||% 40L)

  handles <- cv_object_handles(store)
  if (!length(handles)) return(invisible(0L))
  recs <- stats::setNames(lapply(handles, function(h) get(h, envir = store)), handles)
  live <- handles[!vapply(recs, function(r) isTRUE(r$evicted), logical(1))]
  if (length(live) <= keep) return(invisible(0L))

  protected <- .cv_object_inflight_handles(sess)
  evictable <- setdiff(live, protected)
  n_over <- length(live) - keep
  if (n_over <= 0L || !length(evictable)) return(invisible(0L))

  # Oldest-accessed-first, ordered by the monotonic per-store `seq` counter
  # (not the wall-clock `last_accessed` string) -- see cv_object_next_seq()
  # for why: cv_now()'s whole-second resolution means a burst of accesses
  # within one second all tie on last_accessed, and order()'s tie-break
  # (input-vector position, i.e. cv_object_handles()'s alphabetical order)
  # could then evict a just-touched object ahead of a genuinely older one.
  # `seq` is unique and strictly increasing, so this is always correct.
  # Never evict more than the number of unprotected live objects available,
  # so a session that's mostly actively in use is left alone even if over
  # `keep`.
  seqs <- vapply(evictable, function(h) as.numeric(recs[[h]]$seq %||% 0L), numeric(1))
  evict_handles <- evictable[order(seqs)][seq_len(min(n_over, length(evictable)))]

  for (h in evict_handles) {
    old <- recs[[h]]
    assign(h, list(
      evicted       = TRUE,
      descriptor    = old$descriptor,
      created       = old$created,
      last_accessed = old$last_accessed,
      evicted_at    = cv_now(),
      source        = old$source
    ), envir = store)
  }
  invisible(length(evict_handles))
}

#' List all handles
#' @keywords internal
cv_object_handles <- function(store) {
  ls(envir = store, all.names = FALSE)
}

#' Handles of all loaded objects whose type is in `types`.
#'
#' A handle whose value cannot be typed is excluded rather than erroring: this
#' feeds tool-argument auto-resolution, where one unreadable object must not stop
#' the others from being offered.
#' @param store object store environment.
#' @param types character vector of canonical types (see `cv_object_type()`), or
#'   NULL/empty to return every handle.
#' @return a character vector of handles, possibly empty.
#' @keywords internal
cv_objects_of_type <- function(store, types = NULL) {
  hs <- cv_object_handles(store)
  if (is.null(types) || !length(types)) return(hs)
  keep <- vapply(hs, function(h) {
    tryCatch(cv_object_type(cv_object_get(store, h)) %in% types,
             error = function(e) FALSE)
  }, logical(1))
  hs[keep]
}

#' Get the descriptor for a handle (safe to send to LLM / client)
#' @keywords internal
cv_object_descriptor <- function(store, handle) {
  if (!cv_object_exists(store, handle)) return(NULL)
  get(handle, envir = store)$descriptor
}

#' All descriptors in the store (list) - the "what's loaded" view for the LLM
#' @keywords internal
cv_object_descriptors <- function(store) {
  lapply(cv_object_handles(store), function(h) cv_object_descriptor(store, h))
}

# ---- Handle-prefix heuristics -----------------------------------------------

#' Build a handle from a user-supplied display name + the object's type prefix.
#'
#' The upload "Optional name" field lets the user label an object (e.g. "pbmc
#' counts"). We turn that into a stable, handle-safe id by prefixing the object
#' type (mat_/obj_/df_/...) and sanitizing the name: lowercase, runs of
#' non-alphanumeric characters collapsed to single underscores, trimmed. Returns
#' NULL when the name is empty/blank (caller then falls back to a random id).
#' @keywords internal
cv_handle_from_name <- function(value, name) {
  if (is.null(name) || length(name) == 0) return(NULL)
  nm <- trimws(as.character(name)[1])
  if (!nzchar(nm)) return(NULL)
  nm <- tolower(nm)
  nm <- gsub("[^a-z0-9]+", "_", nm, perl = TRUE)
  nm <- gsub("^_+|_+$", "", nm, perl = TRUE)
  if (!nzchar(nm)) return(NULL)
  paste0(cv_handle_prefix(value), "_", nm)
}

#' Derive a result handle that inherits its base name from the input handle(s).
#'
#' Naming inheritance: a clustoCell run on `obj_pbmc3k` should produce
#' `clusto_pbmc3k`, not a random id - the handle then tells the user (and the
#' model) where the object came from. The base is the FIRST input handle with
#' its type prefix stripped (`obj_pbmc3k` -> `pbmc3k`); the result prefix comes
#' from the new object's class. If the candidate is already taken, suffix `_2`,
#' `_3`, ... until free. When `inherit_from` is empty/NULL we fall back to the
#' historical random id.
#' @param store object store environment.
#' @param value the new R object (its class picks the prefix).
#' @param inherit_from character vector of input handles (first one wins).
#' @return a unique handle (character).
#' @keywords internal
cv_derived_handle <- function(store, value, inherit_from = NULL) {
  prefix <- cv_handle_prefix(value)
  inherit_from <- as.character(inherit_from %||% character(0))
  inherit_from <- inherit_from[nzchar(inherit_from)]
  if (!length(inherit_from)) return(cv_new_id(prefix = prefix))
  base <- inherit_from[1]
  # Strip the known type prefix (obj_/clusto_/...); if none matches, use the
  # whole handle as the base.
  base <- sub("^(obj|sce|spe|clusto|markoclust|markocell|purity|features|typo|cellset|mat|df)_", "", base, perl = TRUE)
  # Same sanitization as cv_handle_from_name: lowercase, non-alnum -> _, trimmed.
  base <- tolower(base)
  base <- gsub("[^a-z0-9]+", "_", base, perl = TRUE)
  base <- gsub("^_+|_+$", "", base, perl = TRUE)
  if (!nzchar(base)) return(cv_new_id(prefix = prefix))
  candidate <- paste0(prefix, "_", base)
  if (!cv_object_exists(store, candidate)) return(candidate)
  k <- 2L
  while (cv_object_exists(store, paste0(candidate, "_", k))) k <- k + 1L
  paste0(candidate, "_", k)
}

#' Append a method + target-set tag to the inherit-base for a result handle.
#'
#' WHY (Round XXIV): an annotation run on a specific set of clusters should
#' produce a handle that SAYS so - `typo_pbmc_markerdb_c1_c3` or
#' `typo_pbmc_llm_c1sub1_c3sub2` - not a bare `typo_pbmc` / `typo_pbmc_2`. The
#' handle is an opaque reference (resolved by exact lookup, never name-parsed),
#' so a longer descriptive name is safe for every downstream tool.
#'
#' The tag is appended to the FIRST inherit handle's base; cv_derived_handle()
#' then strips only the LEADING type prefix and re-sanitizes, so the embedded
#' method/set tag survives intact and the usual `_2`/`_3` collision suffix still
#' applies (an identical repeat run -> `..._2`).
#'
#' @param inherit_from character vector of input handles (first one wins).
#' @param method short method tag, e.g. "markerdb" (typoClust) or "llm"
#'   (annotateCellsLLM / ceLLMarkup). NULL/"" -> no method tag.
#' @param desired_sets character vector of target set names (e.g. c("C1-Sub1")),
#'   or NULL when annotating ALL sets.
#' @return a character vector like `inherit_from` but with the first element's
#'   base augmented; returned unchanged when there is nothing to add.
#' @keywords internal
cv_tagged_inherit <- function(inherit_from, method = NULL, desired_sets = NULL) {
  inherit_from <- as.character(inherit_from %||% character(0))
  inherit_from <- inherit_from[nzchar(inherit_from)]
  if (!length(inherit_from)) return(inherit_from)

  # Method tag (sanitized, lowercase).
  mtag <- ""
  if (!is.null(method) && length(method) && nzchar(method[1])) {
    mtag <- gsub("[^a-z0-9]+", "", tolower(as.character(method)[1]), perl = TRUE)
  }

  # Target-set tag (cap + count): up to 3 explicit set names; ">3" collapses to
  # "<n>sets"; NULL (annotate all) -> "all".
  stag <- ""
  if (is.null(desired_sets)) {
    stag <- "all"
  } else {
    sets <- as.character(unlist(desired_sets, use.names = FALSE))
    sets <- sets[nzchar(sets)]
    if (length(sets)) {
      san <- vapply(sets, function(s) {
        s <- gsub("[^a-z0-9]+", "_", tolower(s), perl = TRUE)
        gsub("^_+|_+$", "", s, perl = TRUE)
      }, character(1))
      san <- san[nzchar(san)]
      if (length(san)) {
        stag <- if (length(san) <= 3L) paste(san, collapse = "_")
                else paste0(length(san), "sets")
      }
    }
  }

  tag <- paste(c(mtag, stag)[nzchar(c(mtag, stag))], collapse = "_")
  if (!nzchar(tag)) return(inherit_from)
  inherit_from[1] <- paste0(inherit_from[1], "_", tag)
  inherit_from
}

#' Choose a handle prefix based on object class
#' @keywords internal
cv_handle_prefix <- function(value) {
  cl <- class(value)[1]
  switch(cl,
    "Seurat"                = "obj",
    "SingleCellExperiment"  = "sce",
    "SpatialExperiment"     = "spe",
    "ClustoCell"            = "clusto",
    "MarkoClust"            = "markoclust",
    "MarkoCell"             = "markocell",
    "MarkerPurity"          = "purity",
    "DatasetMarkers"        = "features",
    "TypoClust"             = "typo",
    "CellSet"               = "cellset",
    "dgCMatrix"             = "mat",
    "matrix"                = "mat",
    "data.frame"            = "df",
    "obj"
  )
}

# ---- Object type detection (drives the typed registry) ----------------------

#' Return a canonical object-type string used by the typed tool registry
#'
#' The registry declares which input_object_types a tool accepts and what
#' output_object_type it produces; matching these prevents invalid tool chains
#' (e.g. addClustoData before clustoCell).
#' @keywords internal
cv_object_type <- function(value) {
  if (inherits(value, "Seurat")) return("Seurat")
  if (inherits(value, "SpatialExperiment")) return("SpatialExperiment")
  if (inherits(value, "SingleCellExperiment")) return("SingleCellExperiment")
  if (inherits(value, "ClustoCell")) return("ClustoCell")
  if (inherits(value, "MarkoClust")) return("MarkoClust")
  if (inherits(value, "MarkoCell")) return("MarkoCell")
  if (inherits(value, "MarkerPurity")) return("MarkerPurity")
  if (inherits(value, "DatasetMarkers")) return("DatasetMarkers")
  if (inherits(value, "TypoClust")) return("TypoClust")
  if (inherits(value, "CellSet")) return("CellSet")
  if (inherits(value, "dgCMatrix")) return("dgCMatrix")
  if (is.matrix(value)) return("matrix")
  if (is.data.frame(value)) return("data.frame")
  class(value)[1]
}

# ---- Descriptors: compact, LLM-safe summaries -------------------------------

#' Build a compact descriptor for any supported object
#'
#' A descriptor is a small named list with: handle, type, class, source, summary
#' (a one-line human string), and type-specific fields (dims, metadata columns,
#' cluster counts, ...). It NEVER contains the full matrix / expression data.
#'
#' @section The cv_describe_* contract:
#'
#' Descriptors are the ONLY thing the language model ever sees about a user's
#' object -- the object itself stays server-side behind a handle. That makes the
#' rules below load-bearing rather than stylistic: each one exists because
#' breaking it produced a real, shipped defect. Anyone adding a type must satisfy
#' all seven, and
#' `tests/testthat/test-round60-descriptor-contract-safe.R` asserts them
#' mechanically against every branch of the `switch()` below.
#'
#' \enumerate{
#'   \item \strong{Return only type-specific fields.} A `cv_describe_*()`
#'     returns a named list and must NOT set `handle`, `type`, `class`, `source`
#'     or `summary`. This function supplies those; `summary` in particular is
#'     computed last, from the merged descriptor, so a descriptor that sets it is
#'     silently overwritten.
#'   \item \strong{Never throw.} These run on objects of unknown provenance,
#'     during `cv_object_put()`, on the request thread. A throw here fails the
#'     user's upload -- not just its summary. Every field that reaches into an
#'     object's structure is wrapped in `tryCatch()` with a typed fallback
#'     (`character(0)`, `NA_integer_`), which is why the bodies below look more
#'     defensive than the data warrants.
#'   \item \strong{Stay bounded.} The descriptor is embedded in the system prompt
#'     on every turn. Cap any id/name list at `CV_DESC_MAX_IDS` and any preview
#'     at a handful of elements. Never include a matrix, a layer, or a full
#'     marker table.
#'   \item \strong{Ship every capped list with its TRUE total.} A field holding a
#'     capped list must be accompanied by an `n_*` count computed *before*
#'     capping, and `cv_summary_line()` must route the pair through
#'     `.cv_id_list_text(ids, total = n_*)`. This is the Round LVI invariant: a
#'     capped list whose total is derived from the already-capped vector can
#'     never announce the shortfall, so ids vanish silently from a prompt that
#'     simultaneously forbids the model from guessing an id it cannot see.
#'   \item \strong{Sort ids with `.cv_natural_sort()`.} Lexicographic order both
#'     reads wrongly (C10 before C2) and, combined with capping, drops ids from
#'     the MIDDLE of the range instead of the end.
#'   \item \strong{Distinguish "unknown" from "none".} `NA_integer_` means the
#'     count could not be determined; `0L` means there genuinely are none.
#'     `cv_summary_line()` renders these differently on purpose -- "sub-clusters
#'     n/a" versus "no sub-clusters" -- and collapsing them produced a summary
#'     reading "NA major clusters, NA sub-clusters" after a clustering that had
#'     in fact succeeded (the original Issue 3).
#'   \item \strong{Add a `cv_summary_line()` branch too.} A new `switch()` branch
#'     here without a matching branch there does not error; it falls through to
#'     `"<type> object"`, and the model is told nothing useful about an object it
#'     is expected to reason about.
#' }
#'
#' @param value the object to describe.
#' @param handle its store handle, if it has one yet.
#' @param source free-text provenance ("upload", a tool name, ...).
#' @return a named list: the base fields, the type-specific fields, and
#'   `summary`.
#' @keywords internal
cv_describe_object <- function(value, handle = NA_character_, source = "") {
  type <- cv_object_type(value)
  base <- list(
    handle  = handle,
    type    = type,
    class   = class(value)[1],
    source  = source
  )
  extra <- switch(type,
    "Seurat"               = cv_describe_seurat(value),
    "SpatialExperiment"    = cv_describe_sce(value),
    "SingleCellExperiment" = cv_describe_sce(value),
    "dgCMatrix"            = cv_describe_matrix(value),
    "matrix"               = cv_describe_matrix(value),
    "ClustoCell"           = cv_describe_clustocell(value),
    "MarkoClust"           = cv_describe_markoresult(value, "MarkoClust"),
    "MarkoCell"            = cv_describe_markoresult(value, "MarkoCell"),
    "MarkerPurity"         = cv_describe_markerpurity(value),
    "DatasetMarkers"       = cv_describe_datasetmarkers(value),
    "TypoClust"            = cv_describe_typoclust(value),
    "CellSet"              = cv_describe_cellset(value),
    "data.frame"           = cv_describe_df(value),
    cv_describe_fallback(value)
  )
  out <- c(base, extra)
  out$summary <- cv_summary_line(out)
  out
}

#' Describe a Seurat object.
#'
#' `dim()` on a Seurat is features x cells, the opposite of the cells-as-rows
#' convention a reader might expect, so the two counts are assigned explicitly
#' rather than positionally. `layers` is read from the DEFAULT assay only --
#' enumerating every assay's layers is unbounded on a multimodal object, and the
#' default assay is what a tool will actually operate on. `data_kind` matters
#' more than it looks: it is what stops a tool from log-transforming values that
#' are already log-transformed (see `cv_detect_log_transformed()`).
#' @param x a Seurat object.
#' @return a named list of type-specific descriptor fields.
#' @keywords internal
cv_describe_seurat <- function(x) {
  md_cols <- tryCatch(colnames(x[[]]), error = function(e) character(0))
  assays  <- tryCatch(names(x@assays), error = function(e) character(0))
  reds    <- tryCatch(names(x@reductions), error = function(e) character(0))
  dims    <- tryCatch(dim(x), error = function(e) c(NA, NA))  # features x cells
  layers  <- tryCatch({
    da <- Seurat::DefaultAssay(x)
    if (!is.null(da) && da %in% assays) SeuratObject::Layers(x[[da]]) else character(0)
  }, error = function(e) character(0))
  list(
    n_cells        = if (length(dims) == 2) dims[2] else NA_integer_,
    n_features     = if (length(dims) == 2) dims[1] else NA_integer_,
    assays         = assays,
    default_assay  = tryCatch(Seurat::DefaultAssay(x), error = function(e) NA_character_),
    layers         = layers,
    data_kind      = tryCatch(cv_detect_log_transformed(x), error = function(e) "unknown"),
    reductions     = reds,
    metadata_cols  = md_cols
  )
}

#' Describe a SingleCellExperiment or SpatialExperiment.
#'
#' Both classes route here: the fields the agent needs (dims, assay names,
#' colData columns) are read through the same `SummarizedExperiment` accessors
#' for either, so a separate branch would be two copies of one function. The
#' spatial coordinates of a SpatialExperiment are deliberately NOT summarized --
#' no tool consumes them yet, and an unused field in the system prompt is a cost
#' paid on every turn.
#' @param x a SingleCellExperiment or SpatialExperiment.
#' @return a named list of type-specific descriptor fields.
#' @keywords internal
cv_describe_sce <- function(x) {
  md_cols <- tryCatch(colnames(SummarizedExperiment::colData(x)), error = function(e) character(0))
  assays  <- tryCatch(SummarizedExperiment::assayNames(x), error = function(e) character(0))
  dims    <- tryCatch(dim(x), error = function(e) c(NA, NA))
  list(
    n_cells       = if (length(dims) == 2) dims[2] else NA_integer_,
    n_features    = if (length(dims) == 2) dims[1] else NA_integer_,
    assays        = assays,
    metadata_cols = md_cols
  )
}

#' Describe a bare count matrix, dense or sparse.
#'
#' Genes are rows in CelliVerse's convention, which is why `n_features` is
#' `nrow()`. The `*_head` previews exist so the model can recognise what it is
#' looking at -- gene symbols versus Ensembl ids, barcodes versus sample names --
#' without any of the values being sent. Five elements is enough to tell those
#' apart and small enough to cost nothing.
#' @param x a matrix or dgCMatrix.
#' @return a named list of type-specific descriptor fields.
#' @keywords internal
cv_describe_matrix <- function(x) {
  list(
    n_features = nrow(x),   # genes are rows in CelliVerse convention
    n_cells    = ncol(x),
    sparse     = inherits(x, "dgCMatrix"),
    data_kind  = cv_detect_log_transformed(x),
    rownames_head = utils::head(rownames(x), 5),
    colnames_head = utils::head(colnames(x), 5)
  )
}

# ---- Counts-vs-log detection + matrix->Seurat conversion --------------------

#' Heuristically detect whether a count matrix / Seurat layer holds RAW counts
#' or already NORMALIZED log-transformed values.
#'
#' WHY: users sometimes load data whose "counts" layer actually holds
#' log-normalized values. Feeding that to a tool with log1p=TRUE would
#' double-transform. Heuristic (best-effort): sample the values; data is
#' treated as LOG/NORMALIZED when (a) any sampled value is non-integer, OR
#' (b) the max sampled value is suspiciously small for raw UMI counts (< ~30).
#' Returns "counts", "log", or "unknown" (empty/undetermined).
#' @keywords internal
cv_detect_log_transformed <- function(x, assay = NULL, layer = "counts") {
  vals <- tryCatch({
    m <- x
    if (inherits(x, "Seurat")) {
      da <- if (!is.null(assay)) assay else Seurat::DefaultAssay(x)
      m <- SeuratObject::LayerData(x[[da]], layer = layer)
    }
    if (is.null(m)) return("unknown")
    # Sample up to ~200k non-zero values for speed on large sparse matrices.
    if (inherits(m, "dgCMatrix")) {
      v <- m@x
    } else {
      v <- as.numeric(as.matrix(m))
    }
    # Round LXXXII: SUBSAMPLE BEFORE FILTERING when the vector is large.
    #
    # MEASURED, on the user's own failure. `v[is.finite(v) & v != 0]` on a real
    # dataset's non-zero vector allocates four full-length temporaries -- two
    # logicals, their `&`, and the subset copy -- and then throws 99.94% of the
    # result away one line later, because the classification only ever looks at
    # a 200,000-value sample. On a 27,578 x 208,506 matrix (330M stored values,
    # 3.7 GB) that is roughly 3.2 GB of transient allocation and ~14 s spent
    # deciding one word: "counts" or "log".
    #
    # Timed at quarter scale in the Round LXXXII sandbox (70M stored values):
    # the mask cost 1.3 s and 266 MB, the subset 2.2 s and 533 MB -- and this
    # ran inside cv_object_put(), so every load of a large matrix paid it twice
    # (once for the matrix, once for the Seurat built from it).
    #
    # Taking the sample first makes the whole thing O(200k) instead of O(nnz).
    # It is NOT a different statistic: for a dgCMatrix `v` is the stored entries
    # by construction, so `!= 0` removes only explicitly-stored zeros, and both
    # orders sample from the same population. The seeded-and-restored RNG idiom
    # below is unchanged, so the answer stays reproducible -- which is the whole
    # point of Round LXIV (D2), recorded there.
    n_sample <- 2e5
    if (length(v) > n_sample) {
      old <- if (exists(".Random.seed", envir = .GlobalEnv))
               get(".Random.seed", envir = .GlobalEnv) else NULL
      set.seed(1234L)
      # Over-sample, because the finite/non-zero filter below removes some.
      v <- v[sample.int(length(v), min(length(v), n_sample * 2))]
      if (!is.null(old)) assign(".Random.seed", old, envir = .GlobalEnv)
    }
    v <- v[is.finite(v) & v != 0]
    # BOTH set.seed() calls below are belt and braces, and break-verification is
    # what established it: removing EITHER one alone leaves the result
    # reproducible, because the surviving seed fixes the RNG state that the
    # other sampling step then starts from. Removing both makes the verdict a
    # coin toss on knife-edge data (measured: 10 "counts" / 10 "log" over 20
    # calls on a matrix whose only large values are three in a million). They
    # are both kept because a future edit that deletes one must not silently
    # depend on the other.
    #
    # Round LXIV (D2): this subsample decides `log1p` for every downstream
    # clustoCell run via cv_adjust_log1p_layer(), so it MUST be reproducible.
    # Unseeded, the classification below (has_fraction || max < 30) flipped
    # between runs on the same object whenever the largest values were rare --
    # measured 27x "counts" / 3x "log" across 30 identical calls on one matrix.
    # The consequence was not cosmetic: the same request could run a materially
    # different analysis on consecutive turns and announce it as a deliberate
    # decision. Uses the same local-seed idiom as cv_make_cellset(), which also
    # restores the global RNG so a describe call never perturbs the user's own
    # stream (verified: set.seed(7); runif(1) was returning a different value
    # when a describe call ran in between).
    if (length(v) > n_sample) {
      old <- if (exists(".Random.seed", envir = .GlobalEnv))
               get(".Random.seed", envir = .GlobalEnv) else NULL
      set.seed(1234L)
      v <- sample(v, n_sample)
      if (!is.null(old)) assign(".Random.seed", old, envir = .GlobalEnv)
    }
    v
  }, error = function(e) numeric(0))
  if (!length(vals)) return("unknown")
  has_fraction <- any(abs(vals - round(vals)) > 1e-6)
  mx <- suppressWarnings(max(vals, na.rm = TRUE))
  if (has_fraction || (is.finite(mx) && mx < 30)) "log" else "counts"
}

#' Convert a (sparse) matrix or data.frame of counts into a Seurat object.
#'
#' Agent-layer helper (does NOT touch CelliVerse analysis functions). Assumes
#' the CelliVerse genes-x-cells convention. A data.frame is coerced to a numeric
#' matrix; a non-numeric frame returns NULL so the caller skips conversion.
#' @keywords internal
cv_matrix_to_seurat <- function(x) {
  m <- x
  if (inherits(x, "data.frame")) {
    # Coerce only if every column is numeric/integer; otherwise not count data.
    ok <- all(vapply(x, function(col) is.numeric(col) || is.integer(col), logical(1)))
    if (!ok) return(NULL)
    m <- as.matrix(x)
    if (is.null(rownames(m)) || !nzchar(rownames(m)[1] %||% "")) return(NULL)
  }
  if (!(inherits(m, "matrix") || inherits(m, "dgCMatrix"))) return(NULL)
  if (!is.numeric(m) && !inherits(m, "dgCMatrix")) return(NULL)
  so <- Seurat::CreateSeuratObject(counts = m)
  so
}

#' Describe a ClustoCell (and, since MarkoClust carries that class, a
#' MarkoClust result too).
#'
#' The one to read carefully. `n_major_clusters` / `n_sub_clusters` are the TRUE
#' totals, computed before `major_labels` / `sub_labels` are capped, because
#' `cv_summary_line()` compares the pair to announce any shortfall (contract rule
#' 4). `n_sub_clusters` also distinguishes 0 from NA (rule 6): a `markoClust` run
#' with `identify_subclusters = FALSE` genuinely has none, which must not read as
#' "n/a". And "Isolated" is excluded from `sub_labels` because it is a real label
#' `clustoCell()` assigns to unassigned cells, not a set anything can annotate.
#' @param x a ClustoCell object.
#' @return a named list of type-specific descriptor fields.
#' @keywords internal
cv_describe_clustocell <- function(x) {
  # Real ClustoCell structure (verified against the package's own results):
  #   x$clusters$major_clusters       named chr vector: cell -> "C1".."Cn"
  #   x$clusters$sub_clusters         list, one entry per major cluster
  #   x$clusters$merged_sub_clusters  named chr vector: cell -> "C1-Sub1" ...
  #   x$markers                       nested marker lists
  # The previous implementation treated x$clusters as a flat data.frame and so
  # the counts silently fell through to NA even though clustering succeeded.
  n_major <- NA_integer_; n_sub <- NA_integer_; n_cells <- NA_integer_
  major_labels <- character(0)
  sub_labels <- character(0)
  cl <- x$clusters
  if (!is.null(cl) && is.list(cl)) {
    mj <- cl$major_clusters
    if (!is.null(mj)) {
      major_labels <- .cv_natural_sort(unique(as.character(mj)))
      n_major <- length(major_labels)
      n_cells <- length(mj)
    }
    if (!is.null(cl$merged_sub_clusters)) {
      # Round XXV: expose the ACTUAL sub-cluster ids (not just a count) so the
      # agent model can see them in the system-prompt object list and pass real
      # ids to annotateCellsLLM desired_sets instead of hallucinating names.
      # "Isolated" is a real label clustoCell assigns to unassigned cells, not
      # an annotatable sub-cluster, so exclude it.
      sl <- .cv_natural_sort(unique(as.character(cl$merged_sub_clusters)))
      sub_labels <- sl[sl != "Isolated"]
      n_sub <- length(sub_labels)
    } else if (!is.null(cl$sub_clusters)) {
      n_sub <- length(cl$sub_clusters)
    } else {
      # Neither present => sub-clustering was not requested/computed (e.g.
      # markoClust with identify_subclusters=FALSE). Report 0, not NA, so the
      # summary reads "no sub-clusters" instead of a misleading "NA".
      n_sub <- 0L
    }
  }
  list(
    slots            = names(x),
    n_cells          = n_cells,
    n_major_clusters = n_major,
    n_sub_clusters   = n_sub,
    # Round LVI: was head(.,20)/head(.,40) -- low enough that ordinary
    # clusterings lost ids, and silently. n_major_clusters/n_sub_clusters above
    # carry the TRUE totals, so cv_summary_line() can announce any shortfall.
    major_labels     = utils::head(major_labels, CV_DESC_MAX_IDS),
    sub_labels       = utils::head(sub_labels, CV_DESC_MAX_IDS),
    has_markers      = !is.null(x$markers),
    has_globally_pure = !is.null(x$globally_pure_ranked) || !is.null(x$globally_pure_high),
    has_quiescent    = !is.null(x$quiescent_cells) && length(x$quiescent_cells) > 0,
    has_isolated     = !is.null(x$isolated_cells) && length(x$isolated_cells) > 0,
    # Round LXX (audit #16): the two flags above have been computed here since
    # the descriptor was written and cv_summary_line() rendered neither, so a
    # clustering that set a third of its cells aside read exactly like one that
    # set none aside. A boolean alone could only ever say "some", which is the
    # least useful true statement available, so carry the counts too.
    #
    # tryCatch + NA_integer_ per contract rules 2 and 6: NA means the count
    # could not be determined, 0L means there genuinely are none, and
    # cv_summary_line() renders those two differently on purpose.
    n_quiescent      = tryCatch(length(x$quiescent_cells %||% character(0)),
                                error = function(e) NA_integer_),
    n_isolated       = tryCatch(length(x$isolated_cells %||% character(0)),
                                error = function(e) NA_integer_),
    is_sketched      = !is.null(x$sketched_cells)
  )
}

#' Describe a MarkoCell result.
#'
#' The structure is two levels deep and the levels are easy to mistake for each
#' other: `$cell_subset_markers` is keyed by marker TYPE (positive / negative /
#' medium), and the analysed cell-subset names live one level in. Reading names
#' off the outer list would report "positive, negative" as if they were subsets.
#' The loop picks the first non-empty type because every type holds the same set
#' of subsets; which one supplies the names does not matter, only that an empty
#' one is skipped.
#'
#' `type` is accepted for call-site symmetry with the MarkoClust branch of
#' `cv_describe_object()`'s switch; the returned fields are the same either way,
#' since a genuine MarkoClust is class ClustoCell and never reaches here.
#' @param x a MarkoCell object.
#' @param type the type label from the dispatch switch.
#' @return a named list of type-specific descriptor fields.
#' @keywords internal
cv_describe_markoresult <- function(x, type) {
  # MarkoCell: $cell_subset_markers (named list, one entry per analysed subset)
  #            + globally_pure_* + quiescent_cells.
  # MarkoClust in this package is class "ClustoCell" and is described by
  # cv_describe_clustocell; this branch handles genuine MarkoCell objects.
  # $cell_subset_markers is keyed by marker TYPE (positive/negative/medium);
  # the actual analysed cell-subset names live one level in (e.g. "s_1_cc_C1").
  n_subsets <- NA_integer_; subset_names <- character(0)
  csm <- x$cell_subset_markers
  if (!is.null(csm) && is.list(csm)) {
    inner <- NULL
    for (k in names(csm)) if (is.list(csm[[k]]) && length(csm[[k]])) { inner <- csm[[k]]; break }
    if (!is.null(inner)) {
      subset_names <- names(inner)
      n_subsets <- length(inner)
    }
  }
  list(
    slots            = names(x),
    marker_types     = names(csm),
    n_cell_subsets   = n_subsets,
    subset_names     = utils::head(.cv_natural_sort(subset_names), CV_DESC_MAX_IDS),
    has_globally_pure = !is.null(x$globally_pure_ranked) || !is.null(x$globally_pure_high)
  )
}

#' Describe a MarkerPurity result.
#'
#' Three levels: the single top-level element names the GROUPING
#' (`within_clusters` or `within_cells`), inside it the keys are marker types,
#' and inside those are the analysed group ids. The ids are unioned across marker
#' types rather than taken from the first, because a group can be absent from one
#' type's table while present in another -- taking the first would under-report
#' the set the model is allowed to reference.
#' @param x a MarkerPurity object.
#' @return a named list of type-specific descriptor fields.
#' @keywords internal
cv_describe_markerpurity <- function(x) {
  # MarkerPurity: single element ($within_clusters or $within_cells) keyed by
  # marker TYPE (positive/negative/medium), each of which is keyed by the
  # analysed group id(s) (cluster id or cell-subset name).
  grp <- names(x)[1] %||% NA_character_
  by_type <- if (length(x)) x[[1]] else NULL
  group_ids <- character(0)
  if (!is.null(by_type) && is.list(by_type)) {
    for (k in names(by_type)) if (is.list(by_type[[k]]) && length(by_type[[k]])) {
      group_ids <- union(group_ids, names(by_type[[k]]))
    }
  }
  list(
    slots       = names(x),
    grouping    = grp,
    marker_types = if (!is.null(by_type)) names(by_type) else character(0),
    n_groups    = length(group_ids),
    group_ids   = utils::head(.cv_natural_sort(group_ids), CV_DESC_MAX_IDS),
    # Round LXXVI: the ANSWER, not just the shape. See .cv_markerpurity_findings().
    findings    = .cv_markerpurity_findings(by_type, group_ids)
  )
}

# ---- What markerPurity actually found (Round LXXVI, from live use) -----------
#
# THE REPORT. Asked "what is the purity of marker CD8A in C1", the agent ran
# markerPurity -- correctly -- and then answered "0.0000 (0% purity)", "CD8A is
# not expressed in C1". The object it had just created said, reproduced here:
#
#   $within_clusters$positive_markers$C1 -> 0 rows
#   $within_clusters$negative_markers$C1 -> 0 rows
#   $within_clusters$medium_markers$C1   -> CD8A, Gini 0.75, Purity 0.25, Rank 1
#
# The number the user asked for was computed, stored, and reached nobody. The
# summary said only:
#
#   "MarkerPurity: purity assessed within_clusters across 1 group(s) [C1]"
#
# -- the SHAPE of the result and nothing about the result. So the model had
# nothing to answer from, went looking elsewhere, read a top-10 POSITIVE marker
# table, and inferred zero.
#
# THIS IS THE ENFORCEMENT MECHANISM for the item, and the prompt rule is not.
# Round LXVII's rule 2h already told the model that a top-N table cannot answer
# a purity question; it did it regardless. Putting the answer in front of the
# model removes the step where it can go wrong, which a rule cannot.
#
# ALL THREE CLASSES. positive/negative/medium are enumerated together and a
# class with no hit is named as empty, because "no positive hit" was exactly the
# fact that got mistaken for "purity zero".
#
# KEPT SHORT ON PURPOSE. This lands in cv_summary_line(), which cv_system_prompt()
# re-injects for every loaded object on EVERY turn -- the same per-turn budget
# Round LXXV's #29 refused to spend. One gene in one cluster is a few words; a
# 20-gene sweep is bounded and says so rather than pasting a table.

CV_PURITY_FINDINGS_MAX <- 12L

#' Flatten a MarkerPurity's per-type/per-group tables into one compact record.
#' @keywords internal
.cv_markerpurity_findings <- function(by_type, group_ids) {
  if (is.null(by_type) || !is.list(by_type) || !length(group_ids)) return(list())
  out <- list(); n <- 0L; dropped <- 0L
  for (g in group_ids) {
    hits <- character(0); empty <- character(0)
    for (k in names(by_type)) {
      df <- tryCatch(by_type[[k]][[g]], error = function(e) NULL)
      # "positive_markers" -> "positive"
      lbl <- sub("_markers$", "", k)
      if (!is.data.frame(df) || !nrow(df)) { empty <- c(empty, lbl); next }
      for (i in seq_len(nrow(df))) {
        if (n >= CV_PURITY_FINDINGS_MAX) { dropped <- dropped + 1L; next }
        pu <- suppressWarnings(as.numeric(df$Purity[i] %||% NA))
        rk <- suppressWarnings(as.integer(df$Rank[i] %||% NA))
        hits <- c(hits, sprintf("%s %s%s%s",
          as.character(df$Feature[i] %||% "?"), lbl,
          if (is.na(pu)) "" else sprintf(" purity %.4f", pu),
          if (is.na(rk)) "" else sprintf(" (rank %d)", rk)))
        n <- n + 1L
      }
    }
    out[[g]] <- list(hits = hits, empty_types = empty)
  }
  attr(out, "dropped") <- dropped
  out
}

#' Render the findings for one group as a clause.
#' @keywords internal
.cv_markerpurity_findings_text <- function(findings) {
  if (!length(findings)) return("")
  parts <- character(0)
  for (g in names(findings)) {
    f <- findings[[g]]
    if (length(f$hits)) {
      cl <- paste0(g, ": ", paste(f$hits, collapse = ", "))
      # Naming the EMPTY classes matters as much as the hits: "no positive hit"
      # is the fact that was read as "purity zero".
      if (length(f$empty_types))
        cl <- paste0(cl, " (none as ", paste(f$empty_types, collapse = "/"), ")")
      parts <- c(parts, cl)
    } else {
      parts <- c(parts, sprintf("%s: no marker of any class", g))
    }
  }
  drop <- attr(findings, "dropped") %||% 0L
  sprintf(". Found -- %s%s", paste(parts, collapse = "; "),
          if (drop > 0L) sprintf(" ... (+%d more, read the object for the rest)", drop) else "")
}

#' Describe a DatasetMarkers result.
#'
#' `$combined_markers` is a flat character vector of gene names (the deduplicated
#' union of cluster and sub-cluster positive markers), so `length()` is the count
#' -- not `nrow()`, which would be NULL and surface as a missing count.
#' `markers_head` is capped at 15: enough for the model to see what kind of
#' markers these are, far short of a marker table.
#' @param x a DatasetMarkers object.
#' @return a named list of type-specific descriptor fields.
#' @keywords internal
cv_describe_datasetmarkers <- function(x) {
  # $combined_markers is a character vector of gene names (deduplicated union of
  # cluster + sub-cluster positive markers). length() is the right count.
  n <- tryCatch(length(x$combined_markers), error = function(e) NA_integer_)
  list(
    slots              = names(x),
    n_combined_markers = n,
    markers_head       = tryCatch(utils::head(as.character(x$combined_markers), 15),
                                  error = function(e) character(0))
  )
}

#' Describe a TypoClust annotation result.
#'
#' Unlike every other descriptor, this one carries a RESULT and not just a shape:
#' `top_labels` holds the rank-1 cell type per annotated set. That is deliberate
#' and is the fix from Round XVIII -- with only counts exposed, the model
#' hallucinated the annotation table and reported the pbmc3k platelet cluster as
#' "NK cells" while `typoClust()` had correctly returned Platelet. The predicted
#' labels are small, and they are the whole point of the object.
#' @param x a TypoClust object.
#' @return a named list of type-specific descriptor fields.
#' @keywords internal
cv_describe_typoclust <- function(x) {
  sets <- tryCatch(names(x$cell_types), error = function(e) character(0))
  # Round LXIV: carry the annotation CONTEXT, not just the labels.
  #
  # The user asked for a species picker and then could not tell, from the
  # result, whether their choice had actually been used -- a fair complaint:
  # tissue and condition appear beside each cell type ("NK Cell
  # (Blood/Healthy)") but species appeared nowhere at all, in the card, the
  # summary, or the descriptor. An agent that quietly does the right thing and
  # an agent that quietly does the wrong thing looked identical here.
  #
  # Read defensively: metadata is absent on a hand-built or older object, and a
  # descriptor must never throw (the contract Round LX pinned).
  md <- tryCatch(x$metadata, error = function(e) NULL)
  # `md$species` on an atomic vector throws ("$ operator is invalid for atomic
  # vectors"), and a descriptor must NEVER throw -- the contract Round LX
  # pinned. Caught by this round's own malformed-object test rather than in
  # production, which is the whole reason that test exists.
  if (!is.list(md)) md <- list()
  one <- function(v) {
    if (is.null(v) || !length(v)) return(NULL)
    v <- as.character(v)[1]
    if (is.na(v) || !nzchar(v)) NULL else v
  }
  list(slots = names(x), annotated_sets = sets, n_sets = length(sets),
       top_labels = cv_typoclust_top_labels(x),
       species = one(md$species),
       ann_method = one(md$ann_method))
}

# ---- Cluster/set id lists (Round LVI) ---------------------------------------
#
# The system prompt tells the model, verbatim: "SET IDS: pass ONLY ids that
# appear in the loaded object's summary (the system prompt lists every real
# cluster/sub-cluster id) - NEVER invent or guess a set id."
#
# That promise was false. Descriptors truncated their id lists (majors at 20,
# subs at 40, subsets and groups at 20) and cv_summary_line() then truncated
# AGAIN (at 6), with no marker in either case. Worse, ids were sorted
# LEXICOGRAPHICALLY before cutting, so on a 30-cluster object the twenty kept
# were C1, C10-C19, C2, C20-C27 and the ten dropped were C3, C4, C5, C6, C7,
# C8, C9, C28, C29, C30 -- ordinary low-numbered clusters, silently invisible to
# a model that is simultaneously forbidden from guessing an id it cannot see.
#
# There WAS a ", ..." marker for the sub-cluster list, and it could never fire:
# it tested `length(d$sub_labels) > 40` against a vector the descriptor had
# already cut to 40. A dead condition guarding a silent truncation.
#
# The three helpers below fix all of that in one place: natural ordering so C2
# precedes C10, a safety bound high enough that no realistic clustering reaches
# it, and truncation that is always ANNOUNCED because it is measured against the
# true total rather than against the already-shortened list.

#' Upper bound on how many ids a descriptor stores.
#'
#' Deliberately far above any realistic clustering (the previous 20/40 were
#' not). This exists to stop a pathological object from putting an unbounded
#' string in the system prompt, not to summarize -- reaching it is announced.
CV_DESC_MAX_IDS <- 500L

#' Sort ids the way a human reads them: C2 before C10, C1-Sub2 before C1-Sub10.
#'
#' Plain sort() is lexicographic, which both reads wrongly and -- when combined
#' with truncation -- drops ids from the MIDDLE of the range rather than the end.
#' Every digit run is zero-padded to a fixed width to build the sort key, so the
#' comparison stays a string comparison (no numeric overflow, no locale
#' surprises) while ordering numerically.
#' @param x character vector of ids.
#' @return `x`, reordered.
#' @keywords internal
.cv_natural_sort <- function(x) {
  if (!length(x)) return(x)
  x <- as.character(x)
  key <- vapply(x, function(s) {
    parts <- regmatches(s, gregexpr("[0-9]+|[^0-9]+", s))[[1]]
    paste(vapply(parts, function(p) {
      if (grepl("^[0-9]+$", p)) formatC(p, width = 12, flag = "0") else p
    }, character(1)), collapse = "")
  }, character(1), USE.NAMES = FALSE)
  x[order(key, method = "radix")]
}

#' Render an id list for a summary line, announcing any truncation.
#'
#' @param ids the ids the descriptor actually holds (already bounded).
#' @param total the TRUE number of ids the object has. Compared against
#'   `length(ids)`, so the "+N more" marker is measured against reality rather
#'   than against the list that was already cut -- the bug this replaces.
#' @param max_show optional further cap for display; NULL shows everything the
#'   descriptor holds, which is the default because the model is instructed to
#'   take set ids from here.
#' @return a bracketed string like " [C1, C2, C3 ... (+7 more)]", or "".
#' @keywords internal
.cv_id_list_text <- function(ids, total = NULL, max_show = NULL) {
  ids <- as.character(ids %||% character(0))
  if (!length(ids)) return("")
  total <- suppressWarnings(as.integer(total %||% length(ids)))
  if (is.na(total) || total < length(ids)) total <- length(ids)
  shown <- if (is.null(max_show)) ids else utils::head(ids, max_show)
  more <- total - length(shown)
  sprintf(" [%s%s]", paste(shown, collapse = ", "),
          if (more > 0L) sprintf(" ... (+%d more)", more) else "")
}

#' The rank-1 row of one TypoClust set's candidate table.
#'
#' Round LV (Batch 5a): this four-line idiom existed verbatim in two files -
#' here (inside cv_typoclust_top_labels) and in .cv_typoclust_tissue_warning()
#' (agent_tools_core.R). Byte-identical in both, so extracting it changes no
#' behaviour; the point is that a non-obvious idiom with a tryCatch fallback
#' now has ONE definition. Batch 3a found the copy-paste that had already
#' drifted; this is the same shape, caught before it did.
#'
#' The fallbacks are deliberate and are the reason this is not a one-liner: a
#' TypoClust may come from celliverse::typoClust() (which ranks) or from
#' annotateCellsLLM (which may not), and a malformed Rank column must degrade
#' to "first row" rather than error.
#' @param df one set's candidate data.frame.
#' @return a one-row data.frame.
#' @keywords internal
.cv_typoclust_rank1_row <- function(df) {
  tryCatch({
    if ("Rank" %in% names(df)) df[df$Rank == 1L, , drop = FALSE][1, , drop = FALSE]
    else df[1, , drop = FALSE]
  }, error = function(e) df[1, , drop = FALSE])
}

#' Compact "set: rank-1 cell type" map for a TypoClust object.
#'
#' WHY this exists (Round XVIII): the agent previously surfaced only
#' "Created TypoClust: N annotated set(s) [...]" to the model, NEVER the
#' predicted labels. The model then HALLUCINATED the annotation table and
#' defaulted the pbmc3k platelet cluster (C5) to the common "NK cells" - even
#' though celliverse::typoClust() correctly returns Platelet. Exposing the
#' rank-1 label per set makes the model report the REAL annotation.
#'
#' Handles both TypoClust flavours:
#'   * markerDB (celliverse::typoClust): columns CellType/Tissue/Condition/
#'     Combined_Score/Rank -> "Platelet (Blood/Healthy)".
#'   * ceLLMarkup (annotateCellsLLM): columns CellType/Confidence/Rank/Reason
#'     -> "B cell". Tissue/Condition live in x$metadata, not per-row.
#' @param x a TypoClust object.
#' @return a named character vector: name = set id, value = label string.
#'   Sets whose label could not be read are dropped, so the result may be
#'   shorter than `names(x$cell_types)`.
#' @keywords internal
cv_typoclust_top_labels <- function(x) {
  ct <- tryCatch(x$cell_types, error = function(e) NULL)
  if (is.null(ct) || !length(ct)) return(character(0))
  meta_tissue    <- tryCatch(x$metadata$tissue,    error = function(e) NULL)
  meta_condition <- tryCatch(x$metadata$condition, error = function(e) NULL)
  out <- vapply(names(ct), function(s) {
    df <- ct[[s]]
    if (is.null(df) || !is.data.frame(df) || !nrow(df) || !("CellType" %in% names(df)))
      return(NA_character_)
    r1 <- .cv_typoclust_rank1_row(df)
    lbl <- as.character(r1$CellType[1] %||% NA_character_)
    if (is.na(lbl) || !nzchar(lbl)) return(NA_character_)
    # Append tissue/condition context when present (markerDB per-row; ceLLMarkup
    # from metadata). Skip NA/empty so ceLLMarkup stays compact.
    tis <- if ("Tissue" %in% names(r1)) as.character(r1$Tissue[1]) else meta_tissue
    con <- if ("Condition" %in% names(r1)) as.character(r1$Condition[1]) else meta_condition
    ctx <- c(tis, con)
    ctx <- ctx[!is.na(ctx) & nzchar(ctx)]
    base <- if (length(ctx)) paste0(lbl, " (", paste(ctx, collapse = "/"), ")") else lbl
    # Round LXXV (audit #26): how strong the call is, appended compactly.
    paste0(base, .cv_typoclust_margin_text(df))
  }, character(1))
  out[!is.na(out)]
}

# ---- How strong is the rank-1 call? (Round LXXV, audit #26) ------------------
#
# THE COMPLAINT: "a 0.9 call and a 0.3 call do not read identically". Measured,
# they read EXACTLY identically -- no score reaches the transcript, the model,
# the system prompt, the tool card, the Results tab or the downloadable TXT.
# `cv_typoclust_top_labels()` already held the full rank-1 ROW and read only
# `CellType`; the number was one field access away and discarded.
#
# WHY NOT JUST PRINT THE NUMBER. The two annotation paths carry different and
# incomparable quantities, and neither is meaningful alone:
#
#   markerDB   Combined_Score -- UNBOUNDED and not comparable across sets.
#              Measured in this codebase's own fixtures: 14241, 8550, 4578.7,
#              100, 90. "NK Cell (4578.7)" tells a reader nothing at all.
#   agent LLM  Confidence -- in [0,1], but it is the model's own unvalidated
#              claim, parsed with no range check. "NK Cell (0.92)" reads as a
#              measurement and is not one.
#
# WHAT IS SCALE-FREE AND MEANINGFUL ON BOTH: the SEPARATION from the runner-up.
# A rank-1 that beats rank-2 six times over is a different statement from one
# that beats it by 2%, and that difference survives both scales. It is also the
# quantity this codebase already trusts: `inherit_score_ratio` (Rounds LXXI /
# LXXII, default 0.5 at the user's choice) gates parent inheritance on exactly
# this ratio, so the concept is established rather than invented here.
#
# THE RUNNER-UP IS NAMED ONLY WHEN IT IS CLOSE. Printing "next: X" on every
# label would double the length of a line that is re-injected into the system
# prompt every turn, to say "and the alternative was much worse" -- which is the
# uninteresting case. At CV_TYPO_NEAR_TIE and above, the alternative is the
# whole story, and that is when it is named.
#
# Kept COMPACT on purpose. This string travels into cv_summary_line(), which
# cv_system_prompt() re-injects for every loaded object on every single turn. A
# sentence per set would be a permanent per-turn tax; "[1.6x next]" is eleven
# characters and carries the same decision.

# WHERE THIS NUMBER COMES FROM, and it is not arbitrary. The first version used
# 0.8 (rank-2 within 20%) and was WRONG, caught by running it against this
# codebase's own worked example: pbmc3k C1 scores NK Cell 4578.7 against CD8+
# Alpha-Beta T Cell 2990.0, a ratio of 0.653. Round LXXI established that C1 is
# a genuinely MIXED compartment whose sub-clustering separates the two -- it is
# the single clearest "these two are both real" case in the project -- and 0.8
# said nothing about it.
#
# 0.5 is `inherit_score_ratio`'s default, chosen by the user in Round LXXII
# after a measured sweep, for the same underlying judgement: how close does a
# runner-up have to be before it is still in the running? Using one number for
# both means the label names an alternative exactly when the annotator would
# have inherited from it, and one threshold has to be defended instead of two.
#
# Checked against the clean cases so it does not cry wolf: a clear markerDB
# winner sits at 0.162 and a confident LLM call at 0.337 -- both silent.
CV_TYPO_NEAR_TIE <- 0.5

#' The margin between rank-1 and rank-2 for one set, as a compact suffix.
#'
#' @param df one set's candidate data.frame.
#' @return "" when there is nothing to say, else " [...]".
#' @keywords internal
.cv_typoclust_margin_text <- function(df) {
  tryCatch({
    if (is.null(df) || !is.data.frame(df) || nrow(df) < 1L) return("")
    # Which quantity does this flavour carry? markerDB -> Combined_Score,
    # agent LLM -> Confidence. Standalone ceLLMarkup() carries BOTH, on a third
    # scale again (Confidence x n_markers), so Confidence is preferred there:
    # it is the only one of the three that means the same thing everywhere.
    col <- if ("Confidence" %in% names(df)) "Confidence" else
           if ("Combined_Score" %in% names(df)) "Combined_Score" else NA_character_
    if (is.na(col)) return("")
    v <- suppressWarnings(as.numeric(df[[col]]))
    ct <- as.character(df$CellType %||% rep(NA_character_, length(v)))
    ord <- if ("Rank" %in% names(df)) order(suppressWarnings(as.integer(df$Rank)),
                                            na.last = TRUE) else order(-v, na.last = TRUE)
    v <- v[ord]; ct <- ct[ord]
    keep <- !is.na(v)
    v <- v[keep]; ct <- ct[keep]
    if (!length(v)) return("")
    s1 <- v[1]
    conf <- identical(col, "Confidence")
    if (length(v) < 2L)
      return(if (conf) sprintf(" [conf %.2f, only candidate]", s1)
             else " [only candidate]")
    s2 <- v[2]
    # A non-positive or non-finite rank-1 cannot carry a ratio. Say the number
    # and stop rather than printing Inf or a negative multiple.
    if (!is.finite(s1) || !is.finite(s2) || s1 <= 0)
      return(if (conf) sprintf(" [conf %.2f]", s1) else "")
    ratio <- s2 / s1
    near <- is.finite(ratio) && ratio >= CV_TYPO_NEAR_TIE
    runner <- if (near && !is.na(ct[2]) && nzchar(ct[2])) sprintf(": %s", ct[2]) else ""
    mult <- if (s2 > 0) s1 / s2 else Inf
    if (conf) {
      if (near) sprintf(" [conf %.2f, close 2nd %.2f%s]", s1, s2, runner)
      else      sprintf(" [conf %.2f]", s1)
    } else {
      if (near) sprintf(" [%.2fx next%s]", mult, runner)
      else if (is.finite(mult)) sprintf(" [%.1fx next]", mult)
      else " [clear]"
    }
  }, error = function(e) "")
}

#' Format the rank-1 labels as a compact one-line string for the result text,
#' e.g. "C1: B cells (Blood/Healthy); C2: CD4+ T cell; C5: Platelet".
#' @keywords internal
cv_typoclust_labels_text <- function(x, max_sets = 25L) {
  lbls <- cv_typoclust_top_labels(x)
  if (!length(lbls)) return("")
  sets <- names(lbls)
  shown <- utils::head(seq_along(lbls), max_sets)
  body <- paste(vapply(shown, function(i) paste0(sets[i], ": ", lbls[[i]]), character(1)),
                collapse = "; ")
  if (length(lbls) > max_sets)
    body <- paste0(body, sprintf("; ... (+%d more)", length(lbls) - max_sets))
  body
}

# Round LV (Batch 5a): cv_describe_generic_list() removed - zero call sites.
# cv_describe_object()'s switch above has no branch for a plain list, so such an
# object falls through to cv_describe_fallback() (which reports `length`). Note
# for whoever wonders whether this was a loss rather than dead weight: it would
# have reported `slots = names(x)`, which IS more useful for a named list. Adding
# a "list" branch to that switch is a behaviour change (it alters what the model
# is told about an object), so it belongs in a batch that allows one, not here.

#' Describe a plain data.frame.
#'
#' Column NAMES only, never a row of values: a data.frame in the store is
#' typically a marker or annotation table, and its rows are exactly the data the
#' descriptor exists to keep out of the prompt. `columns` is uncapped because a
#' table wide enough to matter does not occur here, and the model needs the full
#' set to name a column in a follow-up call.
#' @param x a data.frame.
#' @return a named list of type-specific descriptor fields.
#' @keywords internal
cv_describe_df <- function(x) {
  list(nrow = nrow(x), ncol = ncol(x), columns = colnames(x))
}

#' Describe a CellSet (a reusable set of cell barcodes from a cluster/subset)
#' @keywords internal
cv_describe_cellset <- function(x) {
  list(
    n_cells       = length(x$cells %||% character(0)),
    cluster       = x$cluster %||% NA_character_,
    level         = x$level %||% NA_character_,
    source_handle = x$source_handle %||% NA_character_,
    sampled       = isTRUE(x$sampled),
    seed          = x$seed %||% NA_integer_,
    n_total       = x$n_total %||% NA_integer_,
    preview       = utils::head(x$cells %||% character(0), 5)
  )
}

#' Describe an object of a type the store does not know.
#'
#' The end of `cv_describe_object()`'s switch. `length()` is the only thing safe
#' to ask of an arbitrary R object without risking a throw (contract rule 2), and
#' an unknown object still gets a handle and a summary so the user can see that
#' their upload arrived and can be downloaded again.
#' @param x any object.
#' @return a named list with a single `length` field.
#' @keywords internal
cv_describe_fallback <- function(x) {
  list(length = length(x))
}

#' One "N cells set aside" clause for cv_summary_line(), or nothing.
#'
#' Round LXX (audit #16). Silent when the flag is FALSE, because "0 isolated
#' cells" on every ordinary clustering is noise that trains the reader to skip
#' the whole line. When the flag is TRUE but the count is NA the wording drops
#' to "some": contract rule 6 says unknown and none must not be collapsed, and
#' inventing a number here would be exactly that collapse in the other
#' direction.
#' @keywords internal
.cv_set_aside_text <- function(flag, n, label) {
  if (!isTRUE(flag)) return(character(0))
  n <- suppressWarnings(as.integer(n %||% NA))
  if (is.na(n) || n <= 0L) return(sprintf("some %s cells", label))
  sprintf("%d %s cell%s", n, label, if (n == 1L) "" else "s")
}

#' Build the one-line human summary shown to the LLM
#' @keywords internal
cv_summary_line <- function(d) {
  t <- d$type
  if (t %in% c("Seurat", "SingleCellExperiment", "SpatialExperiment")) {
    return(sprintf("%s: %s cells x %s features%s", t,
                   d$n_cells %||% "?", d$n_features %||% "?",
                   if (length(d$metadata_cols)) sprintf("; %d metadata cols", length(d$metadata_cols)) else ""))
  }
  if (t %in% c("dgCMatrix", "matrix")) {
    return(sprintf("%s: %s features x %s cells", t, d$n_features %||% "?", d$n_cells %||% "?"))
  }
  if (t == "ClustoCell") {
    # Round XXV: list the real sub-cluster ids so the model passes valid ids to
    # annotateCellsLLM desired_sets instead of inventing names.
    # Round LVI: every id the descriptor holds is shown, and any shortfall is
    # measured against n_major_clusters / n_sub_clusters -- the true totals --
    # so a truncation can never again be silent.
    lbls     <- .cv_id_list_text(d$major_labels, total = d$n_major_clusters)
    sub_lbls <- .cv_id_list_text(d$sub_labels,   total = d$n_sub_clusters)
    sub_txt <- if (is.na(d$n_sub_clusters %||% NA)) "sub-clusters n/a"
               else if ((d$n_sub_clusters %||% 0L) == 0L) "no sub-clusters"
               else sprintf("%s sub-clusters%s", d$n_sub_clusters, sub_lbls)
    # Round LXX (audit #16): the set-aside cells, at last. clustoCell holds out
    # two kinds -- quiescent (no positive-marker expression at all) and isolated
    # (unconnected, or in a major cluster below isolated_cluster_thresh, 5 by
    # default) -- and neither has ever appeared in the summary the model reads.
    # Appended rather than woven in, so every existing prefix match still holds.
    set_aside <- c(
      .cv_set_aside_text(d$has_isolated,  d$n_isolated,  "isolated"),
      .cv_set_aside_text(d$has_quiescent, d$n_quiescent, "quiescent"))
    return(sprintf("ClustoCell: %s major clusters%s, %s%s%s%s",
                   d$n_major_clusters %||% "?", lbls, sub_txt,
                   if (isTRUE(d$has_markers)) "; markers present" else "",
                   if (isTRUE(d$is_sketched)) "; sketched" else "",
                   if (length(set_aside)) paste0("; ", paste(set_aside, collapse = "; ")) else ""))
  }
  if (t == "TypoClust") {
    # Build the "set: rank-1 label" summary directly from top_labels (named vec).
    lbl_txt <- if (length(d$top_labels)) {
      paste(vapply(names(d$top_labels),
                   function(s) paste0(s, ": ", d$top_labels[[s]]), character(1)),
            collapse = "; ")
    } else ""
    base <- sprintf("TypoClust: %d annotated set(s)%s", d$n_sets %||% 0L,
                    .cv_id_list_text(d$annotated_sets, total = d$n_sets))
    # Round LXIV: name the species (and the method) that produced these labels,
    # so the user can VERIFY the picker's answer was used rather than take it on
    # trust. Both are omitted when unknown -- an older object must not gain a
    # confident-looking "human" it never ran with.
    ctx <- c(if (!is.null(d$species)) paste0("species=", d$species),
             if (!is.null(d$ann_method)) paste0("method=", d$ann_method))
    if (length(ctx)) base <- paste0(base, " (", paste(ctx, collapse = ", "), ")")
    return(if (nzchar(lbl_txt)) paste0(base, ". Top cell types: ", lbl_txt) else base)
  }
  if (t == "DatasetMarkers") {
    return(sprintf("DatasetMarkers: %s combined markers", d$n_combined_markers %||% "?"))
  }
  if (t == "MarkoCell") {
    nm <- .cv_id_list_text(d$subset_names, total = d$n_cell_subsets)
    return(sprintf("MarkoCell: markers for %s cell subset(s)%s", d$n_cell_subsets %||% "?", nm))
  }
  if (t == "MarkerPurity") {
    gl <- .cv_id_list_text(d$group_ids, total = d$n_groups)
    # Round LXXVI: the numbers, not only the shape. Without this the summary
    # described the result and omitted the result.
    return(sprintf("MarkerPurity: purity assessed %s across %s group(s)%s%s",
                   d$grouping %||% "", d$n_groups %||% "?", gl,
                   .cv_markerpurity_findings_text(d$findings)))
  }
  if (t == "MarkoClust") {
    return(sprintf("%s object", t))
  }
  if (t == "data.frame") {
    return(sprintf("data.frame: %d rows x %d cols", d$nrow %||% 0L, d$ncol %||% 0L))
  }
  if (t == "CellSet") {
    samp <- if (isTRUE(d$sampled))
              sprintf(", sampled from %s (seed %s)", d$n_total %||% "?", d$seed %||% "?")
            else ""
    return(sprintf("CellSet: %s cell(s) from cluster %s%s",
                   d$n_cells %||% 0L, d$cluster %||% "?", samp))
  }
  sprintf("%s object", t)
}
