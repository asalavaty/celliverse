# =============================================================================
# CelliVerse Agent — session management
#
# A session bundles everything for one user conversation:
#   - object_store : server-side R objects keyed by handle (see object_store.R)
#   - history      : conversation messages (role/content/tool events)
#   - jobs         : async job registry (see worker.R)
#   - config       : effective config for this session (provider/model/temp)
#   - artifacts_dir: where plots/tables for this session are written
#
# Persistence model (as approved): sessions live in memory for speed; history +
# object *metadata* (descriptors, NOT the giant objects) + artifacts are
# snapshotted to ~/.celliverse/sessions/<id>/ so a restart can restore context.
# Large objects are re-loaded on demand (re-run), never serialized wholesale.
# =============================================================================

# Global in-memory registry of live sessions (package-internal environment).
.cv_sessions <- new.env(parent = emptyenv())

# Round LII (Batch 4a): the write-coalescing registry.
#
# cv_session_add_message() used to call cv_session_snapshot() on every single
# message. That rewrites the WHOLE conversation as JSON each time, so a session
# pays O(n^2) across its life; measured on a byte-compiled install it costs
# 2.72 ms at 10 messages, 15.70 ms at 200 and 28.95 ms at 400, ran 6x per turn
# (~95 ms/turn at 200 messages) and totalled ~4 s of pure serialization over a
# full-length 400-message session. It now marks the session dirty instead, and
# the turn's terminal points flush once (see cv_session_flush() below for the
# full list of flush sites and the durability window this trades).
#
# Keyed by session id; the VALUE is the numeric time at which the session FIRST
# became dirty -- not the most recent change -- so cv_session_dirty_age() is a
# true upper bound on how long any pending write has been sitting in memory.
#
# Deliberately a separate environment rather than a field on the session record:
# several call sites hold a session copy across an add_message
# (`sess <- cv_session_get(id)` ... `cv_session_set(sess)`), and a flag living
# inside `sess` would be silently clobbered when that stale copy is written back.
.cv_session_dirty <- new.env(parent = emptyenv())

#' Create a new session
#' @param session_id optional explicit id.
#' @param config effective config list (defaults to cv_load_config()).
#' @return the session id.
#' @noRd
cv_session_new <- function(session_id = NULL, config = cv_load_config()) {
  if (is.null(session_id)) session_id <- cv_new_id("sess")
  adir <- file.path(cv_sessions_dir(), session_id, "artifacts")
  dir.create(adir, recursive = TRUE, showWarnings = FALSE)

  sess <- list(
    id           = session_id,
    # Batch 3b item 2: the store needs to know which session owns it so
    # cv_object_evict_stale() (agent_object_store.R) can look up THIS
    # session's config/jobs at eviction time without threading session_id
    # through every one of cv_object_put()'s/cv_object_update()'s call sites.
    # A store created directly in a test (cv_object_store_new() with no
    # argument) has no session_id attached, so eviction is simply a no-op
    # for those -- existing tests are unaffected.
    object_store = cv_object_store_new(session_id),
    history      = list(),                # list of message lists
    jobs         = new.env(parent = emptyenv()),
    config       = config,
    artifacts_dir = adir,
    detached_descriptors = list(),
    created      = cv_now(),
    updated      = cv_now()
  )
  assign(session_id, sess, envir = .cv_sessions)
  # Batch 3b item 2: sweep old, idle sessions out of memory AFTER registering
  # the new one, so .cv_sessions never accumulates without bound over the life
  # of a long-running server -- see cv_sessions_evict_stale() below for what
  # "idle" means and why evicting from memory is safe.
  tryCatch(cv_sessions_evict_stale(exclude = session_id), error = function(e) NULL)
  cv_session_snapshot(session_id)
  session_id
}

#' Get a session object (errors if missing)
#' @noRd
cv_session_get <- function(session_id) {
  if (!exists(session_id, envir = .cv_sessions, inherits = FALSE)) {
    cli::cli_abort("Unknown session id {.val {session_id}}.")
  }
  get(session_id, envir = .cv_sessions)
}

#' Does a session exist in memory?
#' @noRd
cv_session_exists <- function(session_id) {
  is.character(session_id) && length(session_id) == 1L &&
    exists(session_id, envir = .cv_sessions, inherits = FALSE)
}

#' Persist a mutated session back into the registry
#' @noRd
cv_session_set <- function(sess) {
  assign(sess$id, sess, envir = .cv_sessions)
  invisible(sess)
}

#' Push hot-swappable config fields onto every in-memory session immediately.
#'
#' Called by cv_api_update_settings so a live Settings change (any provider)
#' reaches sessions that are already open, without waiting for their next turn.
#' Only LLM-routing / operational fields are overlaid (see
#' .cv_hotswap_config_fields); object store, history and artifacts are left
#' untouched. Sessions only persisted on disk are refreshed lazily on restore.
#' @param cfg the freshly-saved effective config.
#' @noRd
cv_sessions_apply_config <- function(cfg) {
  ids <- ls(envir = .cv_sessions)
  for (id in ids) {
    sess <- tryCatch(get(id, envir = .cv_sessions), error = function(e) NULL)
    if (is.null(sess) || is.null(sess$config)) next
    for (nm in .cv_hotswap_config_fields()) {
      if (!is.null(cfg[[nm]])) sess$config[[nm]] <- cfg[[nm]]
    }
    assign(id, sess, envir = .cv_sessions)
  }
  invisible(length(ids))
}

#' Append a message to a session's conversation history
#' @param session_id session id.
#' @param role one of "user", "assistant", "system", "tool".
#' @param content character message content.
#' @param ... extra fields to attach (e.g. tool_call_id, name, tool_calls).
#' @noRd
cv_session_add_message <- function(session_id, role, content = NULL, ...) {
  sess <- cv_session_get(session_id)
  msg <- c(list(role = role, content = content, ts = cv_now()), list(...))
  sess$history <- c(sess$history, list(msg))
  sess$updated <- cv_now()
  cv_session_set(sess)
  # Batch 3b item 2: cap sess$history itself (distinct from cv_budget_history()
  # in agent_loop.R, which only ever builds a trimmed COPY for one LLM call
  # and never touches the stored history) -- see cv_history_evict_stale()
  # below. Evict before snapshotting so a long session's on-disk copy stays
  # bounded too, not just the in-memory one.
  tryCatch(cv_history_evict_stale(session_id), error = function(e) NULL)
  # Round LII (Batch 4a): mark, do not write. The old line here was
  #   cv_session_snapshot(session_id)  # cheap: history + metadata only
  # and that comment was measurably wrong -- see .cv_session_dirty above for the
  # numbers. The write is now deferred to the end of the turn.
  cv_session_mark_dirty(session_id)
  invisible(msg)
}

#' Trim `sess$history` itself once it grows past a hard cap.
#'
#' `cv_budget_history()` (agent_loop.R) only ever builds a trimmed COPY of the
#' history for a single LLM call -- it never mutates or replaces the stored
#' `sess$history`, so a very long-lived session (many turns over hours/days,
#' never restarted) accumulates every message it has ever sent or received,
#' without bound, in memory AND in the on-disk snapshot. This is a genuinely
#' different problem from the token-budget trim: a message can be small in
#' tokens but the LIST holding thousands of them still grows forever.
#'
#' Oldest whole tool-call/tool-result GROUPS (see `cv_group_history_atomic()`)
#' are dropped first, exactly like `cv_budget_history()`'s own grouping, so
#' this can never orphan a tool-result message the way trimming individual
#' messages could. Unlike the job/object stores, nothing here is ever
#' "protected" from eviction -- a message, once evicted, only ever affects
#' what the model can see of its OWN past conversation, which is already a
#' lossy/summarized view by design (that's what the token budget already
#' does turn to turn); it does not orphan a live resource the way evicting an
#' in-flight job or an object a running tool call still needs would.
#' @param session_id session id.
#' @param keep max messages to retain; defaults to the session's
#'   `history_message_limit` config (see cv_default_config()).
#' @return invisibly, the number of messages evicted.
#' @noRd
cv_history_evict_stale <- function(session_id, keep = NULL) {
  sess <- cv_session_get(session_id)
  hist <- sess$history %||% list()
  if (is.null(keep)) keep <- as.integer(sess$config$history_message_limit %||% 400L)
  if (length(hist) <= keep) return(invisible(0L))

  groups <- cv_group_history_atomic(hist)
  # Walk groups from NEWEST to OLDEST, keeping whole groups until adding the
  # next (older) one would exceed `keep` -- mirrors cv_budget_history()'s own
  # reverse walk, just counting messages instead of estimated tokens.
  kept_rev <- list()
  n_kept <- 0L
  for (gi in rev(seq_along(groups))) {
    grp <- groups[[gi]]
    if (n_kept + length(grp) > keep && n_kept > 0L) break
    for (m in rev(grp)) kept_rev[[length(kept_rev) + 1L]] <- m
    n_kept <- n_kept + length(grp)
  }
  new_hist <- rev(kept_rev)
  n_evicted <- length(hist) - length(new_hist)
  if (n_evicted <= 0L) return(invisible(0L))

  sess$history <- new_hist
  cv_session_set(sess)
  invisible(n_evicted)
}

#' Return the conversation history (list of messages)
#' @noRd
cv_session_history <- function(session_id) {
  cv_session_get(session_id)$history
}

#' Convenience: the object store for a session
#' @noRd
cv_session_store <- function(session_id) {
  cv_session_get(session_id)$object_store
}

# ---- Dirty tracking / deferred writes (Round LII, Batch 4a) -----------------

#' Mark a session as carrying changes that are not yet on disk.
#'
#' Records the time of the FIRST unflushed change, not the latest, so the age
#' `cv_session_flush_stale()` checks is a true bound on how long any pending
#' write has been waiting rather than a timer that a steady stream of messages
#' could keep resetting forever.
#' @return invisibly TRUE.
#' @noRd
cv_session_mark_dirty <- function(session_id) {
  if (!exists(session_id, envir = .cv_session_dirty, inherits = FALSE)) {
    # Numeric Sys.time(), NOT cv_now(). cv_now() returns a FORMATTED string, and
    # as.POSIXct() on it parses only the date part -- which is precisely how the
    # Round XLVI heavy-tool timeout clock came to measure "seconds since
    # midnight" and cancel every heavy job for most of the day. Storing epoch
    # seconds directly leaves nothing to parse and no way to repeat that bug.
    assign(session_id, as.numeric(Sys.time()), envir = .cv_session_dirty)
  }
  invisible(TRUE)
}

#' Does a session have changes that are not yet on disk?
#' @noRd
cv_session_is_dirty <- function(session_id) {
  is.character(session_id) && length(session_id) == 1L &&
    exists(session_id, envir = .cv_session_dirty, inherits = FALSE)
}

#' Seconds since a session first became dirty; 0 if it has nothing pending.
#' @noRd
cv_session_dirty_age <- function(session_id) {
  if (!cv_session_is_dirty(session_id)) return(0)
  as.numeric(Sys.time()) - get(session_id, envir = .cv_session_dirty)
}

#' Forget a session's pending-write marker.
#' @noRd
cv_session_clear_dirty <- function(session_id) {
  if (is.character(session_id) && length(session_id) == 1L &&
      exists(session_id, envir = .cv_session_dirty, inherits = FALSE)) {
    rm(list = session_id, envir = .cv_session_dirty)
  }
  invisible(TRUE)
}

#' Write a session to disk if (and only if) it has pending changes.
#'
#' This is the counterpart to `cv_session_mark_dirty()` and the single place the
#' deferred write actually happens. It is called from every terminal point of a
#' turn, so the on-disk snapshot is complete whenever a turn is not running:
#'
#'   - `settle()` and `fail()` in `cv_start_turn()` (agent_turns.R) — the async
#'     server path's success and failure/cancel endings;
#'   - `cv_turn_cancel()` (agent_turns.R) — a cancel arriving from the API
#'     between steps, which reaches neither of the above;
#'   - `cv_agent_turn()`'s `on.exit` when `stepwise = FALSE` (agent_loop.R) —
#'     the synchronous path (`/api/chat/sync`, SSE, tests), covering every one
#'     of its many return points including the error ones;
#'   - `cv_sessions_evict_stale()` below, before a session leaves memory;
#'   - `cv_session_flush_stale()` from `cv_api_chat_poll()`, which bounds how
#'     long a LONG turn (a multi-minute heavy tool) may hold unwritten messages.
#'
#' The durability window this trades: a hard process kill in the middle of a
#' turn loses that turn's messages from disk. Every other loss path — a clean
#' shutdown, an eviction, a failed turn, a cancelled turn — flushes. A turn
#' interrupted by a process kill is lost as a turn anyway, which is why this is
#' the right side of the trade for a ~6x reduction in session I/O.
#' @param force write even if nothing is marked dirty.
#' @return invisibly TRUE if a snapshot was written.
#' @noRd
cv_session_flush <- function(session_id, force = FALSE) {
  if (!force && !cv_session_is_dirty(session_id)) return(invisible(FALSE))
  if (!cv_session_exists(session_id)) {
    # Nothing in memory left to write (already evicted, or never existed).
    # Drop the marker so it cannot leak into .cv_session_dirty forever.
    cv_session_clear_dirty(session_id)
    return(invisible(FALSE))
  }
  # cv_session_snapshot() clears the dirty marker itself on a successful write,
  # so a snapshot taken by any OTHER call site (object upload, restore, session
  # creation) also satisfies a pending flush -- there is no way to write the
  # file and still believe a write is outstanding.
  cv_session_snapshot(session_id)
  invisible(TRUE)
}

#' Flush a session only once its pending write has waited longer than `max_age`.
#'
#' Called from `cv_api_chat_poll()`. A normal turn finishes well inside
#' `max_age` and therefore still writes exactly once, at its terminal point; a
#' turn running a multi-minute heavy tool would otherwise hold every message it
#' has accumulated in memory for the whole run, so this bounds that exposure
#' without reintroducing a write per poll.
#' @param max_age seconds a pending write may wait before a poll forces it.
#' @return invisibly TRUE if a snapshot was written.
#' @noRd
cv_session_flush_stale <- function(session_id, max_age = 5) {
  if (!cv_session_is_dirty(session_id)) return(invisible(FALSE))
  if (cv_session_dirty_age(session_id) < max_age) return(invisible(FALSE))
  cv_session_flush(session_id)
}

# ---- Snapshot / restore -----------------------------------------------------

#' Snapshot a session to disk (history + object descriptors + config)
#'
#' Deliberately does NOT serialize the large objects themselves — only their
#' descriptors, so a restored session knows what *was* loaded and can prompt the
#' user to reload/recompute. Artifacts already live on disk.
#' @noRd
cv_session_snapshot <- function(session_id) {
  sess <- cv_session_get(session_id)
  sdir <- file.path(cv_sessions_dir(), session_id)
  dir.create(sdir, recursive = TRUE, showWarnings = FALSE)
  snap <- list(
    id          = sess$id,
    created     = sess$created,
    updated     = cv_now(),
    config      = sess$config,
    history     = sess$history,
    descriptors = cv_object_descriptors(sess$object_store)
  )
  final_path <- file.path(sdir, "session.json")
  # BATCH1 FIX (rebuilt from scratch): write to a temp file in the same
  # directory and rename into place. write_json() previously wrote straight
  # to session.json; a crash or kill mid-write (this happens on EVERY
  # message, synchronously) could leave a truncated/corrupt file behind.
  # file.rename() within the same filesystem is atomic, so readers only ever
  # see a complete old or complete new file. Purely disk I/O on session
  # metadata (history + object descriptors, never the loaded object data
  # itself, see the module comment above) -- no interaction with the LLM
  # call path.
  tmp_path <- file.path(sdir, sprintf(".session.json.%s.tmp", cv_new_id("w")))
  jsonlite::write_json(
    snap, tmp_path,
    auto_unbox = TRUE, pretty = TRUE, null = "null", force = TRUE
  )
  ok <- file.rename(tmp_path, final_path)
  if (!ok) {
    # Cross-filesystem or locked-file fallback: copy then clean up the temp.
    file.copy(tmp_path, final_path, overwrite = TRUE)
    unlink(tmp_path)
  }
  # Round LII: the file is now on disk, so whatever was pending is pending no
  # longer. Cleared HERE rather than in cv_session_flush() so that a snapshot
  # taken by any other call site (upload, restore, session creation, eviction)
  # also satisfies a pending flush, and so a snapshot that THREW above leaves
  # the marker set and gets retried at the next flush point instead of silently
  # losing the write.
  cv_session_clear_dirty(session_id)
  invisible(final_path)
}

#' Restore a session's history + descriptors from disk into memory
#'
#' The object store comes back empty (objects are not persisted); descriptors
#' are attached as "detached" records so the UI can show what was loaded and the
#' agent can tell the user to reload. Returns the session id.
#' @noRd
cv_session_restore <- function(session_id) {
  path <- file.path(cv_sessions_dir(), session_id, "session.json")
  if (!file.exists(path)) {
    cli::cli_abort("No snapshot found for session {.val {session_id}}.")
  }
  # BATCH1 FIX (rebuilt from scratch): a corrupted/truncated session.json
  # (e.g. from a crash before the atomic-write fix above existed, or written
  # by an older version of this package) previously threw a raw, unhandled
  # jsonlite parse error here instead of a clean, actionable message.
  snap <- tryCatch(jsonlite::read_json(path, simplifyVector = FALSE), error = function(e) {
    # conditionMessage(e) is dynamic, data-controlled text (it can echo back
    # fragments of the malformed JSON itself) and cli treats every string
    # passed to cli_abort() as a glue template. Any literal unbalanced "{" or
    # "}" in that text crashes glue with "Expecting '}'" instead of surfacing
    # the intended clean error. Escape literal braces by doubling them before
    # interpolation.
    safe_msg <- gsub("}", "}}", gsub("{", "{{", conditionMessage(e), fixed = TRUE), fixed = TRUE)
    cli::cli_abort(c(
      "Session snapshot for {.val {session_id}} is corrupted and could not be read.",
      "x" = safe_msg
    ))
  })
  cfg <- tryCatch(snap$config, error = function(e) NULL)
  if (is.null(cfg)) cfg <- cv_load_config()
  cv_session_new(session_id = session_id, config = cfg)
  sess <- cv_session_get(session_id)
  sess$history <- snap$history %||% list()
  sess$updated <- snap$updated %||% sess$updated
  # Attach detached descriptors so the client can render "previously loaded".
  sess$detached_descriptors <- snap$descriptors %||% list()
  cv_session_set(sess)
  # BATCH1 FIX (data loss, rebuilt from scratch): cv_session_new() above
  # snapshots an EMPTY history to disk as part of its own initialization,
  # before we repopulate history in memory just now. Without re-snapshotting
  # here, session.json on disk stays empty until the next new message -- if
  # the process is killed again before that happens, the restored history is
  # permanently lost on the NEXT restore attempt even though it was
  # successfully recovered just now.
  cv_session_snapshot(session_id)
  session_id
}

#' Build a lightweight summary record for one session id.
#'
#' Reads metadata from the in-memory session when live, otherwise from the
#' on-disk snapshot (session.json). Always returns a list with the same shape
#' so the client can render every row; unreadable sessions degrade gracefully
#' (id preserved, counts NA) instead of breaking the list.
#' @noRd
cv_session_summary <- function(session_id) {
  base <- list(id = session_id, created = NA_character_,
               updated = NA_character_, n_messages = NA_integer_)
  # Prefer the live in-memory session (freshest state).
  if (cv_session_exists(session_id)) {
    sess <- tryCatch(cv_session_get(session_id), error = function(e) NULL)
    if (!is.null(sess)) {
      base$created    <- sess$created %||% NA_character_
      base$updated    <- sess$updated %||% sess$created %||% NA_character_
      base$n_messages <- length(sess$history %||% list())
      return(base)
    }
  }
  # Otherwise read the on-disk snapshot.
  path <- file.path(cv_sessions_dir(), session_id, "session.json")
  if (file.exists(path)) {
    snap <- tryCatch(jsonlite::read_json(path, simplifyVector = FALSE),
                     error = function(e) NULL)
    if (!is.null(snap)) {
      base$created    <- snap$created %||% NA_character_
      base$updated    <- snap$updated %||% snap$created %||% NA_character_
      base$n_messages <- length(snap$history %||% list())
    }
  }
  base
}

#' List known sessions as summary records (in memory + on disk).
#'
#' Returns an unnamed list of objects `{id, created, updated, n_messages}`,
#' most-recently-updated first. Previously this returned a bare character
#' vector, which the History UI mis-read as objects (blank rows, `undefined`
#' ids). Returning structured records fixes that at the source.
#' @noRd
cv_session_list <- function() {
  mem  <- ls(envir = .cv_sessions)
  disk <- tryCatch(list.dirs(cv_sessions_dir(), recursive = FALSE, full.names = FALSE),
                   error = function(e) character(0))
  # Round LXVIII: a directory alone is not a session. cv_session_delete() keeps
  # `artifacts/` by default, so a deleted session leaves its directory behind
  # holding only that — and this function used to list it, producing a History
  # row with no timestamp and no message count for a conversation the user had
  # just deleted. A session counts as listable when it is live in memory, or
  # when its snapshot is on disk.
  disk <- disk[file.exists(file.path(cv_sessions_dir(), disk, "session.json"))]
  ids  <- unique(c(mem, disk))
  if (!length(ids)) return(list())
  out <- lapply(ids, cv_session_summary)
  # Sort most-recent-first by `updated` (fall back to created); NAs last.
  keys <- vapply(out, function(s) s$updated %||% s$created %||% "", character(1))
  out  <- out[order(keys, decreasing = TRUE)]
  unname(out)
}

# ---- Deletion ---------------------------------------------------------------

#' Is this string safe to use as a session directory name?
#'
#' Round LXVIII (audit #67). `cv_session_delete()` takes an id straight off a
#' URL path and turns it into a directory it then removes RECURSIVELY, so the
#' id is the whole of the trust boundary. `../..` would escape
#' `cv_sessions_dir()` and delete something else entirely — the same defect
#' class as D7's zip-slip, which reached `exdir` the same way.
#'
#' Two independent checks, because either alone has a hole:
#'   * a character-class allowlist, which rejects `/`, `\` and `..` outright.
#'     Every id this package mints is `cv_new_id()`'s `prefix_HHMMSS` + six
#'     alphanumerics, so nothing legitimate is excluded;
#'   * a resolved-path containment check, which is what actually protects the
#'     filesystem if the allowlist is ever widened. A symlinked session
#'     directory would pass the first test and fail this one.
#' @return TRUE only if `session_id` is a plain, containable directory name.
#' @noRd
cv_session_id_is_safe <- function(session_id) {
  if (!is.character(session_id) || length(session_id) != 1L || is.na(session_id)) return(FALSE)
  if (!nzchar(session_id) || nchar(session_id) > 128L) return(FALSE)
  if (!grepl("^[A-Za-z0-9_-]+$", session_id)) return(FALSE)
  root <- normalizePath(cv_sessions_dir(), winslash = "/", mustWork = FALSE)
  # mustWork = FALSE: the directory may legitimately not exist (an in-memory
  # session that has never been snapshotted). We are testing the SHAPE of the
  # resolved path, not its existence.
  target <- normalizePath(file.path(cv_sessions_dir(), session_id),
                          winslash = "/", mustWork = FALSE)
  # Trailing separator on the prefix so ".../sessions-evil" cannot match
  # ".../sessions".
  startsWith(target, paste0(sub("/+$", "", root), "/"))
}

#' Does this session have a job that is still queued or running?
#'
#' Shared by `cv_session_delete()` and worth stating: a job's live process
#' handle and eventual result exist only in memory, so deleting the session
#' under it orphans the child process with nowhere to report back to. This is
#' the same protection `cv_sessions_evict_stale()` already applies for the same
#' reason; factored out here rather than copied, because a second copy of a
#' safety check is how the first one drifts.
#' @noRd
cv_session_has_live_job <- function(session_id) {
  if (!cv_session_exists(session_id)) return(FALSE)
  sess <- tryCatch(cv_session_get(session_id), error = function(e) NULL)
  if (is.null(sess) || is.null(sess$jobs)) return(FALSE)
  jids <- ls(envir = sess$jobs)
  if (!length(jids)) return(FALSE)
  any(vapply(jids, function(j) {
    isTRUE(get(j, envir = sess$jobs)$status %in% c("queued", "running"))
  }, logical(1)))
}

#' Delete a session: its in-memory record and its directory on disk.
#'
#' Round LXVIII (audit #67): until now there was no way to erase a transcript
#' at all — a conversation containing embargoed marker genes stayed in
#' `~/.celliverse/sessions/` for good.
#'
#' The session's `artifacts/` directory is KEPT by default and removed only on
#' an explicit `include_artifacts = TRUE`. The transcript and the files it
#' produced are different things to want gone, and the control that calls this
#' deletes conversations, not results — a figure already on disk must not
#' disappear because the chat around it was cleared.
#'
#' Keeping the artifacts means the session DIRECTORY survives, holding nothing
#' but `artifacts/`. That is why `cv_session_list()` now requires a
#' `session.json` (or a live in-memory record) before it lists an id: without
#' that, a deleted session would keep appearing in History as a row with no
#' timestamp and no message count — deleted from disk, still on screen.
#'
#' Refuses rather than throws for the two cases a caller must tell apart
#' (`unsafe_id`, `live_job`), so the API layer can map each to its own message
#' and status. A missing directory is NOT a refusal: deleting a session that
#' exists only in memory is a legitimate success.
#' @param include_artifacts also remove the session's `artifacts/` directory.
#' @return a list `(ok, reason, removed_dir)`; `reason` is `""` when `ok`.
#' @noRd
cv_session_delete <- function(session_id, include_artifacts = FALSE) {
  if (!cv_session_id_is_safe(session_id))
    return(list(ok = FALSE, reason = "unsafe_id", removed_dir = FALSE))
  if (cv_session_has_live_job(session_id))
    return(list(ok = FALSE, reason = "live_job", removed_dir = FALSE))

  sdir <- file.path(cv_sessions_dir(), session_id)
  removed <- FALSE
  if (dir.exists(sdir)) {
    if (isTRUE(include_artifacts)) {
      unlink(sdir, recursive = TRUE, force = TRUE)
    } else {
      # Everything in the session directory except artifacts/. `all.files` picks
      # up the atomic-write temp files (".session.json.<id>.tmp") that a crash
      # mid-snapshot can leave behind; without it a delete would tidy the
      # transcript and leave a copy of it beside the hole.
      keep <- "artifacts"
      entries <- setdiff(
        list.files(sdir, all.files = TRUE, no.. = TRUE), keep)
      if (length(entries))
        unlink(file.path(sdir, entries), recursive = TRUE, force = TRUE)
    }
    removed <- !file.exists(file.path(sdir, "session.json"))
  }

  # The in-memory record and its dirty marker go regardless of what happened on
  # disk: the caller asked for this session to stop existing. Dropping the
  # dirty marker matters — a session flushed after this point would write its
  # snapshot straight back.
  if (exists(session_id, envir = .cv_sessions, inherits = FALSE))
    rm(list = session_id, envir = .cv_sessions)
  cv_session_clear_dirty(session_id)

  list(ok = TRUE, reason = "", removed_dir = removed)
}

#' Evict old, IDLE sessions from the in-memory registry once it grows past a
#' cap, so a long-running server does not accumulate every session it has
#' ever seen (each carrying its own object store, potentially holding large
#' Seurat/SCE objects, plus full conversation history) for the rest of the
#' process's life.
#'
#' Evicting a session from `.cv_sessions` is safe by design, not a new risk:
#' every evicted session is flushed to disk immediately before it is removed
#' (see the loop at the bottom of this function -- as of Round LII that flush
#' is load-bearing, not defensive, because `cv_session_add_message()` no longer
#' writes on every message), and `cv_api_get_session()` already knows how to
#' `cv_session_restore()` an unknown session id from that snapshot on demand
#' -- this is the exact same recovery path a full server restart already
#' relies on (see the module comment at the top of this file). Evicting an
#' idle session from memory just triggers that same, already-tested recovery
#' path a little earlier than a restart would, if the user ever comes back to
#' it; its loaded objects are NOT restored (by design, same as a restart),
#' but its history and "previously loaded" descriptors are.
#'
#' A session with any job still `queued`/`running` is never evicted regardless
#' of age, since (unlike history/descriptors) an in-flight job's live process
#' handle and eventual result are not persisted to disk at all -- evicting
#' that session would silently orphan the job. The session that is currently
#' being created (`exclude`) is likewise never a candidate, for a more subtle
#' reason: `cv_now()` has only whole-second resolution, so a burst of
#' sessions created within the same second (routine in an automated test
#' suite; possible in real use too) can tie exactly on `updated`, and R's
#' `order()` breaks ties by the tied elements' position in `ls()`'s
#' ALPHABETICAL id ordering -- unrelated to actual creation order. Without
#' this exclusion, the brand-new session `cv_session_new()` is about to
#' return could itself be the one evicted a few lines later, in the very call
#' that created it.
#' @param keep max sessions to retain in memory; defaults to the global
#'   `session_registry_limit` config (see cv_default_config()). There is no
#'   single "current session" to read this from (the cap applies across every
#'   session at once), so this reads the live effective config directly,
#'   same as other whole-server settings.
#' @param exclude character vector of session ids to never evict regardless
#'   of age (see above).
#' @return invisibly, the number of sessions evicted.
#' @noRd
cv_sessions_evict_stale <- function(keep = NULL, exclude = character(0)) {
  ids <- ls(envir = .cv_sessions)
  if (is.null(keep)) {
    keep <- as.integer(tryCatch(cv_load_config()$session_registry_limit,
                                error = function(e) NULL) %||% 30L)
  }
  if (length(ids) <= keep) return(invisible(0L))

  recs <- stats::setNames(lapply(ids, function(i) get(i, envir = .cv_sessions)), ids)
  has_live_job <- function(sess) {
    if (is.null(sess$jobs)) return(FALSE)
    jids <- ls(envir = sess$jobs)
    if (!length(jids)) return(FALSE)
    any(vapply(jids, function(j) {
      isTRUE(get(j, envir = sess$jobs)$status %in% c("queued", "running"))
    }, logical(1)))
  }
  protected <- vapply(recs, has_live_job, logical(1)) | (ids %in% exclude)
  evictable_ids <- ids[!protected]
  n_over <- length(ids) - keep
  if (n_over <= 0L || !length(evictable_ids)) return(invisible(0L))

  updated <- vapply(evictable_ids, function(i) recs[[i]]$updated %||% recs[[i]]$created %||% "",
                    character(1))
  # Oldest-first; never evict more than the number of non-protected sessions
  # available, so a server with mostly-active sessions is left alone even if
  # over `keep`.
  evict_ids <- evictable_ids[order(updated)][seq_len(min(n_over, length(evictable_ids)))]

  for (eid in evict_ids) {
    # Final snapshot before the session leaves memory. Round LII promoted this
    # from defensive to LOAD-BEARING: it used to be a near-no-op because
    # cv_session_add_message() snapshotted on every message, but writes are now
    # coalesced to the end of a turn, so this is the last chance to persist a
    # session whose turn ended without a subsequent flush point. `force = TRUE`
    # because a session that was only ever created (never messaged) is not
    # marked dirty yet still deserves a snapshot on its way out.
    #
    # This is also the only production path that removes a session from
    # .cv_sessions, so flushing here means no in-memory session can be dropped
    # with unwritten history.
    tryCatch(cv_session_flush(eid, force = TRUE), error = function(e) NULL)
    rm(list = eid, envir = .cv_sessions)
    # The session is gone from memory; drop any marker it left behind so
    # .cv_session_dirty cannot grow without bound on a long-running server.
    cv_session_clear_dirty(eid)
  }
  invisible(length(evict_ids))
}
