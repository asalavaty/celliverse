# =============================================================================
# CelliVerse Agent — asynchronous chat turns + per-turn event buffer
#
# WHY THIS EXISTS
# ---------------
# plumber/httpuv cannot flush a partial HTTP response within a single request
# (no in-handler res$write; no HTTP/2 server push). The original /api/chat route
# therefore only *appeared* to stream: it assembled every SSE frame into a
# buffer and returned the whole thing after the turn finished. The user saw the
# full answer appear at once, with no live "thinking"/tool/token feedback.
#
# THE FIX (async turn + client polling)
# --------------------------------------
# 1. POST /api/chat/start creates a *turn record* in the session, schedules the
#    agent turn to run on the event loop (later::later, delay 0) and returns a
#    turn_id immediately.
# 2. As the turn runs, its on_event callback APPENDS each event (iteration,
#    token, tool_start, tool_result, assistant, done, ...) to the turn's
#    in-memory event buffer.
# 3. The browser polls GET /api/chat/poll?session=&turn=&cursor= every ~350 ms.
#    Each poll returns the events after `cursor`, the new cursor, and whether the
#    turn is finished. The client renders events live -> real ChatGPT-style
#    feedback (immediate "thinking", live tool progress, token-by-token answer).
#
# WHY IN-PROCESS (not a callr child)
# ----------------------------------
# The agent turn mutates the session's in-memory object store (tools put
# ClustoCell/etc. objects there by handle). Running the turn in a child process
# would strand those objects in the child. So the turn runs in the *parent*.
# httpuv is single-threaded.
#
# CORRECTION (Round XLI, measured over real HTTP against a real plumber server
# for the first time): the previous text here claimed "the heavy-tool
# dispatcher already pumps the event loop (later::run_now) while a heavy tool
# runs, so poll requests are still serviced during long operations". That is
# FALSE, and it was load-bearing in how this design was justified.
# later::run_now() advances `later` CALLBACKS -- which is why a heavy job's
# progress events are generated on schedule -- but it does NOT service httpuv
# HTTP REQUESTS. Measured: a real umapPlot heavy dispatch left a single client
# poll blocked for 26,420 ms and then returned all 53 buffered events at once,
# including all 47 progress events emitted at 400ms intervals during the job.
# The client is blind for the ENTIRE turn, heavy tools included -- not just
# during short LLM HTTP calls.
#
# The fix is the items 4/7 redesign (step-machine turn + non-blocking heavy
# dispatch), tracked in CHANGES.md; this comment is corrected here so the next
# reader does not inherit the wrong mental model.
#
# The synchronous /api/chat/sync route is retained unchanged as a fallback for
# tests and non-polling clients.
# =============================================================================

# ---- Tuned scheduling constants (Round LV, Batch 5a) ------------------------
#
# These three delays are the load-bearing numbers of the Batch B step machine.
# Each was measured rather than chosen, and each has a paragraph at its use site
# explaining what breaks at other values -- but each was written as a bare
# literal, so a future edit could change one without ever meeting the reasoning.
# Naming them puts the "this is tuned, not arbitrary" signal at the value
# itself. The frontend already does this (REASSURE_AFTER_MS,
# SHOW_ELAPSED_AFTER_MS in ToolRun.tsx); the R side did not.
#
# Values are unchanged. This is a rename, not a retune.

# Between two steps of a running turn. Must be non-zero: a callback that is
# already runnable gets picked straight back up by the in-flight run_now() that
# is executing us, chaining step after step without returning to httpuv -- the
# exact freeze Batch B exists to remove. Measured: at 0.01s a poll intermittently
# caught two steps (~4,000 ms); at 0.05s it consistently catches one (~1,900 ms).
CV_TURN_STEP_DELAY_SEC <- 0.05

# While a heavy job runs. Human-scale on purpose: the job reports its own
# progress through on_event, so this tick exists only to notice it finished.
# Polling at the step cadence would burn the CPU the job is trying to use.
CV_TURN_WAITING_DELAY_SEC <- 0.25

# Before the turn starts at all. Guarantees the FIRST poll returns immediately
# with the buffered "thinking" event instead of being the request that runs the
# whole turn, so the user sees live feedback even for tool-less answers.
CV_TURN_START_DELAY_SEC <- 0.15

#' Ensure the session has a turn registry (an environment of turn records).
#' @noRd
cv_turns_env <- function(session_id) {
  sess <- cv_session_get(session_id)
  if (is.null(sess$turns) || !is.environment(sess$turns)) {
    sess$turns <- new.env(parent = emptyenv())
    cv_session_set(sess)
  }
  cv_session_get(session_id)$turns
}

#' Create a new turn record and return its id.
#' @noRd
cv_turn_new <- function(session_id, message) {
  turns <- cv_turns_env(session_id)
  turn_id <- cv_new_id("turn")
  rec <- list(
    id       = turn_id,
    session  = session_id,
    message  = message,
    status   = "queued",          # queued -> running -> done | error | cancelled
    events   = list(),            # ordered event list; index is the cursor
    error    = NULL,
    job_ids  = character(0),      # heavy jobs launched by this turn (for cancel)
    cancel   = FALSE,             # cooperative cancel flag
    created  = cv_now(),
    updated  = cv_now()
  )
  assign(turn_id, rec, envir = turns)
  turn_id
}

#' Fetch a turn record (or NULL).
#' @noRd
cv_turn_get <- function(session_id, turn_id) {
  turns <- cv_turns_env(session_id)
  if (!exists(turn_id, envir = turns, inherits = FALSE)) return(NULL)
  get(turn_id, envir = turns)
}

#' Update fields on a turn record.
#' @noRd
cv_turn_update <- function(session_id, turn_id, ...) {
  turns <- cv_turns_env(session_id)
  if (!exists(turn_id, envir = turns, inherits = FALSE)) return(invisible(NULL))
  rec <- get(turn_id, envir = turns)
  upd <- list(...)
  for (nm in names(upd)) rec[[nm]] <- upd[[nm]]
  rec$updated <- cv_now()
  assign(turn_id, rec, envir = turns)
  invisible(rec)
}

#' Append one event to a turn's buffer. Events are stored with a monotonically
#' increasing index == length(events) after append (the cursor value clients use).
#' @noRd
cv_turn_append_event <- function(session_id, turn_id, event) {
  turns <- cv_turns_env(session_id)
  if (!exists(turn_id, envir = turns, inherits = FALSE)) return(invisible(NULL))
  rec <- get(turn_id, envir = turns)
  # Round XXII: drop NULL fields so plumber's unboxedJSON (null="list") cannot
  # turn them into `{}` on the wire (which crashed React with error #31).
  event <- cv_json_sanitize(event)
  # Normalize: ensure a $type and a sequence index.
  event$type <- event$type %||% "message"
  event$seq  <- length(rec$events) + 1L
  rec$events[[length(rec$events) + 1L]] <- event
  rec$updated <- cv_now()
  assign(turn_id, rec, envir = turns)
  invisible(event$seq)
}

#' Public poll view: events strictly after `cursor`, the new cursor, status.
#' @noRd
cv_turn_poll <- function(session_id, turn_id, cursor = 0L) {
  rec <- cv_turn_get(session_id, turn_id)
  if (is.null(rec)) return(NULL)
  cursor <- suppressWarnings(as.integer(cursor)); if (is.na(cursor) || cursor < 0L) cursor <- 0L
  n <- length(rec$events)
  new_events <- if (cursor < n) rec$events[(cursor + 1L):n] else list()
  list(
    turn    = turn_id,
    status  = rec$status,
    done    = rec$status %in% c("done", "error", "cancelled"),
    cursor  = n,
    events  = unname(new_events),
    error   = rec$error
  )
}

#' Mark a turn cancelled and kill any heavy jobs it launched.
#' @noRd
cv_turn_cancel <- function(session_id, turn_id) {
  rec <- cv_turn_get(session_id, turn_id)
  if (is.null(rec)) return(FALSE)
  cv_turn_update(session_id, turn_id, cancel = TRUE)
  for (jid in rec$job_ids) tryCatch(cv_job_cancel(session_id, jid), error = function(e) NULL)
  if (!(rec$status %in% c("done", "error", "cancelled"))) {
    cv_turn_append_event(session_id, turn_id, list(type = "cancelled"))
    cv_turn_update(session_id, turn_id, status = "cancelled")
  }
  # Round LII (Batch 4a): a cancel arriving from the API lands here, between two
  # scheduled steps, and marks the turn cancelled directly. `advance()` sees the
  # terminal status on its next tick and returns WITHOUT calling settle() or
  # fail(), so neither of the usual flush points fires for a cancelled turn --
  # this is the one that does.
  #
  # Round LXXV (audit #33): and for exactly the same reason, neither of the
  # usual INDEX points fires either. Stopping a turn is the case where the user
  # most wants whatever already finished -- that is generally WHY they stopped
  # it -- and it was the case that showed them nothing.
  tryCatch(cv_session_flush(session_id), error = function(e) NULL)
  cv_index_artifacts_safe(session_id, when = "turn cancel")
  TRUE
}

#' Is there already a turn in flight (queued or running) for this session?
#' Returns the active turn id, or NULL. Used to enforce one-turn-per-session so
#' a second /api/chat/start (or a poll-pumped re-entrant schedule) cannot run a
#' concurrent turn on the same session — the root cause of the clustoCell loop.
#' @noRd
cv_session_active_turn <- function(session_id) {
  turns <- cv_turns_env(session_id)
  ids <- ls(envir = turns)
  for (id in ids) {
    rec <- get(id, envir = turns)
    if (isTRUE(rec$status %in% c("queued", "running"))) return(id)
  }
  NULL
}

#' Start an asynchronous chat turn. Returns the turn_id immediately; the turn
#' runs on the event loop via later::later and streams events into the buffer.
#'
#' Enforces one in-flight turn per session: if a turn is already queued/running,
#' this returns a small marker list (not a new turn) so the API layer can tell
#' the client "still working" instead of launching a second, concurrent
#' `cv_agent_turn` on the same session (which, sharing the object store but not
#' a ledger, is what let clustoCell run in a loop).
#' @noRd
cv_start_turn <- function(session_id, message) {
  busy <- cv_session_active_turn(session_id)
  if (!is.null(busy)) {
    return(list(busy = TRUE, turn = busy))
  }
  turn_id <- cv_turn_new(session_id, message)

  # on_event appends to the buffer. It also records heavy-job ids so a cancel can
  # kill in-flight background tools.
  on_event <- function(ev) {
    if (!is.null(ev$job) && identical(ev$type, "job_start")) {
      rec <- cv_turn_get(session_id, turn_id)
      cv_turn_update(session_id, turn_id, job_ids = unique(c(rec$job_ids, ev$job)))
    }
    cv_turn_append_event(session_id, turn_id, ev)
    # Cooperative cancellation: if flagged, abort the turn from inside a tool.
    rec <- cv_turn_get(session_id, turn_id)
    if (isTRUE(rec$cancel)) cli::cli_abort("Turn cancelled by user.")
    invisible()
  }

  # ---- Round XLV (Batch B, item 4): advance the turn ONE STEP per tick -------
  #
  # This used to be a single call to cv_agent_turn(), which ran the whole turn
  # inside one `later` callback. httpuv is single-threaded and cannot preempt R,
  # so the poll request that happened to pick up that callback could not return
  # until the turn had finished -- measured at 5,900 ms for a 3-iteration turn
  # and 26,420 ms with a real heavy tool. `later::run_now(timeoutSecs = 0.05)`
  # in cv_api_chat_poll() reads like a guard against exactly this and is not
  # one: it bounds how long run_now WAITS for work, not how long a callback may
  # RUN.
  #
  # cv_agent_turn(stepwise = TRUE) now hands back a machine whose step() runs a
  # single iteration. After each step we re-schedule ourselves and RETURN,
  # giving the thread back to httpuv so queued HTTP requests are serviced before
  # the next iteration starts.
  machine <- NULL

  # Round LII (Batch 4a): the turn's two terminal points are also the write
  # points for the session snapshot. cv_session_add_message() now only marks the
  # session dirty (it used to rewrite the whole history to disk on every one of
  # the ~6 messages a turn adds), so the single flush here is what makes the
  # turn's messages durable. It must happen on BOTH endings -- a turn that
  # errored or was cancelled still added real messages the user can see in the
  # transcript, and losing them on restart would be a regression, not a saving.
  flush_session <- function() {
    tryCatch(cv_session_flush(session_id), error = function(e) NULL)
  }

  settle <- function() {
    rec <- cv_turn_get(session_id, turn_id)
    if (!is.null(rec) && !(rec$status %in% c("done", "error", "cancelled"))) {
      cv_turn_update(session_id, turn_id, status = "done")
    }
    flush_session()
  }

  fail <- function(e) {
    on.exit(flush_session(), add = TRUE)
    # Round LXXV (audit #33): a turn that produced an object and THEN failed
    # left that object live in the store but absent from the artifacts index,
    # so Results showed nothing at all -- the object was not lost, only
    # invisible. Indexing runs before the status update, and swallows its own
    # errors, so a bookkeeping problem can never mask the user's real one.
    on.exit(cv_index_artifacts_safe(session_id, when = "turn failure"), add = TRUE,
            after = FALSE)
    raw <- conditionMessage(e)
    # Distinguish a user cancel from a genuine failure.
    if (grepl("cancelled", raw, ignore.case = TRUE)) {
      cv_turn_update(session_id, turn_id, status = "cancelled")
    } else {
      # Clean the raw (ANSI-laden, parent-chained) condition for display so the
      # user sees a readable message, not a cli/curl dump.
      msg <- cv_clean_turn_error(raw)
      if (!nzchar(msg)) msg <- "The turn failed unexpectedly."
      # Round LXIV (D6): carry the technical cause in `detail`, the same shape
      # the HTTP envelope already uses (cv_api_err(message, status, detail)) and
      # that the chat UI now reveals behind a toggle. cv_clean_turn_error()
      # produces the friendly sentence; cv_clean_error() gives the underlying R
      # error with ANSI stripped and length capped. Omitted when the two are the
      # same string, because a toggle that opens onto a repeat of the line above
      # it is noise rather than disclosure.
      det <- cv_clean_error(raw)
      ev <- list(type = "error", error = msg)
      if (nzchar(det %||% "") && !identical(det, msg)) ev$detail <- det
      cv_turn_append_event(session_id, turn_id, ev)
      cv_turn_update(session_id, turn_id, status = "error", error = msg)
    }
  }

  advance <- function() {
    rec <- cv_turn_get(session_id, turn_id)
    # A cancel (or an error from a previous step) ends the walk immediately.
    if (is.null(rec) || rec$status %in% c("done", "error", "cancelled")) return(invisible())
    tryCatch({
      if (is.null(machine)) {
        # Round XLVI: async = TRUE -- a heavy tool suspends the turn instead of
        # spinning in a wait loop. The sync path (/api/chat/sync, tests) keeps
        # the blocking dispatcher, so its behaviour is untouched.
        disp <- cv_make_dispatcher(session_id, on_event = on_event, async = TRUE)
        out <- cv_agent_turn(session_id, message, on_event = on_event,
                             dispatch = disp, stream = TRUE, turn_id = turn_id,
                             stepwise = TRUE)
        # A turn answered entirely in pre-flight (annotation clarification, ...)
        # never reaches the loop and returns its result list directly. It has
        # already emitted its own events; there is nothing left to step.
        if (!inherits(out, "cv_turn_machine")) { settle(); return(invisible()) }
        machine <<- out
      }
      outcome <- machine$step()
      if (identical(outcome, "waiting")) {
        # A heavy job is still running. Check back at a human-scale cadence
        # rather than the step cadence: the job reports its own progress through
        # on_event, so this tick exists only to notice that it finished. Polling
        # it every 50 ms would burn the CPU that the job is trying to use.
        later::later(advance, delay = CV_TURN_WAITING_DELAY_SEC)
        return(invisible())
      }
      if (identical(outcome, "done") || machine$exhausted()) {
        machine$finish()          # artifact sync + the terminal "done" event
        settle()
        return(invisible())
      }
      # Yield. The delay must be non-zero, and big enough to be worth defending:
      # a callback that is already runnable gets picked straight back up by the
      # in-flight run_now() that is executing us, chaining step after step
      # without ever returning to httpuv -- the exact freeze this change exists
      # to remove. cv_api_chat_poll()'s own pump is bounded by `all = FALSE`,
      # but httpuv's internal service loop pumps `later` too and we do not
      # control its flags, so the delay is what guarantees an I/O window there.
      # Measured: at 0.01s a poll intermittently caught two steps (~4,000 ms);
      # at 0.05s it consistently catches one (~1,900 ms). 50 ms against a
      # multi-second step is not a cost worth optimising back.
      later::later(advance, delay = CV_TURN_STEP_DELAY_SEC)
    }, error = fail)
  }

  runner <- function() {
    # Idempotent: if this turn already advanced past `queued` (e.g. a doubly
    # pumped scheduler fired the runner twice), do nothing. Only a queued turn
    # is eligible to start, so the turn body runs at most once.
    rec0 <- cv_turn_get(session_id, turn_id)
    if (is.null(rec0) || !identical(rec0$status, "queued")) return(invisible())
    cv_turn_update(session_id, turn_id, status = "running")
    advance()
  }

  # Emit an immediate "thinking" event so the very first poll shows feedback.
  cv_turn_append_event(session_id, turn_id, list(type = "thinking"))
  # Schedule the turn with a SMALL delay (not 0). The turn runs in-process on
  # httpuv's single thread, and the LLM HTTP call is synchronous/blocking; once
  # a poll's later::run_now() starts the runner it cannot return until the whole
  # turn finishes. A short delay guarantees the FIRST poll (a brief run_now that
  # fires before the delay elapses) returns immediately with the buffered
  # "thinking" event, so the user sees live feedback even for tool-less answers
  # that would otherwise block silently for several seconds. A later poll then
  # drives the runner. (For turns that dispatch heavy tools to the worker pool,
  # the runner yields between events, so token/tool progress streams as before.)
  later::later(runner, delay = CV_TURN_START_DELAY_SEC)
  turn_id
}
