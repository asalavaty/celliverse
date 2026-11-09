# =============================================================================
# CelliVerse Agent — async worker pool (callr) + job registry
#
# Heavy CelliVerse tools (clustoCell, markoClust, markoCell, markerPurity) are
# CPU-bound and can run for minutes. Running them inline would freeze the chat.
# Instead we run each heavy tool in a background R process (callr::r_bg) drawn
# from a bounded pool, poll it non-blockingly via later::later, stream progress
# to the client, and put the returned object back into the session store.
#
# CPU BUDGETING (important): each heavy CelliVerse function takes num_threads.
# If P workers each use T threads on a C-core box, P*T must be <= C or we
# oversubscribe and everything slows down. cv_thread_budget() computes T from
# the configured pool size so the agent never oversubscribes.
#
# Job lifecycle: queued -> running -> (done | error | cancelled).
# A job record lives in session$jobs (an environment). The API layer streams a
# job's status/progress over SSE (GET /jobs/:id/stream) and can cancel it
# (POST /jobs/:id/cancel -> processx kill).
# =============================================================================

#' Threads-per-worker so pool_size * threads <= available cores.
#' @noRd
cv_thread_budget <- function(config = cv_load_config()) {
  cores <- tryCatch(parallel::detectCores(logical = TRUE), error = function(e) NA_integer_)
  if (is.na(cores) || cores < 1L) cores <- 1L
  pool <- max(1L, as.integer(config$worker_pool_size %||% 2L))
  max(1L, as.integer(floor(cores / pool)))
}

#' Create a job record and register it in a session's job environment.
#' @param input_handles Batch 3b item 2: the object-store handle(s) this job
#'   reads as input, recorded so cv_object_evict_stale() (agent_object_
#'   store.R) can skip evicting them while this job is still queued/running --
#'   see .cv_object_inflight_handles() there for why.
#' @noRd
cv_job_new <- function(session_id, tool_name, args_public = list(),
                       input_handles = character(0)) {
  job_id <- cv_new_id("job")
  sess <- cv_session_get(session_id)
  rec <- list(
    id = job_id, session_id = session_id, tool = tool_name,
    status = "queued", progress = 0, message = "queued",
    args = args_public, input_handles = input_handles,
    created = cv_now(), updated = cv_now(),
    process = NULL, result = NULL, error = NULL, log_file = NULL
  )
  assign(job_id, rec, envir = sess$jobs)
  # Batch 2b fix: sweep old, terminal job records AFTER adding the new one, so
  # the registry never holds more than the configured cap (the brand-new
  # "queued" job itself is never evictable, since eviction only ever touches
  # terminal jobs). See cv_job_evict_stale() below for why unbounded growth
  # was a real problem.
  tryCatch(cv_job_evict_stale(session_id), error = function(e) NULL)
  job_id
}

#' Evict old, TERMINAL job records once a session's job registry grows past
#' `keep` entries, so a long-running session (many heavy-tool runs over its
#' lifetime) does not accumulate an unbounded number of job records.
#'
#' Each job record holds a live callr process handle (`rec$process`) even
#' after the job finishes -- nothing previously ever removed it. Over a long
#' session that runs many heavy tools (clustoCell, markoClust, ...), `sess$jobs`
#' grew without limit: every `ls(envir = sess$jobs)` call (the jobs-list API,
#' the running-jobs count, every poll() tick) got slower over the session's
#' life, and each retained process handle keeps its stdio pipe/PID bookkeeping
#' alive rather than letting it be garbage-collected.
#'
#' Only TERMINAL jobs (done/error/cancelled) are ever evicted, oldest first by
#' `updated` timestamp -- an in-flight (queued/running) job is never touched
#' regardless of how many terminal jobs exist. This mirrors a simple
#' capped-history policy: recent job status stays available for the UI's Jobs
#' panel and `GET /api/jobs`, older history quietly ages out.
#' @param session_id session id.
#' @param keep max terminal jobs to retain; defaults to the session's
#'   `job_history_limit` config (see cv_default_config()).
#' @return invisibly, the number of records evicted.
#' @noRd
cv_job_evict_stale <- function(session_id, keep = NULL) {
  sess <- cv_session_get(session_id)
  if (is.null(sess) || is.null(sess$jobs)) return(invisible(0L))
  if (is.null(keep)) keep <- as.integer(sess$config$job_history_limit %||% 50L)
  ids <- ls(envir = sess$jobs)
  if (length(ids) <= keep) return(invisible(0L))

  recs <- stats::setNames(lapply(ids, function(i) get(i, envir = sess$jobs)), ids)
  is_terminal <- vapply(recs, function(r) isTRUE(r$status %in% c("done", "error", "cancelled")),
                        logical(1))
  terminal_ids <- ids[is_terminal]
  n_over <- length(ids) - keep
  if (n_over <= 0L || !length(terminal_ids)) return(invisible(0L))

  terminal_updated <- vapply(terminal_ids, function(i) recs[[i]]$updated %||% recs[[i]]$created %||% "",
                             character(1))
  # Oldest-first; never evict more than the number of terminal jobs available,
  # so a session with mostly in-flight jobs is left alone even if over `keep`.
  evict_ids <- terminal_ids[order(terminal_updated)][seq_len(min(n_over, length(terminal_ids)))]

  for (eid in evict_ids) {
    rec <- recs[[eid]]
    # Defensive: a terminal job's process should already be dead, but release
    # the handle explicitly rather than relying on GC to notice.
    if (!is.null(rec$process)) tryCatch(rec$process$kill(), error = function(e) NULL)
    rm(list = eid, envir = sess$jobs)
  }
  invisible(length(evict_ids))
}

#' Fetch a job record (or NULL).
#' @noRd
cv_job_get <- function(session_id, job_id) {
  sess <- cv_session_get(session_id)
  if (!exists(job_id, envir = sess$jobs, inherits = FALSE)) return(NULL)
  get(job_id, envir = sess$jobs)
}

#' Update fields on a job record.
#' @noRd
cv_job_update <- function(session_id, job_id, ...) {
  sess <- cv_session_get(session_id)
  if (!exists(job_id, envir = sess$jobs, inherits = FALSE)) return(invisible(NULL))
  rec <- get(job_id, envir = sess$jobs)
  upd <- list(...)
  for (nm in names(upd)) rec[[nm]] <- upd[[nm]]
  rec$updated <- cv_now()
  assign(job_id, rec, envir = sess$jobs)
  invisible(rec)
}

#' A public (client-safe) view of a job record — no process handle / raw result.
#' @noRd
cv_job_public <- function(rec) {
  if (is.null(rec)) return(NULL)
  list(id = rec$id, tool = rec$tool, status = rec$status,
       progress = rec$progress, message = rec$message,
       created = rec$created, updated = rec$updated,
       handle = tryCatch(rec$result$handle, error = function(e) NULL),
       error = rec$error)
}

#' Count running jobs across a session (for simple pool-size enforcement).
#'
#' A job only counts if it is BOTH marked status=="running" AND has a real,
#' currently-alive process attached (mirrors poll()'s own liveness check,
#' `tryCatch(p$is_alive(), error = function(e) FALSE)`). Round XXXIII (Batch
#' 3b, item 7) live testing found that a status=="running" LABEL alone is not
#' reliable evidence a worker slot is actually occupied: a job record can be
#' left at status=="running" with no attached process, or with a process that
#' has already exited/been killed outside the normal poll()/cv_job_cancel()
#' path (e.g. a crash, an external kill, or a record built directly for unit
#' testing). Since cv_jobs_running_global() (below) sums this across every
#' session for the LIFETIME of the R process, even ONE such stale "running"
#' record would otherwise inflate the count forever and permanently wedge
#' admission control for every session's heavy tools -- a full deadlock, far
#' worse than the unbounded-concurrency risk this safety valve exists to fix.
#' Checking real process liveness makes the count self-healing regardless of
#' how a record ended up mislabeled.
#' @noRd
cv_jobs_running <- function(session_id) {
  sess <- cv_session_get(session_id)
  ids <- ls(envir = sess$jobs)
  sum(vapply(ids, function(i) {
    rec <- get(i, envir = sess$jobs)
    identical(rec$status, "running") && !is.null(rec$process) &&
      isTRUE(tryCatch(rec$process$is_alive(), error = function(e) FALSE))
  }, logical(1)))
}

#' Count running heavy jobs ACROSS EVERY LIVE SESSION (not just one).
#'
#' Round XXXIII (Batch 3b, item 7): worker_pool_size / cv_thread_budget()'s
#' whole rationale (P workers * T threads <= C cores) only holds if P bounds
#' the number of concurrent heavy workers for the WHOLE R process, not one
#' session at a time -- two sessions each "within" a per-session cap of 2
#' could still spawn 4 concurrent callr children on a machine budgeted for 2.
#' This sums cv_jobs_running() over every session in the package-global
#' .cv_sessions registry (agent_session.R), giving the true process-wide count
#' cv_launch_heavy()'s admission control checks against.
#' @noRd
cv_jobs_running_global <- function() {
  sids <- ls(envir = .cv_sessions)
  if (!length(sids)) return(0L)
  sum(vapply(sids, cv_jobs_running, integer(1)))
}

# ---- The heavy-tool launcher ------------------------------------------------

#' Launch a heavy tool in a background R process and wire up non-blocking polling.
#'
#' Round XXXIII (Batch 3b, item 7): the job is now admitted through
#' .cv_worker_admit_or_queue() rather than spawning unconditionally --
#' worker_pool_size (config, default 2L) is enforced as a real, process-wide
#' cap on concurrent heavy-tool dispatch (see that function's doc for why
#' "process-wide" and not "per session"). cv_job_new() leaves the record
#' status="queued"; it only flips to "running" once .cv_worker_start_job()
#' actually spawns the child, which may happen immediately (pool has room) or
#' after one or more later::later() retries (pool is full). This is a
#' standalone interim safety valve, NOT the full heavy-tool reentrancy
#' redesign (that remains deferred, tied to the still-open Round XXIX
#' finding) -- see CHANGES.md Round XXXIII.
#'
#' @param session_id  session id (objects + job registry live here).
#' @param tool        the cv_tool object.
#' @param call_args   resolved args (handles still strings) WITH attr "handle_args".
#' @param on_event    streaming callback (job_start/progress/job_done/job_error,
#'   plus job_queued while waiting for a free worker-pool slot).
#' @param poll_ms     polling interval for later::later (also the admission
#'   retry cadence while queued).
#' @return job_id (character). Result is delivered asynchronously; when done, the
#'   produced object is put into the session store and on_event("job_done") fires.
#' @noRd
cv_launch_heavy <- function(session_id, tool, call_args, on_event = NULL,
                            poll_ms = 400L) {
  config <- cv_session_get(session_id)$config
  threads <- cv_thread_budget(config)
  pool_size <- max(1L, as.integer(config$worker_pool_size %||% 2L))

  # Materialize handle args into real objects to ship to the child process.
  store <- cv_session_get(session_id)$object_store
  handle_args <- attr(call_args, "handle_args") %||% character(0)
  # Capture the input handle(s) BEFORE materialization so the result object can
  # inherit the input's base name (obj_pbmc3k -> clusto_pbmc3k).
  inherit_from <- .cv_input_handles(tool, call_args, handle_args)
  # Round XXIV: descriptive handle for the annotation tools — append a method +
  # target-set tag so the result names what it is (typo_pbmc_markerdb_c1_c3).
  # typoClust is HEAVY and reaches the store via this worker path, so the tag
  # must be applied here as well as in the inline handler.
  if (identical(tool$name, "typoClust")) {
    inherit_from <- cv_tagged_inherit(inherit_from, method = "markerdb",
                                      desired_sets = call_args$desired_sets)
  }
  # Round LXIV (D1): run the tool's declared pre-dispatch preparation, the same
  # code its light-path handler runs. Before this, a heavy tool's handler
  # preamble simply never executed -- markoCell's CellSet expansion and its
  # subset guard were dead in production because cv_make_dispatcher() only calls
  # tool$handler() for `cost == "light"`. Declared via cv_tool(prepare=) rather
  # than another `if (identical(tool$name, ...))` branch here, because this is
  # the third time this class of drift has been fixed one tool at a time.
  #
  # Deliberately NOT wrapped in tryCatch: the guard's abort is the whole point,
  # and it must reach the model so it can re-route to getClusterMarkers.
  if (is.function(tool$prepare)) {
    prepped <- tool$prepare(store, call_args, tool, handle_args)
    if (!is.null(prepped$args)) {
      # Preserve the attributes cv_resolve_args() attached (cv_tool,
      # handle_args): downstream materialization reads them.
      keep <- attributes(call_args)
      call_args <- prepped$args
      for (a in setdiff(names(keep), names(attributes(call_args))))
        attr(call_args, a) <- keep[[a]]
    }
    if (!is.null(prepped$inherit_from)) inherit_from <- prepped$inherit_from
  }
  materialized <- call_args
  for (nm in handle_args) materialized[[nm]] <- cv_object_get(store, materialized[[nm]])
  # Array-of-handle params (e.g. typoClust 'objects') -> LIST of real objects.
  # These are NOT in handle_args (they are validated + kept as strings by
  # cv_resolve_args); resolve them by spec so the child gets real objects, not
  # handle strings (which would crash the CelliVerse function).
  materialized <- .cv_materialize_array_handles(store, tool, materialized)
  # typoClust: normalise tissue/condition (lowercase + validate against the
  # Marker DB vocab) BEFORE shipping to the child, so a wrongly-cased model
  # value like 'Blood' works instead of aborting in the worker on every retry.
  if (identical(tool$name, "typoClust")) {
    # Do NOT swallow a genuine invalid-value abort: that error lists the valid
    # values and must reach the model so it can self-correct. Only fall back to
    # the un-normalised args if the shim itself unexpectedly fails (e.g. vocab
    # unloadable) - in that case pass through and let typoClust decide.
    materialized <- .cv_normalize_tissue_condition(materialized)
  }
  # Capture the (normalized) tissue filter so the result text can decide whether
  # the cross-tissue advisory applies (only when the run was NOT tissue-filtered).
  typoclust_tissue_arg <- if (identical(tool$name, "typoClust")) materialized$tissue else NULL
  # Override num_threads to the per-worker budget if the tool exposes it.
  if ("num_threads" %in% names(tool$parameters)) materialized$num_threads <- threads

  # Batch 3b item 2: record this job's input handle(s) so cv_object_evict_
  # stale() (agent_object_store.R) never evicts one of them while this job is
  # still queued/running -- see .cv_object_inflight_handles() there.
  job_id <- cv_job_new(session_id, tool$name,
                       args_public = call_args[setdiff(names(call_args), handle_args)],
                       input_handles = inherit_from)
  log_file <- file.path(cv_session_get(session_id)$artifacts_dir, paste0(job_id, ".log"))

  ctx <- list(session_id = session_id, tool = tool, job_id = job_id,
              materialized = materialized, log_file = log_file,
              on_event = on_event, poll_ms = poll_ms,
              inherit_from = inherit_from,
              typoclust_tissue_arg = typoclust_tissue_arg,
              threads = threads, pool_size = pool_size)
  .cv_worker_admit_or_queue(ctx)
  job_id
}

#' Admission control for the heavy-worker pool (Round XXXIII, Batch 3b item 7).
#'
#' worker_pool_size / cv_thread_budget()'s whole rationale (P workers * T
#' threads <= C cores, see cv_thread_budget()'s doc above) only holds if P
#' bounds the number of CONCURRENT heavy workers for the whole R process --
#' not one session at a time. Before this fix, cv_jobs_running() existed
#' (per-session count) but was never called anywhere, so cv_launch_heavy()
#' spawned a new callr child completely unconditionally: N
#' concurrently-overlapping heavy-tool sessions could spawn N children with no
#' cap, oversubscribing the CPU threads were budgeted against and (per the
#' Batch 3b investigation) growing the later::run_now() reentrancy stack depth
#' roughly linearly with N.
#'
#' If fewer than `ctx$pool_size` heavy jobs are currently "running" across
#' EVERY live session (cv_jobs_running_global()), the job is admitted
#' immediately. Otherwise it is left "queued" and this function re-checks on
#' the next later::later() tick, at the same poll_ms cadence used for
#' progress polling, until a slot frees or the job is cancelled. A job
#' cancelled while still queued has rec$process == NULL, so cv_job_cancel()
#' marks it "cancelled" directly (no live process to kill); the next
#' admission tick sees the terminal status and the retry chain stops without
#' ever spawning a child for a cancelled job.
#'
#' This is an interim, standalone safety valve -- it caps concurrent DISPATCH,
#' not the deeper later::run_now() reentrancy issue itself (a queued job still
#' waits inside cv_make_dispatcher()'s existing blocking wait loop, which
#' already pumps later::run_now() and already enforces tool_timeout_sec, so a
#' job stuck queued forever still times out exactly as before). The full
#' reentrancy redesign remains deferred to a future round tied to the
#' still-open Round XXIX finding.
#' @noRd
.cv_worker_admit_or_queue <- function(ctx) {
  rec <- cv_job_get(ctx$session_id, ctx$job_id)
  if (is.null(rec) || rec$status %in% c("done", "error", "cancelled")) return(invisible())

  running <- cv_jobs_running_global()
  if (running < ctx$pool_size) {
    # INCIDENT FIX (Round XXXIX): the count-based cap above is necessary but
    # not sufficient. worker_pool_size bounds how MANY heavy children may run;
    # nothing bounded how much MEMORY they collectively demand. Measured on
    # the user's own 2,700-cell dataset, a single heavy umapPlot child peaks
    # at ~650 MB RSS (dominated by Seurat's NormalizeData/ScaleData -- i.e.
    # legitimate compute, not a leak), with the parent process at ~700 MB, so
    # the default pool of 2 sanctions a ~1.3 GB concurrent burst with no
    # regard for whether the machine has 1.3 GB to give.
    #
    # That is the shape of the reported incident: three
    # `userspace watchdog timeout: no successful checkins from WindowServer`
    # kernel panics on a 24 GB Apple Silicon Mac that was ALREADY 1.8 GB into
    # its swap file, running a large corporate app stack plus a GPU-offloaded
    # local LLM. GPU-offloaded model weights are WIRED -- they cannot be
    # compressed or paged out -- so pressure from an additional
    # multi-hundred-MB burst lands on everything else, WindowServer included,
    # and macOS panics the machine rather than jetsam-killing R. This is a
    # contributing-load fix, NOT a proven root cause, and is labelled as such
    # in CHANGES.md.
    #
    # THE NO-DEADLOCK PROPERTY (deliberate, and what makes this safe to ship):
    # the memory gate is consulted ONLY when this job would be an ADDITIONAL
    # concurrent job (running >= 1). The FIRST heavy job is never gated, no
    # matter how little memory is free. A machine permanently below the
    # threshold therefore degrades to running heavy tools strictly serially --
    # it can never reach a state where nothing is admitted at all. This
    # mirrors the lesson of Round XXXIII, where a count-based gate that COULD
    # wedge permanently was judged far worse than the risk it mitigated.
    # cv_available_memory_mb() returning NA (platform it cannot measure) is
    # likewise treated as "no opinion" and admits, so behaviour on any such OS
    # is exactly what it was before this round.
    min_free <- suppressWarnings(as.numeric(
      cv_session_get(ctx$session_id)$config$heavy_job_min_free_mb %||% 0))
    avail <- if (isTRUE(running >= 1L) && isTRUE(is.finite(min_free)) && isTRUE(min_free > 0))
      cv_available_memory_mb() else NA_real_
    if (!is.na(avail) && avail < min_free) {
      cv_job_update(ctx$session_id, ctx$job_id, status = "queued",
                    message = sprintf(
                      "Waiting for memory: %.0f MB free, %.0f MB needed before starting another heavy tool (%d already running).",
                      avail, min_free, running))
      cv_emit(ctx$on_event, "job_queued", job = ctx$job_id, tool = ctx$tool$name,
              running = running, pool_size = ctx$pool_size,
              reason = "memory", available_mb = round(avail),
              min_free_mb = min_free)
      later::later(function() .cv_worker_admit_or_queue(ctx), ctx$poll_ms / 1000)
      return(invisible())
    }
    .cv_worker_start_job(ctx)
    return(invisible())
  }

  cv_job_update(ctx$session_id, ctx$job_id, status = "queued",
                message = sprintf("Waiting for a free worker slot (%d/%d running).",
                                  running, ctx$pool_size))
  cv_emit(ctx$on_event, "job_queued", job = ctx$job_id, tool = ctx$tool$name,
          running = running, pool_size = ctx$pool_size, reason = "pool")
  later::later(function() .cv_worker_admit_or_queue(ctx), ctx$poll_ms / 1000)
  invisible()
}

#' Actually spawn the child process for an admitted heavy job and start its
#' non-blocking poll loop. Split out of cv_launch_heavy() (Round XXXIII,
#' Batch 3b item 7) so admission control can defer this call until a
#' worker-pool slot is free; the body below is otherwise unchanged from the
#' pre-Round-XXXIII cv_launch_heavy().
#' @noRd
.cv_worker_start_job <- function(ctx) {
  session_id <- ctx$session_id; tool <- ctx$tool; job_id <- ctx$job_id
  materialized <- ctx$materialized; log_file <- ctx$log_file
  on_event <- ctx$on_event; poll_ms <- ctx$poll_ms
  inherit_from <- ctx$inherit_from; typoclust_tissue_arg <- ctx$typoclust_tissue_arg
  threads <- ctx$threads

  cv_job_update(session_id, job_id, status = "running", message = "started",
                log_file = log_file, progress = 5)
  cv_emit(on_event, "job_start", job = job_id, tool = tool$name, threads = threads)

  # INCIDENT FIX (Round XXXVIII, live-reproduced 2026-08-11 against the user's
  # own 2,700-cell dataset): a local-model session ran umapPlot's heavy path
  # (fresh UMAP needed, so .cv_umap_plot_compute() had to compute one) and the
  # user's Mac restarted. Reproduced here as a ~7x wall-clock blowup (a real
  # UMAP compute that takes ~11s called directly took ~75-80s launched exactly
  # as this function launches it) that is completely independent of thread
  # count, BLAS, `future` parallelism, or callr's environment -- all ruled out
  # empirically. Root cause, isolated via Rprof(): passing `materialized`
  # (the resolved Seurat object + call args) through callr::r_bg()'s `args=`
  # and then dispatching with `do.call(fn, args)` -- BOTH of these, callr's
  # own internal bootstrap AND the do.call() below -- construct a call object
  # with each argument VALUE embedded as a literal constant, not a symbol
  # reference. For a large S4 object that literal sits on the call stack for
  # the entire duration of the call. Seurat::ScaleData() (via ScaleData.Assay/
  # ScaleData.default's internal `Parenting()` helper, used to find its own
  # calling context) walks sys.calls() and deparses ancestor frames looking
  # for a named caller -- when one of those ancestor frames is a do.call()-
  # built call with the whole Seurat object embedded, deparsing/stringifying
  # it is what actually burns the time: as.character() alone accounted for
  # 97% of ScaleData()'s wall time in the reproduction (confirmed: a plain
  # in-process call to the identical function, with no do.call() anywhere in
  # its ancestry, took ~11s; do.call()-dispatching the exact same call
  # in-process, no callr involved at all, took ~32s on its own). Every other
  # heavy tool (clustoCell, markoClust, markoCell, markerPurity, typoClust,
  # clustoCell_TransferLabel) funnels through this SAME child_fun/do.call()
  # pattern and three of them (clustoCell, markoClust, clustoCell_
  # TransferLabel) call ScaleData()/NormalizeData() internally too, so this
  # was not umapPlot-specific -- it is fixed once, here, for every heavy tool.
  #
  # Fix has two parts, both required (verified empirically -- either alone
  # left a large residual slowdown):
  #  1) Never let a large object live inside the `args=` list handed to
  #     callr::r_bg() at all. `materialized` is saveRDS()'d to a temp file in
  #     THIS (parent) process and only the (tiny) file path travels through
  #     `args=`, so callr's own internal do.call() only ever embeds a short
  #     string -- cheap to deparse regardless of how large the real object is.
  #  2) Inside the child, dispatch via a call built from SYMBOLS bound in a
  #     private environment (eval(as.call(...), envir = env)), never
  #     do.call(fn, args) -- so no NEW literal-embedding call is created once
  #     the big object is back in memory there either. The symbols are
  #     explicitly NAMED (matched to names(args), not positional) so argument
  #     order in `materialized` can never misalign with fn's formals.
  materialized_file <- file.path(cv_session_get(session_id)$artifacts_dir,
                                 paste0(job_id, "_args.rds"))
  # Round LIII (Batch 4b): `compress = FALSE` is load-bearing, not a tweak.
  #
  # saveRDS() gzips by default, and this call runs on the SAME single thread
  # that serves HTTP -- cv_launch_heavy() is reached from inside a `later`
  # callback, and R cannot be preempted. So for as long as the compressor runs,
  # httpuv answers nothing and the UI is frozen, not merely slow. Measured
  # against a real httpuv server with a separate client process polling an
  # instant route every 50 ms:
  #
  #      object    gzip write   worst client request   median request
  #    16.3 MB        433 ms            410 ms             1.2 ms
  #    64.4 MB      1,703 ms          1,672 ms             1.2 ms
  #   192.5 MB      5,296 ms          5,262 ms             1.2 ms
  #
  # Worst-case latency tracks the write at 95-99% while the median stays flat,
  # so the entire block is this one line. That is the exact property Round XLV's
  # step machine exists to protect -- no single poll may block for the length of
  # the work -- and a 192 MB Seurat object is a modest real dataset.
  #
  # Compression buys nothing HERE, which is what makes this safe rather than a
  # trade: the file is written by this process, read back once by a child on the
  # same machine seconds later, and unlinked by that child's on.exit(). It is
  # never archived, never copied, never sent anywhere. All the default buys is
  # a smaller file that no one ever keeps, at the cost of freezing the UI.
  #
  #      object    gzip write   raw write     gzip read   raw read
  #    64.4 MB      1,687 ms       92 ms        269 ms      90 ms
  #   192.5 MB      5,164 ms    1,133 ms        607 ms     280 ms
  #
  # 18x faster at 64 MB, 4.6x at 192 MB, and the child's read back is 2-3x
  # faster too, so the job also starts sooner. The cost is a larger short-lived
  # temp file (192 MB rather than 12.5 MB) per in-flight heavy job, bounded by
  # the Round XXXIII worker-pool admission cap and deleted when the job ends.
  #
  # Everything about the Round XXXVIII mechanism above is UNCHANGED: still a
  # temp file, still saveRDS/readRDS, still only a short path travelling through
  # callr's `args=`, still symbol-based dispatch in the child. Only the encoding
  # of the bytes changes.
  #
  # NOTE for anyone tempted to do the same on the way back: the parent also
  # deserializes the child's RESULT on this thread (p$get_result(), in poll()
  # below). That file is written by callr itself, which exposes no compression
  # option, so it cannot be changed here. It is also a much smaller effect --
  # decompressing costs roughly an eighth of what compressing does (607 ms vs
  # 5,164 ms at 192 MB) -- which is why it is documented rather than worked
  # around.
  saveRDS(materialized, materialized_file, compress = FALSE)

  # The function executed in the CHILD process. It must be self-contained:
  # load celliverse, call the function by name, return the result object.
  # Colours OFF at the source so cli/crayon never write ANSI SGR codes into the
  # child's messages/errors (which would otherwise leak into the failure text).
  child_fun <- function(fun_name, materialized_path) {
    Sys.setenv(NO_COLOR = "1")
    options(cli.num_colors = 1L, crayon.enabled = FALSE)
    suppressPackageStartupMessages(library(celliverse))
    on.exit(unlink(materialized_path), add = TRUE)
    args <- readRDS(materialized_path)
    fn <- utils::getFromNamespace(fun_name, "celliverse")
    sym_args <- lapply(names(args), as.symbol)
    names(sym_args) <- names(args)
    call_env <- list2env(args, parent = parent.frame())
    eval(as.call(c(list(fn), sym_args)), envir = call_env)
  }

  proc <- callr::r_bg(
    func = child_fun,
    # Round XXXIV: most heavy tools ARE a real top-level celliverse::<name>()
    # function (clustoCell, markoClust, ...), so fun_name == tool$name always
    # worked. A tool whose public name has no matching standalone function --
    # only an inline `handler` closure bound to the session's object store,
    # like umapPlot -- sets `heavy_impl` to the internal function the child
    # should call instead; every other tool leaves it NULL and this is a
    # no-op (tool$name unchanged).
    args = list(fun_name = tool$heavy_impl %||% tool$name,
               materialized_path = materialized_file),
    stdout = log_file, stderr = log_file, supervise = TRUE,
    package = FALSE,
    # Belt-and-suspenders: also disable colour via the child's environment,
    # appended to callr's safe baseline env (do NOT replace it).
    #
    # INCIDENT FIX (Round XXXV): `threads` (cv_thread_budget(), above) was
    # ALREADY computed here and already overrides a tool's own `num_threads`
    # PARAMETER when the tool exposes one (see below) -- CelliVerse's own
    # functions (clustoCell, ...) route num_threads into custom Rcpp routines
    # that honour it directly. umapPlot's heavy_impl, `.cv_umap_plot_compute()`,
    # calls Seurat::ScaleData()/RunPCA()/RunUMAP() -- none of which expose a
    # thread-count PARAMETER at all -- so it was the one heavy tool whose
    # native (BLAS/OpenMP/RcppParallel) thread usage was completely
    # unconstrained by cv_thread_budget(), unlike every other heavy tool.
    # Setting the standard thread-limiting environment variables that BLAS
    # (OPENBLAS_NUM_THREADS, VECLIB_MAXIMUM_THREADS -- the latter is what
    # macOS's Accelerate framework reads), OpenMP (OMP_NUM_THREADS), and
    # RcppParallel (RCPP_PARALLEL_NUM_THREADS, what uwot's neighbor search
    # uses) all read closes this gap GENERICALLY for every heavy tool's child
    # process, not just ones with an explicit num_threads parameter -- a
    # tool that already sets num_threads itself is unaffected (its own
    # override wins), and a tool like umapPlot that has no such lever now
    # gets one for free. Reported context: a live user session ran umapPlot's
    # heavy path (fresh UMAP needed) concurrently with a local LLM already
    # holding significant CPU/RAM, and observed a full machine restart --
    # unbounded native thread spawning stacking on top of that load is a
    # credible contributor. This could not be reproduced as a literal crash
    # in this sandbox (a single-tenant Linux container behaves differently
    # under memory/thread pressure than the user's own machine), so unlike
    # every other fix in this engagement, this one is a defensive close of a
    # confirmed code-level gap, not an empirically reproduce-then-fix cycle --
    # flagged explicitly in the round's writeup rather than claimed as
    # equally verified.
    env = c(callr::rcmd_safe_env(), NO_COLOR = "1", R_CLI_NUM_COLORS = "1",
            OMP_NUM_THREADS = as.character(threads),
            OPENBLAS_NUM_THREADS = as.character(threads),
            VECLIB_MAXIMUM_THREADS = as.character(threads),
            RCPP_PARALLEL_NUM_THREADS = as.character(threads),
            MKL_NUM_THREADS = as.character(threads))
  )
  cv_job_update(session_id, job_id, process = proc)

  # Non-blocking poll loop via later::later. Reads the child's log for progress
  # hints and finalizes when the process exits.
  poll <- function() {
    rec <- cv_job_get(session_id, job_id)
    if (is.null(rec) || rec$status %in% c("done", "error", "cancelled")) return(invisible())
    p <- rec$process
    alive <- tryCatch(p$is_alive(), error = function(e) FALSE)
    # crude progress: bump toward 90% while alive; parse log tail for a % if present
    prog <- min(90, (rec$progress %||% 5) + 3)
    msg <- cv_tail_progress(rec$log_file) %||% rec$message
    cv_job_update(session_id, job_id, progress = prog, message = msg)
    # Round XLVII: carry the TOOL NAME on every progress event. The chat UI has
    # to attach live progress to the right tool card, and a job id alone means it
    # must remember a job->tool map built from an earlier `job_start` -- which it
    # loses on a page reload mid-run. One extra field removes that whole class of
    # bug at the source.
    cv_emit(on_event, "progress", job = job_id, tool = tool$name,
            progress = prog, message = msg)

    if (alive) { later::later(poll, poll_ms / 1000); return(invisible()) }

    # The child already unlinks materialized_file itself (on.exit in
    # child_fun) once it has read it back -- this is only a safety net for
    # the case where the process died before ever reaching that point (e.g.
    # crashed during startup/library load), so the temp args file doesn't
    # linger in the session's artifacts directory. Harmless no-op otherwise.
    tryCatch(unlink(materialized_file), error = function(e) NULL)

    # Process finished — collect result or error.
    # Round LXXV (audit #30): keep the STATUS, not just whether it was zero.
    # processx returns the negative signal number for a signal-killed child, so
    # an OOM-kill and an analytical abort are already distinguishable here at no
    # cost, and until this round both were flattened to `FALSE`. Measured:
    #   SIGKILLed child -> -9      R stop() child -> 1      clean child -> 0
    exit_status <- tryCatch(p$get_exit_status(), error = function(e) NA_integer_)
    ok <- isTRUE(!is.na(exit_status) && exit_status == 0)
    if (ok) {
      value <- tryCatch(p$get_result(), error = function(e) structure(conditionMessage(e), class = "cv_worker_fail"))
      if (inherits(value, "cv_worker_fail")) {
        emsg <- cv_clean_error(as.character(value))
        if (!nzchar(emsg)) emsg <- sprintf("%s failed.", tool$name)
        cv_job_update(session_id, job_id, status = "error", error = emsg, progress = 100)
        cv_emit(on_event, "job_error", job = job_id, error = emsg)
      } else {
        # Put the produced object into the store (parent process) and build a record.
        st <- cv_session_get(session_id)$object_store
        # Round XXXIX: if the child computed a dimensional reduction while
        # doing its job, cache it back onto the SOURCE object before building
        # the result record, so the next call for the same object takes the
        # cheap inline path instead of paying the full heavy pipeline again.
        # Generic and opt-in: a no-op for every tool that does not return a
        # `reductions_added` field, which today is all of them but umapPlot.
        value <- .cv_persist_returned_reductions(st, inherit_from, value)
        rr <- cv_result_from_value(st, tool, value, inherit_from = inherit_from,
                                   typoclust_tissue = typoclust_tissue_arg)
        # INCIDENT FIX (Round XXXV, live-reproduced 2026-08-11): for a "plot"
        # result (currently only umapPlot's heavy path), rr still holds the RAW
        # plot_object (a ggplot/patchwork S3 object full of environments,
        # quosures, and ggproto objects) at this point -- cv_result_from_value()
        # deliberately passes it through unchanged so cv_render_result() can
        # find it (see that function's own comment). Every OTHER place that
        # emits a tool result to the client (agent_loop.R, 3 call sites) calls
        # cv_render_result() first, which replaces plot_object with a
        # lightweight $artifact file reference before the result is ever
        # JSON-serialized. This job_done event, however, is emitted HERE,
        # directly from the worker's own poll callback, BEFORE the dispatcher's
        # blocking wait loop in cv_make_dispatcher() ever hands control back to
        # agent_loop.R -- so without rendering here too, the raw ggplot object
        # reached jsonlite's serializer directly and threw "cannot unclass an
        # environment" with no route-level tryCatch to catch it, surfacing to
        # the browser as a bare "GET /chat/poll ... -> 500" (reproduced live
        # against the pre-fix code: a synthetic job_done event carrying a real
        # umapPlot plot_object failed jsonlite::toJSON() identically). Rendering
        # here is idempotent -- cv_render_result() is a no-op if called again
        # later on an already-rendered result (plot_object is already NULL by
        # then) -- so this does not double-render or duplicate artifact files.
        sess <- cv_session_get(session_id)
        rr <- tryCatch(
          cv_render_result(rr, sess$artifacts_dir, session_id = session_id,
                           basename = paste0(tool$name, "_", cv_new_id("art"))),
          error = function(e) rr)
        cv_job_update(session_id, job_id, status = "done", progress = 100,
                      message = "done", result = rr)
        # Round LXXV (D5): the same strip as the light path, through the same
        # helper. This pair has drifted four times; it does not drift here.
        cv_emit(on_event, "job_done", job = job_id, tool = tool$name,
                handle = rr$handle %||% NA, summary = rr$text %||% NA,
                result = cv_result_for_browser(rr))
      }
    } else {
      # Round LXXV (audit #30): classify before reporting. The old message was
      # "Worker process failed: <log tail>" for every cause -- an out-of-memory
      # kill and a bad argument read identically, while the LLM side of the same
      # product has had a full memory remedy since Round XXXI.
      cls <- cv_classify_worker_failure(exit_status, cv_read_log(rec$log_file), tool$name)
      cv_job_update(session_id, job_id, status = "error", error = cls$message,
                    progress = 100)
      cv_emit(on_event, "job_error", job = job_id, error = cls$message,
              cause = cls$cause, detail = cls$detail,
              log_url = cv_artifact_url(session_id, basename(rec$log_file)))
    }
  }
  later::later(poll, poll_ms / 1000)
  invisible()
}

#' Test-support probe for the Round XXXVIII heavy-dispatch fix above.
#'
#' Reports the length of the longest deparsed call on the current stack.
#' `do.call(fn, args)` (and callr::r_bg()'s own internal dispatch, before this
#' round's fix) embeds each argument VALUE as a literal constant in the call
#' object it constructs; deparsing a frame that embeds a large object then
#' produces an enormous string. Calling by symbols bound in an environment
#' (this round's fix) never puts more than a short variable name in any call
#' object, regardless of how large the underlying object is. This gives the
#' regression test in test-worker.R a fast (~milliseconds), deterministic,
#' Seurat-version-independent way to confirm the fix without a real multi-
#' second ScaleData() call: dispatch this AS a heavy tool's heavy_impl with a
#' large argument and assert the returned length stays small.
#' @noRd
.cv_probe_max_frame_deparse_len <- function(...) {
  n <- sys.nframe()
  max_len <- 0L
  for (i in seq_len(n)) {
    len <- tryCatch(nchar(paste(deparse(sys.call(i)), collapse = "")),
                    error = function(e) 0L)
    if (is.finite(len)) max_len <- max(max_len, len)
  }
  max_len
}

#' Test-support probe: echoes its named arguments straight back as a list.
#'
#' Used by the regression test guarding the named-symbol-call fix above
#' against a real bug found while building it: constructing the dispatch
#' call from UNNAMED symbols (as.call(c(fn, lapply(names(args), as.symbol))),
#' with no names attached to that list) matches purely by POSITION, so if
#' `materialized`'s field order ever drifts from the target function's
#' formal order, argument values silently swap. This lets a test dispatch
#' args in a deliberately shuffled order and confirm each one still lands on
#' the correct formal by name.
#' @noRd
.cv_test_echo_args <- function(first, second, third) {
  list(first = first, second = second, third = third)
}

#' Write a tool's computed dimensional reduction(s) back onto its SOURCE object.
#'
#' Round XXXIX. A tool may return a `reductions_added` field: a named list of
#' Seurat DimReduc objects it computed while doing its real job. This applies
#' them to the object the tool READ (same handle, in place -- no duplicate
#' object, mirroring addClustoData/addTypoData) and then strips the field, so
#' it never reaches the model, the client, or the stored result record.
#'
#' Why this exists: `.cv_umap_plot_compute()` computed a full
#' NormalizeData -> ScaleData -> RunPCA -> RunUMAP pipeline on a local copy
#' and discarded it, so the stored object never gained an embedding and every
#' subsequent umapPlot paid the same ~650 MB / ~27s cost again. Persisting the
#' embedding makes the SECOND and later UMAPs of an object hit
#' .cv_umap_plot_dispatch_cost()'s "light" branch: an inline re-draw, no child
#' process, no heavy memory burst at all.
#'
#' Deliberately conservative -- this runs on a heavy job's completion callback,
#' potentially long after the job launched, by which time the stored object may
#' have been replaced or mutated by something else:
#'   * the handle must still exist and still hold a Seurat object;
#'   * the embedding's cell names must be IDENTICAL, and in identical order, to
#'     the stored object's -- otherwise the embedding belongs to a different
#'     object than the one now under that handle and writing it would silently
#'     mismatch cells to coordinates. A mismatch is skipped, not forced.
#' Any error at all leaves the result untouched: caching an embedding is an
#' optimisation, and it must never be able to fail a plot the user asked for.
#' @param store   the session's object store.
#' @param handle  the source handle(s); only the first is used.
#' @param res     the tool's returned value.
#' The caller-visible result is otherwise returned EXACTLY as the tool built
#' it -- `text` is deliberately not annotated. Caching the embedding is an
#' internal optimisation, not a second thing the tool did on the user's
#' behalf, and the pre-existing end-to-end contract in
#' `test-umap-plot-cost-safe.R` pins `text` to end at the reduction name. That
#' test passing unchanged is the evidence this round changed cost, not
#' behaviour.
#' @return `res` with `reductions_added` removed. Nothing else is modified.
#' @noRd
.cv_persist_returned_reductions <- function(store, handle, res) {
  if (!is.list(res)) return(res)
  added <- res[["reductions_added"]]
  res[["reductions_added"]] <- NULL
  if (is.null(added) || !length(added)) return(res)
  h <- if (is.character(handle) && length(handle)) handle[[1]] else NULL
  if (is.null(h) || is.na(h) || !nzchar(h)) return(res)

  applied <- tryCatch({
    if (!cv_object_exists(store, h)) {
      0L
    } else {
      so <- cv_object_get(store, h)
      if (!methods::is(so, "Seurat")) {
        0L
      } else {
        cells <- colnames(so)
        n <- 0L
        for (nm in names(added)) {
          dr <- added[[nm]]
          if (is.null(dr)) next
          emb <- tryCatch(SeuratObject::Embeddings(dr), error = function(e) NULL)
          if (is.null(emb) || is.null(rownames(emb))) next
          if (!identical(rownames(emb), cells)) next
          so[[nm]] <- dr
          n <- n + 1L
        }
        if (n > 0L) cv_object_update(store, h, so, source = "umapPlot() [embedding cached]")
        n
      }
    }
  }, error = function(e) 0L)

  invisible(applied)  # for tests/telemetry; the result itself is untouched
  res
}

#' Build a result record from a raw returned value, mirroring the inline handlers.
#' Objects go to the store; tables/plots are tagged for rendering downstream.
#' @noRd
cv_result_from_value <- function(store, tool, value, inherit_from = NULL,
                                 typoclust_tissue = NULL) {
  produces <- tool$produces %||% "object"

  # Round XXXIV (Batch 3b item 3): a "plot"-producing heavy tool's child
  # process (umapPlot's heavy_impl, .cv_umap_plot_compute()) returns the SAME
  # list(kind="plot", plot_object=, text=) shape an inline handler would --
  # NOT a bare analysis object. Pass it through unchanged so
  # cv_render_result() finds plot_object exactly where it expects it. Without
  # this, the generic $value fallback below would swallow the whole list
  # under `value`, silently dropping plot_object (the plot would never be
  # rendered to an artifact) and the tool's own descriptive text.
  if (identical(produces, "plot") && is.list(value) &&
      identical(value$kind %||% NA_character_, "plot") &&
      !is.null(value[["plot_object"]])) {
    return(value)
  }

  if (produces == "object" || !is.null(cv_object_type(value)) && cv_object_type(value) %in%
      c("ClustoCell","MarkoClust","MarkoCell","MarkerPurity","DatasetMarkers","TypoClust","Seurat","SingleCellExperiment")) {
    handle <- cv_object_put(store, value,
                            handle = cv_derived_handle(store, value, inherit_from),
                            source = paste0(tool$name, "()"))
    txt <- sprintf("Created %s (handle: %s).",
                   cv_object_descriptor(store, handle)$summary, handle)
    # Round LXIX (audit #23/#24/#25): these two used to be pasted onto `txt`,
    # which is why a results-invalidating cross-tissue caveat and a standing
    # advertisement for the other annotation method arrived as one sentence
    # under one green tick. They are different things and now say so.
    #
    # This is the PRODUCTION path for typoClust (cost = "heavy"); the tool's own
    # inline handler in agent_tools_core.R is the light/direct-call mirror and
    # was changed identically. The two have to be kept in step by hand, which
    # the comment at that site has said since Round XIX -- and a test now
    # asserts they produce the same codes rather than trusting the comment.
    ws <- list()
    if (identical(tool$name, "typoClust")) {
      warn <- .cv_typoclust_tissue_warning(value, typoclust_tissue)
      # may_invalidate: both branches of this advisory say the top-ranked cell
      # type may be wrong, which is the entire result.
      if (nzchar(warn)) ws <- c(ws, list(cv_warn("may_invalidate", warn, "typoclust_tissue")))
    }
    # Round LXXII: the hierarchical-annotation disclosure. Raised here AND in the
    # tool's inline handler through the same helper, because this pair has
    # drifted three times and Round LXIX's rule is that anything raised in both
    # goes through one helper with a test comparing the codes.
    local({
      n <- .cv_inheritance_note(value)
      if (!is.null(n)) ws <<- c(ws, list(cv_warn("info", n, "inherited_major_cluster")))
    })
    # Round LXXIV (audit #16): degenerate-clustering checks, on the production
    # path. Raised through the same helper as the inline handler so the two
    # cannot drift -- Round LXIX's rule for anything raised on both.
    ws <- c(ws, .cv_clustering_warnings(value))
    if (!is.null(tool$result_note) && nzchar(tool$result_note))
      # info: a standing note about method choice, so it is definitionally not
      # a signal about THIS result.
      #
      # Round LXXX (audit #89): once per SESSION. It used to fire on every
      # successful run, so annotating six sub-clusters printed the same
      # paragraph six times. Same helper as the light path in
      # agent_tools_core.R -- a note that is once-per-session on one dispatch
      # path and once-per-call on the other is exactly the drift that has bitten
      # this codebase four times.
      ws <- c(ws, .cv_session_note_once(store, "result_note", tool$result_note))
    return(cv_result_add_warnings(
      list(kind = "object", handle = handle,
           descriptor = cv_object_descriptor(store, handle),
           text = txt),
      ws))
  }
  # Non-object outputs (rare for heavy tools) — return as-is under 'value'.
  list(kind = produces, value = value, text = sprintf("%s completed.", tool$name))
}

#' Cancel a running job (processx kill).
#'
#' Race condition (fixed): the Stop button calls this from the API layer at
#' whatever moment the user clicks it, completely independent of the
#' non-blocking poll() loop in cv_launch_heavy() that normally collects a
#' finished worker's result (poll() only runs every `poll_ms`, and only when
#' its later::later() tick fires). If the background worker process finishes
#' -- successfully or not -- in the window between its last exit-check and
#' cv_job_cancel() being called, the job record still reads status="running"
#' (poll() hasn't caught up yet), even though `rec$process` is no longer
#' alive. Previously this function did not check for that window at all: it
#' killed the (already-dead, so this was a harmless no-op) process handle and
#' unconditionally overwrote the record with status="cancelled" -- discarding
#' the real result. Once overwritten, poll()'s own guard
#' (`rec$status %in% c("done","error","cancelled")`) makes its NEXT tick bail
#' out immediately, so the already-computed object (potentially minutes of
#' compute, e.g. a finished clustoCell run) was silently thrown away and the
#' client was told the job was cancelled when it had actually succeeded.
#'
#' Fix: if the job already reached a terminal state, there is nothing to
#' cancel -- return FALSE without re-emitting a misleading "job_cancelled"
#' over a job that already finished. If the process itself has already
#' exited but the record has not been finalized yet, do NOT mark it
#' cancelled either -- leave the record as-is (still "running") so the
#' already-scheduled poll() tick (every "running" job always has exactly one
#' pending via its own later::later() rescheduling) collects the real
#' result/error moments later, exactly as it would have without a cancel
#' request racing it. Only a genuinely still-alive process is actually
#' killed and marked cancelled.
#' @noRd
cv_job_cancel <- function(session_id, job_id, on_event = NULL) {
  rec <- cv_job_get(session_id, job_id)
  if (is.null(rec)) return(FALSE)
  if (rec$status %in% c("done", "error", "cancelled")) return(FALSE)
  if (!is.null(rec$process)) {
    alive <- tryCatch(rec$process$is_alive(), error = function(e) FALSE)
    if (!alive) {
      # The worker already finished; let the pending poll() tick finalize it
      # with the real result/error instead of clobbering the record here.
      return(FALSE)
    }
    tryCatch(rec$process$kill(), error = function(e) NULL)
  }
  # Round XLVII: keep the LAST OBSERVED progress rather than forcing 100. A
  # cancelled job did not reach 100% of anything, and the Jobs panel rendered
  # the contradiction literally -- "clustoCell: cancelled (100%)". Preserving
  # the real value tells the user how far it actually got before it stopped.
  cv_job_update(session_id, job_id, status = "cancelled",
                message = "cancelled by user", progress = rec$progress %||% 0)
  cv_emit(on_event, "job_cancelled", job = job_id)
  TRUE
}

# ---- Worker failure classification (Round LXXV, audit #30) -------------------
#
# THE ASYMMETRY THE AUDIT NAMES. The LLM side of this product has classified
# memory failures since Round XXXI: `cv_llm_connect_hint()` (agent_llm.R:273)
# pattern-matches "runner has unexpectedly stopped" and "out of memory" and
# answers with a real remedy -- free RAM, lower num_ctx, pick a smaller model.
# The analysis side, which is the half that actually allocates gigabytes, said
# "Worker process failed: <log tail>" for every cause equally.
#
# THE SIGNAL WAS ALREADY THERE AND ALREADY DISCARDED. `p$get_exit_status()` was
# called and immediately compared `== 0`, throwing the value away. processx
# returns the negative signal number for a signal-killed child. Measured in this
# sandbox:
#
#   SIGKILLed child (p$kill())      -> -9
#   Rscript -e "stop('analytical')" ->  1
#   clean exit                      ->  0
#
# So the two failures differ by a value already in hand. An OOM-killed process
# is the hard case precisely because it leaves NO R error in the log -- the
# kernel kills it mid-allocation -- which is why the old code's log-tail branch
# fell through to "exited unexpectedly", the least informative sentence
# available, in exactly the situation that most needs a remedy.
#
# THREE CAUSES, and the classification is deliberately conservative:
#   "memory"     -- killed by a signal, or the log names an allocation failure.
#   "analytical" -- a non-zero exit with an R error in the log. The callee said
#                   what was wrong; repeat it and add nothing.
#   "unknown"    -- non-zero exit, no signal, no readable log.
#
# A signal other than SIGKILL is NOT called memory: SIGTERM/SIGINT are what a
# cancel or a shutdown look like, and claiming "out of memory" there would be a
# confident wrong answer, which is worse than the vague one it replaces.
#
# The remedy voice matches the standing error contract: say what happened and
# what to do next, no apology, no exclamation mark, technical cause carried
# separately in `detail` for the disclosure toggle Round LXIV wired up.

CV_SIGKILL <- 9L

#' Classify a failed worker process and write the message the user reads.
#'
#' @param exit_status integer exit status from processx (negative = signal).
#' @param log_text the worker log tail (may be "").
#' @param tool_name the tool that was running, named in the message.
#' @return list(cause, message, detail) -- `cause` is one of "memory",
#'   "analytical", "unknown" and is what tests assert on, never the prose.
#' @noRd
cv_classify_worker_failure <- function(exit_status, log_text = "", tool_name = "The analysis") {
  st  <- suppressWarnings(as.integer(exit_status))
  log <- as.character(log_text %||% "")
  err <- cv_clean_error(log)
  tn  <- if (is.null(tool_name) || !nzchar(tool_name)) "The analysis" else tool_name

  # An allocation failure that R itself reported before dying.
  mem_in_log <- grepl(paste0("cannot allocate (vector|memory)|std::bad_alloc|",
                             "out of memory|OutOfMemory|Cannot allocate memory|",
                             "vector memory (limit|exhausted)"),
                      log, ignore.case = TRUE, perl = TRUE)
  # Round LXXX (audit #80). Three spellings of "killed by signal N", because
  # which one arrives depends on who reports it:
  #   -N   processx/callr's get_exit_status() -- what THIS codebase receives
  #        today, and the only one that was handled;
  #   128+N  the shell convention, so 137 for SIGKILL;
  #   N      a bare signal number.
  # Writing the test for this family is what surfaced the gap: 137 and 9 both
  # classified as "unknown", which would have told a user whose analysis was
  # OOM-killed to "try running it again" -- the one piece of advice that cannot
  # work. The live path is unchanged; the other two are a widening, not a guess
  # about it.
  killed <- !is.na(st) && (st < 0L || st == CV_SIGKILL || st == 128L + CV_SIGKILL)
  sig    <- if (!is.na(st) && st < 0L) abs(st)
            else if (!is.na(st) && st == 128L + CV_SIGKILL) CV_SIGKILL
            else if (!is.na(st) && st == CV_SIGKILL) CV_SIGKILL
            else NA_integer_
  # Only SIGKILL is read as memory pressure; see the note above.
  oom_kill <- !is.na(sig) && sig == CV_SIGKILL

  if (mem_in_log || oom_kill) {
    msg <- sprintf(paste0(
      "%s ran out of memory and the analysis process was stopped before it finished. ",
      "The machine could not give it as much RAM as the data needed. Try it on fewer ",
      "cells (sketch the object, or run one cluster at a time), close other applications ",
      "-- and if you are running a local model, unload it while the analysis runs, since ",
      "it is holding memory this needs. Nothing was written, so re-running after freeing ",
      "memory is safe."), tn)
    det <- if (oom_kill && !mem_in_log)
      sprintf(paste0("Worker killed by signal %d (SIGKILL) with no R error recorded, which ",
                     "is what an operating-system out-of-memory kill looks like."), sig)
    else if (nzchar(err)) err else "The worker process was killed before it could report an error."
    return(list(cause = "memory", message = msg, detail = det))
  }

  if (nzchar(err)) {
    # The callee said what was wrong. Repeat it and add nothing -- inventing a
    # remedy for an error we have not classified is how a confident wrong answer
    # gets written.
    return(list(cause = "analytical",
                message = sprintf("%s stopped with an error: %s", tn, err),
                detail = err))
  }

  msg <- if (killed)
    sprintf(paste0("%s was stopped by the system before it finished, and left no error ",
                   "behind. If you did not stop it yourself, try running it again."), tn)
  else
    sprintf(paste0("%s ended without finishing and without reporting why. Try running it ",
                   "again; if it keeps happening, the run log has the process's own output."), tn)
  det <- if (!is.na(st))
    sprintf("Worker exited with status %d%s.", st,
            if (killed) sprintf(" (killed by signal %d)", sig) else "")
  else "The worker's exit status could not be read."
  list(cause = "unknown", message = msg, detail = det)
}

# ---- Log helpers (progress heuristics) --------------------------------------

#' Read the entire worker log (bounded).
#' @noRd
cv_read_log <- function(path, max_chars = 2000L) {
  if (is.null(path) || !file.exists(path)) return("")
  txt <- tryCatch(paste(readLines(path, warn = FALSE), collapse = "\n"), error = function(e) "")
  if (nchar(txt) > max_chars) paste0("...", substr(txt, nchar(txt) - max_chars, nchar(txt))) else txt
}

#' Extract a human progress line from the tail of a worker log, if any.
#' CelliVerse uses cli/log_message; we surface the last informative line.
#' @noRd
cv_tail_progress <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  ls <- tryCatch(readLines(path, warn = FALSE), error = function(e) character(0))
  ls <- ls[nzchar(trimws(ls))]
  if (!length(ls)) return(NULL)
  # Strip any ANSI so progress lines surfaced to the client stay clean too.
  cv_strip_ansi(utils::tail(ls, 1))
}

#' The condition a non-blocking dispatcher raises while a heavy job is still
#' running. It inherits from "error" so it unwinds the step, but callers that
#' understand it (cv_run_tool_call, the step machine) re-raise rather than
#' converting it into a tool failure.
#' @noRd
cv_job_pending_condition <- function(job_id) {
  structure(
    class = c("cv_job_pending", "error", "condition"),
    list(message = paste0("heavy job pending: ", job_id), call = NULL, job = job_id))
}

# ---- Dispatcher factory for the agent loop ----------------------------------

#' Build a dispatcher closure for cv_agent_turn().
#'
#' Light tools run inline (fast). Heavy tools are launched on the worker pool;
#' because the agent loop expects a synchronous result to feed back to the LLM,
#' this dispatcher BLOCKS on the heavy job's completion using a bounded wait loop
#' that pumps the event loop (later::run_now). Progress still streams via
#' on_event throughout. This keeps the *agent turn* linear while the *server*
#' stays responsive to other sessions (each turn runs in its own future/promise
#' at the API layer).
#'
#' @param session_id session id.
#' @param on_event   streaming callback.
#' @param timeout_sec per-tool hard timeout.
#' @param async when TRUE, a heavy tool does NOT block. The job is launched once
#'   and the dispatcher raises a `cv_job_pending` condition; every later call for
#'   the SAME tool-call id either returns the finished result or raises pending
#'   again. Round XLVI / Batch B item 7.
#'
#'   WHY THE BLOCKING PATH CANNOT SIMPLY BE MADE POLITE: the wait loop below
#'   pumps `later::run_now()`, which advances `later` callbacks but does NOT
#'   service httpuv HTTP requests -- httpuv is single-threaded and cannot preempt
#'   R. So for as long as R sits in that loop the client is blind, no matter how
#'   often it is pumped. Measured: 26,420 ms for one real umapPlot dispatch, with
#'   all 53 buffered events (including 47 on-schedule progress events) delivered
#'   in a single poll afterwards. The only fix is for R to STOP RUNNING, which
#'   means the turn must suspend -- hence the condition.
#' @noRd
cv_make_dispatcher <- function(session_id, on_event = NULL, timeout_sec = NULL,
                               async = FALSE) {
  config <- cv_session_get(session_id)$config
  timeout_sec <- timeout_sec %||% config$tool_timeout_sec %||% 1800L
  # call-id -> job-id, so a resumed step re-finds the job it already launched
  # instead of starting a second one.
  jobs <- new.env(parent = emptyenv())
  function(tool, call_args, call_id = NULL) {
    store <- cv_session_get(session_id)$object_store
    # Round XXXIV (Batch 3b item 3): a tool with genuinely data-dependent cost
    # (umapPlot: near-free when a reduction already exists, a full
    # RunPCA+RunUMAP pipeline when it doesn't) can supply `dispatch_cost`, a
    # function(store, call_args) consulted PER CALL instead of the static
    # `cost` field. Falls back to the static value on any classifier error so
    # a broken classifier degrades to today's behavior rather than crashing
    # the turn.
    cost <- tool$cost
    if (is.function(tool$dispatch_cost)) {
      cost <- tryCatch(tool$dispatch_cost(store, call_args), error = function(e) tool$cost)
    }
    if (identical(cost, "light")) {
      return(tool$handler(store, call_args))
    }
    # ---- Heavy, NON-BLOCKING (async = TRUE) --------------------------------
    # Launch once, then report "pending" until the job reaches a terminal state.
    # The caller (cv_agent_turn()'s step machine) records its position before
    # dispatching, so the condition unwinds the step cleanly and the same tool
    # call is retried on a later tick, at which point this returns the result.
    if (isTRUE(async)) {
      key <- paste0("call:", call_id %||% "")
      known <- nzchar(call_id %||% "") && exists(key, envir = jobs, inherits = FALSE)
      if (!known) {
        jid <- cv_launch_heavy(session_id, tool, call_args, on_event = on_event)
        # Record a NUMERIC launch time next to the job id. See the timeout check
        # below for why the job record's own `created` field must not be used.
        if (nzchar(call_id %||% ""))
          assign(key, list(id = jid, t0 = Sys.time()), envir = jobs)
        stop(cv_job_pending_condition(jid))
      }
      entry <- get(key, envir = jobs)
      jid <- entry$id
      rec <- cv_job_get(session_id, jid)
      if (is.null(rec)) {
        cli::cli_abort("Tool {.val {tool$name}} lost track of its background job.")
      }
      if (rec$status %in% c("done", "error", "cancelled")) {
        if (identical(rec$status, "done")) return(rec$result)
        cli::cli_abort("Tool {.val {tool$name}} failed: {cv_clean_error(rec$error %||% rec$status)}")
      }
      # Still running. There is no longer a single call frame spanning the wait
      # -- each tick is a fresh invocation of this closure -- so the start time
      # has to be remembered somewhere. It is kept as a NUMERIC Sys.time() in
      # `jobs` above, deliberately NOT read from `rec$created`.
      #
      # ROUND XLVII BUG, caught by test-round44 only because the clock moved:
      # cv_job_new() stores `created = cv_now()`, and cv_now() formats as
      # "%Y-%m-%dT%H:%M:%S%z" -- e.g. "2026-08-12T01:49:27+0000". Base
      # as.POSIXct() does not understand that layout: it silently parses the
      # DATE and discards the time, yielding local midnight. The elapsed time
      # then came out as "seconds since midnight", so a job launched at 01:49
      # measured 6,567s and was cancelled instantly against the 1800s budget --
      # while the very same code passed when run before ~00:30. Every heavy tool
      # would have been killed seconds after starting, for most of the day.
      if (as.numeric(difftime(Sys.time(), entry$t0, units = "secs")) > timeout_sec) {
        cv_job_cancel(session_id, jid, on_event = on_event)
        cli::cli_abort("Tool {.val {tool$name}} exceeded the timeout of {timeout_sec}s and was cancelled.")
      }
      stop(cv_job_pending_condition(jid))
    }

    # ---- Heavy, BLOCKING (async = FALSE; sync path + tests, unchanged) ------
    job_id <- cv_launch_heavy(session_id, tool, call_args, on_event = on_event)
    t0 <- Sys.time()
    repeat {
      # Batch 3b item 2 (found during this round's own full-suite
      # verification): `later::run_now()` pumps the ONE process-wide event
      # loop, which can also hold callbacks left behind by a completely
      # unrelated turn/session (e.g. one already torn down elsewhere) --
      # this is the same shared-loop reentrancy fragility already tracked as
      # the still-open Round XXIX finding, whose full redesign is deferred to
      # a later round. If one of those unrelated callbacks errors (observed
      # live: "Unknown session id" from a stale turn poller), the error
      # otherwise propagates all the way up through run_now() and aborts THIS
      # tool's wait loop even though it has nothing to do with this job.
      # Swallow it here, same defensive pattern already used at the other
      # run_now() pump site (cv_api_chat_poll(), agent_api.R) -- this loop
      # already re-checks its OWN job's status and timeout on every
      # iteration, so a skipped pump just means the next 0.2s tick tries
      # again; it does not mask a failure of the job this call is waiting on.
      tryCatch(later::run_now(timeoutSecs = 0.2), error = function(e) NULL)
      rec <- cv_job_get(session_id, job_id)
      if (!is.null(rec) && rec$status %in% c("done", "error", "cancelled")) break
      if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout_sec) {
        cv_job_cancel(session_id, job_id, on_event = on_event)
        cli::cli_abort("Tool {.val {tool$name}} exceeded the timeout of {timeout_sec}s and was cancelled.")
      }
    }
    rec <- cv_job_get(session_id, job_id)
    if (identical(rec$status, "done")) return(rec$result)
    cli::cli_abort("Tool {.val {tool$name}} failed: {cv_clean_error(rec$error %||% rec$status)}")
  }
}
