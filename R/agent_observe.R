# =============================================================================
# CelliVerse Agent — local observability (Round LXXX, audit #68-#71)
#
# WHY THIS FILE EXISTS, and what it deliberately is not.
#
# `agent_utils.R` line 3 has advertised "logging" among its shared helpers since
# the file was written, and there was no logger anywhere in the package. Light
# tools left no record at all; heavy tools left a per-job stdout file that is
# discarded with the session. Every performance number in CHANGES.md was
# obtained by writing a bespoke harness AFTER someone complained, because the
# ordinary run produced nothing to look at. And every adapter parses the
# provider's `usage` block and returns it, and nothing in the entire codebase
# reads it -- so on a metered default provider there was no answer at all to
# "what did that clustoCell turn cost me?".
#
# THIS IS LOCAL AND ONLY LOCAL. Nothing here opens a socket, and nothing here
# may ever be made to. The audit's category 3b item 1 rules out networked
# telemetry outright for this product -- it is a research tool handling
# unpublished and potentially clinical data, and usage analytics that left the
# machine would be a betrayal of its premise. The local version is item 71 and
# is worth doing; the networked version is not, at any scale. A future reader
# looking for the "send this somewhere" hook should stop here: its absence is
# the design.
#
# The log lives under ~/.celliverse/logs/ as newline-delimited JSON, one object
# per line, because that is the format you can answer a question with using
# tools the user already has -- `jq`, `readLines`, `grep` -- without this
# package's help and without a schema migration when a field is added.
# =============================================================================

#' Directory holding the local event log.
#' @noRd
cv_log_dir <- function() file.path(cv_home_dir(), "logs")

#' Path to the current event log file.
#'
#' One file per DAY rather than one per session or one forever. Per-session
#' would make "how often does clustoCell fail?" a question about forty files;
#' one-forever grows without bound, which is the failure Round LXI and Batch 3b
#' spent two rounds fixing everywhere else in this package.
#' @noRd
cv_log_path <- function(day = format(Sys.Date(), "%Y-%m-%d")) {
  file.path(cv_log_dir(), sprintf("agent-%s.jsonl", day))
}

#' How many days of log files to keep.
#'
#' Bounded on purpose, and bounded by AGE rather than size: the question this
#' log answers ("was it always this slow?") is a question about recent history,
#' and a size cap would silently drop the oldest events mid-file.
#' @noRd
CV_LOG_KEEP_DAYS <- 30L

#' Is local event logging on?
#'
#' ON by default, because a log nobody enabled is a log nobody has when they
#' need it -- and this one never leaves the machine, so the usual reason to
#' default telemetry off does not apply. `CELLIVERSE_NO_LOG=1` turns it off for
#' anyone who wants a completely silent install.
#' @noRd
cv_logging_enabled <- function() {
  off <- Sys.getenv("CELLIVERSE_NO_LOG", unset = "")
  !(nzchar(off) && !identical(tolower(off), "0") && !identical(tolower(off), "false"))
}

#' Append one event to the local JSONL log.
#'
#' NEVER THROWS, and never warns. Every call site is on a path that is doing
#' something the user asked for; a logger that can fail a turn is worse than no
#' logger, and this one is invoked from inside error handlers where a second
#' error would replace the real one.
#'
#' Fields are written flat so a line can be read without knowing the schema:
#' `ts`, `event`, `session`, plus whatever the caller passes. NULLs are dropped
#' rather than serialized as `{}` -- the same reason `cv_json_sanitize()` exists
#' for the API layer, and it is reused here rather than reimplemented.
#'
#' @param event short event name, e.g. "turn", "tool", "llm".
#' @param session session id, or NULL.
#' @param ... additional flat fields.
#' @return invisibly TRUE if written, FALSE otherwise.
#' @noRd
cv_log_event <- function(event, session = NULL, ...) {
  if (!cv_logging_enabled()) return(invisible(FALSE))
  tryCatch({
    rec <- c(list(ts = cv_now(), event = as.character(event)[1]),
             if (!is.null(session)) list(session = as.character(session)[1]) else NULL,
             list(...))
    rec <- cv_json_sanitize(rec)
    dir.create(cv_log_dir(), recursive = TRUE, showWarnings = FALSE)
    line <- jsonlite::toJSON(rec, auto_unbox = TRUE, null = "null", digits = 6)
    # append = TRUE with a single write is atomic enough for one process; the
    # agent is single-writer by construction (one R session owns ~/.celliverse).
    cat(line, "\n", sep = "", file = cv_log_path(), append = TRUE)
    invisible(TRUE)
  }, error = function(e) invisible(FALSE))
}

#' Delete log files older than `keep_days`.
#'
#' Called opportunistically from the same place sessions are pruned, so it costs
#' nothing on a normal turn.
#' @noRd
cv_log_prune <- function(keep_days = CV_LOG_KEEP_DAYS) {
  tryCatch({
    d <- cv_log_dir()
    if (!dir.exists(d)) return(invisible(0L))
    fs <- list.files(d, pattern = "^agent-\\d{4}-\\d{2}-\\d{2}\\.jsonl$", full.names = TRUE)
    if (!length(fs)) return(invisible(0L))
    days <- as.Date(sub("^agent-", "", sub("\\.jsonl$", "", basename(fs))))
    old <- fs[!is.na(days) & days < (Sys.Date() - keep_days)]
    if (length(old)) unlink(old)
    invisible(length(old))
  }, error = function(e) invisible(0L))
}

# ---- Token usage (audit #68) -------------------------------------------------

#' Normalise a provider's `usage` block into three integers.
#'
#' Round LXXX (audit #68). Every adapter already parses `usage` and returns it
#' on the response, and NOTHING read it -- so with a metered default provider
#' the product could not answer "what does a turn cost?" at all.
#'
#' The three providers spell it three ways, which is why this exists rather than
#' a field access at the call site:
#'   OpenAI-compatible : prompt_tokens / completion_tokens / total_tokens
#'   Anthropic         : input_tokens / output_tokens (no total)
#'   Gemini            : promptTokenCount / candidatesTokenCount / totalTokenCount
#' Ollama reports nothing, and gets NA rather than 0 -- a local model genuinely
#' has no token cost, and recording a confident zero would make an average over
#' mixed providers wrong in a way nobody would notice.
#'
#' @param usage the `usage` element of a cv_chat() response.
#' @return list(prompt=, completion=, total=), each an integer or NA_integer_.
#' @noRd
cv_usage_tokens <- function(usage) {
  na <- list(prompt = NA_integer_, completion = NA_integer_, total = NA_integer_)
  if (is.null(usage) || !is.list(usage)) return(na)
  pick <- function(...) {
    for (k in c(...)) {
      v <- usage[[k]]
      if (!is.null(v) && length(v) == 1L) {
        n <- suppressWarnings(as.integer(v))
        if (!is.na(n)) return(n)
      }
    }
    NA_integer_
  }
  p <- pick("prompt_tokens", "input_tokens", "promptTokenCount")
  c_ <- pick("completion_tokens", "output_tokens", "candidatesTokenCount")
  t <- pick("total_tokens", "totalTokenCount")
  # Anthropic reports no total. Deriving it is arithmetic on two numbers the
  # provider gave us, not an estimate, so it is safe to fill in.
  if (is.na(t) && !is.na(p) && !is.na(c_)) t <- p + c_
  list(prompt = p, completion = c_, total = t)
}

#' Accumulate token usage across the LLM calls of one turn.
#'
#' A turn is several round-trips (one per tool-calling iteration plus the final
#' interpretation), so the number a user cares about is the SUM, not the last
#' call's. Kept as a small mutable accumulator rather than a running total on
#' the turn record, because the loop that makes the calls does not own that
#' record.
#' @noRd
cv_usage_acc_new <- function() {
  e <- new.env(parent = emptyenv())
  e$prompt <- 0L; e$completion <- 0L; e$total <- 0L; e$calls <- 0L; e$known <- 0L
  e
}

#' @rdname cv_usage_acc_new
#' @noRd
cv_usage_acc_add <- function(acc, usage) {
  if (!is.environment(acc)) return(invisible(acc))
  u <- cv_usage_tokens(usage)
  acc$calls <- acc$calls + 1L
  # A call whose provider reported nothing still counts as a CALL but not as a
  # measured one, so `known < calls` is the honest marker that the totals below
  # are a floor rather than the whole story.
  if (!is.na(u$total)) {
    acc$known <- acc$known + 1L
    acc$prompt     <- acc$prompt     + (if (is.na(u$prompt)) 0L else u$prompt)
    acc$completion <- acc$completion + (if (is.na(u$completion)) 0L else u$completion)
    acc$total      <- acc$total      + u$total
  }
  invisible(acc)
}

#' @rdname cv_usage_acc_new
#' @noRd
cv_usage_acc_get <- function(acc) {
  if (!is.environment(acc)) return(NULL)
  list(prompt_tokens = acc$prompt, completion_tokens = acc$completion,
       total_tokens = acc$total, llm_calls = acc$calls,
       llm_calls_with_usage = acc$known)
}

# ---- Per-phase timing (audit #69) --------------------------------------------

#' A per-turn phase timer.
#'
#' Round LXXX (audit #69). Every performance number in this project's history
#' was produced by a bespoke harness written AFTER a complaint -- the session
#' snapshot being O(n^2), the gzip freeze, the poll latency. Each investigation
#' started by building the measurement, which is the expensive part and which is
#' now always there.
#'
#' Three phases, because three is what the shape of a turn actually has and a
#' finer breakdown would be a number nobody acts on: time inside the MODEL,
#' time inside TOOLS, and everything else (`other` = wall clock minus the two,
#' which is rendering, serialization and the loop itself). `other` is derived
#' rather than measured so the three always sum to the wall clock and cannot
#' drift into implying time was unaccounted for.
#' @noRd
cv_phase_timer_new <- function() {
  e <- new.env(parent = emptyenv())
  e$start <- Sys.time(); e$llm <- 0; e$tool <- 0
  e
}

#' Run `expr`, adding its elapsed seconds to `phase` on the timer.
#' @noRd
cv_phase_time <- function(timer, phase, expr) {
  if (!is.environment(timer)) return(force(expr))
  t0 <- Sys.time()
  on.exit({
    d <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    timer[[phase]] <- (timer[[phase]] %||% 0) + d
  }, add = TRUE)
  force(expr)
}

#' @rdname cv_phase_timer_new
#' @noRd
cv_phase_timer_get <- function(timer) {
  if (!is.environment(timer)) return(NULL)
  wall <- as.numeric(difftime(Sys.time(), timer$start, units = "secs"))
  rnd <- function(x) round(as.numeric(x), 3)
  list(wall_sec = rnd(wall), llm_sec = rnd(timer$llm), tool_sec = rnd(timer$tool),
       other_sec = rnd(max(0, wall - timer$llm - timer$tool)))
}

# ---- Turn summary (audit #71) ------------------------------------------------

#' Summarise the local log into the three numbers it exists to answer.
#'
#' Round LXXX (audit #71). Reads the JSONL back and derives completion rate,
#' tool-failure rate and latency. Deliberately DERIVED from the log rather than
#' maintained as a counter alongside it: a counter and a log can disagree, and
#' when they do the log is right, so there is no reason to keep both.
#'
#' Returns NULL when there is nothing to summarise, rather than a record of
#' zeroes -- "0% of turns completed" and "no turns yet" are different statements
#' and only one of them is true on a fresh install.
#' @param days how many days back to read.
#' @return list of summary fields, or NULL.
#' @noRd
cv_log_summary <- function(days = 7L) {
  tryCatch({
    d <- cv_log_dir()
    if (!dir.exists(d)) return(NULL)
    want <- format(Sys.Date() - seq_len(max(1L, days)) + 1L, "%Y-%m-%d")
    fs <- file.path(d, sprintf("agent-%s.jsonl", want))
    fs <- fs[file.exists(fs)]
    if (!length(fs)) return(NULL)
    lines <- unlist(lapply(fs, readLines, warn = FALSE), use.names = FALSE)
    lines <- lines[nzchar(trimws(lines))]
    if (!length(lines)) return(NULL)
    recs <- lapply(lines, function(l)
      tryCatch(jsonlite::fromJSON(l, simplifyVector = FALSE), error = function(e) NULL))
    recs <- Filter(Negate(is.null), recs)
    if (!length(recs)) return(NULL)

    ev <- vapply(recs, function(r) as.character(r$event %||% ""), character(1))
    turns <- recs[ev == "turn"]
    tools <- recs[ev == "tool"]

    num <- function(xs, f) {
      v <- vapply(xs, function(r) {
        x <- f(r)
        if (is.null(x) || length(x) != 1L) return(NA_real_)
        suppressWarnings(as.numeric(x))
      }, numeric(1))
      v[is.finite(v)]
    }
    st <- vapply(turns, function(r) as.character(r$status %||% ""), character(1))
    ts <- vapply(tools, function(r) as.character(r$status %||% ""), character(1))
    wall <- num(turns, function(r) r$wall_sec)
    toks <- num(turns, function(r) r$total_tokens)

    list(
      days = days,
      turns = length(turns),
      turns_completed = sum(st == "done"),
      turns_failed = sum(st == "error"),
      turns_cancelled = sum(st == "cancelled"),
      completion_rate = if (length(turns)) round(sum(st == "done") / length(turns), 3) else NA_real_,
      tool_calls = length(tools),
      tool_failures = sum(ts == "error"),
      tool_failure_rate = if (length(tools)) round(sum(ts == "error") / length(tools), 3) else NA_real_,
      # Median, not mean: one 30-minute clustering run would drag a mean into
      # describing a turn nobody had.
      median_turn_sec = if (length(wall)) round(stats::median(wall), 2) else NA_real_,
      p90_turn_sec = if (length(wall)) round(unname(stats::quantile(wall, 0.9)), 2) else NA_real_,
      total_tokens = if (length(toks)) sum(toks) else NA_real_,
      # The honest marker: how many turns actually carried a token figure. A
      # local-model session reports none, and a total over three of forty turns
      # must not read as a total over forty.
      turns_with_tokens = length(toks)
    )
  }, error = function(e) NULL)
}

# ---- What the agent can do, and what to try (audit #60 + #61) ---------------

#' The example prompts shown on the first screen.
#'
#' Round LXXX (audit #61). Good example prompts already existed -- inside
#' `cv_system_prompt()`, where only the MODEL ever saw them -- while the empty
#' chat screen offered two, in prose, that could not be clicked.
#'
#' This is the single source of truth, served to the client over `/api/intro`.
#' A second copy in TypeScript is how a screen ends up advertising a phrasing
#' the parser no longer supports; Round XLIX already had one hardcoded frontend
#' list drift from its R source (the upload `accept=` filter offered only .rds
#' while seven formats were readable).
#'
#' Ordered as a WORKFLOW, not as a feature list: someone reading them in order
#' is reading the pipeline this package is for. The last two are deliberately
#' the ones a new user does not know they can ask.
#' @noRd
cv_example_prompts <- function() {
  list(
    list(label = "Cluster this dataset",
         text  = "cluster this dataset"),
    list(label = "Cluster and annotate the cell types",
         text  = "cluster this dataset and annotate the cell types"),
    list(label = "Top 10 markers of C1",
         text  = "show the top 10 ranked markers of C1"),
    list(label = "UMAP coloured by cluster",
         text  = "draw a UMAP coloured by the ClustoCell clusters"),
    list(label = "Is CD8A a marker of C1, and how pure?",
         text  = "is CD8A a marker of C1, and what is its purity?"),
    list(label = "Annotate with the LLM instead",
         text  = "annotate the clusters using the LLM method"),
    list(label = "What can you do?",
         text  = "what can you do?")
  )
}

#' One line per thing the agent can actually do.
#'
#' Round LXXX (audit #60). The onboarding card was "entirely about API keys and
#' contains not one sentence about what it can analyse" -- a first screen that
#' explains how to pay for something without saying what it is.
#'
#' Phrased as CAPABILITIES with the tool that provides each, because the names
#' are what the user will see on the tool cards afterwards, and a first screen
#' that teaches the vocabulary of the transcript is doing two jobs at once.
#' @noRd
cv_capability_lines <- function() {
  c(
    "Cluster single-cell data with clustoCell (EWCSR), including sub-clusters.",
    "Find and rank marker genes per cluster, sub-cluster or custom cell subset.",
    "Score marker PURITY for a cluster, and say so plainly when a marker is not pure.",
    "Annotate cell types from the curated CelliVerse Marker DB, or with an LLM.",
    "Draw UMAPs, marker dot plots, annotation plots and signature heatmaps.",
    "Export every result as CSV, and every figure as SVG, PDF and 300-dpi PNG."
  )
}
