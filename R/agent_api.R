# =============================================================================
# CelliVerse Agent - API handler layer (transport-agnostic core)
#
# These functions implement the *logic* of each endpoint independent of Plumber
# so they can be unit-tested without a running server. inst/plumber/plumber.R is
# a thin routing shell that parses the request and calls into here.
#
# Endpoints (see plumber.R for the routes):
#   GET  /health                 -> cv_api_health()
#   GET  /registry               -> cv_api_registry()
#   GET  /settings               -> cv_api_get_settings()
#   POST /settings               -> cv_api_update_settings(body)
#   GET  /ollama/models          -> cv_api_ollama_models()
#   POST /session                -> cv_api_new_session()
#   GET  /session/:id            -> cv_api_get_session(id)
#   GET  /sessions               -> cv_api_list_sessions()
#   POST /objects/load           -> cv_api_load_object(session, body)
#   POST /objects/upload         -> cv_api_upload_object(session, file)
#   GET  /objects                -> cv_api_list_objects(session)
#   GET  /objects/:handle        -> cv_api_object_detail(session, handle)
#   POST /chat            (SSE)   -> cv_api_chat_stream(session, message, con)
#   POST /chat/sync              -> cv_api_chat_sync(session, message)   [tests]
#   GET  /jobs                    -> cv_api_list_jobs(session)
#   GET  /jobs/:id                -> cv_api_job_status(session, id)
#   POST /jobs/:id/cancel        -> cv_api_cancel_job(session, id)
#   GET  /artifacts/:id          -> handled in plumber.R (file serving)
# =============================================================================

#' Standard JSON envelope for API responses.
#' @noRd
cv_api_ok <- function(data = list(), ...) c(list(ok = TRUE, data = data), list(...))
#' @noRd
cv_api_err <- function(message, status = 400L, detail = NULL) {
  out <- list(ok = FALSE, error = message, status = status)
  # Batch 8b: an OPTIONAL second field carrying the underlying technical text.
  # The user's chosen shape: a calm sentence in `error`, the raw cause in
  # `detail`, revealed by the UI behind a toggle rather than shown by default.
  # Omitted entirely when there is nothing extra to say, so the common case
  # stays exactly the two-field envelope every existing client already reads.
  if (!is.null(detail) && nzchar(detail)) out$detail <- detail
  out
}

# Round LV (Batch 5a) removed cv_api_err_status(), correctly -- it had zero call
# sites. Its deletion note claimed the routes it was written for "each set
# res$status inline from the envelope's own status, so deleting it leaves no
# route without an HTTP status."
#
# Batch 8b MEASURED that claim and it was false: 3 routes of 26 did so; the
# other 22 returned HTTP 200 carrying a 404 in the body. The deletion was still
# right -- dead code is dead -- but the justification described a world that did
# not exist, which is why the status is now applied by cv_status() in plumber.R
# for every route at once instead of by each route remembering.

#' The top-level API error boundary (Batch 8b).
#'
#' Installed via `plumber::pr_set_error()` in `cv_build_router()`. Before it
#' existed, an unhandled throw anywhere in a handler produced plumber's default
#' `{"error":"500 - Internal server error"}` -- a dead end for the user and for
#' whoever they report it to.
#'
#' Returns the standard error envelope so a client parses a crash exactly as it
#' parses an expected failure: `ok`, `error` (a calm sentence), `status`, and
#' `detail` (the underlying R message). It is deliberately generic about CAUSE,
#' because anything arriving here was by definition not anticipated, and
#' specific about CONSEQUENCE -- "your session is intact" is the thing a user
#' actually wants to know, and it is true: a handler throwing does not disturb
#' the session registry or the object store.
#' @param req,res the plumber request/response.
#' @param err the condition that was raised.
#' @return the error envelope, serialized by plumber as JSON.
#' @noRd
cv_api_error_boundary <- function(req, res, err) {
  res$status <- 500L
  detail <- tryCatch(conditionMessage(err), error = function(e) "")
  # cv_clean_error() strips the R call prefix ("Error in f(x): ") that makes a
  # message read like a stack trace rather than a sentence.
  detail <- tryCatch(cv_clean_error(detail), error = function(e) detail)
  path <- tryCatch(as.character(req$PATH_INFO %||% ""), error = function(e) "")

  # Round LXXXIV, from live use. R CANNOT HOLD A REQUEST BODY OVER 2 GiB.
  #
  # The user uploaded the same 3.8 GB file through the browser and got the
  # generic 500 again; the technical details this round finally showed said:
  #
  #   long vectors not supported yet: .../Rinlinedfuns.h:551
  #
  # That is a HARD limit of the transport, not a shortage of memory and not
  # something a bigger machine fixes: plumber reads the whole multipart body
  # into ONE raw vector, and several of the C paths that touch it are indexed by
  # a 32-bit int, so anything at or above 2^31 bytes throws no matter what.
  # Round LXXXII's size warning was right to point at the path box and wrong to
  # imply the upload might still work if you had the RAM.
  #
  # Recognised here rather than in the upload handler because the throw happens
  # in plumber's own body parser, BEFORE any of our code runs -- the error
  # boundary is the only place that sees it.
  if (grepl("long vector", detail, ignore.case = TRUE) ||
      grepl("cannot allocate|vector memory", detail, ignore.case = TRUE)) {
    out <- cv_api_err(paste0(
      "That file is too large to send through the browser: R cannot hold a ",
      "request body of 2 GB or more in one piece, whatever the machine's ",
      "memory. Nothing is wrong with the file or with your session. Use the ",
      "\"load a file already on the server\" box below instead - it reads the ",
      "file directly and has no size ceiling."),
      status = 413L, detail = if (nzchar(detail)) detail else NULL)
    # A flag the UI can act on, rather than prose a button has to parse.
    out$use_path_box <- TRUE
    res$status <- 413L
    return(out)
  }

  cv_api_err(
    paste0("Something went wrong on the server",
           if (nzchar(path)) paste0(" while handling ", path) else "",
           ". Your session and everything loaded in it are intact - it is worth ",
           "trying again."),
    status = 500L,
    detail = if (nzchar(detail)) detail else NULL
  )
}

# ---- Meta -------------------------------------------------------------------

#' @noRd
cv_api_health <- function() {
  ver <- tryCatch(as.character(utils::packageVersion("celliverse")),
                  error = function(e) "dev")
  cv_api_ok(list(
    status = "running",
    package = "celliverse",
    version = ver,
    providers = .cv_supported_providers,
    time = cv_now()
  ))
}

#' @noRd
cv_api_registry <- function(include_advanced = FALSE) {
  cv_api_ok(list(tools = cv_registry_metadata(cv_registry())))
}

# ---- Settings ---------------------------------------------------------------

#' @noRd
cv_api_get_settings <- function() {
  cfg <- cv_load_config()
  cv_api_ok(cv_config_public(cfg))
}

#' List locally-installed Ollama models (defensive; never errors).
#'
#' Queries the local Ollama daemon's `GET {host}/api/tags` and returns the model
#' names it reports. If Ollama is not running / not installed / unreachable, or
#' the response cannot be parsed, returns an EMPTY installed list plus a
#' `reachable = FALSE` flag so the UI can show a "not installed" hint instead of
#' erroring. Also echoes the recommended light/strong tier ids so the Settings
#' page can flag which recommended models are (not) present.
#' @noRd
cv_api_ollama_models <- function(cfg = cv_load_config()) {
  host <- cfg$ollama_host %||% "http://localhost:11434"
  tiers <- list(light = .cv_model_tiers$light,
                recommended = .cv_model_tiers$recommended,
                strong = .cv_model_tiers$strong)
  res <- tryCatch({
    url <- paste0(sub("/+$", "", host), "/api/tags")
    resp <- httr2::request(url) |>
      httr2::req_timeout(2) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()
    if (httr2::resp_status(resp) >= 400) {
      list(reachable = FALSE, installed = character(0))
    } else {
      body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
      models <- body$models
      names_vec <- if (is.data.frame(models)) {
        as.character(models$name %||% character(0))
      } else if (is.list(models) && length(models)) {
        vapply(models, function(m) as.character(m$name %||% ""), character(1))
      } else character(0)
      names_vec <- names_vec[nzchar(names_vec)]
      list(reachable = TRUE, installed = unname(names_vec))
    }
  }, error = function(e) list(reachable = FALSE, installed = character(0)))

  installed <- res$installed
  # Normalized view of installed names for tier matching: Ollama reports full
  # names like "dengcao/Qwen3-30B-A3B-Instruct-2507:latest" and "qwen3:8b" may
  # be stored as "qwen3:8b" or pulled as "qwen3:8b" (implicit :latest). Match
  # case-insensitively and ignore a trailing ":latest" on EITHER side so a
  # community repack of the same weights still counts as installed.
  norm <- function(x) tolower(sub(":latest$", "", x %||% ""))
  installed_norm <- norm(installed)
  tier_hit <- function(id) {
    id_norm <- norm(id)
    hit <- installed_norm == id_norm
    # Also accept "anynamespace/<base-id>" community repacks (e.g. a manual
    # dengcao/ pull of the recommended weights).
    base <- sub("^[^/]+/", "", id_norm)
    hit <- hit | (grepl("/", installed_norm, fixed = TRUE) &
                    sub("^[^/]+/", "", installed_norm) == base)
    any(hit)
  }
  cv_api_ok(list(
    reachable = isTRUE(res$reachable),
    installed = as.list(installed),
    tiers = tiers,
    # Convenience flags for the UI (is each recommended tier present?).
    has_light       = tier_hit(tiers$light),
    has_recommended = tier_hit(tiers$recommended),
    has_strong      = tier_hit(tiers$strong)
  ))
}

#' Update settings live (no restart). Writes to config.json and returns the
#' public (redacted) config.
#'
#' Provider/model/temperature/keys/host all hot-swap on the NEXT turn: the agent
#' loop re-reads the on-disk config each turn (cv_effective_config) and overlays
#' the routing/operational fields onto every session, so a change reaches even a
#' session that was already open. New sessions also read fresh config. As a
#' belt-and-suspenders step we additionally push the hot-swappable fields into
#' any in-memory sessions right now, so /api/session reflects the change
#' immediately (not only after the next turn).
#' @noRd
cv_api_update_settings <- function(body) {
  cfg <- cv_load_config()
  # Round XXXIV: the provider key fields in this allow-list now come from
  # .cv_provider_registry instead of being hand-listed here.
  allowed <- c("default_provider", "default_model", "temperature", "top_p", "seed",
               .cv_provider_key_fields(),
               "ollama_host", "lmstudio_host", "ollama_keep_alive", "ollama_num_ctx",
               "worker_pool_size", "max_tool_iters", "tool_timeout_sec",
               "history_token_budget", "max_repeat_stall")
  for (nm in intersect(names(body), allowed)) cfg[[nm]] <- body[[nm]]
  if (!is.null(cfg$default_provider) && !(cfg$default_provider %in% .cv_supported_providers)) {
    return(cv_api_err(sprintf(
      "'%s' is not a provider I recognise. Pick one from the list in Settings.",
      cfg$default_provider)))
  }
  cv_save_config(cfg)
  # Propagate hot-swappable fields into live in-memory sessions immediately.
  tryCatch(cv_sessions_apply_config(cfg), error = function(e)
    cli::cli_warn("Could not propagate settings to live sessions: {conditionMessage(e)}"))
  cv_api_ok(cv_config_public(cfg))
}

#' Round LXXX (audit #71): the local turn summary.
#'
#' Derived from the JSONL event log on demand rather than kept as a live
#' counter, because a counter and a log can disagree and the log is the one
#' that is right.
#'
#' Returns `summary = NULL` when there is nothing to summarise. A fresh install
#' should say "no turns yet", not "0% of turns completed" -- those are different
#' statements and only one of them is true.
#' @noRd
cv_api_log_summary <- function(days = 7L) {
  d <- suppressWarnings(as.integer(days))
  if (is.na(d) || d < 1L) d <- 7L
  if (d > 365L) d <- 365L
  cv_api_ok(list(summary = cv_log_summary(d),
                 enabled = cv_logging_enabled(),
                 log_dir = cv_log_dir(),
                 keep_days = CV_LOG_KEEP_DAYS))
}

#' Round LXXX (audit #63): load the bundled demo dataset into a session.
#'
#' The audit's complaint was that a fully-configured user still had NOTHING to
#' run: the onboarding card ends with a working API key and an empty object
#' list, and the next step requires them to go and find a .rds. That is the
#' point at which a first-time user leaves.
#'
#' `SeuratObject::pbmc_small` needs no new dependency -- SeuratObject is what
#' Seurat itself is built on, and this package already calls
#' `SeuratObject::LayerData()`. It is 80 cells x 230 genes, which is the
#' relevant property: everything downstream of it finishes in seconds, so the
#' demo teaches the WORKFLOW without a first-time user's first experience being
#' a thirty-minute clustering run.
#'
#' Deliberately loaded as an ordinary object through the ordinary path, with an
#' ordinary handle. A demo that lived in a special mode would teach a workflow
#' the real one does not have.
#' @noRd
cv_api_load_demo <- function(session_id) {
  if (!cv_session_exists(session_id)) return(cv_api_err(
    "That session is not available any more. Reload the page to start a fresh one.",
    404L))
  if (!requireNamespace("SeuratObject", quietly = TRUE)) {
    return(cv_api_err(
      paste("The demo dataset lives in the SeuratObject package, which is not",
            "installed here. Install it with install.packages(\"SeuratObject\"),",
            "or upload your own dataset under Data / Upload."),
      400L))
  }
  obj <- tryCatch({
    e <- new.env(parent = emptyenv())
    utils::data("pbmc_small", package = "SeuratObject", envir = e)
    get("pbmc_small", envir = e)
  }, error = function(e) NULL)
  if (is.null(obj)) return(cv_api_err(
    "The demo dataset could not be loaded from the SeuratObject package.", 500L,
    detail = "utils::data(\"pbmc_small\", package = \"SeuratObject\") returned nothing."))
  store <- cv_session_get(session_id)$object_store
  h <- cv_object_put(store, obj, handle = "obj_demo_pbmc", source = "SeuratObject::pbmc_small")
  tryCatch(cv_session_snapshot(session_id), error = function(e) NULL)
  cv_api_ok(list(
    handle = h,
    descriptor = cv_object_descriptor(store, h),
    note = paste("Loaded the demo dataset (pbmc_small: 80 cells, 230 genes) as",
                 h, "- small enough that every step finishes in seconds.")))
}

#' Round LXXX (audit #61 + #62): the example prompts and the format list, from
#' the server.
#'
#' The audit's point on #61 was that the GOOD example prompts already existed --
#' parser-validated, inside `cv_system_prompt()` -- "where only the model sees
#' them". The empty chat screen offered exactly two, in prose, unclickable.
#'
#' Served from R rather than retyped in the client so there is ONE list. A
#' second copy in TypeScript is how the client ends up advertising a phrasing
#' the parser stopped supporting -- and this project has already had one
#' hardcoded frontend list drift from its R source (Round XLIX's upload
#' `accept=` filter, which offered only .rds while seven formats were readable).
#'
#' `formats` comes from `cv_supported_formats()`, which is the same function the
#' upload path checks against, so the screen cannot understate what the build
#' can read -- which is #62's complaint exactly.
#' @noRd
cv_api_intro <- function() {
  cv_api_ok(list(
    examples = cv_example_prompts(),
    formats = cv_supported_formats(),
    can_do = cv_capability_lines()
  ))
}

# ---- Large datasets (Round LXXXII) ------------------------------------------

#' Advice on sending a file of a given size through the browser.
#'
#' Called on FILE SELECTION, not on submit: `File.size` is known to the browser
#' without reading a byte, so the answer arrives before the upload rather than
#' after it has spent several GB failing. See `cv_upload_advice()` for the
#' derivation and for why this informs rather than refuses.
#' @noRd
cv_api_upload_advice <- function(bytes) {
  cv_api_ok(cv_upload_advice(bytes))
}

#' Build a Seurat from a loaded matrix, on demand.
#'
#' Round LXXXIII. The "Build it anyway" half of the Upload page's decline, and
#' the same code path the `toSeurat` tool uses -- one implementation, three
#' doors. It exists because Round LXXXII made the automatic conversion a
#' DECISION rather than a WARNING: the agent declined on a 24 GB Mac that could
#' have done it easily, and left the user with no way to say "do it anyway"
#' short of a chat message that resolved to nothing.
#' @noRd
cv_api_to_seurat <- function(session_id, handle, name = NULL) {
  if (!cv_session_exists(session_id)) return(cv_api_err(
    "That session is not available any more. Reload the page to start a fresh one.", 404L))
  store <- cv_session_get(session_id)$object_store
  out <- tryCatch(cv_build_seurat_from_handle(store, handle, name), error = function(e) e)
  if (inherits(out, "error")) return(cv_api_err(cv_clean_error(conditionMessage(out)), 400L))
  tryCatch(cv_session_snapshot(session_id), error = function(e) NULL)
  cv_api_ok(list(handle = out$handle, descriptor = out$descriptor, note = out$text))
}

# ---- Saved prompts (Round LXXXI, E2) ----------------------------------------
#
# Three routes, one payload. Every one of them returns the WHOLE list, so the
# client never merges server state into its own copy and two open tabs cannot
# drift -- the same contract /settings has used since Batch 1.
#
# `builtin` travels with each row because the rail needs it: removing a starter
# hides it, removing your own deletes it, and the button that does it should say
# which. Deciding that in TypeScript from the id prefix would put the rule in
# two places.

#' @noRd
cv_api_prompts <- function() {
  store <- cv_prompts_load()
  prompts <- cv_prompts_all(store)
  cv_api_ok(list(prompts = prompts,
                 categories = cv_prompt_categories(prompts),
                 # How many starters are currently hidden, so the rail can offer
                 # to put them back ONLY when there is something to put back.
                 hidden = length(store$hidden),
                 max = CV_PROMPTS_MAX))
}

#' @noRd
cv_api_prompt_add <- function(body) {
  if (!is.list(body)) body <- list()
  out <- tryCatch(
    cv_prompts_add(label = body[["label"]], text = body[["text"]],
                   category = body[["category"]] %||% CV_PROMPT_DEFAULT_CATEGORY),
    error = function(e) e)
  if (inherits(out, "error")) return(cv_api_err(conditionMessage(out), 400L))
  cv_api_prompts()
}

#' @noRd
cv_api_prompt_remove <- function(id) {
  out <- tryCatch(cv_prompts_remove(id), error = function(e) e)
  if (inherits(out, "error")) return(cv_api_err(conditionMessage(out), 400L))
  cv_api_prompts()
}

#' @noRd
cv_api_prompts_restore <- function() {
  tryCatch(cv_prompts_restore_builtins(), error = function(e) NULL)
  cv_api_prompts()
}

# ---- Sessions ---------------------------------------------------------------

#' @noRd
cv_api_new_session <- function() {
  sid <- cv_session_new()
  cv_api_ok(list(session_id = sid))
}

#' @noRd
cv_api_get_session <- function(session_id) {
  if (!cv_session_exists(session_id)) {
    # try to restore from disk
    ok <- tryCatch({ cv_session_restore(session_id); TRUE }, error = function(e) FALSE)
    if (!ok) return(cv_api_err(sprintf(
      "That session (%s) is not available any more - it may have expired, or the server may have restarted. Reload the page to start a fresh one.",
      session_id), 404L))
  }
  sess <- cv_session_get(session_id)
  cv_api_ok(list(
    session_id = sess$id,
    created = sess$created,
    config = cv_config_public(sess$config),
    objects = cv_object_descriptors(sess$object_store),
    detached = sess$detached_descriptors %||% list(),
    history_len = length(sess$history),
    # Full history so the UI can rehydrate the chat transcript after a reload
    # (App.tsx reads full?.history ?? []; messages carry role/content/ts).
    history = sess$history
  ))
}

#' @noRd
cv_api_list_sessions <- function() cv_api_ok(list(sessions = cv_session_list()))

#' Delete one saved session (audit #67).
#'
#' Round LXVIII. There was no way to erase a transcript at all: a conversation
#' containing embargoed marker genes stayed in `~/.celliverse/sessions/`
#' indefinitely, and the History tab could only ever add to the pile.
#'
#' The session's `artifacts/` directory is KEPT - see `cv_session_delete()`.
#'
#' Each refusal gets its own sentence and its own status, because the two mean
#' different things to whoever pressed the button: a live job is "wait, then
#' try again", an unusable id is "this is not a session".
#' @noRd
cv_api_delete_session <- function(session_id) {
  res <- cv_session_delete(session_id)
  if (isTRUE(res$ok))
    return(cv_api_ok(list(session_id = session_id, deleted = TRUE)))
  if (identical(res$reason, "live_job"))
    return(cv_api_err(paste0(
      "This conversation still has a tool running. Stop it, or wait for it to ",
      "finish, then delete it."), 409L))
  if (identical(res$reason, "unsafe_id"))
    return(cv_api_err("That is not a session id I can act on.", 400L,
                      detail = sprintf("Rejected session id: %s",
                                       paste(as.character(session_id), collapse = " "))))
  cv_api_err("That conversation could not be deleted.", 500L,
             detail = sprintf("cv_session_delete reason: %s", res$reason))
}

# ---- Objects ----------------------------------------------------------------

#' Load an object from an .rds/.qs path on the server into the session store.
#' (For local desktop use; the file lives on the user's own machine.)
#' @noRd
cv_api_load_object <- function(session_id, body) {
  if (!cv_session_exists(session_id)) return(cv_api_err(
    "That session is not available any more. Reload the page to start a fresh one.",
    404L))
  path <- body$path
  if (is.null(path) || !file.exists(path)) return(cv_api_err(
    "I could not find that file. Check the path, or use the File Upload tab to send it directly.",
    404L))
  # Round XLIX: this used to be a bare readRDS(), so an .rds was the ONLY thing
  # anyone could load -- a CSV of counts or a 10x MTX triplet, which is what
  # most people actually have, failed with "could not read as .rds". Format
  # dispatch lives in agent_ingest.R; everything downstream of here (handle
  # naming, the auto-Seurat conversion, the store) is unchanged and format
  # agnostic, which is why this is a one-line integration.
  #
  # `body$filename` carries the ORIGINAL name when the caller wrote the bytes to
  # a temp file, since the extension is the only thing identifying the format.
  read <- tryCatch(cv_read_dataset_file(path, filename = body$filename %||% body$source),
                   error = function(e) e)
  if (inherits(read, "error")) {
    return(cv_api_err(cv_clean_error(conditionMessage(read)), 400L))
  }
  obj <- read$object
  read_note <- read$note
  store <- cv_session_get(session_id)$object_store
  src <- body$source %||% basename(path)
  # Honour the upload "Optional name": build a type-prefixed, sanitized handle
  # (e.g. "pbmc counts" -> mat_pbmc_counts). Duplicate names are blocked inside
  # cv_object_put with a clear error. No name -> random handle (unchanged).
  req_handle <- body$handle
  if (is.null(req_handle) || !nzchar(req_handle %||% "")) {
    req_handle <- cv_handle_from_name(obj, body$name)
  }
  handle <- tryCatch(
    cv_object_put(store, obj, handle = req_handle, source = src),
    error = function(e) e
  )
  if (inherits(handle, "error")) {
    return(cv_api_err(conditionMessage(handle), 409L))
  }

  # Auto-Seurat conversion (agent layer only): if the uploaded object is a
  # (sparse) matrix or data.frame - not already a Seurat - also create a Seurat
  # object so downstream visualization/analysis tools can consume it, keeping
  # the original handle too. Non-fatal: a non-count data.frame just skips.
  seurat_handle <- NULL
  seurat_note <- NULL
  adv <- NULL          # set only on the matrix branch; NULL means "not asked"
  if (cv_object_type(obj) %in% c("dgCMatrix", "matrix", "data.frame")) {
    # Round LXXXII: ASK WHETHER IT FITS FIRST.
    #
    # The Seurat carries its own full copy of the counts -- measured: holding a
    # 0.8 GB dgCMatrix cost 1.98 GB, holding it plus the Seurat built from it
    # cost 3.21 GB, and dropping the matrix afterwards freed nothing. Doing that
    # unconditionally is what turns "your dataset is large" into "the R session
    # died holding your dataset", which is what the user hit on a 3.7 GB matrix.
    #
    # Declining is not a failure: the matrix is loaded and has a handle, and the
    # note carries the one sentence that performs the conversion later. See
    # cv_conversion_advice() in agent_bigdata.R -- unmeasurable machine or
    # unmeasurable object means CONVERT, so nothing changes anywhere the
    # question cannot be answered.
    adv <- tryCatch(cv_conversion_advice(obj, handle),
                    error = function(e) list(convert = TRUE, reason = NULL))
    conv <- if (isFALSE(adv$convert)) NULL else
      tryCatch(cv_matrix_to_seurat(obj), error = function(e) e)
    if (isFALSE(adv$convert)) seurat_note <- adv$reason
    if (!inherits(conv, "error") && !is.null(conv)) {
      sh <- cv_handle_from_name(conv, body$name)
      # Avoid colliding with the just-created original handle or any existing one.
      if (!is.null(sh) && (identical(sh, handle) || cv_object_exists(store, sh))) sh <- NULL
      seurat_handle <- tryCatch(
        cv_object_put(store, conv, handle = sh, source = paste0(src, " (auto-Seurat)")),
        error = function(e) NULL
      )
      if (!is.null(seurat_handle)) {
        seurat_note <- sprintf(
          "I also created a Seurat object `%s` from your %s so it is ready for clustering and visualization.",
          seurat_handle, cv_object_type(obj))
      }
    }
  }

  # Refresh the on-disk snapshot so session.json's descriptors reflect the
  # new object immediately (not only after the next chat message).
  tryCatch(cv_session_snapshot(session_id), error = function(e) NULL)

  cv_api_ok(list(
    handle = handle,
    descriptor = cv_object_descriptor(store, handle),
    # Round LXXXIII: the client needs to know the conversion was SKIPPED, not
    # impossible, so it can offer to do it anyway. Round LXXXII returned only
    # prose, which a button cannot read.
    seurat_skipped = isTRUE(isFALSE(adv$convert)),
    seurat_needs_mb = adv$needs_mb,
    seurat_budget_mb = adv$available_mb,
    seurat_handle = seurat_handle,
    seurat_descriptor = if (!is.null(seurat_handle)) cv_object_descriptor(store, seurat_handle) else NULL,
    # Anything that had to be INFERRED while reading (matrix orientation,
    # placeholder gene/cell names, which object was picked out of an .RData)
    # travels back with the auto-Seurat note. An assumption the user can see is
    # one they can correct; a silent transpose is a wrong analysis.
    note = paste(stats::na.omit(c(read_note, seurat_note)), collapse = " ")
  ))
}

#' Handle a multipart file upload: pull `session` + the file part out of a
#' parsed multipart body (plumber's `@parser multi` shape) and load the object.
#'
#' Kept here (not in plumber.R) so it is unit-testable without a live server.
#' `body` is req$body: a named list where text fields look like
#' list(value=<raw|chr>) and the file part looks like
#' list(value=<raw>, filename=..., content_type=..., ...). `session_arg` is the
#' value plumber may have bound to the handler's `session` parameter (query
#' string or the auto-bound form field), which can be NULL, a string, raw bytes,
#' or the whole part list.
#' @noRd
cv_api_upload_multipart <- function(body, session_arg = NULL) {
  if (is.null(body) || !length(body)) return(cv_api_err("No file came through with that upload. Pick a file and try again.", 400L))

  # Coerce any of {NULL, string, raw, list(value=...)} to a scalar string.
  as_txt <- function(v) {
    if (is.null(v) || length(v) == 0) return("")
    if (is.list(v)) {
      v <- if (!is.null(v$value)) v$value else v[[1]]
      if (is.null(v) || length(v) == 0) return("")
    }
    if (is.raw(v)) v <- rawToChar(v)
    out <- as.character(v)
    if (length(out) == 0) return("")
    trimws(out[1])
  }

  session <- as_txt(session_arg)
  if (!nzchar(session)) session <- as_txt(body$session)
  if (!nzchar(session)) {
    return(cv_api_err(
      "That upload did not say which session it belongs to. Reload the page and try again.",
      400L))
  }

  # File part: prefer one with a non-empty $filename; else first non-session
  # part whose $value is raw.
  file_part <- NULL; fname <- NULL
  for (nm in names(body)) {
    p <- body[[nm]]
    if (is.list(p) && !is.null(p$filename) && nzchar(p$filename %||% "")) { file_part <- p; fname <- p$filename; break }
  }
  if (is.null(file_part)) {
    for (nm in names(body)) {
      if (identical(nm, "session")) next
      p <- body[[nm]]
      val <- if (is.list(p)) p$value else p
      if (is.raw(val)) { file_part <- if (is.list(p)) p else list(value = p); fname <- (if (is.list(p)) p$filename else NULL) %||% nm; break }
    }
  }
  if (is.null(file_part)) return(cv_api_err(
    "That upload did not include a file. Pick a file and try again - if your browser blocked it, dragging the file onto the page usually works.",
    400L))

  raw_val <- if (is.list(file_part)) file_part$value else file_part
  if (is.null(raw_val) || !is.raw(raw_val) || !length(raw_val)) {
    return(cv_api_err(
    "That file came through empty. Check it opens on your machine, then try the upload again.", 400L))
  }
  # Round LXXIX (audit #56): decide on the FILENAME, before a second full copy
  # of the payload is written to disk. See cv_upload_extension_problem() for
  # what this does and does not check -- in particular, it is not and must not
  # become a size limit.
  bad <- cv_upload_extension_problem(fname)
  if (!is.null(bad)) return(cv_api_err(bad$message, 400L, detail = bad$detail))
  ext <- tools::file_ext(fname %||% "")
  tmp <- tempfile(fileext = if (nzchar(ext)) paste0(".", ext) else ".rds")
  writeBin(as.raw(raw_val), tmp)
  # Batch 8a: delete the staged copy once the load has been attempted -- on the
  # failure path as much as the success path, which is why it is an on.exit and
  # not an unlink after the call.
  #
  # WHY, measured: this is a full second copy of whatever the user uploaded, and
  # nothing ever removed it. cv_api_load_object() reads the file into an R object
  # and puts THE OBJECT in the store; the only other use of `path` is
  # `basename(path)` for a display string, so nothing downstream holds the file.
  # Without this, every upload left its whole payload in tempdir() for the life
  # of the server process -- and run_celliverse_agent() is meant to be left
  # running. A 650 MB Seurat (the size measured on the user's own dataset during
  # the Round XXXIX investigation) leaks 650 MB per upload, and re-uploading
  # after a failed load leaks again.
  on.exit(unlink(tmp), add = TRUE)
  # Pull the optional display name out of the multipart form (the client sends
  # it as a text field alongside the file) so the object gets a named handle.
  up_name <- as_txt(body$name)
  cv_api_load_object(session, list(
    path = tmp, handle = NULL, source = fname,
    # Round XLIX: the temp file keeps the extension, but pass the original name
    # explicitly so format detection never depends on that continuing to hold.
    filename = fname,
    name = if (nzchar(up_name)) up_name else NULL
  ))
}

#' Register an already-loaded object (used by tests / programmatic callers).
#' @noRd
cv_api_put_object <- function(session_id, value, source = "api", handle = NULL) {
  store <- cv_session_get(session_id)$object_store
  h <- cv_object_put(store, value, handle = handle, source = source)
  tryCatch(cv_session_snapshot(session_id), error = function(e) NULL)
  cv_api_ok(list(handle = h, descriptor = cv_object_descriptor(store, h)))
}

#' @noRd
cv_api_list_objects <- function(session_id) {
  if (!cv_session_exists(session_id)) return(cv_api_err(
    "That session is not available any more. Reload the page to start a fresh one.",
    404L))
  store <- cv_session_get(session_id)$object_store
  cv_api_ok(list(objects = cv_object_descriptors(store)))
}

#' @noRd
cv_api_object_detail <- function(session_id, handle) {
  if (!cv_session_exists(session_id)) return(cv_api_err(
    "That session is not available any more. Reload the page to start a fresh one.",
    404L))
  store <- cv_session_get(session_id)$object_store
  d <- cv_object_descriptor(store, handle)
  if (is.null(d)) return(cv_api_err(sprintf(
    "There is no object called '%s' in this session. Open the Results tab to see what is loaded, or upload it again.",
    handle), 404L))
  cv_api_ok(list(descriptor = d))
}

# ---- Artifacts (results tab: figures / RDS objects / CSV / TXT + zip) -------

#' List all downloadable results artifacts for a session (the Results manifest).
#'
#' Read-only: it (re)builds the manifest by scanning the session artifacts dir
#' and attaching object provenance, but never re-serializes or deletes object
#' files (those are owned by the end-of-turn sync). This keeps the endpoint
#' cheap enough to poll and safe on a restored session whose store is empty.
#' @noRd
cv_api_list_artifacts <- function(session_id) {
  if (!cv_session_exists(session_id)) return(cv_api_err(
    "That session is not available any more. Reload the page to start a fresh one.",
    404L))
  sess <- cv_session_get(session_id)
  manifest <- tryCatch(
    cv_build_manifest(sess$object_store, sess$artifacts_dir, session_id),
    error = function(e) {
      cli::cli_warn("Could not build artifact manifest: {conditionMessage(e)}")
      list(session = session_id, generated = cv_now(), n = 0L, artifacts = list())
    })
  cv_api_ok(manifest)
}

#' Build a zip of ALL session artifacts and return its temp path (raw bytes are
#' streamed by the plumber shell). Refreshes the manifest first so the bundle is
#' self-describing. Returns an error envelope when there is nothing to download.
#' @noRd
cv_api_artifacts_zip <- function(session_id) {
  if (!cv_session_exists(session_id)) return(cv_api_err(
    "That session is not available any more. Reload the page to start a fresh one.",
    404L))
  sess <- cv_session_get(session_id)
  # Round LIV: this is the "prepare everything" moment. Objects are no longer
  # serialized at the end of every turn (see cv_index_object_artifacts()), so
  # anything still pending is written HERE, while the user is deliberately
  # waiting for a download and the Results tab is telling them so. Already
  # materialized objects are skipped by the signature check inside, so a second
  # "Download all" on an unchanged session costs nothing.
  tryCatch(
    cv_sync_object_artifacts(sess$object_store, sess$artifacts_dir, session_id),
    error = function(e) cli::cli_warn("Could not prepare all objects: {conditionMessage(e)}"))
  tryCatch(cv_build_manifest(sess$object_store, sess$artifacts_dir, session_id),
           error = function(e) NULL)
  zp <- tryCatch(cv_artifacts_zip_file(sess$artifacts_dir, session_id),
                 error = function(e) NULL)
  if (is.null(zp) || !file.exists(zp)) return(cv_api_err(
    "There is nothing to download yet. Run an analysis first and its plots, tables and objects will appear here.",
    404L))
  list(ok = TRUE, path = zp)
}

#' Resolve a single session artifact by name to a servable descriptor (path +
#' content type + optional download filename), or an error envelope. Raw
#' bytes are read and written to the response by the plumber shell, exactly
#' like `cv_api_artifacts_zip()` above -- this keeps the path-traversal guard
#' and the extension -> Content-Type mapping unit-testable as plain function
#' calls (Batch 3 architecture fix: this logic used to live directly in
#' plumber.R, the one route in that file that wasn't a thin delegation to an
#' `agent_api.R` handler).
#' @noRd
cv_api_serve_artifact <- function(session_id, name) {
  # Only a bare filename is allowed -- no path separators, no "..".
  if (is.null(name) || !nzchar(name) || grepl("[/\\\\]|\\.\\.", name)) {
    return(cv_api_err(
      "That file name is not one I can serve. Use the download links in the Results tab.",
      400L))
  }
  adir <- file.path(cv_sessions_dir(), session_id, "artifacts")
  path <- file.path(adir, name)
  # Round LIV: a missing file is no longer automatically a 404. Objects are
  # indexed at the end of a turn and serialized only on demand, so the Results
  # tab legitimately offers rows whose bytes do not exist yet. If this name
  # belongs to a pending object that is still live in the store, write THAT ONE
  # object now and serve it -- clicking one row never pays for the rest of the
  # session. Anything else (a stale link, a typo, an object lost to a restart)
  # still 404s exactly as before.
  if (!file.exists(path)) {
    tryCatch(cv_materialize_pending_artifact(session_id, name), error = function(e) NULL)
    if (!file.exists(path)) return(cv_api_err(
      "That result file is not there any more. Reopen the Results tab to see what is available.",
      404L))
  }
  ext <- tolower(tools::file_ext(name))
  # Round LXXV (audit #31): `log` was missing, so a job log fell to
  # NOTE: see cv_api_table_page() below for D5's paging route, which reuses this
  # function's traversal guard rather than writing a second one.
  # application/octet-stream and, because it was also absent from the
  # disposition list below, downloaded as an unnamed opaque blob. A run log is
  # the one artifact whose whole purpose is to be READ, usually in the middle of
  # diagnosing a failure -- text/plain makes it open in the browser tab.
  ctype <- switch(ext, svg = "image/svg+xml", png = "image/png",
                  csv = "text/csv", json = "application/json",
                  txt = "text/plain; charset=utf-8",
                  log = "text/plain; charset=utf-8",
                  rds = "application/octet-stream", "application/octet-stream")
  # RDS/txt downloads: hint a filename so the browser saves rather than guesses.
  # `log` is deliberately NOT in this list: naming it forces a download, which
  # is the behaviour this round is removing.
  disposition_filename <- if (ext %in% c("rds", "txt")) name else NULL
  list(ok = TRUE, path = path, content_type = ctype,
       disposition_filename = disposition_filename)
}

# ---- Table paging (Round LXXV, D5 / audit #46) -------------------------------
#
# THE DEFECT, as measured. `Artifacts.tsx:39` destructures
# `const [rows] = useState(art?.rows ?? [])` WITHOUT a setter, so the component
# is structurally incapable of changing page. It renders rows 1-50 of 5,000 and
# truthfully captions "page 1/100". `.pager` and `.pager button` have existed in
# styles.css since the beginning with ZERO usages in any .tsx -- paging was
# designed, styled, and never wired.
#
# WHAT THE RE-SLICE READS. Not the in-memory frame: the per-turn ledger keeps
# only status/handle/summary, and a browser reload rebuilds the transcript from
# role/content messages and skips tool messages entirely, so the card is gone
# anyway. What DOES survive is the CSV -- cv_render_table() writes the FULL
# frame to <artifacts_dir>/<basename>.csv on the first render, and it outlives
# the turn, a restart, and even session deletion. So paging re-reads that file.
# It is the same bytes the "Download CSV" link already serves, which means the
# page the user sees and the file they download cannot disagree.
#
# THE 91x FINDING. Measured on a 5,000-row table: the poll payload is 307,280
# bytes, of which 303,900 is `result$table` -- the full frame, which the
# frontend NEVER reads (Chat.tsx:567 takes `res.table_artifact` and nothing
# else). Stripping it leaves 3,380 bytes. The model's view is unaffected: it is
# built from the same result before the strip and is separately head-capped with
# an honest `truncated` flag. See .cv_result_for_browser() in agent_render.R.

#' Serve one page of a table artifact's CSV.
#'
#' @param session_id session id.
#' @param name the CSV filename from the table artifact's `csv.filename`.
#' @param page 1-based page number (clamped).
#' @param page_size rows per page (clamped).
#' @noRd
cv_api_table_page <- function(session_id, name, page = 1L, page_size = 50L) {
  # Same traversal guard as cv_api_serve_artifact(), same message: a bare
  # filename only, no separators, no "..".
  if (is.null(name) || !nzchar(name) || grepl("[/\\\\]|\\.\\.", name)) {
    return(cv_api_err(
      "That file name is not one I can read. Use the table shown in the chat.", 400L))
  }
  if (!identical(tolower(tools::file_ext(name)), "csv")) {
    return(cv_api_err("Only table files can be paged.", 400L))
  }
  path <- file.path(cv_sessions_dir(), session_id, "artifacts", name)
  if (!file.exists(path)) {
    return(cv_api_err(
      "That table is not there any more. Re-run the step to get it back.", 404L))
  }
  df <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) e)
  if (inherits(df, "error")) {
    return(cv_api_err("That table could not be read.", 500L,
                      detail = cv_clean_error(conditionMessage(df))))
  }
  sl <- .cv_page_slice(nrow(df), page, page_size)
  rows <- if (nrow(df) == 0L) df else df[sl$from:sl$to, , drop = FALSE]
  cv_api_ok(list(
    # I() so a single-column table serializes `columns` as a 1-element ARRAY
    # rather than a bare string. plumber's unboxedJSON uses auto_unbox=TRUE, and
    # the client does cols.map(...) -- without this, a one-column table crashes
    # the component that this whole route exists to fix.
    columns = I(as.character(colnames(df))),
    nrow = nrow(df), ncol = ncol(df),
    page = sl$page, page_size = sl$page_size, n_pages = sl$n_pages,
    rows = rows))
}

# ---- Chat (synchronous form used by tests; SSE form below) ------------------

#' Run a full agent turn synchronously and return the final answer + events.
#' The SSE handler wraps the same call with a streaming on_event.
#' @noRd
cv_api_chat_sync <- function(session_id, message) {
  if (!cv_session_exists(session_id)) return(cv_api_err(
    "That session is not available any more. Reload the page to start a fresh one.",
    404L))
  if (is.null(message) || !nzchar(trimws(message %||% ""))) {
    return(cv_api_err("There was no message to send - type something first.", 400L))
  }
  collected <- list()
  on_event <- function(ev) { collected[[length(collected) + 1L]] <<- ev; invisible() }
  disp <- cv_make_dispatcher(session_id, on_event = on_event)
  # BATCH3 FIX: this call site used to let cv_agent_turn()'s error escape
  # raw, unlike its two siblings (cv_api_chat_stream() below, and
  # cv_start_turn()'s runner() in agent_turns.R), both of which convert a
  # failure into the app's standard clean-error contract. An uncaught error
  # here would bubble through plumber as an opaque generic 500 instead of
  # this module's documented {ok:false, error:...} envelope. Wrapping it the
  # same way the other two already are makes all three call sites of
  # cv_agent_turn() consistent.
  res <- tryCatch(
    cv_agent_turn(session_id, message, on_event = on_event, dispatch = disp, stream = FALSE),
    error = function(e) {
      msg <- cv_clean_turn_error(conditionMessage(e))
      if (!nzchar(msg)) msg <- "The turn failed unexpectedly."
      structure(list(.cv_sync_error = msg), class = "cv_sync_turn_error")
    }
  )
  if (inherits(res, "cv_sync_turn_error")) {
    return(cv_api_err(res$.cv_sync_error, 500L))
  }
  cv_api_ok(list(content = res$content, iterations = res$iterations,
                 tool_calls = res$tool_calls, events = collected))
}

# ---- Async chat: start / poll / cancel (live-feedback transport) ------------

#' Start an async chat turn. Returns { turn } immediately; the turn runs on the
#' event loop and its events are read incrementally via cv_api_chat_poll().
#' @noRd
cv_api_chat_start <- function(session_id, message) {
  if (!cv_session_exists(session_id)) return(cv_api_err(
    "That session is not available any more. Reload the page to start a fresh one.",
    404L))
  if (is.null(message) || !nzchar(trimws(message %||% ""))) {
    return(cv_api_err("There was no message to send - type something first.", 400L))
  }
  started <- cv_start_turn(session_id, message)
  # One-turn-per-session: if a turn is already in flight, tell the client to
  # wait instead of launching a concurrent turn (the loop-causing path).
  if (is.list(started) && isTRUE(started$busy)) {
    return(cv_api_ok(list(turn = started$turn, cursor = 0L, status = "busy",
                          busy = TRUE,
                          message = "Still working on your previous message - please wait for it to finish before sending another.")))
  }
  cv_api_ok(list(turn = started, cursor = 0L, status = "running"))
}

#' Poll an async chat turn for new events after `cursor`.
#' Returns { turn, status, done, cursor, events, error }.
#' @noRd
cv_api_chat_poll <- function(session_id, turn_id, cursor = 0L) {
  if (!cv_session_exists(session_id)) return(cv_api_err(
    "That session is not available any more. Reload the page to start a fresh one.",
    404L))
  # Pump the event loop so a poll actively advances in-process work (turn steps
  # scheduled via later) even between browser requests.
  #
  # Round XLV: `all = FALSE` is the load-bearing argument here, and its absence
  # still cost a full extra step even after the step-machine landed. run_now()
  # defaults to `all = TRUE`, meaning "keep executing ready callbacks, INCLUDING
  # ones scheduled while executing" -- so the turn's own
  # `later::later(advance, ...)` re-schedule was picked straight back up by the
  # same pump and this one handler ran two steps. Measured over real HTTP:
  # 3,931 ms per poll with all = TRUE, ~2,000 ms (one LLM call) with all = FALSE.
  #
  # `timeoutSecs = 0` likewise means "run what is already ready, never WAIT for
  # more". Together they bound a poll to advancing the turn by at most one step,
  # which is the property test-round44-poll-latency-http-safe.R asserts.
  tryCatch(later::run_now(timeoutSecs = 0, all = FALSE), error = function(e) NULL)
  # Round LII (Batch 4a): bound how long a LONG turn may hold unwritten history.
  # Session writes are coalesced to the end of a turn, which for a normal 2-5 s
  # turn means exactly one write; but a turn running a multi-minute heavy tool
  # would otherwise keep every message it accumulates in memory for the whole
  # run. This flushes only once the oldest pending write has waited past the
  # threshold, so a normal turn still writes once and a long one writes rarely
  # -- NOT once per poll, which at a sub-second poll cadence would be far worse
  # than the per-message writes this round removed.
  tryCatch(cv_session_flush_stale(session_id), error = function(e) NULL)
  out <- cv_turn_poll(session_id, turn_id, cursor)
  if (is.null(out)) return(cv_api_err(
    "That message is no longer being tracked - it most likely finished already. Reload the page if the chat looks out of date.",
    404L))
  # Round XXII: drop NULL fields (e.g. error=NULL) so they serialize as absent,
  # not `{}`, under plumber's unboxedJSON (null="list").
  cv_api_ok(cv_json_sanitize(out))
}

#' Cancel an in-flight async chat turn (and any heavy jobs it launched).
#' @noRd
cv_api_chat_cancel <- function(session_id, turn_id) {
  if (!cv_session_exists(session_id)) return(cv_api_err(
    "That session is not available any more. Reload the page to start a fresh one.",
    404L))
  ok <- cv_turn_cancel(session_id, turn_id)
  if (!isTRUE(ok)) return(cv_api_err(
    "That message is no longer being tracked - it most likely finished already. Reload the page if the chat looks out of date.",
    404L))
  cv_api_ok(list(turn = turn_id, status = "cancelled"))
}

# ---- SSE helpers ------------------------------------------------------------

#' Write one SSE event to a connection.
#' @noRd
cv_sse_send <- function(con, event) {
  type <- event$type %||% "message"
  payload <- jsonlite::toJSON(event, auto_unbox = TRUE, null = "null", na = "string", force = TRUE)
  cat(sprintf("event: %s\ndata: %s\n\n", type, payload), file = con)
  flush(con)
  invisible()
}

#' The streaming chat handler. `con` is a writable connection (the Plumber
#' response body stream). Runs the agent turn, flushing every event as SSE.
#' Because the dispatcher pumps later::run_now internally, tokens and job
#' progress arrive here in real time.
#' @noRd
cv_api_chat_stream <- function(session_id, message, con) {
  if (!cv_session_exists(session_id)) { cv_sse_send(con, list(type="error", error="Unknown session.")); return(invisible()) }
  if (is.null(message) || !nzchar(trimws(message %||% ""))) {
    cv_sse_send(con, list(type="error", error="Empty message.")); return(invisible())
  }
  on_event <- function(ev) cv_sse_send(con, ev)
  disp <- cv_make_dispatcher(session_id, on_event = on_event)
  tryCatch(
    cv_agent_turn(session_id, message, on_event = on_event, dispatch = disp, stream = TRUE),
    error = function(e) {
      msg <- cv_clean_turn_error(conditionMessage(e))
      if (!nzchar(msg)) msg <- "The turn failed unexpectedly."
      cv_sse_send(con, list(type = "error", error = msg))
    }
  )
  invisible()
}

# ---- Jobs -------------------------------------------------------------------

#' @noRd
cv_api_list_jobs <- function(session_id) {
  if (!cv_session_exists(session_id)) return(cv_api_err(
    "That session is not available any more. Reload the page to start a fresh one.",
    404L))
  sess <- cv_session_get(session_id)
  ids <- ls(envir = sess$jobs)
  jobs <- lapply(ids, function(i) cv_job_public(get(i, envir = sess$jobs)))
  cv_api_ok(list(jobs = jobs))
}

#' @noRd
cv_api_job_status <- function(session_id, job_id) {
  # Batch 8b: guard the SESSION, not just the job. cv_job_get() calls
  # cv_session_get(), which cli_abort()s on an unknown session id -- so an
  # unknown session reached the error boundary as a 500 while an unknown JOB
  # in a valid session returned a clean 404. Two sibling handlers behaving
  # differently for the same class of bad input is exactly the inconsistency
  # Batch 3a found in cv_api_chat_sync(); cv_api_list_jobs() right above
  # already guards this way.
  if (!cv_session_exists(session_id)) return(cv_api_err(
    "That session is not available any more. Reload the page to start a fresh one.",
    404L))
  rec <- cv_job_get(session_id, job_id)
  if (is.null(rec)) return(cv_api_err(
    "That job is not in this session any more. Finished jobs are cleared after a while; if it was still running, reload the page to see where it got to.",
    404L))
  cv_api_ok(cv_job_public(rec))
}

#' @noRd
cv_api_cancel_job <- function(session_id, job_id) {
  # BATCH2B FIX: cv_job_cancel() now legitimately returns FALSE in two cases
  # that are NOT "unknown job" -- (1) the job already reached a terminal
  # state, or (2) its worker process already finished but the race-safe guard
  # (see agent_worker.R) deliberately left the record untouched so the
  # in-flight poll() tick can finalize it with the real result. Both are the
  # job existing and simply not needing (or not yet ready for) cancellation --
  # reporting either as a 404 "Unknown job" would be wrong and would surface
  # a confusing error to the client for a Stop click that arrived a moment
  # too late. Check existence directly, independent of cv_job_cancel()'s
  # return value, and report the job's actual resulting status either way.
  # Batch 8b: same guard as cv_api_job_status() above -- cv_job_get() calls
  # cv_session_get(), which cli_abort()s on an unknown session id -- so an
  # unknown session reached the error boundary as a 500 while an unknown JOB
  # in a valid session returned a clean 404. Two sibling handlers behaving
  # differently for the same class of bad input is exactly the inconsistency
  # Batch 3a found in cv_api_chat_sync(); cv_api_list_jobs() right above
  # already guards this way.
  if (!cv_session_exists(session_id)) return(cv_api_err(
    "That session is not available any more. Reload the page to start a fresh one.",
    404L))
  if (is.null(cv_job_get(session_id, job_id))) return(cv_api_err(
    "That job is not in this session any more. Finished jobs are cleared after a while; if it was still running, reload the page to see where it got to.",
    404L))
  cv_job_cancel(session_id, job_id)
  rec <- cv_job_get(session_id, job_id)
  cv_api_ok(list(cancelled = identical(rec$status, "cancelled"),
                 status = rec$status, job_id = job_id))
}

# ---- Local model downloads (ollama pull / lms get) ---------------------------

#' Launch a background local-model download as a session job.
#'
#' Shared by the Ollama (`ollama pull <model>`) and LM Studio (`lms get <model>
#' --yes`) pull endpoints. Validates the model id, resolves the provider's CLI
#' binary, then runs the download in a callr background process whose stdout
#' goes to the job log; the existing GET /api/jobs/<id> polling surfaces
#' progress (parsed from the log tail) to the UI.
#'
#' @param session_id session id (the job registry lives on the session).
#' @param body request body; needs `model` (non-empty, safe charset).
#' @param provider "ollama" or "lmstudio".
#' @noRd
cv_api_pull_model <- function(session_id, body, provider = c("ollama", "lmstudio")) {
  provider <- match.arg(provider)
  if (!cv_session_exists(session_id)) return(cv_api_err(
    "That session is not available any more. Reload the page to start a fresh one.",
    404L))
  model <- as.character(body$model %||% "")[1]
  if (!nzchar(model)) return(cv_api_err("No model was selected. Choose one from the list and try again.", 400L))
  # Model ids are passed to a shell-free system2() call, but keep the charset
  # tight anyway (covers ollama name:tag and lms owner/name@quant forms).
  if (!grepl("^[A-Za-z0-9._:/@-]+$", model)) {
    return(cv_api_err(
    "That model id contains characters I cannot use. Model ids may contain letters, digits and . _ : / @ - only.",
    400L))
  }
  bin <- if (provider == "ollama") "ollama" else "lms"
  exe <- cv_detect_binary(bin)
  if (is.null(exe)) {
    msg <- if (provider == "ollama")
      "Ollama is not installed (or not on PATH). See https://ollama.com/download, then retry."
    else
      "The LM Studio CLI (`lms`) was not found. Install LM Studio (https://lmstudio.ai) and run it once, then retry - or download the model inside the LM Studio app."
    return(cv_api_err(msg, 400L))
  }
  args <- if (provider == "ollama") c("pull", model) else c("get", model, "--yes")

  job_id <- cv_job_new(session_id, paste0(provider, "_pull"),
                       args_public = list(model = model))
  log_file <- file.path(cv_session_get(session_id)$artifacts_dir, paste0(job_id, ".log"))
  cv_job_update(session_id, job_id, status = "running", progress = 2,
                message = sprintf("downloading %s", model), log_file = log_file)

  child_fun <- function(exe, args) {
    Sys.setenv(NO_COLOR = "1")
    # system2 inherits this process's stdout/stderr, which callr redirects to
    # the job log file - so `ollama pull` / `lms get` progress lands there.
    code <- system2(exe, args)
    list(exit_code = code)
  }
  proc <- callr::r_bg(
    func = child_fun,
    args = list(exe = unname(exe), args = args),
    stdout = log_file, stderr = log_file, supervise = TRUE,
    package = FALSE,
    env = c(callr::rcmd_safe_env(), NO_COLOR = "1", R_CLI_NUM_COLORS = "1")
  )
  cv_job_update(session_id, job_id, process = proc)

  # Non-blocking completion watcher (same later::later pattern as heavy jobs).
  .cv_pull_repoll(session_id, job_id, delay = 1)
  cv_api_ok(list(job_id = job_id, model = model, provider = provider,
                 status = "running"))
}

#' Re-arm the pull-job watcher (kept as a helper so the closure stays small).
#' @noRd
.cv_pull_repoll <- function(session_id, job_id, delay = 1.5) {
  later::later(function() {
    rec <- cv_job_get(session_id, job_id)
    if (is.null(rec) || rec$status != "running") return(invisible(NULL))
    if (rec$process$is_alive()) {
      msg <- cv_tail_progress(rec$log_file) %||% rec$message
      cv_job_update(session_id, job_id, message = msg)
      return(.cv_pull_repoll(session_id, job_id, delay))
    }
    res <- tryCatch(rec$process$get_result(), error = function(e) e)
    ok <- !inherits(res, "error") && identical(as.integer(res$exit_code %||% 1L), 0L)
    model <- rec$args$model %||% "model"
    if (ok) {
      cv_job_update(session_id, job_id, status = "done", progress = 100,
                    message = sprintf("%s downloaded", model))
    } else {
      err <- cv_clean_error(cv_read_log(rec$log_file))
      if (!nzchar(err)) err <- sprintf("download of %s failed", model)
      cv_job_update(session_id, job_id, status = "error", error = err,
                    progress = 100, message = "download failed")
    }
    invisible(NULL)
  }, delay)
}
