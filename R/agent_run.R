# =============================================================================
# CelliVerse Agent — run entry point
#
# run_celliverse_agent() plumbs inst/plumber/plumber.R, serves the prebuilt
# React app from inst/react-app/, binds to 127.0.0.1 (localhost only), and opens
# a browser.
#
# LIVE FEEDBACK: plumber/httpuv cannot flush a partial HTTP response within a
# single request (no in-handler res$write; no HTTP/2 push), so live token/tool
# feedback is delivered by the async-turn + polling API instead of SSE:
#   POST /api/chat/start  -> { turn }         (returns immediately)
#   GET  /api/chat/poll   -> { events, done } (client polls ~350 ms)
#   POST /api/chat/cancel -> cancel a turn
# The legacy /api/chat (SSE-buffered) and /api/chat/sync routes are retained as
# fallbacks for non-polling clients and tests.
# =============================================================================

#' Is a TCP port already accepting connections on `host`?
#'
#' A dependency-free pre-flight: try to open a *client* socket to the port. If
#' the connection succeeds, something is already listening there (e.g. a stale
#' `run_celliverse_agent()` from an earlier session). Used to fail fast with a
#' clear message instead of httpuv's cryptic "createTcpServer: address already
#' in use" -> "Failed to create server".
#' @noRd
cv_port_in_use <- function(port, host = "127.0.0.1", timeout = 0.3) {
  con <- tryCatch(
    suppressWarnings(socketConnection(host = host, port = port, server = FALSE,
                                       blocking = TRUE, open = "r+",
                                       timeout = timeout)),
    error = function(e) NULL)
  if (is.null(con)) return(FALSE)
  close(con)
  TRUE
}

#' Resolve the port to actually bind, auto-incrementing past a busy one.
#'
#' If `port` is free, it is returned unchanged. If it is busy and
#' `port_scan = TRUE` (default), probe `port + 1`, `port + 2`, ... up to
#' `max_port_tries` steps and return the first free port, printing a note that
#' names both the busy port and the chosen one. If `port_scan = FALSE`, a busy
#' port is a hard error with an actionable message (the old behaviour). If no
#' free port is found within the scan window, it errors with a clear range.
#'
#' Valid TCP ports are 1-65535; the scan is clamped to that ceiling.
#' @return an integer port that was free at probe time.
#' @noRd
cv_resolve_port <- function(port, host = "127.0.0.1", port_scan = TRUE,
                            max_port_tries = 20L) {
  port <- as.integer(port)
  max_port_tries <- max(1L, as.integer(max_port_tries))

  if (!cv_port_in_use(port, host = host)) return(port)

  # Requested port is busy.
  if (!isTRUE(port_scan)) {
    cli::cli_abort(c(
      "Port {.val {port}} on {.val {host}} is already in use.",
      "i" = "A previous CelliVerse agent is probably still running there.",
      "*" = "Reconnect by opening {.url {sprintf('http://%s:%d/', host, port)}} in your browser, or",
      "*" = "stop the old server (Ctrl+C / Esc in its R session, or restart R), then re-run, or",
      "*" = "start on a different port: {.code run_celliverse_agent(port = {port + 1L})}, or",
      "*" = "let it find a free port automatically: {.code run_celliverse_agent(port_scan = TRUE)}."
    ))
  }

  # Scan upward for the first free port within the window.
  last_try <- min(65535L, port + max_port_tries)
  for (cand in seq.int(port + 1L, last_try)) {
    if (!cv_port_in_use(cand, host = host)) {
      cli::cli_alert_info(
        "Port {.val {port}} was busy; using the next free port {.val {cand}} instead.")
      return(as.integer(cand))
    }
  }
  cli::cli_abort(c(
    "No free port found in {.val {port}}-{.val {last_try}} on {.val {host}}.",
    "i" = "All {max_port_tries} ports after {.val {port}} were in use.",
    "*" = "Free one of them (stop other servers), or",
    "*" = "pick a clearly-free port: {.code run_celliverse_agent(port = <n>)}, or",
    "*" = "widen the search: {.code run_celliverse_agent(max_port_tries = 100L)}."
  ))
}

#' Print a one-line FIRST-RUN onboarding hint at launch.
#'
#' The default provider is OpenRouter on a free model, which needs AN API key
#' (OpenRouter has no key-less endpoint). When the active provider is OpenRouter
#' but no key is configured yet, point the user at the in-app setup card (and
#' the free key page). When the provider is Ollama but the binary is missing,
#' point at the installer. Silent otherwise so we never nag a working setup.
#' @noRd
cv_first_run_hint <- function(cfg) {
  provider <- cfg$default_provider %||% ""
  if (identical(provider, "openrouter") && !nzchar(cfg$openrouter_key %||% "")) {
    cli::cli_alert_info(paste0(
      "OpenRouter needs a free API key. Paste it in the app's welcome card or ",
      "Settings (get one at {.url https://openrouter.ai/keys}); the default model ",
      "{.val ", cfg$default_model %||% "qwen/qwen3-30b-a3b-instruct-2507",
      "} is free for a limited quota up to your OpenRouter credit."))
    return(invisible(TRUE))
  }
  if (identical(provider, "ollama") && is.null(cv_detect_binary("ollama"))) {
    cli::cli_alert_info(paste0(
      "Ollama is not installed. Run {.code install_celliverse_agent()} to set it ",
      "up, or switch to an OpenRouter model in Settings (no install needed)."))
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

#' Print a one-line SUGGESTION to install a stronger local model.
#'
#' Fires only when the active provider is Ollama, the current model is NOT
#' already the best tier this machine's RAM can run, and that best tier is
#' above "light". It NEVER pulls a model — it only tells the user how to
#' upgrade if they want more reliable tool-calling. Silent when RAM is
#' undetectable (never blocks launch).
#' @noRd
cv_suggest_strong_model <- function(cfg) {
  if (!identical(cfg$default_provider %||% "", "ollama")) return(invisible(FALSE))
  rec <- cv_local_recommendation()
  if (identical(rec$tier, "light")) return(invisible(FALSE))
  best_id <- .cv_model_tiers[[rec$tier]]
  if (identical(cfg$default_model %||% "", best_id)) return(invisible(FALSE))
  cli::cli_alert_info(paste0(
    "Tip: this machine can run the ", rec$tier, " tier. For more reliable ",
    "tool-calling, install it with ",
    "{.code install_celliverse_agent(tier = \"", rec$tier, "\")} then select ",
    "{.val ", best_id, "} in Settings."))
  invisible(TRUE)
}

#' Launch the CelliVerse agent (API + UI)
#'
#' Starts the local plumber API, serves the prebuilt React UI from
#' `inst/react-app/`, binds to localhost, and (optionally) opens a browser.
#' The agent runs cloud-first: no local model is required — pick a cloud
#' provider + API key in Settings, or install Ollama / LM Studio later for
#' fully-offline local models.
#'
#' @param port TCP port to bind (default from config, else 8000).
#' @param host host to bind; keep 127.0.0.1 for localhost-only (default).
#' @param provider optional provider override for this run (writes to config).
#' @param model optional model override for this run.
#' @param browser open a browser window automatically.
#' @param background run the server in a background process (returns a handle)
#'   instead of blocking the console.
#' @param port_scan if `TRUE` (default) and `port` is already in use, bind the
#'   next free port instead of failing; if `FALSE`, a busy port is a hard error.
#' @param max_port_tries how many ports above `port` to probe when scanning
#'   (default 20).
#' @return invisibly, the plumber router (foreground) or a process handle
#'   (background).
#' @export
run_celliverse_agent <- function(port = NULL, host = "127.0.0.1",
                                  provider = NULL, model = NULL,
                                  browser = interactive(), background = FALSE,
                                  port_scan = TRUE, max_port_tries = 20L) {
  cv_require_agent_deps()
  cv_ensure_home()
  # Round LXXX (audit #70): drop event logs older than CV_LOG_KEEP_DAYS.
  # Once per server start, not per turn: the log is bounded by age and a
  # directory scan on every turn would be a cost paid for nothing.
  tryCatch(cv_log_prune(), error = function(e) NULL)

  cfg <- cv_load_config()
  if (!is.null(provider)) cfg$default_provider <- provider
  if (!is.null(model))    cfg$default_model    <- model
  if (!is.null(provider) || !is.null(model)) cv_save_config(cfg)
  if (is.null(port)) port <- cfg$port %||% 8000L

  # Pre-flight: pick the port to bind. If the requested port is busy (usually a
  # stale agent from an earlier session), auto-increment to the next free port
  # so the launch never dies with httpuv's cryptic "address already in use"
  # AFTER a misleading "running" banner. `port_scan = FALSE` restores the old
  # hard-fail behaviour. The chosen port is used for BOTH the foreground and
  # background launch paths below, and reported to the user.
  port <- cv_resolve_port(port, host = host, port_scan = port_scan,
                          max_port_tries = max_port_tries)

  if (isTRUE(background)) {
    return(cv_run_background(port = port, host = host))
  }

  pr <- cv_build_router()
  url <- sprintf("http://%s:%d/", host, port)
  cli::cli_alert_success("CelliVerse agent running at {.url {url}}")
  cli::cli_alert_info("Provider: {.val {cfg$default_provider}} | Model: {.val {cfg$default_model}}")
  cv_first_run_hint(cfg)         # one-line onboarding hint (no key / ollama)
  cv_suggest_strong_model(cfg)   # one-line RAM-based hint (suggest only)
  cli::cli_alert_info("Press Ctrl+C (or Esc in RStudio) to stop.")
  if (isTRUE(browser)) {
    later::later(function() utils::browseURL(url), delay = 1.0)
  }
  pr$run(host = host, port = port, docs = FALSE)
  invisible(pr)
}

#' Cache-aware static file router (a PlumberStatic subclass).
#'
#' plumber's built-in static handler sets Content-Type/Content-Length/
#' Last-Modified but NO Cache-Control. The Vite bundle filename carries a
#' content hash (index-<hash>.js) that CHANGES on every rebuild, and index.html
#' references the current hash. With no cache headers a browser may serve a
#' STALE cached index.html that requests the OLD (now-deleted) hash -> the
#' module script 404s and the page flashes then renders blank. Fix: index.html
#' (and any non-hashed file) is served `no-cache, must-revalidate` so the
#' browser always revalidates it, while the immutable hashed assets may be
#' cached forever.
#' @noRd
.cv_cache_control_for <- function(path) {
  base <- basename(path %||% "")
  if (grepl("^index-[A-Za-z0-9_-]+\\.(js|css)$", base)) {
    "public, max-age=31536000, immutable"
  } else {
    "no-cache, must-revalidate"
  }
}

#' @noRd
.cv_static_router <- function(direc) {
  # Look up R6/plumber classes via their namespaces (both are in Suggests, so
  # avoid `::` hard imports). plumber itself requires R6, so if plumber is
  # available R6 is too.
  R6Class <- get("R6Class", envir = asNamespace("R6"))
  PlumberStatic <- get("PlumberStatic", envir = asNamespace("plumber"))
  PlumberStaticCC <- R6Class(
    "PlumberStaticCC",
    inherit = PlumberStatic,
    public = list(
      initialize = function(direc, options) {
        super$initialize(direc, options)
        # Wrap the static-asset handler PlumberStatic just registered so every
        # served file also carries the right Cache-Control header. The handler
        # function lives in the filter's private env (`private$func`), executed
        # by the filter's exec() method.
        for (flt in private$filts) {
          if (startsWith(flt$name, "static-asset")) {
            fpriv <- flt$.__enclos_env__$private
            orig <- fpriv$func
            fpriv$func <- function(req, res) {
              out <- orig(req = req, res = res)
              if (inherits(out, "PlumberResponse") && !is.null(out$status) &&
                  out$status == 200) {
                out$setHeader("Cache-Control",
                              .cv_cache_control_for(req$PATH_INFO))
              }
              out
            }
          }
        }
      }
    )
  )
  PlumberStaticCC$new(direc)
}

#' Build the plumber router: mount API routes + static React app.
#' @noRd
cv_build_router <- function() {
  plumber_file <- system.file("plumber", "plumber.R", package = "celliverse")
  if (!nzchar(plumber_file)) {
    # dev fallback: look relative to source tree
    plumber_file <- file.path("inst", "plumber", "plumber.R")
  }
  pr <- plumber::plumb(plumber_file)
  # BATCH1 FIX (rebuilt from scratch, evidence-based): drop plumber's own
  # built-in "json" (and "multi") parser from the router-wide defaults. These
  # run BEFORE any handler code and, on a malformed Content-Type: application/
  # json body, throw inside plumber's own machinery -- a failure point every
  # handler's own manual JSON parsing (cv_parse_json_body() in plumber.R,
  # reading req$postBody directly) cannot see or catch, because the handler is
  # never reached. Verified empirically: without this, a malformed JSON POST
  # to a real running server returns an opaque generic 500; with it, the
  # handler's own parser runs instead and returns a clear 400. The one
  # multipart-upload endpoint (`/api/objects/upload`) is unaffected -- it
  # carries its own explicit `#* @parser multi` tag, which overrides this
  # router-wide default regardless of what's listed here.
  pr <- plumber::pr_set_parsers(pr, c("form", "text", "octet"))

  # Batch 8b: a top-level error boundary. There was none, so ANY unhandled throw
  # in any handler reached plumber's default and the client got
  # `{"error":"500 - Internal server error"}` -- no message, nothing actionable.
  # Reproduced over real HTTP on two live paths: POST /api/objects/load with a
  # path that does not exist, and POST /api/jobs/<unknown>/cancel. In the first
  # of those the honest answer is simply "I couldn't find that file", and the
  # user got a bare 500 instead.
  #
  # The shape is the user's choice (2026-08-13): a calm sentence they can act
  # on, plus the underlying R error in a SEPARATE `detail` field the UI reveals
  # behind a toggle -- so a bug report carries the real text without a stack
  # trace ever being the first thing anyone reads.
  #
  # This is a backstop, not the primary path. A handler that knows what went
  # wrong still returns its own cv_api_err() with a specific message and status;
  # anything reaching here is by definition unforeseen, which is why the message
  # says so rather than guessing.
  pr <- plumber::pr_set_error(pr, cv_api_error_boundary)

  # Serve the prebuilt React SPA from inst/react-app/ at "/".
  www <- system.file("react-app", package = "celliverse")
  if (!nzchar(www) || !dir.exists(www) || !length(list.files(www))) {
    # dev fallback: source tree (running from the package root, not installed)
    dev_www <- file.path("inst", "react-app")
    if (dir.exists(dev_www) && length(list.files(dev_www))) www <- dev_www
  }
  # pr_static stores the path and resolves files lazily at request time, so it
  # must be absolute (a relative dev-fallback path would break once the server
  # process's working directory differs).
  if (nzchar(www) && dir.exists(www)) www <- normalizePath(www, mustWork = FALSE)
  if (nzchar(www) && dir.exists(www) && length(list.files(www))) {
    # Mount the cache-aware static router (NOT pr_static): it serves the files
    # exactly like plumber's built-in static handler but ALSO sets
    # Cache-Control per file type, which a router-level pr_filter cannot do
    # (the mounted PlumberStatic sub-router handles the request itself, so a
    # parent filter never runs for those responses).
    pr <- plumber::pr_mount(pr, "/", .cv_static_router(www))
    # SPA fallback: unknown non-API GET routes return index.html so client-side
    # routing works on refresh.
    pr <- plumber::pr_get(pr, "/__spa_fallback", function(res) {
      idx <- file.path(www, "index.html")
      if (file.exists(idx)) { res$body <- readBin(idx, "raw", file.info(idx)$size)
        res$setHeader("Content-Type", "text/html")
        res$setHeader("Cache-Control", "no-cache, must-revalidate"); res } else res
    })
  } else {
    cli::cli_alert_warning(paste(
      "No prebuilt React app found in inst/react-app/.",
      "The API is up; build the frontend (see setup guide) or use /api/* directly."))
  }
  pr
}

#' Run the server in a supervised background R process.
#' @noRd
cv_run_background <- function(port, host) {
  # `port` is already resolved (free) by the caller. Bind it exactly in the
  # child (port_scan = FALSE) so the background server never silently drifts to
  # a different port than the one we report here.
  script <- function(port, host) {
    suppressPackageStartupMessages(library(celliverse))
    celliverse::run_celliverse_agent(port = port, host = host, browser = FALSE,
                                     port_scan = FALSE)
  }
  proc <- callr::r_bg(script, args = list(port = port, host = host),
                      supervise = TRUE, package = FALSE)
  url <- sprintf("http://%s:%d/", host, port)
  cli::cli_alert_success("CelliVerse agent starting in background at {.url {url}} (pid {proc$get_pid()}).")
  cli::cli_alert_info("Stop it with: proc$kill()  (the returned handle).")
  invisible(proc)
}
