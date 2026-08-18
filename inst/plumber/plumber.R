# =============================================================================
# CelliVerse Agent — Plumber routing shell
#
# This is a THIN transport layer. It parses each HTTP request and delegates to
# the transport-agnostic handlers in R/agent_api.R. Keeping logic out of here
# means the endpoints are unit-testable without a running server.
#
# Served by run_celliverse_agent(), which plumbs this file and binds to
# 127.0.0.1 (localhost only). The prebuilt React app in inst/react-app/ is
# served as static files from "/". CORS is permissive for localhost dev.
#
# Auth: none in v1 (localhost only). A token-gate hook is documented below and
# left commented so a future version can require a bearer token.
# =============================================================================

library(plumber)

# All agent functions live in the celliverse package as internal (unexported)
# objects. When the package is installed, they resolve via its namespace. When
# running from the source tree during development (files sourced into the global
# environment, package NOT installed), they resolve from there instead.
#
# cv_h("name") returns the handler function from whichever context is available,
# so this routing shell is identical in installed and dev modes.
cv_h <- function(name) {
  if ("celliverse" %in% loadedNamespaces()) {
    ns <- asNamespace("celliverse")
    if (exists(name, envir = ns, inherits = FALSE)) return(get(name, envir = ns))
  }
  get(name, envir = globalenv())
}

# BATCH1 FIX (rebuilt from scratch, evidence-based): this file is sourced
# standalone by plumber::plumb() -- it is NOT compiled into the celliverse
# package's own namespace the way R/*.R files are, so a plain function like
# cv_h() above can look up package internals per-call via asNamespace(), but
# an INFIX OPERATOR like `%||%` (used below and package-wide) cannot -- R must
# resolve the operator itself via lexical scoping before it can even evaluate
# either side. `%||%` became part of base R in R 4.4.0, so on R >= 4.4 this is
# already covered and this line is a harmless no-op shadow. On R < 4.0-4.3
# (celliverse's own stated `Depends: R (>= 4.0.0)`), `%||%` is NOT otherwise
# visible here and every use below would fail with "could not find function".
# Defining it locally costs nothing and closes that portability gap either way.
`%||%` <- function(x, y) if (is.null(x)) y else x

# BATCH1 FIX (rebuilt from scratch, evidence-based -- verified against a live
# server before/after): every POST handler used
#   tryCatch(jsonlite::fromJSON(req$postBody, ...), error = function(e) list())
# inside the handler. Verified empirically against the ORIGINAL, unmodified
# code that this does NOT crash the server process -- but a malformed JSON
# body still returns an opaque, generic "500 - Internal server error" instead
# of a clear 400, because plumber's OWN default body parser (registered
# router-wide) also attempts to parse the same Content-Type: application/json
# body BEFORE the handler ever runs, and throws there -- a failure point the
# handler's own tryCatch cannot see or catch, since it never gets called. The
# fix is two-part: (1) this shared helper parses `req$postBody` (the raw body
# string) manually, exactly as every handler already did, but now returns a
# clean 400 with an explicit message when parsing fails on a NON-EMPTY body
# (an empty body is unchanged -- still `list()`, since some routes accept no
# fields); (2) `cv_build_router()` (R/agent_run.R) drops plumber's own "json"
# parser from the router-wide defaults so it never gets a chance to throw
# before this handler code runs. `/api/objects/upload` is unaffected -- it
# carries its own explicit `#* @parser multi` tag, which overrides the
# router-wide default regardless.
cv_parse_json_body <- function(req, res = NULL) {
  raw <- req$postBody %||% ""
  if (!nzchar(trimws(raw))) return(list())
  parsed <- tryCatch(jsonlite::fromJSON(raw, simplifyVector = TRUE), error = function(e) e)
  if (inherits(parsed, "error")) {
    if (!is.null(res)) {
      res$status <- 400L
      return(list(ok = FALSE, parse_error = TRUE, status = 400L,
                  error = paste0("That request could not be read - its body was not valid JSON. ",
                                 "This usually means the page needs a reload.")))
    }
    return(list())
  }
  if (is.null(parsed)) list() else parsed
}

# Batch 8b: apply the HTTP status the error envelope ALREADY carries.
#
# THE DEFECT THIS FIXES, measured over real HTTP before the change: 22 of 26
# routes returned **HTTP 200** with a body of
# `{"ok":false,"error":"Unknown session.","status":404}`. The envelope has
# always carried the right status; almost nothing applied it.
#
# That is not merely an API-contract wart. The browser client already has the
# correct error path -- `if (!r.ok) throw new Error(await errMsg(r, ...))`,
# where errMsg() reads the envelope's own `error` string -- and it could never
# be reached, because `r.ok` is true for a 200. So `unwrap()` handed the error
# envelope back where a session object was expected, and the UI rendered blank
# fields instead of saying anything. Setting the status here is what switches
# that path on.
#
# Applied through one helper rather than 22 copies of the same two lines: the
# thing that went wrong the first time was 22 call sites each responsible for
# remembering.
cv_status <- function(res, out) {
  if (is.list(out) && isFALSE(out$ok)) res$status <- out$status %||% 400L
  out
}

#* @apiTitle CelliVerse Agent API
#* @apiDescription Localhost API powering the CelliVerse single-cell analysis agent.

# ---- Global filters ---------------------------------------------------------

# BATCH1 FIX (security, rebuilt from scratch): `Access-Control-Allow-Origin: *`
# on a server that can load/mutate local files, run analyses, and hold API
# keys means ANY web page open in the user's browser -- not just this app --
# can fetch() these endpoints from JS and read the JSON back, purely because
# the server happens to be listening on 127.0.0.1. Binding to localhost does
# NOT stop this; it is exactly what makes the drive-by "attack a local server
# from a random open tab" pattern work. This does not add friction for the app
# itself or local dev tooling (curl, the desktop app's own webview, etc.
# either send no Origin header or one that matches the pattern below) -- it
# only stops OTHER origins from being allowed to read responses.
#
# CELLIVERSE_ALLOWED_ORIGIN can be set to add one extra explicit origin (e.g.
# a non-default dev port) beyond the localhost/127.0.0.1 pattern already
# allowed on any port.
.cv_is_allowed_origin <- function(origin) {
  if (is.null(origin) || !nzchar(origin)) return(FALSE)
  if (grepl("^https?://(localhost|127\\.0\\.0\\.1|\\[::1\\])(:[0-9]+)?/?$", origin, ignore.case = TRUE)) {
    return(TRUE)
  }
  extra <- Sys.getenv("CELLIVERSE_ALLOWED_ORIGIN", "")
  nzchar(extra) && identical(origin, extra)
}

#* CORS for localhost dev (React dev server on :5173 or the bundled app).
#* @filter cors
function(req, res) {
  origin <- req$HTTP_ORIGIN
  if (.cv_is_allowed_origin(origin)) {
    res$setHeader("Access-Control-Allow-Origin", origin)
    res$setHeader("Vary", "Origin")
  }
  # No Access-Control-Allow-Origin header at all for any other origin: the
  # browser will block that page's JS from reading the response (it may still
  # receive a 200 on a simple GET, but cannot read the body cross-origin).
  # Round LXVIII: DELETE joins the list because /api/session/<id> now exists.
  # Without it a legitimate cross-origin caller -- the Vite dev server on :5173,
  # which IS in the allow-list -- would have its DELETE preflight refused while
  # its GETs and POSTs worked, which is the kind of half-working that costs an
  # afternoon to diagnose.
  res$setHeader("Access-Control-Allow-Methods", "GET,POST,DELETE,OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")
  if (identical(req$REQUEST_METHOD, "OPTIONS")) {
    res$status <- 200L
    return(list())
  }
  # Round LXIV Batch 2a: withholding the CORS header stops a foreign page
  # READING the response; it does not stop the request from being PERFORMED.
  # For a state-changing request that distinction is the whole risk: a page open
  # in another tab can POST to 127.0.0.1 and, say, repoint the provider host or
  # start a job, and simply not care that it cannot read the reply. So a
  # state-changing request carrying a disallowed Origin is refused outright.
  #
  # Round LXVIII widened this from POST to POST-or-DELETE, in the same round
  # that added the first DELETE route. The guard was written when POST was the
  # only state-changing verb in the API; leaving it method-specific would have
  # shipped a destructive endpoint -- one that erases transcripts -- outside the
  # protection every other state-changing endpoint already has. The browser's
  # own preflight would very likely have blocked it anyway, but that is the
  # argument this comment already declines to rely on for POST.
  #
  # An ABSENT Origin is allowed on purpose: curl, an R script, and the app's own
  # same-origin fetches send none, and refusing those would break normal use to
  # defend against a browser attack the browser itself is announcing.
  if (req$REQUEST_METHOD %in% c("POST", "DELETE") &&
      !is.null(origin) && nzchar(origin) && !.cv_is_allowed_origin(origin)) {
    res$status <- 403L
    return(list(ok = FALSE, status = 403L,
                error = "That request came from a page CelliVerse does not recognise, so it was not carried out.",
                detail = paste0("Origin '", origin, "' is not in the allow-list. ",
                                "Use the app served by run_celliverse_agent(), or set ",
                                "CELLIVERSE_ALLOWED_ORIGIN if you are running your own front end.")))
  }
  plumber::forward()
}

# ---- Optional token gate (DISABLED in v1; documented for later) -------------
# To require a token later, set CELLIVERSE_API_TOKEN and uncomment this filter.
# NOTE: keep these as plain '#' (not '#*') while disabled, otherwise plumber
# parses the @filter annotation and attaches it to the next real endpoint.
#
# #* @filter auth
# function(req, res) {
#   want <- Sys.getenv("CELLIVERSE_API_TOKEN", "")
#   if (nzchar(want)) {
#     got <- sub("^Bearer ", "", req$HTTP_AUTHORIZATION %||% "")
#     if (!identical(got, want)) { res$status <- 401L; return(list(ok = FALSE, error = "Unauthorized")) }
#   }
#   plumber::forward()
# }

# ---- Meta -------------------------------------------------------------------

#* Health check
#* @get /api/health
#* @serializer unboxedJSON
function(res) cv_status(res, cv_h("cv_api_health")())

#* Tool registry (for Package Browser / Tool Inspector)
#* @get /api/registry
#* @serializer unboxedJSON
function(res, include_advanced = "false") {
  cv_status(res, cv_h("cv_api_registry")(include_advanced = isTRUE(as.logical(include_advanced))))
}

# ---- Settings ---------------------------------------------------------------

#* Get current settings (secrets redacted to booleans)
#* @get /api/settings
#* @serializer unboxedJSON
function(res) cv_status(res, cv_h("cv_api_get_settings")())

#* Update settings live (no restart)
#* @post /api/settings
#* @serializer unboxedJSON
function(req, res) {
  body <- cv_parse_json_body(req, res)
  if (isTRUE(body$parse_error)) return(body)
  cv_status(res, cv_h("cv_api_update_settings")(body))
}

#* Locally-installed Ollama models (empty + reachable=false if Ollama is down)
#* @get /api/ollama/models
#* @serializer unboxedJSON
function(res) cv_status(res, cv_h("cv_api_ollama_models")())

#* Selectable models for a provider (live list when a key is set, else curated).
#* Never returns the raw API key.
#* @get /api/models
#* @serializer unboxedJSON
function(res, provider = NULL) cv_status(res, cv_h("cv_api_provider_models")(provider))

#* Download an Ollama model in the background (ollama pull); poll /api/jobs/<id>
#* @post /api/ollama/pull
#* @serializer unboxedJSON
function(req, res) {
  body <- cv_parse_json_body(req, res)
  if (isTRUE(body$parse_error)) return(body)
  cv_status(res, cv_h("cv_api_pull_model")(body$session %||% body$session_id, body, provider = "ollama"))
}

#* Download an LM Studio model in the background (lms get --yes); poll /api/jobs/<id>
#* @post /api/lmstudio/pull
#* @serializer unboxedJSON
function(req, res) {
  body <- cv_parse_json_body(req, res)
  if (isTRUE(body$parse_error)) return(body)
  cv_status(res, cv_h("cv_api_pull_model")(body$session %||% body$session_id, body, provider = "lmstudio"))
}

# ---- Sessions ---------------------------------------------------------------

#* Create a new session
#* @post /api/session
#* @serializer unboxedJSON
function(res) cv_status(res, cv_h("cv_api_new_session")())

#* Get a session (restores from disk snapshot if needed)
#* @get /api/session/<id>
#* @serializer unboxedJSON
function(res, id) cv_status(res, cv_h("cv_api_get_session")(id))

#* List all sessions
#* @get /api/sessions
#* @serializer unboxedJSON
function(res) cv_status(res, cv_h("cv_api_list_sessions")())

#* Delete one saved session. Keeps its artifacts/ directory (audit #67).
#* @delete /api/session/<id>
#* @serializer unboxedJSON
function(res, id) cv_status(res, cv_h("cv_api_delete_session")(id))

#* Round LXXX (audit #71): the LOCAL turn summary, derived from
#* ~/.celliverse/logs. Nothing here leaves the machine, and nothing may be made
#* to -- see the header of R/agent_observe.R.
#* @get /api/log-summary
#* @serializer unboxedJSON
function(res, days = 7) cv_status(res, cv_h("cv_api_log_summary")(days))

# ---- Objects ----------------------------------------------------------------

#* Round LXXX (audit #60/#61/#62): what the agent can do, what to try, and what
#* it can read. Served from R so the screen and the parser cannot disagree.
#* @get /api/intro
#* @serializer unboxedJSON
function(res) cv_status(res, cv_h("cv_api_intro")())

#* Round LXXXII: what a browser upload of `bytes` would cost, and what this
#* machine has. Asked when a file is CHOSEN, so the answer arrives before the
#* upload rather than after it fails. Advice only - there is no size limit.
#* @get /api/upload-advice
#* @serializer unboxedJSON
function(res, bytes = 0) cv_status(res, cv_h("cv_api_upload_advice")(bytes))

#* Round LXXXIII: build a Seurat from a loaded matrix on demand. Same code the
#* `toSeurat` tool runs; this is the Upload page's "Build it anyway" button.
#* @post /api/objects/to-seurat
#* @serializer unboxedJSON
function(req, res) {
  body <- cv_parse_json_body(req, res)
  if (isTRUE(body$parse_error)) return(body)
  cv_status(res, cv_h("cv_api_to_seurat")(body$session, body$handle, body$name))
}

# ---- Saved prompts (Round LXXXI, E2) ----------------------------------------
# Persisted in ~/.celliverse/prompts.json, NOT in the browser, so a favourite
# survives a different browser, a private window and a second machine pointed at
# the same R server. Every route returns the whole list.

#* The saved prompts: built-in starters minus the hidden ones, plus the user's.
#* @get /api/prompts
#* @serializer unboxedJSON
function(res) cv_status(res, cv_h("cv_api_prompts")())

#* Save one prompt. Body: {label, text, category}.
#* @post /api/prompts
#* @serializer unboxedJSON
function(req, res) {
  body <- cv_parse_json_body(req, res)
  if (isTRUE(body$parse_error)) return(body)
  cv_status(res, cv_h("cv_api_prompt_add")(body))
}

#* Remove one prompt by id. A built-in is hidden; one of your own is deleted.
#* Body: {id}. POST rather than DELETE because the id travels in the body and
#* carries a colon, which a path segment would have to escape twice.
#* @post /api/prompts/remove
#* @serializer unboxedJSON
function(req, res) {
  body <- cv_parse_json_body(req, res)
  if (isTRUE(body$parse_error)) return(body)
  cv_status(res, cv_h("cv_api_prompt_remove")(body$id))
}

#* Put every hidden built-in starter back.
#* @post /api/prompts/restore
#* @serializer unboxedJSON
function(res) cv_status(res, cv_h("cv_api_prompts_restore")())

#* Round LXXX (audit #63): load the bundled demo dataset (SeuratObject::pbmc_small).
#* @post /api/objects/demo
#* @serializer unboxedJSON
function(res, session = NULL) cv_status(res, cv_h("cv_api_load_demo")(session))

#* Load an object from a server-side .rds path into a session
#* @post /api/objects/load
#* @serializer unboxedJSON
function(req, res, session = NULL) {
  body <- cv_parse_json_body(req, res)
  if (isTRUE(body$parse_error)) return(body)
  # Batch 8b, found by probing the live server: `session` was a required formal,
  # so plumber demanded it as a QUERY parameter -- but the browser client sends
  # it in the JSON body (`jpost("/objects/load", { session, path, name })`), and
  # the router-wide JSON parser was deliberately removed in Batch 1, so it never
  # arrived as an argument. Every real call therefore threw
  # `argument "session" is missing, with no default` before the handler ran, and
  # the user got a bare 500. Accept either, exactly as the two /pull routes
  # already do.
  cv_status(res, cv_h("cv_api_load_object")(session %||% body$session %||% body$session_id, body))
}

#* Upload a data file (.rds) — multipart. Saves to the session dir, then loads.
#* @post /api/objects/upload
#* @serializer unboxedJSON
#* @parser multi
function(req, res, session = NULL) {
  # `@parser multi` parses the multipart body into req$body as a named list:
  #   req$body$file    -> list(value = <raw bytes>, filename, content_type, ...)
  #   req$body$session -> list(value = "<sid>")   (a plain FormData text field)
  #
  # plumber logs a harmless "No suitable parser found to handle request body
  # type application/rds" while trying to *further* parse the file part by its
  # declared Content-Type (browsers label a .rds File application/rds,
  # application/octet-stream, or leave it blank; plumber has no content parser
  # for those). Only the OPTIONAL `$parsed` field fails — the RAW bytes in
  # `$value` are always preserved (verified: readRDS round-trips for all three
  # Content-Types). All the fiddly extraction + session coercion lives in the
  # testable helper cv_api_upload_multipart(); this shell just maps status.
  out <- cv_h("cv_api_upload_multipart")(req$body, session)
  if (isFALSE(out$ok)) res$status <- out$status %||% 400L
  out
}

#* List objects in a session
#* @get /api/objects
#* @serializer unboxedJSON
function(res, session) cv_status(res, cv_h("cv_api_list_objects")(session))

#* Object detail (descriptor)
#* @get /api/objects/<handle>
#* @serializer unboxedJSON
function(res, session, handle) cv_status(res, cv_h("cv_api_object_detail")(session, handle))

# ---- Chat -------------------------------------------------------------------

#* Chat (legacy SSE-buffered). NOTE: plumber cannot flush a partial response
#* within one request, so this route returns all SSE frames *after* the turn
#* completes (not truly live). Kept for backward compatibility; for live
#* feedback use POST /api/chat/start + GET /api/chat/poll.
#* @post /api/chat
#* @serializer cat
function(req, res) {
  # NOTE: this endpoint is `@serializer cat`, so the return value must stay a
  # plain character string (not a plumber response object) -- setting
  # res$status directly still works, only the RETURN VALUE has to be text.
  body <- cv_parse_json_body(req, res)
  if (isTRUE(body$parse_error)) {
    res$status <- 400L
    return(jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"))
  }
  session <- body$session
  message <- body$message
  res$setHeader("Content-Type", "text/event-stream")
  res$setHeader("Cache-Control", "no-cache")
  res$setHeader("Connection", "keep-alive")
  res$setHeader("X-Accel-Buffering", "no")
  # Stream into a text connection, then hand the accumulated body back. For true
  # incremental flush, run_celliverse_agent() uses httpuv's streaming response;
  # this cat-serializer path assembles SSE frames the client parses identically.
  tc <- textConnection("sse_buf", open = "w", local = TRUE)
  cv_h("cv_api_chat_stream")(session, message, tc)
  close(tc)
  paste(sse_buf, collapse = "\n")
}

#* Chat (synchronous, non-streaming) — simpler clients / debugging / tests.
#* @post /api/chat/sync
#* @serializer unboxedJSON
function(req, res) {
  body <- cv_parse_json_body(req, res)
  if (isTRUE(body$parse_error)) return(body)
  cv_status(res, cv_h("cv_api_chat_sync")(body$session, body$message))
}

#* Chat (async) — START a turn. Returns { turn } immediately; the turn runs in
#* the background and its events are read incrementally via /api/chat/poll.
#* This is the transport that delivers live "thinking"/tool/token feedback,
#* because plumber cannot flush a partial response within a single request.
#* @post /api/chat/start
#* @serializer unboxedJSON
function(req, res) {
  body <- cv_parse_json_body(req, res)
  if (isTRUE(body$parse_error)) return(body)
  cv_status(res, cv_h("cv_api_chat_start")(body$session, body$message))
}

#* Chat (async) — POLL a turn for new events after `cursor`.
#* Returns { turn, status, done, cursor, events, error }. The client calls this
#* every ~350 ms until `done` is true.
#* @get /api/chat/poll
#* @serializer unboxedJSON
function(res, session, turn, cursor = 0) {
  cv_status(res, cv_h("cv_api_chat_poll")(session, turn, suppressWarnings(as.integer(cursor))))
}

#* Chat (async) — CANCEL an in-flight turn (and any heavy jobs it launched).
#* @post /api/chat/cancel
#* @serializer unboxedJSON
function(req, res) {
  body <- cv_parse_json_body(req, res)
  if (isTRUE(body$parse_error)) return(body)
  cv_status(res, cv_h("cv_api_chat_cancel")(body$session, body$turn))
}

# ---- Jobs -------------------------------------------------------------------

#* List jobs in a session
#* @get /api/jobs
#* @serializer unboxedJSON
function(res, session) cv_status(res, cv_h("cv_api_list_jobs")(session))

#* Job status
#* @get /api/jobs/<id>
#* @serializer unboxedJSON
function(res, session, id) cv_status(res, cv_h("cv_api_job_status")(session, id))

#* Cancel a job
#* @post /api/jobs/<id>/cancel
#* @serializer unboxedJSON
function(req, res, id, session = NULL) {
  # Batch 8b: same defect as /objects/load above, and worse here -- the client
  # calls `jpost("/jobs/<id>/cancel", {})` with no session at all, so the Cancel
  # button on the Logs page could only ever produce a 500. The job id already
  # identifies the job; the session is looked up from the body when present and
  # otherwise resolved by the handler.
  body <- cv_parse_json_body(req, res)
  if (isTRUE(body$parse_error)) return(body)
  cv_status(res, cv_h("cv_api_cancel_job")(session %||% body$session %||% body$session_id, id))
}

# ---- Artifacts (results tab: figures / RDS objects / CSV / TXT + zip) --------

#* List all downloadable results artifacts for a session (the Results manifest).
#* @get /api/artifacts
#* @serializer unboxedJSON
function(res, session) cv_status(res, cv_h("cv_api_list_artifacts")(session))

#* One page of a table artifact (Round LXXV, D5). Re-slices the CSV that
#* cv_render_table() already wrote, so the page shown and the file downloaded
#* cannot disagree. Out-of-range pages clamp rather than error.
#* @get /api/table
#* @serializer unboxedJSON
function(res, session, name, page = 1, page_size = 50) {
  cv_status(res, cv_h("cv_api_table_page")(session, name,
                                           suppressWarnings(as.integer(page)),
                                           suppressWarnings(as.integer(page_size))))
}

#* Download ALL session artifacts as one zip.
#* @get /api/artifacts.zip
function(session, res) {
  out <- cv_h("cv_api_artifacts_zip")(session)
  if (isFALSE(out$ok)) {
    res$status <- out$status %||% 404L
    res$setHeader("Content-Type", "application/json")
    res$body <- jsonlite::toJSON(out, auto_unbox = TRUE, null = "null")
    return(res)
  }
  res$setHeader("Content-Type", "application/zip")
  res$setHeader("Content-Disposition",
                sprintf('attachment; filename="celliverse_%s_results.zip"', session))
  res$body <- readBin(out$path, "raw", n = file.info(out$path)$size)
  # Batch 8b: the zip is built fresh per click into tempfile() and, until now,
  # never removed -- one more file per "download all" for the life of the
  # server. It could not be cleaned up inside cv_artifacts_zip_file(), which
  # returns the path precisely so it can be served; the right moment is here,
  # the instant its bytes are in res$body and the path is no longer needed.
  # Held back from 8a for exactly that reason: unlinking it a frame too early
  # breaks a download instead of leaking a file.
  unlink(out$path)
  res
}

#* Serve a session artifact file (svg/png/csv/json/rds/txt) by name.
#* @get /api/artifacts/<session>/<name>
function(session, name, res) {
  # BATCH3 FIX: the path-traversal guard and extension -> Content-Type
  # mapping now live in cv_api_serve_artifact() (agent_api.R), unit-testable
  # as a plain function call, matching every other route in this file
  # (including /api/artifacts.zip just above). This shell only translates the
  # returned descriptor into the actual plumber response.
  out <- cv_h("cv_api_serve_artifact")(session, name)
  if (isFALSE(out$ok)) {
    res$status <- out$status %||% 404L
    res$setHeader("Content-Type", "application/json")
    res$body <- jsonlite::toJSON(out, auto_unbox = TRUE, null = "null")
    return(res)
  }
  res$setHeader("Content-Type", out$content_type)
  if (!is.null(out$disposition_filename)) {
    res$setHeader("Content-Disposition",
                  sprintf('attachment; filename="%s"', out$disposition_filename))
  }
  res$body <- readBin(out$path, "raw", n = file.info(out$path)$size)
  res
}
