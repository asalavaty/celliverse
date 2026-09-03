# =============================================================================
# CelliVerse Agent — LLM abstraction (the single chat() entry point)
#
# The MOST IMPORTANT rule of this layer (per design): the rest of the codebase
# calls ONLY cv_chat(). It never calls provider-specific functions directly.
# cv_chat() dispatches via switch(provider, ...) to an adapter, and every
# adapter normalizes requests AND responses into ONE internal schema so the
# agent loop is completely provider-independent.
#
# Internal normalized message schema (list of messages):
#   list(role = "system"|"user"|"assistant"|"tool",
#        content = <chr or NULL>,
#        tool_calls = <list of list(id=, name=, arguments=<named list>)> | NULL,
#        tool_call_id = <chr> | NULL,   # for role == "tool"
#        name = <chr> | NULL)           # tool name for role == "tool"
#
# Internal normalized response schema (what every adapter returns):
#   list(content = <chr or NULL>,
#        tool_calls = <list of list(id=, name=, arguments=<named list>)> | NULL,
#        finish_reason = <chr>,         # "stop" | "tool_calls" | "length" | ...
#        raw = <the raw parsed provider response>,
#        usage = <list or NULL>)
#
# Streaming: when stream=TRUE and on_delta is a function, adapters call
# on_delta(list(type="token", text=...)) as tokens arrive, and
# on_delta(list(type="tool_call", ...)) / on_delta(list(type="done", ...)).
# =============================================================================

#' The single LLM entry point used everywhere in the agent
#'
#' @param messages internal-schema list of messages.
#' @param provider one of .cv_supported_providers.
#' @param model model id string.
#' @param tools optional list of provider-neutral tool specs (from cv_tools_specs()).
#' @param temperature sampling temperature.
#' @param stream logical; stream tokens via on_delta if provided.
#' @param on_delta optional callback(list) for streaming events.
#' @param config effective config (for keys/hosts).
#' @param ... extra provider params.
#' @return internal normalized response list.
#' @noRd
cv_chat <- function(messages, provider, model, tools = NULL, temperature = 0.2,
                    stream = FALSE, on_delta = NULL, config = cv_load_config(),
                    response_format = NULL, ...) {
  if (!provider %in% .cv_supported_providers) {
    cli::cli_abort(c(
      "Unknown LLM provider {.val {provider}}.",
      i = "Supported: {.val {(.cv_supported_providers)}}."
    ))
  }
  # DeepSeek, Groq, OpenRouter and Cerebras all speak the OpenAI Chat
  # Completions wire format, so they route through the OpenAI adapter with a
  # provider-specific base_url and provider tag (the tag picks the right key).
  base_url <- .cv_openai_compatible_base_url(provider, config)
  if (!is.null(base_url)) {
    return(cv_chat_openai(
      messages = messages, model = model, tools = tools,
      temperature = temperature, stream = stream, on_delta = on_delta,
      config = config, base_url = base_url, provider = provider,
      response_format = response_format, ...
    ))
  }
  # Round XXXIV: which cv_chat_*() function a non-OpenAI-compatible provider
  # (openai/anthropic/gemini/ollama) dispatches to now comes from
  # .cv_provider_registry instead of a hardcoded switch. The adapter is
  # stored as a NAME (not a function object) and resolved via get() here, at
  # call time -- well after package load -- so it never depends on this
  # file's position in R CMD INSTALL's file-collation order relative to
  # agent_providers.R.
  adapter <- get(.cv_provider_registry[[provider]]$adapter, mode = "function")
  adapter(messages = messages, model = model, tools = tools,
          temperature = temperature, stream = stream, on_delta = on_delta,
          config = config, response_format = response_format, ...)
}

#' Base URL for an OpenAI-compatible provider, or NULL if not one.
#'
#' `openai` itself returns NULL (it uses the adapter default base_url); only the
#' additional OpenAI-compatible providers return an explicit endpoint here.
#' `lmstudio` is the local LM Studio server (default http://localhost:1234/v1);
#' its host is user-configurable (`lmstudio_host`) so the same provider slot
#' also covers other OpenAI-compatible local servers (llama.cpp, Jan).
#' @noRd
.cv_openai_compatible_base_url <- function(provider, config = NULL) {
  reg <- .cv_provider_registry[[provider]]
  if (is.null(reg) || is.null(reg$base_url)) return(NULL)
  reg$base_url(config)
}

#' Resolve the API key for a provider from config; error with guidance if absent
#' @noRd
cv_provider_key <- function(provider, config) {
  # Round XXXIV: key field + keyless-ness now come from .cv_provider_registry
  # instead of a hardcoded switch. Behavior is unchanged, including for an
  # unrecognized provider (reg is NULL): it's treated as key-needing with no
  # key set, so it still aborts with the same message as before.
  reg <- .cv_provider_registry[[provider]]
  key <- if (!is.null(reg) && !is.null(reg$key_field)) config[[reg$key_field]] else ""
  keyless <- !is.null(reg) && !isTRUE(reg$needs_key)
  if (!keyless && !nzchar(key %||% "")) {
    cli::cli_abort(c(
      "No API key configured for provider {.val {provider}}.",
      i = "Set it in config.json under the directory returned by tools::R_user_dir('celliverse', 'cache') or via the matching environment variable."
    ))
  }
  key %||% ""
}

# ---- Normalization helpers shared by adapters -------------------------------

#' Build a normalized tool_call record
#' @noRd
cv_tool_call <- function(id, name, arguments) {
  args <- arguments %||% list()
  # BATCH2 FIX: a bare, unnamed list() has no `names` attribute, so jsonlite
  # serializes it as a JSON array `[]` rather than an object `{}`. This
  # happens for any zero-argument tool call (a real, common case for
  # no-parameter tools, and for local models that emit `arguments: ""`).
  # When that assistant message is replayed on a later turn, every provider
  # wire format (OpenAI/Anthropic/Gemini/Ollama) requires an object here, not
  # an array -- so tag a truly-empty arguments list as a "named empty" list,
  # which jsonlite always serializes as `{}`. Non-empty arguments are
  # untouched.
  if (length(args) == 0L) {
    # Batch 8b: preserve cv_parse_tool_args()'s failure marker across this
    # normalization. Both a genuinely empty call and an unreadable one are
    # length 0, so the plain setNames() below would silently discard the very
    # attribute that tells them apart -- reinstating the bug one line after it
    # was fixed. Found by writing the test before assuming the wiring held.
    why <- attr(args, "cv_parse_failed", exact = TRUE)
    args <- stats::setNames(list(), character(0))
    if (!is.null(why)) attr(args, "cv_parse_failed") <- why
  }
  # Batch 8a: `id` and `arguments` are both normalized above; `name` was not,
  # and it is the one field that arrives straight off the wire with no default.
  # All five adapter call sites pass a raw model field (`tc$function$name`,
  # `block$name`, `part$functionCall$name`, and the streaming path's `t$name`,
  # which stays NULL if no delta ever carried a name), so a provider that omits
  # it -- or a stream that is cut before the name arrives -- puts NULL here.
  #
  # WHY THAT MATTERED, measured rather than assumed: the loop's unknown-tool
  # message is `sprintf("Unknown tool '%s'", tc$name)`, and sprintf() with a
  # NULL argument returns character(0) -- not a string. cv_clean_error() maps
  # that to "". The model therefore received an error with NO TEXT IN IT,
  # nothing to correct against, and re-emitted the same malformed call until the
  # iteration cap while the user waited through the whole thing for a failure
  # nobody could explain.
  #
  # Normalizing HERE rather than at the message site fixes it once for all five
  # adapters instead of once per call site, and keeps the placeholder visible in
  # the error the model reads, so it can tell "you sent no tool name" apart from
  # "you sent a name I don't have".
  if (is.null(name) || !is.character(name) || length(name) != 1L ||
      is.na(name) || !nzchar(name)) {
    name <- "(missing name)"
  }
  list(id = id %||% cv_new_id("call"), name = name, arguments = args)
}

#' Parse a JSON arguments string safely into a named list
#'
#' Batch 8b: this used to return a bare `list()` for SIX different situations --
#' arguments absent, `""`, `"null"`, JSON truncated mid-stream, a JSON array, or
#' a bare scalar -- making "the model sent nothing" and "the model sent something
#' I could not read" the same value.
#'
#' WHY THAT MATTERED, traced end to end. `clustoCell` declares exactly one
#' required parameter (`data`, a handle) and defaults for the rest. Given
#' `list()`, `cv_resolve_args()` auto-supplies the required handle when a single
#' typed object is loaded, then fills every other default. So a `clustoCell` call
#' whose arguments were truncated by a dropped stream ran a full multi-minute
#' clustering on parameters the model never chose, and reported **success** --
#' the worst failure shape there is, because nothing anywhere says a value was
#' lost. The turn's ledger then refused a correctly-formed retry of the same
#' tool, so the model could not fix it either.
#'
#' A genuinely empty call is still `list()` and still runs, unchanged. Only the
#' unreadable cases are now marked, with a `cv_parse_failed` attribute carrying
#' the reason; `cv_run_tool_call()` turns that into a tool error telling the
#' model to re-send. Marking rather than throwing keeps the decision at the call
#' site, where the tool and its arguments are both in view.
#' @param x the raw `arguments` field from the provider.
#' @return a list. When the input could not be read as a JSON object, the list is
#'   empty and carries `attr(., "cv_parse_failed")`, a short reason string.
#' @noRd
cv_parse_tool_args <- function(x) {
  fail <- function(why) structure(list(), cv_parse_failed = why)
  if (is.null(x)) return(list())
  if (is.list(x)) return(x)
  if (!is.character(x) || length(x) != 1L || is.na(x)) return(fail("not a string"))
  if (!nzchar(trimws(x))) return(list())            # genuinely no arguments
  # "null" is what several providers send for a zero-argument call, so it is an
  # empty call rather than a parse failure.
  if (identical(tolower(trimws(x)), "null")) return(list())
  out <- tryCatch(jsonlite::fromJSON(x, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(out)) return(fail("the arguments were not valid JSON (often a truncated reply)"))
  out <- as.list(out)
  if (!length(out)) return(list())                  # `{}` -- an empty call
  # A JSON array or bare scalar parses fine but has no names, so every declared
  # parameter looks absent downstream -- indistinguishable from `{}` and the
  # reason the truncated case was invisible.
  if (is.null(names(out)) || !any(nzchar(names(out)))) {
    return(fail("the arguments were not a JSON object (an array or bare value was sent)"))
  }
  out
}

#' Standard normalized response constructor
#' @noRd
cv_llm_response <- function(content = NULL, tool_calls = NULL,
                            finish_reason = "stop", raw = NULL, usage = NULL) {
  list(content = content, tool_calls = tool_calls,
       finish_reason = finish_reason, raw = raw, usage = usage)
}

#' Does a normalized response request tool calls?
#' @noRd
cv_response_has_tools <- function(resp) {
  !is.null(resp$tool_calls) && length(resp$tool_calls) > 0L
}

# ---- HTTP helper (httr2) ----------------------------------------------------

#' Turn an HTTP >=400 LLM response into an informative R error.
#'
#' Shared by the non-streaming poster and the Ollama streaming path so both
#' surface the provider's real error body (and an actionable hint for the most
#' common Ollama failures) instead of httr2's bare "HTTP 400 Bad Request".
#' `parsed` is the JSON body as a list (may be empty); `raw_body` is the string
#' body fallback. Always aborts.
#' @noRd
cv_llm_http_error <- function(status, parsed = list(), raw_body = NULL) {
  # Extract a useful message across provider shapes:
  #   OpenAI/Anthropic/Gemini -> $error$message (object)
  #   Ollama                  -> $error (plain string), e.g.
  #     "model 'qwen2.5:7b-instruct' not found, try pulling it first"
  err_field <- parsed$error
  msg <- NULL
  if (is.character(err_field)) msg <- err_field
  else if (is.list(err_field)) msg <- err_field$message %||% err_field$msg
  if (is.null(msg)) msg <- parsed$message
  if (is.null(msg)) msg <- raw_body
  if (is.null(msg) || !nzchar(msg)) msg <- sprintf("HTTP %d with empty body", status)

  hint <- ""
  # Order matters: match the most specific provider phrasings first so a Gemini
  # 404 does not fall through to the Ollama "ollama pull" hint.
  if (grepl("is not found for API version|ListModels|not supported for generateContent", msg, ignore.case = TRUE) ||
      (status == 404L && grepl("models/", msg, ignore.case = TRUE))) {
    # Gemini/cloud: the requested model id is not valid for this provider/API.
    hint <- paste0(
      " -- the model name is not valid for the selected provider. Open Settings, ",
      "confirm the Provider, and enter a model that provider actually serves ",
      "(for Gemini use a CURRENT model, e.g. gemini-2.5-flash, gemini-flash-latest, ",
      "or gemini-2.5-pro; the gemini-1.5-* models were retired by Google in 2025, so ",
      "they now 404. See https://ai.google.dev/gemini-api/docs/models for the live list). ",
      "A leftover model from a different provider (e.g. an Ollama name:tag) also 404s here."
    )
  } else if (grepl("thought.?signature", msg, ignore.case = TRUE)) {
    # Should not occur now that signatures are round-tripped; kept as a clear
    # fallback rather than surfacing Google's raw internal-sounding message.
    hint <- paste0(
      " -- this is a Gemini tool-calling protocol error; update CelliVerse Agent, ",
      "or start a new chat and retry with a current model (e.g. gemini-2.5-flash)."
    )
  } else if (grepl("not found.*pull|try pulling", msg, ignore.case = TRUE) ||
             grepl("model .* not found", msg, ignore.case = TRUE)) {
    hint <- " -- run install_celliverse_agent() or `ollama pull <model>` to download it."
  } else if (grepl("does not support tools|tools.*not support", msg, ignore.case = TRUE)) {
    hint <- " -- this model has no tool-calling support; pick a tool-capable model (e.g. qwen3:8b) or a cloud provider."
  } else if (grepl("API key not valid|API_KEY_INVALID|invalid.*api key|unauthorized|permission denied", msg, ignore.case = TRUE)) {
    hint <- " -- the API key is missing or invalid for this provider; re-enter it in Settings."
  } else if (grepl("model runner has unexpectedly stopped|unexpectedly stopped|runner has stopped", msg, ignore.case = TRUE)) {
    # Ollama kills the model runner when it cannot allocate enough RAM/VRAM
    # (common with larger models at a high num_ctx, or when another local
    # server such as LM Studio is also holding a model in memory).
    hint <- paste0(
      " -- Ollama stopped the model runner, almost always because it could not ",
      "get enough RAM/VRAM. Free memory (close other apps, and quit LM Studio or ",
      "unload its model if it is running), lower the context size in Settings ",
      "(Ollama num_ctx, e.g. 4096), or pick a smaller model. The agent already ",
      "retried once at a lower context size; if this persists it is a memory ",
      "limit, not a bug in the model."
    )
  } else if (grepl("more system memory|out of memory|memory", msg, ignore.case = TRUE)) {
    hint <- " -- the model needs more RAM/VRAM than is available; try a smaller model."
  } else if (status == 404L) {
    # Empty-body / unparsed 404 (Gemini returns this for a bad model id too).
    hint <- paste0(
      " -- a 404 usually means the model name is not valid for the selected ",
      "provider; check the Provider and model in Settings."
    )
  }
  cli::cli_abort("LLM API error ({status}): {msg}{hint}")
}

#' Turn a CONNECTION-level failure (no HTTP status was ever returned) into a
#' clear, provider-named message.
#'
#' `req_perform()` throws before any status when DNS fails, the connection is
#' refused, TLS fails, or the request times out. Left raw, the user sees
#' httr2/curl's "Could not resolve host: generativelanguage.googleapis.com".
#' This classifies the underlying curl message and names the provider + the
#' likely cause, and — because CelliVerse can run fully offline — points cloud
#' failures at Ollama. Always aborts. `provider` may be NULL (generic wording).
#' @noRd
cv_llm_connection_error <- function(provider, cond) {
  raw <- conditionMessage(cond)
  # Idempotency guard: if we are handed a condition we already classified
  # (e.g. a nested handler, or the streaming path re-catching), re-abort with
  # the same message instead of wrapping "Could not reach ..." inside itself.
  if (grepl("^Could not reach ", raw)) cli::cli_abort(raw)
  # httr2 wraps the curl error as the parent; dig for the curl phrasing.
  parent_msg <- tryCatch(conditionMessage(cond$parent), error = function(e) NULL)
  probe <- paste(c(raw, parent_msg), collapse = " ")
  # For a named provider `prov` is just the name (e.g. "gemini"); with no
  # provider it degrades to "the selected provider" so sentences still read.
  prov <- if (!is.null(provider) && nzchar(provider)) provider else "the selected provider"
  # bare name used right after "the " (e.g. "the gemini host"); "target" if unknown
  prov_host <- if (!is.null(provider) && nzchar(provider)) provider else "target"
  is_ollama <- !is.null(provider) && identical(tolower(provider), "ollama")
  is_lmstudio <- !is.null(provider) && identical(tolower(provider), "lmstudio")

  cause <- NULL
  if (grepl("resolve host|Could not resolve|name resolution|getaddrinfo|no address", probe, ignore.case = TRUE)) {
    cause <- if (is_ollama)
      "could not reach the local Ollama server (is Ollama running? start it with `ollama serve`, and check the host in Settings)"
    else if (is_lmstudio)
      "could not reach the local LM Studio server (is LM Studio running? start the server from the Developer page, and check the host in Settings)"
    else
      paste0("could not resolve the ", prov_host, " host - this machine has no internet access to that service, or the host is blocked by a firewall/proxy")
  } else if (grepl("Connection refused|Failed to connect|couldn.t connect|connection reset|cannot open the connection|open.connection", probe, ignore.case = TRUE)) {
    cause <- if (is_ollama)
      "the local Ollama server refused the connection (start it with `ollama serve`, or fix the host/port in Settings)"
    else if (is_lmstudio)
      "the local LM Studio server refused the connection (start it from the Developer page in LM Studio - default port 1234 - or fix the host/port in Settings)"
    else
      paste0("the connection to ", prov, " could not be opened (network blocked, or the service is down)")
  } else if (grepl("[Tt]imed? ?out|timeout", probe)) {
    cause <- paste0("the request to ", prov, " timed out (slow or blocked network connection)")
  } else if (grepl("SSL|TLS|certificate", probe, ignore.case = TRUE)) {
    cause <- paste0("a TLS/certificate error occurred talking to ", prov, " (a proxy may be intercepting HTTPS)")
  } else {
    cause <- paste0("the request to ", prov, " could not be completed")
  }

  tip <- if (is_ollama)
    " Make sure Ollama is installed and running locally."
  else if (is_lmstudio)
    " Make sure LM Studio is installed and its local server is running (Developer page > Start Server, default port 1234)."
  else
    " If you have no internet access here, switch Provider to \"ollama\" in Settings to run a local model fully offline."

  cli::cli_abort("Could not reach {prov}: {cause}.{tip} (details: {raw})")
}

#' POST JSON and return parsed body; used by cloud adapters (non-stream path).
#' `provider` (optional) lets connection failures name the provider clearly.
#' `classify_conn=FALSE` re-raises the raw transport condition unchanged so a
#' caller that owns its own connection wording (e.g. the Ollama wrapper) can
#' classify it once, without this function nesting a generic message first.
#' @noRd
cv_http_post_json <- function(url, body, headers = list(), timeout = 300,
                              provider = NULL, classify_conn = TRUE) {
  req <- httr2::request(url)
  req <- httr2::req_headers(req, !!!headers)
  req <- httr2::req_body_json(req, body, auto_unbox = TRUE)
  req <- httr2::req_timeout(req, timeout)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)  # handle status ourselves
  # A thrown error here is connection-level (no status was returned). HTTP
  # >=400 does NOT throw (req_error above), so it is handled below instead.
  # ONE handler only: httr2_error already inherits from `error`, so a single
  # `error=` catches both transport failures and anything else. Using two
  # sibling handlers double-classified the failure, because the cli_abort()
  # raised inside the first handler was re-caught by the second one, nesting
  # the friendly message inside itself.
  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) if (isTRUE(classify_conn)) cv_llm_connection_error(provider, e) else stop(e))
  status <- httr2::resp_status(resp)
  parse_failed <- FALSE
  parsed <- tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE),
                     error = function(e) { parse_failed <<- TRUE; list() })
  if (status >= 400) {
    raw <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
    cv_llm_http_error(status, parsed, raw)
  }
  # A 2xx/3xx status whose body is NOT valid JSON is not a legitimate empty
  # response from any of these APIs -- every call site (OpenAI-compatible
  # /chat/completions, Anthropic /messages, Gemini generateContent, Ollama
  # /api/chat) always returns JSON on success. A non-JSON success body means
  # something between us and the provider went wrong: a proxy/gateway
  # returning an HTML error or captive-portal page with a 200 status, a
  # truncated/corrupted transfer, a load-balancer health page, etc. Silently
  # degrading this to list() used to make cv_*_parse_response() treat it as
  # an empty-but-fine reply (content=NULL, finish_reason="stop"), so the
  # agent loop just looked like the model said nothing -- hiding the real
  # transport failure entirely. Raise a clear, actionable error instead, with
  # a preview of what was actually returned so it's diagnosable.
  if (parse_failed) {
    raw <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
    snippet <- if (nzchar(trimws(raw %||% ""))) substr(trimws(raw), 1, 300) else "(empty body)"
    prov <- if (!is.null(provider) && nzchar(provider)) provider else "the provider"
    cli::cli_abort(paste0(
      "LLM API error: {prov} returned an HTTP {status} response that is not ",
      "valid JSON, so its reply could not be read. This usually means a proxy, ",
      "gateway, or the provider itself returned an unexpected page instead of ",
      "the real API response, rather than a problem with your request. ",
      "Response body preview: {snippet}"))
  }
  parsed
}
