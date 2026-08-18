# =============================================================================
# CelliVerse Agent — provider adapters
#
# Each adapter converts:
#   internal messages/tools  ->  provider request
#   provider response        ->  internal normalized response
#
# Adding a new provider (Groq, OpenRouter, Together, Mistral, Cohere, ...) means
# writing ONE more adapter with this same signature and registering it in the
# switch() inside cv_chat(). Nothing else in the agent changes.
#
# Signature for every adapter:
#   function(messages, model, tools, temperature, stream, on_delta, config, ...)
#   -> cv_llm_response(...)
#
# Streaming contract (Batch 3, item 10 -- documented, not changed, this round):
# every adapter accepts `stream`/`on_delta` and, when both are set, calls
# on_delta() with normalized {type="token", text=...} events followed by one
# terminal {type="done", finish_reason=...} event. BUT not every adapter
# streams tokens as they actually arrive over the wire:
#   - cv_chat_openai / cv_stream_openai : REAL token-by-token SSE streaming.
#   - cv_chat_ollama / cv_stream_ollama : REAL token-by-token streaming (when
#     no tool schema is attached -- Ollama rejects stream+tools; see the
#     "Ollama streaming + tools must NOT be combined" tests).
#   - cv_chat_anthropic                : FAKE streaming. Anthropic's Messages
#     API does support SSE, but this adapter does not use it; it makes one
#     ordinary blocking call and, if the caller asked to stream, replays the
#     full response as a SINGLE "token" event immediately followed by "done".
#   - cv_chat_gemini                   : FAKE streaming, same shape as
#     Anthropic (one blocking call, one synthetic "token" event, then "done").
# Net effect: a UI that shows incremental "thinking..." text will appear to
# emit the whole answer in one paint for Anthropic/Gemini, whereas OpenAI/
# Ollama fill in progressively. This is a known, accepted simplification
# (real SSE support for Anthropic/Gemini is tracked as a Batch 3b design item,
# not fixed here) -- callers must NOT assume every on_delta("token") event
# carries only a small increment; it may carry the entire message.
# =============================================================================

# -----------------------------------------------------------------------------
# OpenAI (Chat Completions API). Also the template for OpenAI-compatible
# providers (Groq / OpenRouter / Together / Mistral) — they mostly differ by
# base URL + key, so a future adapter can delegate here with a different base.
# -----------------------------------------------------------------------------

#' @param provider which OpenAI-compatible provider this call is for; selects
#'   which configured API key to use (default "openai"). DeepSeek, Groq,
#'   OpenRouter and Cerebras reuse this adapter with their own base_url + key.
#' @param max_output_tokens optional hard cap on the number of tokens the model
#'   may GENERATE for this request (maps to OpenAI's `max_tokens`).
#'
#'   Round XLII. Left unset -- which is what every caller did before this
#'   parameter existed -- an OpenAI-compatible server generates until it decides
#'   to stop or until it runs out of context. On a CLOUD provider that is
#'   harmless: the window is large and the server enforces its own ceiling. On a
#'   LOCAL server (LM Studio / llama.cpp / Jan behind this same adapter) it is
#'   not: the window is whatever the user loaded the model with, often 4096, and
#'   a request whose prompt plus generation exceeds it pushes the runtime into
#'   reallocating its KV cache mid-generation. On Apple Silicon that allocation
#'   lands in unified memory shared with the GPU, and a large enough one can
#'   take the whole machine down rather than just failing the request.
#'
#'   Callers that can PREDICT how long the reply should be (annotateCellsLLM
#'   knows it wants one JSON record per cluster) should pass that prediction so
#'   the server refuses to overrun instead of trying.
#'   Nucleus-sampling and seed values for a request.
#'
#' Round LXXX (audit #92). `top_p`, `top_k` and `seed` appeared ZERO times in
#' this entire file, so every request went out at the provider's default
#' `top_p = 1.0` -- the whole tail of the distribution kept, however low the
#' temperature. Temperature RESCALES probabilities; only nucleus sampling
#' actually removes the tail, and the tail is where a weak model emits a phantom
#' tool call or leaks raw JSON into the prose. This repo has a documented
#' history of both.
#'
#' One helper rather than four copies of the same two lines, because four
#' adapters drifting apart on sampling is precisely the class of bug this
#' codebase has fixed four times elsewhere.
#'
#' `seed` is only meaningful where the provider honours it (OpenAI-compatible
#' and Ollama). Anthropic's Messages API and Gemini's v1beta generationConfig
#' have no seed field, so their adapters simply do not ask for one -- sending an
#' ignored parameter and calling a run reproducible would be worse than not
#' offering it.
#' @param config the loaded config.
#' @return list(top_p = <num or NULL>, seed = <int or NULL>)
#' @noRd
cv_sampling_params <- function(config = list()) {
  num1 <- function(v, lo, hi) {
    if (is.null(v) || !length(v)) return(NULL)
    v <- suppressWarnings(as.numeric(v[[1]]))
    if (length(v) != 1L || is.na(v) || !is.finite(v) || v < lo || v > hi) return(NULL)
    v
  }
  tp <- num1(config$top_p, 0, 1)
  sd <- config$seed
  if (!is.null(sd)) {
    sd <- suppressWarnings(as.integer(sd[[1]]))
    if (length(sd) != 1L || is.na(sd)) sd <- NULL
  }
  list(top_p = tp, seed = sd)
}

cv_chat_openai <- function(messages, model, tools = NULL, temperature = 0.2,
                           stream = FALSE, on_delta = NULL, config = cv_load_config(),
                           base_url = "https://api.openai.com/v1",
                           provider = "openai", response_format = NULL,
                           max_output_tokens = NULL, ...) {
  key <- cv_provider_key(provider, config)
  sp <- cv_sampling_params(config)
  body <- list(
    model = model,
    messages = cv_msgs_to_openai(messages),
    temperature = temperature
  )
  # Round LXXX (audit #92). Omitted, not defaulted, when unset: an explicit
  # top_p = 1.0 is not the same as saying nothing, on providers that treat the
  # two differently.
  if (!is.null(sp$top_p)) body$top_p <- sp$top_p
  if (!is.null(sp$seed))  body$seed  <- sp$seed
  if (!is.null(max_output_tokens)) {
    mt <- suppressWarnings(as.integer(max_output_tokens))
    if (!is.na(mt) && mt > 0L) body$max_tokens <- mt
  }
  if (!is.null(tools) && length(tools)) {
    body$tools <- tools
    body$tool_choice <- "auto"
  }
  # JSON mode: constrain the model to emit a single valid JSON object. Used by
  # annotateCellsLLM/ceLLMarkup so the annotation reply is machine-parseable
  # instead of prose the lenient parser silently degrades to "Unknown".
  # Supported by OpenAI + the OpenAI-compatible providers (OpenRouter, Groq,
  # DeepSeek, Cerebras, LM Studio). Only set when requested AND no tools (the
  # two modes are mutually exclusive on most backends).
  if (!is.null(response_format) && (is.null(tools) || !length(tools))) {
    if (identical(response_format, "json") || identical(response_format, "json_object")) {
      body$response_format <- list(type = "json_object")
    } else if (is.list(response_format)) {
      body$response_format <- response_format
    }
  }
  headers <- list(Authorization = paste("Bearer", key), `Content-Type` = "application/json")
  # OpenRouter recommends (optional) attribution headers; harmless elsewhere and
  # only added for that provider so we don't send stray headers to OpenAI/Groq.
  if (identical(provider, "openrouter")) {
    headers$`HTTP-Referer` <- "https://github.com/celliverse/celliverse"
    headers$`X-Title`      <- "CelliVerse Agent"
  }

  if (isTRUE(stream) && is.function(on_delta)) {
    return(cv_stream_openai(base_url, body, headers, on_delta, provider = provider))
  }
  parsed <- cv_http_post_json(paste0(base_url, "/chat/completions"), body, headers,
                              provider = provider)
  cv_openai_parse_response(parsed)
}

#' Convert internal messages -> OpenAI message array
#' @noRd
cv_msgs_to_openai <- function(messages) {
  lapply(messages, function(m) {
    role <- m$role
    if (role == "tool") {
      return(list(role = "tool", tool_call_id = m$tool_call_id, content = m$content %||% ""))
    }
    out <- list(role = role, content = m$content %||% "")
    if (role == "assistant" && !is.null(m$tool_calls) && length(m$tool_calls)) {
      out$content <- m$content   # may be NULL
      out$tool_calls <- lapply(m$tool_calls, function(tc) {
        list(id = tc$id, type = "function",
             `function` = list(name = tc$name,
                               arguments = jsonlite::toJSON(tc$arguments, auto_unbox = TRUE)))
      })
    }
    out
  })
}

#' Parse OpenAI response -> normalized
#' @noRd
cv_openai_parse_response <- function(parsed) {
  # BATCH2 FIX: a well-formed 2xx response can still carry an empty
  # `choices` array (e.g. certain content-filter responses, or a momentary
  # provider glitch) -- `parsed$choices[[1]]` on a zero-length list throws a
  # raw "subscript out of bounds" error instead of a clear failure. Degrade
  # to NULL and let the existing `msg$content`/`choice$finish_reason` `%||%`
  # handling below take over, exactly as it already does when the whole
  # response body is missing.
  choice <- if (length(parsed$choices)) parsed$choices[[1]] else NULL
  msg <- choice$message
  tcs <- NULL
  if (!is.null(msg$tool_calls) && length(msg$tool_calls)) {
    tcs <- lapply(msg$tool_calls, function(tc) {
      cv_tool_call(tc$id, tc$`function`$name, cv_parse_tool_args(tc$`function`$arguments))
    })
  }
  cv_llm_response(
    content = msg$content,
    tool_calls = tcs,
    finish_reason = choice$finish_reason %||% "stop",
    raw = parsed, usage = parsed$usage
  )
}

#' Streaming path for OpenAI (SSE from the API). Accumulates tokens + tool calls.
#' @noRd
cv_stream_openai <- function(base_url, body, headers, on_delta, provider = "openai") {
  body$stream <- TRUE
  acc_content <- ""
  tool_acc <- list()  # index -> list(id, name, args_str)

  req <- httr2::request(paste0(base_url, "/chat/completions"))
  req <- httr2::req_headers(req, !!!headers)
  req <- httr2::req_body_json(req, body, auto_unbox = TRUE)

  handle_line <- function(line) {
    if (!nzchar(line) || !startsWith(line, "data:")) return(invisible())
    payload <- trimws(sub("^data:", "", line))
    if (identical(payload, "[DONE]")) return(invisible())
    chunk <- tryCatch(jsonlite::fromJSON(payload, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(chunk)) return(invisible())
    delta <- tryCatch(chunk$choices[[1]]$delta, error = function(e) NULL)
    if (is.null(delta)) return(invisible())
    if (!is.null(delta$content) && nzchar(delta$content)) {
      acc_content <<- paste0(acc_content, delta$content)
      on_delta(list(type = "token", text = delta$content))
    }
    if (!is.null(delta$tool_calls)) {
      for (tc in delta$tool_calls) {
        idx <- as.character((tc$index %||% 0) + 1)
        if (is.null(tool_acc[[idx]])) tool_acc[[idx]] <<- list(id = NULL, name = NULL, args = "")
        if (!is.null(tc$id)) tool_acc[[idx]]$id <<- tc$id
        if (!is.null(tc$`function`$name)) tool_acc[[idx]]$name <<- tc$`function`$name
        if (!is.null(tc$`function`$arguments)) tool_acc[[idx]]$args <<- paste0(tool_acc[[idx]]$args, tc$`function`$arguments)
      }
    }
  }

  # httr2 streaming throws on both HTTP-status errors AND transport failures.
  # Use a SINGLE handler and branch on the condition class inside it. (Sibling
  # tryCatch handlers do NOT work here: the cli_abort() raised inside one
  # handler is re-caught by the other, nesting the wrong classifier's message.)
  #   * httr2_http (e.g. 401 bad key, 404 bad model) -> the rich HTTP-status
  #     classifier, which owns key/model/tools hints (response attached as
  #     e$resp, so we recover status + body).
  #   * anything else (DNS/refused/timeout/TLS) -> the connection classifier,
  #     which names the provider and gives the offline hint.
  resp <- tryCatch(
    httr2::req_perform_connection(req),
    error = function(e) {
      if (inherits(e, "httr2_http")) {
        st  <- tryCatch(httr2::resp_status(e$resp), error = function(x) NA_integer_)
        par <- tryCatch(httr2::resp_body_json(e$resp, simplifyVector = FALSE),
                        error = function(x) list())
        raw <- tryCatch(httr2::resp_body_string(e$resp), error = function(x) "")
        cv_llm_http_error(st, par, raw)
      } else {
        cv_llm_connection_error(provider, e)
      }
    })
  on.exit(try(close(resp), silent = TRUE), add = TRUE)
  repeat {
    line <- httr2::resp_stream_lines(resp, lines = 1)
    if (length(line) == 0L) break
    handle_line(line)
  }

  tcs <- NULL
  if (length(tool_acc)) {
    tcs <- lapply(tool_acc, function(t) cv_tool_call(t$id, t$name, cv_parse_tool_args(t$args)))
    tcs <- unname(tcs)
  }
  finish <- if (!is.null(tcs)) "tool_calls" else "stop"
  on_delta(list(type = "done", finish_reason = finish))
  cv_llm_response(content = if (nzchar(acc_content)) acc_content else NULL,
                  tool_calls = tcs, finish_reason = finish)
}

# -----------------------------------------------------------------------------
# Anthropic (Messages API). Tool use via `tools` + `tool_use`/`tool_result`.
# -----------------------------------------------------------------------------

#' @noRd
cv_chat_anthropic <- function(messages, model, tools = NULL, temperature = 0.2,
                              stream = FALSE, on_delta = NULL, config = cv_load_config(),
                              base_url = "https://api.anthropic.com/v1", max_tokens = 4096,
                              response_format = NULL, ...) {
  # Anthropic has no native JSON-mode toggle; strict-JSON is enforced via the
  # prompt (ceLLMarkup already instructs STRICT JSON ONLY). response_format is
  # accepted for interface compatibility and ignored here.
  key <- cv_provider_key("anthropic", config)
  sys <- cv_extract_system(messages)
  sp <- cv_sampling_params(config)
  body <- list(
    model = model,
    max_tokens = max_tokens,
    temperature = temperature,
    messages = cv_msgs_to_anthropic(messages)
  )
  # Round LXXX (audit #92). No `seed` here: the Messages API has no such
  # parameter, and sending one that is ignored while telling the user a run is
  # reproducible would be a lie the code tells for them.
  if (!is.null(sp$top_p)) body$top_p <- sp$top_p
  if (nzchar(sys)) body$system <- sys
  if (!is.null(tools) && length(tools)) body$tools <- cv_tools_to_anthropic(tools)
  headers <- list(`x-api-key` = key, `anthropic-version` = "2023-06-01",
                  `Content-Type` = "application/json")
  # FAKE streaming (see the file-header "Streaming contract" note): Anthropic's
  # Messages API does support real SSE, but this adapter does not use it. We
  # make one ordinary blocking call and, if the caller asked to stream, replay
  # the full response as a single "token" event immediately followed by
  # "done" -- so a streaming UI sees the whole answer appear at once rather
  # than incrementally, unlike the OpenAI/Ollama adapters.
  parsed <- cv_http_post_json(paste0(base_url, "/messages"), body, headers,
                              provider = "anthropic")
  resp <- cv_anthropic_parse_response(parsed)
  if (isTRUE(stream) && is.function(on_delta)) {
    if (!is.null(resp$content)) on_delta(list(type = "token", text = resp$content))
    on_delta(list(type = "done", finish_reason = resp$finish_reason))
  }
  resp
}

#' Pull the concatenated system prompt out of internal messages
#' @noRd
cv_extract_system <- function(messages) {
  sys <- vapply(messages, function(m) if (identical(m$role, "system")) (m$content %||% "") else "", character(1))
  paste(sys[nzchar(sys)], collapse = "\n\n")
}

#' Convert internal messages -> Anthropic messages (system stripped)
#' @noRd
cv_msgs_to_anthropic <- function(messages) {
  msgs <- Filter(function(m) !identical(m$role, "system"), messages)
  lapply(msgs, function(m) {
    if (m$role == "tool") {
      return(list(role = "user", content = list(list(
        type = "tool_result", tool_use_id = m$tool_call_id, content = m$content %||% ""))))
    }
    if (m$role == "assistant" && !is.null(m$tool_calls) && length(m$tool_calls)) {
      blocks <- list()
      if (!is.null(m$content) && nzchar(m$content)) blocks <- c(blocks, list(list(type = "text", text = m$content)))
      blocks <- c(blocks, lapply(m$tool_calls, function(tc) {
        list(type = "tool_use", id = tc$id, name = tc$name, input = tc$arguments)
      }))
      return(list(role = "assistant", content = blocks))
    }
    list(role = m$role, content = m$content %||% "")
  })
}

#' Convert provider-neutral tool specs -> Anthropic tool format
#' @noRd
cv_tools_to_anthropic <- function(tools) {
  lapply(tools, function(t) {
    f <- t$`function`
    list(name = f$name, description = f$description, input_schema = f$parameters)
  })
}

#' Parse Anthropic response -> normalized
#' @noRd
cv_anthropic_parse_response <- function(parsed) {
  content_txt <- NULL; tcs <- list()
  for (block in parsed$content %||% list()) {
    if (identical(block$type, "text")) {
      content_txt <- paste0(content_txt %||% "", block$text)
    } else if (identical(block$type, "tool_use")) {
      tcs <- c(tcs, list(cv_tool_call(block$id, block$name, block$input %||% list())))
    }
  }
  if (!length(tcs)) tcs <- NULL
  cv_llm_response(
    content = content_txt,
    tool_calls = tcs,
    finish_reason = if (!is.null(tcs)) "tool_calls" else (parsed$stop_reason %||% "stop"),
    raw = parsed, usage = parsed$usage
  )
}

# -----------------------------------------------------------------------------
# Google Gemini (generateContent). Tool use via functionDeclarations.
# -----------------------------------------------------------------------------

#' @noRd
cv_chat_gemini <- function(messages, model, tools = NULL, temperature = 0.2,
                           stream = FALSE, on_delta = NULL, config = cv_load_config(),
                           base_url = "https://generativelanguage.googleapis.com/v1beta",
                           response_format = NULL, max_output_tokens = NULL, ...) {
  # Gemini supports responseMimeType="application/json"; set it when JSON mode
  # is requested and no tools are in play (the two are mutually exclusive).
  key <- cv_provider_key("gemini", config)
  sys <- cv_extract_system(messages)
  sp <- cv_sampling_params(config)
  gen_cfg <- list(temperature = temperature)
  # Round LXXX (audit #92). Gemini names these topP / maxOutputTokens inside
  # generationConfig; it has no seed on v1beta.
  if (!is.null(sp$top_p)) gen_cfg$topP <- sp$top_p
  if (!is.null(max_output_tokens)) {
    mt <- suppressWarnings(as.integer(max_output_tokens))
    if (!is.na(mt) && mt > 0L) gen_cfg$maxOutputTokens <- mt
  }
  if (!is.null(response_format) && (is.null(tools) || !length(tools))) {
    if (identical(response_format, "json") || identical(response_format, "json_object")) {
      gen_cfg$responseMimeType <- "application/json"
    }
  }
  body <- list(
    contents = cv_msgs_to_gemini(messages),
    generationConfig = gen_cfg
  )
  if (nzchar(sys)) body$systemInstruction <- list(parts = list(list(text = sys)))
  if (!is.null(tools) && length(tools)) {
    body$tools <- list(list(functionDeclarations = lapply(tools, function(t) {
      f <- t$`function`; list(name = f$name, description = f$description, parameters = f$parameters)
    })))
  }
  url <- sprintf("%s/models/%s:generateContent?key=%s", base_url, model, key)
  headers <- list(`Content-Type` = "application/json")
  parsed <- cv_http_post_json(url, body, headers, provider = "gemini")
  resp <- cv_gemini_parse_response(parsed)
  # FAKE streaming (see the file-header "Streaming contract" note): this uses
  # Gemini's non-streaming generateContent endpoint and, if the caller asked to
  # stream, replays the full response as a single "token" event immediately
  # followed by "done" -- same simplification as cv_chat_anthropic above.
  if (isTRUE(stream) && is.function(on_delta)) {
    if (!is.null(resp$content)) on_delta(list(type = "token", text = resp$content))
    on_delta(list(type = "done", finish_reason = resp$finish_reason))
  }
  resp
}

#' Convert internal messages -> Gemini contents (roles: user/model)
#' @noRd
cv_msgs_to_gemini <- function(messages) {
  msgs <- Filter(function(m) !identical(m$role, "system"), messages)
  lapply(msgs, function(m) {
    if (m$role == "tool") {
      return(list(role = "user", parts = list(list(functionResponse = list(
        name = m$name %||% "tool", response = list(content = m$content %||% ""))))))
    }
    role <- if (m$role == "assistant") "model" else "user"
    if (m$role == "assistant" && !is.null(m$tool_calls) && length(m$tool_calls)) {
      parts <- lapply(m$tool_calls, function(tc) {
        fc <- list(name = tc$name, args = tc$arguments)
        part <- list(functionCall = fc)
        # Round-trip the Gemini 2.x thought signature captured on parse; the API
        # rejects a functionCall part in history that is missing it.
        if (!is.null(tc$thought_signature)) part$thoughtSignature <- tc$thought_signature
        part
      })
      if (!is.null(m$content) && nzchar(m$content)) parts <- c(list(list(text = m$content)), parts)
      return(list(role = role, parts = parts))
    }
    list(role = role, parts = list(list(text = m$content %||% "")))
  })
}

#' Parse Gemini response -> normalized
#' @noRd
cv_gemini_parse_response <- function(parsed) {
  cand <- tryCatch(parsed$candidates[[1]], error = function(e) NULL)
  content_txt <- NULL; tcs <- list()
  for (part in tryCatch(cand$content$parts, error = function(e) list()) %||% list()) {
    if (!is.null(part$text)) content_txt <- paste0(content_txt %||% "", part$text)
    if (!is.null(part$functionCall)) {
      tc <- cv_tool_call(NULL, part$functionCall$name, part$functionCall$args %||% list())
      # Gemini 2.x attaches a `thoughtSignature` to each functionCall part and
      # REQUIRES it to be echoed back verbatim on the next request, or the
      # follow-up call fails with HTTP 400 "Function call is missing a
      # thought_signature". Capture it here (camelCase in the API; snake_case
      # also tolerated) so cv_msgs_to_gemini can round-trip it.
      sig <- part$functionCall$thoughtSignature %||% part$thoughtSignature %||%
             part$functionCall$thought_signature %||% part$thought_signature
      if (!is.null(sig)) tc$thought_signature <- sig
      tcs <- c(tcs, list(tc))
    }
  }
  if (!length(tcs)) tcs <- NULL
  cv_llm_response(
    content = content_txt, tool_calls = tcs,
    finish_reason = if (!is.null(tcs)) "tool_calls" else (cand$finishReason %||% "stop"),
    raw = parsed, usage = parsed$usageMetadata
  )
}

# -----------------------------------------------------------------------------
# Ollama (local, /api/chat). Supports tools for tool-capable models
# (qwen2.5, llama3.1, ...). Streaming via newline-delimited JSON.
# -----------------------------------------------------------------------------

#' @param max_output_tokens optional hard cap on generated tokens (maps to
#'   Ollama's `num_predict`). See cv_chat_openai() for why an uncapped
#'   generation against a LOCAL server is dangerous rather than merely slow.
#' @noRd
cv_chat_ollama <- function(messages, model, tools = NULL, temperature = 0.2,
                           stream = FALSE, on_delta = NULL, config = cv_load_config(),
                           response_format = NULL, max_output_tokens = NULL, ...) {
  host <- config$ollama_host %||% "http://localhost:11434"
  has_tools <- !is.null(tools) && length(tools) > 0L

  # Ollama's /api/chat does NOT support token streaming together with `tools`:
  # when tools are present it returns the assistant message (with any
  # tool_calls) in a single non-streamed response, and asking for stream=true
  # WITH tools makes some builds reject the request with HTTP 400. The agent
  # loop always passes the tool schema, so tool-capable turns must use the
  # non-streaming path. We only stream when there are no tools (e.g. the final
  # natural-language answer, or a tool-less chat). This keeps live token output
  # where it is actually available and avoids the 400.
  do_stream <- isTRUE(stream) && is.function(on_delta) && !has_tools

  sp <- cv_sampling_params(config)
  opts <- list(temperature = temperature)
  # Round LXXX (audit #92). Ollama takes both inside `options`.
  if (!is.null(sp$top_p)) opts$top_p <- sp$top_p
  if (!is.null(sp$seed))  opts$seed  <- sp$seed
  # Small local models default to a 4k context window, which silently truncates
  # the agent's system prompt + tool specs + history and is a major cause of
  # rule-ignoring. Give them a workable window (configurable in Settings).
  num_ctx <- suppressWarnings(as.integer(config$ollama_num_ctx %||% 8192L))
  if (!is.na(num_ctx) && num_ctx > 0L) opts$num_ctx <- num_ctx
  # Round XLII: cap GENERATION as well as context. Without num_predict, Ollama
  # generates until it decides to stop or fills num_ctx -- so a caller that asks
  # for a long structured reply (one JSON record per cluster) can drive the
  # runtime into a mid-generation KV-cache reallocation. See cv_chat_openai().
  if (!is.null(max_output_tokens)) {
    np <- suppressWarnings(as.integer(max_output_tokens))
    if (!is.na(np) && np > 0L) opts$num_predict <- np
  }
  body <- list(
    model = model,
    messages = cv_msgs_to_ollama(messages),
    stream = do_stream,
    options = opts,
    # Keep the model loaded between agent iterations; otherwise every tool
    # round-trip pays a multi-second reload.
    keep_alive = config$ollama_keep_alive %||% "30m"
  )
  if (has_tools) body$tools <- tools  # OpenAI-style tool schema
  # JSON mode: Ollama's /api/chat accepts `format: "json"` to force a valid JSON
  # object reply. Used by annotateCellsLLM/ceLLMarkup so the annotation is
  # machine-parseable. Mutually exclusive with tools (Ollama rejects both).
  if (!is.null(response_format) && !has_tools) {
    if (identical(response_format, "json") || identical(response_format, "json_object")) {
      body$format <- "json"
    }
  }

  if (isTRUE(body$stream)) {
    return(cv_with_ollama_connect_hint(host,
      cv_stream_ollama(host, body, on_delta)))
  }
  # classify_conn=FALSE: let the raw transport error reach the Ollama wrapper,
  # which owns the "ollama serve" wording. Otherwise cv_http_post_json would
  # first wrap it in a generic "Could not reach the selected provider" message
  # and the two would nest.
  post <- function(b) {
    cv_with_ollama_connect_hint(host,
      cv_http_post_json(paste0(host, "/api/chat"), b,
                        list(`Content-Type` = "application/json"),
                        classify_conn = FALSE))
  }
  parsed <- tryCatch(
    post(body),
    # Auto-fallback: a "model runner has unexpectedly stopped" 500 is almost
    # always Ollama failing to allocate RAM/VRAM for the requested context.
    # Retry ONCE at a smaller num_ctx (4096) before surfacing the error - this
    # rescues larger models (e.g. qwen2.5:14b) on memory-tight machines.
    error = function(e) {
      msg <- conditionMessage(e)
      runner_stopped <- grepl("model runner has unexpectedly stopped|unexpectedly stopped",
                              msg, ignore.case = TRUE)
      cur_ctx <- suppressWarnings(as.integer(body$options$num_ctx %||% NA_integer_))
      if (runner_stopped && (is.na(cur_ctx) || cur_ctx > 4096L)) {
        body2 <- body
        body2$options$num_ctx <- 4096L
        return(post(body2))
      }
      stop(e)
    }
  )
  cv_ollama_parse_message(parsed$message, parsed)
}

#' Wrap an Ollama request so a CONNECTION failure (server not running, wrong
#' host/port, DNS) becomes an actionable message instead of a raw httr2/curl
#' condition. HTTP status errors (already turned into cv_llm_http_error by the
#' poster / stream path) are re-raised unchanged so their provider hints
#' survive. Only genuine transport failures get the "is ollama serve running?"
#' hint.
#' @noRd
cv_with_ollama_connect_hint <- function(host, expr) {
  tryCatch(
    expr,
    error = function(e) {
      msg <- conditionMessage(e)
      # Re-raise our own already-informative LLM API errors unchanged so their
      # provider-specific hints (model-not-found, tools-unsupported, ...) survive.
      if (grepl("LLM API error", msg)) stop(e)
      # If cv_http_post_json already classified this as a connection failure
      # (generic "Could not reach ..." wording, because the Ollama call passes
      # no provider), dig out the ORIGINAL curl cause so we can rebuild a single
      # clean Ollama-specific message rather than nesting one inside the other.
      if (grepl("^Could not reach ", msg)) {
        det <- sub(".*\\(details:\\s*", "", msg)   # innermost curl phrasing
        det <- sub("\\)+\\s*$", "", det)
        if (nzchar(det)) msg <- det
      }
      # httr2/curl transport failures for a dead/unreachable local server.
      is_transport <- inherits(e, c("httr2_failure", "curl_error")) ||
        grepl("Could not resolve|Failed to connect|Connection refused|couldn't connect|Timeout was reached|Couldn't connect|Could not connect|Failed to perform HTTP|Could not reach",
              msg, ignore.case = TRUE)
      if (is_transport) {
        cli::cli_abort(c(
          "Cannot reach Ollama at {.url {host}}: {msg}",
          i = "Make sure Ollama is installed and running ({.code ollama serve}), and that the host in Settings is correct (default {.url http://localhost:11434}).",
          i = "To use a cloud model instead, pick a provider (OpenAI/Anthropic/Gemini/DeepSeek/Groq/OpenRouter/Cerebras) and set its API key in Settings."
        ), parent = e)
      }
      # Anything else: re-raise unchanged.
      stop(e)
    }
  )
}

#' Convert internal messages -> Ollama messages
#' @noRd
cv_msgs_to_ollama <- function(messages) {
  lapply(messages, function(m) {
    if (m$role == "tool") {
      # Carry the tool name (and call id) so small local models can associate
      # the result with the call that produced it. Ollama accepts `name` on
      # tool messages; unknown fields are ignored harmlessly.
      out <- list(role = "tool", content = m$content %||% "")
      if (!is.null(m$name) && nzchar(m$name %||% "")) out$name <- m$name
      if (!is.null(m$tool_call_id) && nzchar(m$tool_call_id %||% "")) out$tool_call_id <- m$tool_call_id
      return(out)
    }
    out <- list(role = m$role, content = m$content %||% "")
    if (m$role == "assistant" && !is.null(m$tool_calls) && length(m$tool_calls)) {
      out$tool_calls <- lapply(m$tool_calls, function(tc) {
        list(`function` = list(name = tc$name, arguments = tc$arguments))
      })
    }
    out
  })
}

#' Parse an Ollama message object -> normalized
#' @noRd
cv_ollama_parse_message <- function(msg, raw = NULL) {
  tcs <- NULL
  if (!is.null(msg$tool_calls) && length(msg$tool_calls)) {
    tcs <- lapply(msg$tool_calls, function(tc) {
      args <- tc$`function`$arguments
      cv_tool_call(NULL, tc$`function`$name, if (is.list(args)) args else cv_parse_tool_args(args))
    })
  }
  cv_llm_response(
    content = msg$content,
    tool_calls = tcs,
    finish_reason = if (!is.null(tcs)) "tool_calls" else "stop",
    raw = raw
  )
}

#' Streaming path for Ollama (newline-delimited JSON objects)
#' @noRd
cv_stream_ollama <- function(host, body, on_delta) {
  acc_content <- ""; final_msg <- NULL
  req <- httr2::request(paste0(host, "/api/chat"))
  req <- httr2::req_headers(req, `Content-Type` = "application/json")
  req <- httr2::req_body_json(req, body, auto_unbox = TRUE)
  # Handle status ourselves: otherwise httr2 throws a bare "HTTP 400 Bad
  # Request" and swallows Ollama's JSON error body.
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  resp <- httr2::req_perform_connection(req)
  on.exit(try(close(resp), silent = TRUE), add = TRUE)
  status <- httr2::resp_status(resp)
  if (status >= 400) {
    # Body on an error response is a single (usually non-streamed) JSON object.
    # Drain the streaming connection line-by-line (resp_stream_lines needs a
    # non-negative `lines`; there is no "read all" sentinel).
    err_lines <- character(0)
    repeat {
      ln <- tryCatch(httr2::resp_stream_lines(resp, lines = 1),
                     error = function(e) character(0))
      if (length(ln) == 0L) break
      err_lines <- c(err_lines, ln)
    }
    raw <- paste(err_lines, collapse = "\n")
    parsed <- tryCatch(jsonlite::fromJSON(raw, simplifyVector = FALSE),
                       error = function(e) list())
    cv_llm_http_error(status, parsed, raw)
  }
  repeat {
    line <- httr2::resp_stream_lines(resp, lines = 1)
    if (length(line) == 0L) break
    if (!nzchar(line)) next
    chunk <- tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(chunk)) next
    if (!is.null(chunk$message)) {
      m <- chunk$message
      if (!is.null(m$content) && nzchar(m$content)) {
        acc_content <- paste0(acc_content, m$content)
        on_delta(list(type = "token", text = m$content))
      }
      if (!is.null(m$tool_calls)) final_msg <- m
    }
    if (isTRUE(chunk$done)) { if (is.null(final_msg)) final_msg <- list(content = acc_content); break }
  }
  if (is.null(final_msg)) final_msg <- list(content = acc_content)
  if (is.null(final_msg$content) || !nzchar(final_msg$content %||% "")) final_msg$content <- acc_content
  resp_norm <- cv_ollama_parse_message(final_msg)
  on_delta(list(type = "done", finish_reason = resp_norm$finish_reason))
  resp_norm
}
