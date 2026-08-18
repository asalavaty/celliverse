# =============================================================================
# Provider model listing  (GET /api/models?provider=<p>)
# -----------------------------------------------------------------------------
# Populates the Settings model dropdown. For a given provider we try a LIVE
# fetch of that provider's models endpoint (using the saved key when required);
# on ANY failure (no key / offline / non-200 / parse error) we degrade to a
# small curated shortlist so the dropdown is never empty. The raw key is NEVER
# returned. Response shape (via cv_api_ok):
#   { provider, reachable, source: "live"|"curated", models:[{id,label,
#     context_length?, free?}...], note? }
# Ollama is delegated to the existing local-models handler.
# =============================================================================

#' HTTP GET returning parsed JSON, short timeout, never throws on HTTP status.
#' Returns the parsed body on 2xx, or NULL on any failure.
#' @noRd
cv_models_http_get <- function(url, headers = list(), timeout = 4) {
  tryCatch({
    req <- httr2::request(url) |>
      httr2::req_timeout(timeout) |>
      httr2::req_error(is_error = function(resp) FALSE)
    if (length(headers)) req <- do.call(httr2::req_headers, c(list(req), headers))
    resp <- httr2::req_perform(req)
    if (httr2::resp_status(resp) >= 400) return(NULL)
    httr2::resp_body_json(resp, simplifyVector = FALSE)
  }, error = function(e) NULL)
}

#' Rank + normalize a raw list of {id,label,context_length,free} model entries
#' for display: famous authors first, free surfaced within group, then
#' alphabetical. Author is the slug prefix before "/" when present.
#' @noRd
cv_models_rank <- function(models) {
  if (!length(models)) return(models)
  author_of <- function(id) {
    id <- as.character(id %||% "")
    if (grepl("/", id, fixed = TRUE)) sub("/.*$", "", id) else ""
  }
  arank <- vapply(models, function(m) {
    a <- author_of(m$id)
    idx <- match(a, .cv_model_author_rank)
    if (is.na(idx)) length(.cv_model_author_rank) + 1L else idx
  }, integer(1))
  freev <- vapply(models, function(m) isTRUE(m$free), logical(1))
  idv   <- vapply(models, function(m) as.character(m$id %||% ""), character(1))
  # free first WITHIN the same author group -> order by (author, !free, id)
  ord <- order(arank, !freev, tolower(idv))
  models[ord]
}

#' Curated fallback for a provider, wrapped with a helpful note.
#' @noRd
cv_models_curated <- function(provider, note = NULL) {
  cur <- .cv_curated_models[[provider]] %||% list()
  models <- lapply(cur, function(m) list(
    id = m$id,
    label = m$id,
    context_length = NULL,
    free = isTRUE(m$free)
  ))
  list(
    provider = provider,
    reachable = FALSE,
    source = "curated",
    models = models,
    note = note %||% "Showing a curated shortlist. Add your API key and click Refresh to load the full live list. You can also paste any exact model id."
  )
}

# ---- Per-provider live parsers ----------------------------------------------
# Each returns a list of {id,label,context_length,free} or NULL on failure.

#' OpenAI-compatible /models (openai, deepseek, groq, cerebras, openrouter,
#' lmstudio). OpenRouter and LM Studio need no key for the list; others
#' require it.
#' @noRd
cv_models_openai_like <- function(provider, cfg) {
  base <- if (provider == "openai") "https://api.openai.com/v1"
          else .cv_openai_compatible_base_url(provider, cfg)
  if (is.null(base)) return(NULL)
  key <- tryCatch(cv_provider_key(provider, cfg), error = function(e) "")
  # All except OpenRouter and LM Studio (local, no auth) require a key to
  # list; bail early to trigger curated.
  if (!provider %in% c("openrouter", "lmstudio") && (is.null(key) || !nzchar(key))) return(NULL)
  headers <- if (!is.null(key) && nzchar(key)) list(Authorization = paste("Bearer", key)) else list()
  body <- cv_models_http_get(paste0(sub("/+$", "", base), "/models"), headers)
  if (is.null(body)) return(NULL)
  data <- body$data %||% body$models %||% list()
  if (!length(data)) return(NULL)
  # OpenRouter entries carry name/context_length/pricing; plain OpenAI-style
  # only carry id. Detect a free OpenRouter model from :free suffix or pricing.
  out <- lapply(data, function(m) {
    id <- as.character(m$id %||% m$name %||% "")
    ctx <- m$context_length %||% (if (!is.null(m$top_provider)) m$top_provider$context_length else NULL)
    price0 <- FALSE
    if (!is.null(m$pricing) && !is.null(m$pricing$prompt))
      price0 <- as.character(m$pricing$prompt) %in% c("0", "0.0")
    free <- price0 || grepl(":free$", id)
    list(id = id, label = as.character(m$name %||% id),
         context_length = if (is.null(ctx)) NULL else as.integer(ctx),
         free = isTRUE(free))
  })
  # Drop non-chat variant slugs (":batch") that only apply to the batch API.
  Filter(function(m) !grepl(":batch$", m$id %||% ""), out)
}

#' Anthropic /v1/models (x-api-key + anthropic-version).
#' @noRd
cv_models_anthropic <- function(cfg) {
  key <- tryCatch(cv_provider_key("anthropic", cfg), error = function(e) "")
  if (is.null(key) || !nzchar(key)) return(NULL)
  body <- cv_models_http_get(
    "https://api.anthropic.com/v1/models",
    list(`x-api-key` = key, `anthropic-version` = "2023-06-01"))
  if (is.null(body)) return(NULL)
  data <- body$data %||% list()
  if (!length(data)) return(NULL)
  lapply(data, function(m) list(
    id = as.character(m$id %||% ""),
    label = as.character(m$display_name %||% m$id %||% ""),
    context_length = NULL, free = FALSE))
}

#' Gemini /v1beta/models?key= . Ids come back as "models/gemini-..."; strip the
#' prefix and keep only models that support generateContent.
#' @noRd
cv_models_gemini <- function(cfg) {
  key <- tryCatch(cv_provider_key("gemini", cfg), error = function(e) "")
  if (is.null(key) || !nzchar(key)) return(NULL)
  body <- cv_models_http_get(
    paste0("https://generativelanguage.googleapis.com/v1beta/models?key=",
           utils::URLencode(key, reserved = TRUE)))
  if (is.null(body)) return(NULL)
  data <- body$models %||% list()
  if (!length(data)) return(NULL)
  out <- list()
  for (m in data) {
    methods <- unlist(m$supportedGenerationMethods %||% list())
    if (length(methods) && !("generateContent" %in% methods)) next
    id <- sub("^models/", "", as.character(m$name %||% ""))
    if (!nzchar(id)) next
    out[[length(out) + 1L]] <- list(
      id = id, label = as.character(m$displayName %||% id),
      context_length = if (is.null(m$inputTokenLimit)) NULL else as.integer(m$inputTokenLimit),
      free = FALSE)
  }
  if (!length(out)) return(NULL)
  out
}

#' GET /api/models?provider=<p> handler.
#' @noRd
cv_api_provider_models <- function(provider = NULL, cfg = cv_load_config()) {
  provider <- as.character(provider %||% cfg$default_provider %||% "")[1]
  if (!nzchar(provider) || !(provider %in% .cv_supported_providers)) {
    return(cv_api_err(sprintf("Unknown or missing provider '%s'.", provider)))
  }

  # Ollama: reuse the local-models handler, mapped to the common shape.
  if (provider == "ollama") {
    om <- cv_api_ollama_models(cfg)$data
    installed <- unlist(om$installed %||% list())
    models <- lapply(installed, function(x) list(id = x, label = x,
                                                 context_length = NULL, free = TRUE))
    return(cv_api_ok(list(
      provider = "ollama",
      reachable = isTRUE(om$reachable),
      source = if (isTRUE(om$reachable)) "live" else "curated",
      models = models,
      tiers = om$tiers,
      note = if (isTRUE(om$reachable)) NULL else
        "Ollama is not reachable. Start it with `ollama serve`, then Refresh. Recommended local models are the light/strong tiers."
    )))
  }

  # LM Studio: local server, no key, no curated fallback - when unreachable the
  # honest answer is an empty list plus a "start the server" note.
  if (provider == "lmstudio") {
    raw <- cv_models_openai_like("lmstudio", cfg)
    if (is.null(raw)) {
      host <- cfg$lmstudio_host %||% .cv_lmstudio_default_host
      return(cv_api_ok(list(
        provider = "lmstudio",
        reachable = FALSE,
        source = "live",
        models = list(),
        note = paste0(
          "LM Studio server not reachable at ", host, ". ",
          "Downloading a model inside the LM Studio app does NOT start the server. ",
          "Start it: LM Studio app > Developer page > Start Server (default port 1234), ",
          "or run `lms server start` in a terminal - then click Refresh. ",
          "You can also paste any exact model id (e.g. qwen/qwen3-8b)."
        )
      )))
    }
    raw <- Filter(function(m) nzchar(m$id %||% ""), raw)
    return(cv_api_ok(list(
      provider = "lmstudio",
      reachable = TRUE,
      source = "live",
      models = raw,
      note = NULL
    )))
  }

  # Cloud: try live, else curated. Round XXXIV: which parser applies now
  # comes from .cv_provider_registry's models_kind instead of hand-listing
  # provider names here (ollama/lmstudio, handled above, are also registry
  # models_kind values but have their own dedicated response shapes).
  raw <- switch(.cv_provider_registry[[provider]]$models_kind %||% "",
    openai_like = cv_models_openai_like(provider, cfg),
    anthropic   = cv_models_anthropic(cfg),
    gemini      = cv_models_gemini(cfg),
    NULL
  )
  if (is.null(raw) || !length(raw)) {
    return(cv_api_ok(cv_models_curated(provider)))
  }
  # Normalize (drop empty ids), rank, and return.
  raw <- Filter(function(m) nzchar(m$id %||% ""), raw)
  raw <- cv_models_rank(raw)
  cv_api_ok(list(
    provider = provider,
    reachable = TRUE,
    source = "live",
    models = raw,
    note = NULL
  ))
}
