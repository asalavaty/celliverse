# =============================================================================
# CelliVerse Agent — provider registry (single source of truth)
# -----------------------------------------------------------------------------
# Round XXXIV (Batch 3b-b item 1): before this file existed, provider/API-key
# metadata -- which config field holds a provider's key, which environment
# variable(s) override it, which base URL an OpenAI-wire-format provider uses,
# which adapter function handles a provider's chat call, and which model-
# listing strategy applies -- was hand-enumerated independently in at least 7
# places across the codebase:
#   - agent_llm.R:    .cv_openai_compatible_base_url(), cv_provider_key(),
#                     cv_chat()'s adapter dispatch
#   - agent_utils.R:  .cv_supported_providers, cv_default_config(),
#                     cv_load_config()'s env-var overrides, cv_config_public()
#   - agent_api.R:    cv_api_update_settings()'s field allow-list
#   - agent_loop.R:   .cv_hotswap_config_fields
#   - agent_models.R: cv_api_provider_models()'s dispatch
# Adding a new provider meant touching every one of these by hand, with
# nothing checking they stayed in sync -- exactly the kind of drift risk
# Batch 3a item 3 and Round XXXIII item 2 already found (and fixed) once each,
# elsewhere in this codebase. This file is now the ONE place provider metadata
# is declared; every call site above now derives from `.cv_provider_registry`
# instead of hand-listing providers. Adding a new provider is now: add one
# entry here (plus a chat adapter function, if it doesn't already speak a wire
# format CelliVerse understands) -- nothing else needs to change.
# =============================================================================

#' Single source of truth for every LLM provider the agent can talk to.
#'
#' One entry per provider, in the order providers should be presented
#' (`.cv_supported_providers` is derived from `names()` of this list, so
#' reordering an entry here reorders the provider list everywhere it's used).
#'
#' Fields per entry:
#' \describe{
#'   \item{key_field}{Config field holding the API key, or `NULL` for a local
#'     provider that needs none (ollama, lmstudio).}
#'   \item{key_envs}{Character vector of environment variable names checked,
#'     in order, before falling back to the config-file value. Empty for
#'     keyless providers.}
#'   \item{needs_key}{Logical; `FALSE` for ollama/lmstudio.}
#'   \item{base_url}{`function(config)` returning this provider's base URL, or
#'     `NULL` if it doesn't route through the generic OpenAI-compatible
#'     base-URL path (openai uses its adapter's own hardcoded default;
#'     anthropic/gemini/ollama have dedicated adapters and never look at a
#'     base URL passed in from here).}
#'   \item{adapter}{Name (character, NOT a function object) of the
#'     `cv_chat_*()` function this provider dispatches to when it doesn't go
#'     through the generic OpenAI-compatible base-URL path. Stored as a name
#'     and resolved via `get()` at call time -- inside `cv_chat()`, well after
#'     package load -- so this file's position in R CMD INSTALL's (alphabetical,
#'     no Collate field) file-collation order never matters.}
#'   \item{models_kind}{Which model-listing strategy
#'     `cv_api_provider_models()` uses. `"ollama"` and `"lmstudio"` are handled
#'     by that function's own dedicated branches (each has a distinct response
#'     shape); `"openai_like"`, `"anthropic"`, and `"gemini"` dispatch to the
#'     matching `cv_models_*()` parser in agent_models.R.}
#' }
#' @keywords internal
.cv_provider_registry <- list(
  ollama = list(
    key_field = NULL, key_envs = character(0), needs_key = FALSE,
    base_url = NULL, adapter = "cv_chat_ollama", models_kind = "ollama"
  ),
  lmstudio = list(
    key_field = NULL, key_envs = character(0), needs_key = FALSE,
    base_url = function(config) (config$lmstudio_host %||% .cv_lmstudio_default_host),
    adapter = "cv_chat_openai", models_kind = "lmstudio"
  ),
  openai = list(
    key_field = "openai_key", key_envs = "OPENAI_API_KEY", needs_key = TRUE,
    base_url = NULL, adapter = "cv_chat_openai", models_kind = "openai_like"
  ),
  anthropic = list(
    key_field = "anthropic_key", key_envs = "ANTHROPIC_API_KEY", needs_key = TRUE,
    base_url = NULL, adapter = "cv_chat_anthropic", models_kind = "anthropic"
  ),
  gemini = list(
    key_field = "gemini_key", key_envs = c("GEMINI_API_KEY", "GOOGLE_API_KEY"),
    needs_key = TRUE,
    base_url = NULL, adapter = "cv_chat_gemini", models_kind = "gemini"
  ),
  deepseek = list(
    key_field = "deepseek_key", key_envs = "DEEPSEEK_API_KEY", needs_key = TRUE,
    base_url = function(config) "https://api.deepseek.com/v1",
    adapter = "cv_chat_openai", models_kind = "openai_like"
  ),
  groq = list(
    key_field = "groq_key", key_envs = "GROQ_API_KEY", needs_key = TRUE,
    base_url = function(config) "https://api.groq.com/openai/v1",
    adapter = "cv_chat_openai", models_kind = "openai_like"
  ),
  openrouter = list(
    key_field = "openrouter_key", key_envs = "OPENROUTER_API_KEY", needs_key = TRUE,
    base_url = function(config) "https://openrouter.ai/api/v1",
    adapter = "cv_chat_openai", models_kind = "openai_like"
  ),
  cerebras = list(
    key_field = "cerebras_key", key_envs = "CEREBRAS_API_KEY", needs_key = TRUE,
    base_url = function(config) "https://api.cerebras.ai/v1",
    adapter = "cv_chat_openai", models_kind = "openai_like"
  )
)

#' Every provider key-field name in the registry, e.g.
#' `c("openai_key", "anthropic_key", ...)` -- excludes keyless providers
#' (ollama, lmstudio). Single source for every call site that previously
#' hand-listed the 7 `*_key` config field names.
#' @keywords internal
.cv_provider_key_fields <- function() {
  fields <- vapply(.cv_provider_registry, function(p) p$key_field %||% NA_character_,
                   character(1))
  unname(fields[!is.na(fields)])
}

#' Apply each provider's key-field environment-variable override(s) onto
#' `cfg`, in place of the value already there (from defaults or the config
#' file) when a matching env var is set and non-empty. Mirrors the pre-
#' registry per-provider `env_or()` calls in `cv_load_config()` exactly, just
#' driven by the registry instead of one hardcoded line per provider.
#' @keywords internal
.cv_provider_apply_env_overrides <- function(cfg) {
  for (p in .cv_provider_registry) {
    if (is.null(p$key_field) || !length(p$key_envs)) next
    for (e in p$key_envs) {
      v <- Sys.getenv(e, unset = NA)
      if (!is.na(v) && nzchar(v)) {
        cfg[[p$key_field]] <- v
        break
      }
    }
  }
  cfg
}
