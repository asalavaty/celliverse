# =============================================================================
# CelliVerse Agent - internal utilities
# Shared helpers: dependency checks, config, paths, logging, id generation.
# All functions here are internal (not exported).
# =============================================================================

# ---- Constants --------------------------------------------------------------

# Web/agent runtime packages that are in Suggests (not forced on analysis-only
# users). The agent checks for them at runtime and guides installation.
.cv_agent_runtime_pkgs <- c(
  "plumber", "jsonlite", "httr2", "processx",
  # Round LXXX (audit #74): svglite was missing from this list while
  # cv_save_one_plot() calls `svglite::svglite` bare -- so an agent install
  # without it passed the gate and then failed at the first figure, which is
  # the worst possible time to discover a missing package. SVG is this
  # project's DEFAULT plot format, not an optional extra.
  "later", "promises", "callr", "httpuv", "openssl", "fs", "svglite"
)

#' Minimum Seurat version the agent layer needs.
#'
#' Round LXXX (audit #73). `SeuratObject::LayerData()` is a Seurat 5.x API and
#' is called with no floor anywhere. On Seurat 4 it does not exist, the call
#' errors, `cv_detect_log_transformed()`'s tryCatch swallows the error and
#' returns "unknown" -- and the log1p guard, which exists to stop a
#' double-log-transform silently corrupting an analysis, quietly switches off.
#' Nothing tells the user. That is the worst shape a dependency problem can
#' take: not a failure, a different answer.
#'
#' Declared as a version bound on an Imports entry that already exists, plus
#' this runtime check, because the DESCRIPTION bound only fires at install time
#' and users upgrade and downgrade Seurat underneath an installed package.
#' @noRd
CV_MIN_SEURAT <- "5.0.0"

# Providers the LLM layer knows how to talk to. Round XXXIV: derived from
# .cv_provider_registry (agent_providers.R), the single source of truth for
# provider metadata -- see that file's header comment for the full rationale.
# Safe at package-load time: R CMD INSTALL's default (no Collate field)
# alphabetical file collation sources "agent_providers.R" before
# "agent_utils.R", so .cv_provider_registry already exists when this line runs.
.cv_supported_providers <- names(.cv_provider_registry)

# Local (Ollama) model tiers. Single source of truth for the recommended local
# models so config, installer, launcher and the frontend all agree.
#   - light      : small, fast, modest RAM; the safe default. Tool-calling works
#                  but is less reliable on this size of model.
#   - recommended: MoE with ~3.3B active params - near-light speed with much
#                  stronger tool-calling; the sweet spot for >=24 GB machines.
#   - strong     : largest local tier; most reliable tool-calling / instruction
#                  following; needs ~36 GB RAM to leave headroom.
# Legacy qwen2.5 entries are kept as fallbacks for <=16 GB machines and for
# users who already have them pulled.
.cv_model_tiers <- list(
  light       = "qwen3:8b",           # ~5.2 GB
  recommended = "qwen3:30b-instruct", # ~19 GB, 3.3B active MoE (Qwen3-30B-A3B-Instruct-2507)
  strong      = "qwen3.6:35b"         # ~24 GB, 35B-A3B MoE
)

# Legacy fallbacks (previous curated tiers). Still resolvable via
# cv_model_tier_id() and surfaced as lighter alternatives for small machines.
.cv_model_tiers_legacy <- list(
  light_legacy  = "qwen2.5:7b-instruct",   # ~4.7 GB, <=16 GB machines
  strong_legacy = "qwen2.5:14b-instruct"   # ~9 GB
)

# Minimum TOTAL system RAM (GB) per tier. Used by cv_local_recommendation() and
# the installer to pick the best tier that fits. Headroom above the raw model
# size accounts for KV cache (8k ctx), OS and the R session itself.
.cv_tier_min_ram_gb <- c(light = 8, recommended = 24, strong = 36)

# Default base URL for the LM Studio local server (OpenAI-compatible).
.cv_lmstudio_default_host <- "http://localhost:1234/v1"

# Curated fallback model shortlists per CLOUD provider. Used ONLY when the live
# /models fetch cannot run (no key / offline / non-200 / parse error). The LIVE
# list is always preferred; this is a degraded-mode convenience so the dropdown
# is never empty and famous/capable models are one click away. Free-tier ids are
# flagged so the UI can surface them.
#
# NOTE: these are REAL, current ids captured from each provider's live catalogue
# at implementation time (2026-07). They may age; the live fetch keeps the real
# dropdown current, and users can always paste any exact slug. Ordering puts the
# most broadly useful models first.
#   - Direct providers (openai/anthropic/gemini/deepseek/groq/cerebras) use each
#     vendor's NATIVE model ids (what their own /models endpoint returns).
#   - OpenRouter uses vendor-prefixed slugs ("vendor/model"), matching its API.
.cv_curated_models <- list(
  openai = list(
    list(id = "gpt-4o-mini",            free = FALSE),
    list(id = "gpt-4o",                 free = FALSE),
    list(id = "gpt-4.1-mini",           free = FALSE),
    list(id = "gpt-4.1",                free = FALSE),
    list(id = "o4-mini",                free = FALSE)
  ),
  anthropic = list(
    list(id = "claude-3-5-haiku-latest",  free = FALSE),
    list(id = "claude-3-5-sonnet-latest", free = FALSE),
    list(id = "claude-3-7-sonnet-latest", free = FALSE),
    list(id = "claude-sonnet-4-latest",   free = FALSE),
    list(id = "claude-opus-4-latest",     free = FALSE)
  ),
  gemini = list(
    list(id = "gemini-2.5-flash",       free = FALSE),
    list(id = "gemini-2.5-pro",         free = FALSE),
    list(id = "gemini-flash-latest",    free = FALSE),
    list(id = "gemini-2.0-flash",       free = FALSE)
  ),
  deepseek = list(
    list(id = "deepseek-chat",          free = FALSE),
    list(id = "deepseek-reasoner",      free = FALSE)
  ),
  groq = list(
    list(id = "llama-3.3-70b-versatile",         free = FALSE),
    list(id = "llama-3.1-8b-instant",            free = FALSE),
    list(id = "qwen-2.5-32b",                    free = FALSE),
    list(id = "deepseek-r1-distill-llama-70b",   free = FALSE)
  ),
  cerebras = list(
    list(id = "llama-3.3-70b",          free = FALSE),
    list(id = "llama3.1-8b",            free = FALSE),
    list(id = "qwen-3-32b",             free = FALSE)
  ),
  # OpenRouter: vendor-prefixed slugs. Famous vendors first; a couple of solid
  # free options included (flagged) since OpenRouter has a real free tier.
  openrouter = list(
    list(id = "qwen/qwen3-30b-a3b-instruct-2507", free = FALSE),
    list(id = "anthropic/claude-3-haiku",        free = FALSE),
    list(id = "qwen/qwen3-coder:free",           free = TRUE),
    list(id = "anthropic/claude-3.5-sonnet",     free = FALSE),
    list(id = "anthropic/claude-sonnet-4",       free = FALSE),
    list(id = "openai/gpt-4o-mini",              free = FALSE),
    list(id = "openai/gpt-4o",                   free = FALSE),
    list(id = "google/gemini-2.5-pro",           free = FALSE),
    list(id = "qwen/qwen3.7-flash",              free = FALSE),
    list(id = "qwen/qwen-plus",                  free = FALSE),
    list(id = "deepseek/deepseek-chat",          free = FALSE),
    list(id = "meta-llama/llama-4-maverick",     free = FALSE),
    list(id = "openai/gpt-oss-20b:free",         free = TRUE),
    list(id = "nvidia/nemotron-nano-9b-v2:free", free = TRUE)
  )
)

# Author display order for ranking a LIVE model list (most broadly useful first).
.cv_model_author_rank <- c("anthropic", "openai", "google", "qwen",
                           "deepseek", "meta-llama", "mistralai")

#' Resolve a model tier name to its Ollama model id(s).
#'
#' Accepts "light"/"recommended"/"strong" (one id), "both" (light+strong,
#' back-compat), "all" (all three tiers), "auto" (best tier that fits this
#' machine's RAM), or a legacy tier name ("light_legacy"/"strong_legacy").
#' An unknown value is returned unchanged (treated as an explicit model id) so
#' callers can also pass a literal model name.
#' @noRd
cv_model_tier_id <- function(tier = "light") {
  if (length(tier) == 1L) {
    if (identical(tier, "both")) {
      return(unname(c(.cv_model_tiers$light, .cv_model_tiers$strong)))
    }
    if (identical(tier, "all")) {
      return(unlist(.cv_model_tiers, use.names = FALSE))
    }
    if (identical(tier, "auto")) {
      return(cv_local_recommendation()$model)
    }
  }
  ids <- vapply(tier, function(t) {
    if (!is.null(.cv_model_tiers[[t]])) {
      .cv_model_tiers[[t]]
    } else if (!is.null(.cv_model_tiers_legacy[[t]])) {
      .cv_model_tiers_legacy[[t]]
    } else {
      t
    }
  }, character(1), USE.NAMES = FALSE)
  ids
}

#' Hardware-aware local-model recommendation (OS + RAM -> best tier that fits).
#'
#' Picks the strongest curated Ollama tier whose minimum RAM requirement is met
#' by this machine's TOTAL system RAM, and describes the result in one honest
#' headline for onboarding/installer output. Never errors: when RAM cannot be
#' determined, falls back to the light tier with a note.
#'
#' @return list with os, ram_gb (NA when unknown), tier, model, min_ram_gb,
#'   est_speed, headline, and (for <=16 GB machines) legacy_alt - the lighter
#'   legacy qwen2.5:7b fallback id, else NULL.
#' @noRd
cv_local_recommendation <- function() {
  ram <- cv_system_ram_gb()
  os  <- Sys.info()[["sysname"]] %||% "unknown"
  tiers <- c("strong", "recommended", "light")   # best-first
  picked <- "light"
  if (!is.na(ram)) {
    for (t in tiers) {
      if (ram >= .cv_tier_min_ram_gb[[t]]) { picked <- t; break }
    }
  }
  model <- .cv_model_tiers[[picked]]
  speed <- switch(picked,
    light = "fast on most machines (Apple Silicon / GPU or a modern CPU)",
    recommended = "fast for its quality (MoE, ~3.3B active params) on Apple Silicon / GPU; usable on CPU",
    strong = "comfortable on Apple Silicon / a GPU with >=36 GB RAM; slow on CPU-only"
  )
  ram_txt <- if (is.na(ram)) "unknown RAM" else sprintf("%.0f GB RAM", ram)
  # legacy_alt must be a length-1 string, NOT NULL: jsonlite::toJSON() encodes a
  # NULL list element as `{}` (an empty object), and the React onboarding card
  # renders `{localRec.legacy_alt && ...}` - since `{}` is truthy in JS, React
  # tries to render the empty object as a child and throws "Objects are not
  # valid as a React child" (Minified React error #31), blanking the whole page
  # on machines with >16 GB RAM. Use "" (falsy in JS) when there is no legacy
  # alternative so the field serializes to a string and the UI hides it.
  legacy_alt <- if (!is.na(ram) && ram <= 16) .cv_model_tiers_legacy$light_legacy else ""
  headline <- sprintf(
    "Your machine (%s, %s) can run %s locally (free, private) - %s. Or use the cloud default.",
    os, ram_txt, model, speed
  )
  list(
    os = as.character(os),
    # Keep unknown RAM as logical NA so toJSON emits null (NA_real_ would emit
    # the string "NA"); the frontend treats null/number as "no value".
    ram_gb = if (length(ram) == 0 || is.na(ram)) NA else as.numeric(ram),
    tier = as.character(picked),
    model = as.character(model),
    min_ram_gb = as.numeric(unname(.cv_tier_min_ram_gb[[picked]])),
    est_speed = as.character(speed),
    headline = as.character(headline),
    legacy_alt = as.character(legacy_alt)
  )
}

#' Best-effort total system RAM in GB (cross-platform, never errors).
#'
#' Linux reads /proc/meminfo (MemTotal), macOS uses `sysctl hw.memsize`, and
#' Windows falls back to wmic. Returns NA_real_ when RAM cannot be determined so
#' callers can silently skip the suggestion rather than block launch.
#' @noRd
cv_system_ram_gb <- function() {
  out <- tryCatch({
    sysname <- Sys.info()[["sysname"]]
    if (identical(sysname, "Linux")) {
      mi <- readLines("/proc/meminfo", n = 50, warn = FALSE)
      line <- grep("^MemTotal:", mi, value = TRUE)
      if (!length(line)) return(NA_real_)
      kb <- as.numeric(gsub("[^0-9]", "", line[1]))   # value is in kB
      kb / 1024 / 1024
    } else if (identical(sysname, "Darwin")) {
      v <- suppressWarnings(system2("sysctl", c("-n", "hw.memsize"),
                                    stdout = TRUE, stderr = FALSE))
      bytes <- as.numeric(v[1])
      bytes / 1024^3
    } else if (identical(sysname, "Windows")) {
      v <- suppressWarnings(system2("wmic",
             c("ComputerSystem", "get", "TotalPhysicalMemory"),
             stdout = TRUE, stderr = FALSE))
      num <- suppressWarnings(as.numeric(gsub("[^0-9]", "",
                     paste(v, collapse = " "))))
      num <- num[is.finite(num) & num > 0]
      if (!length(num)) return(NA_real_)
      max(num) / 1024^3
    } else {
      NA_real_
    }
  }, error = function(e) NA_real_)
  if (length(out) != 1L || !is.finite(out)) NA_real_ else out
}

# ---- Dependency management --------------------------------------------------

#' Abort if the installed Seurat is older than the agent layer supports.
#'
#' Round LXXX (audit #73). Separate from `cv_require_agent_deps()`'s
#' missing-package check because the two failures need different sentences: one
#' says "install this", the other says "upgrade this", and a user who reads
#' "install Seurat" while Seurat is plainly installed learns to distrust the
#' message.
#'
#' Returns invisibly TRUE when the version is acceptable OR when it cannot be
#' determined -- an unreadable version is not evidence of an old one, and this
#' must never be the thing that stops a working install from starting.
#' @noRd
cv_check_seurat_version <- function(min_version = CV_MIN_SEURAT) {
  v <- tryCatch(utils::packageVersion("Seurat"), error = function(e) NULL)
  if (is.null(v)) return(invisible(TRUE))
  if (utils::compareVersion(as.character(v), min_version) >= 0) return(invisible(TRUE))
  cli::cli_abort(c(
    "The CelliVerse agent needs Seurat {min_version} or newer; {as.character(v)} is installed.",
    i = paste("Seurat 4 has no {.fn SeuratObject::LayerData}, so the agent cannot tell",
              "raw counts from log-normalized data and would silently apply the wrong",
              "transform rather than fail."),
    i = "Upgrade with {.code install.packages(\"Seurat\")}, then restart R."
  ))
}

#' Ensure agent runtime dependencies are installed
#'
#' The agent's web stack lives in Suggests so that analysis-only users are not
#' forced to install a web server. Agent entry points call this first.
#' @param pkgs character vector of package names to require.
#' @param install logical; if TRUE, attempt to install any missing packages.
#' @return invisibly TRUE if all present (after optional install), else errors.
#' @noRd
cv_require_agent_deps <- function(pkgs = .cv_agent_runtime_pkgs, install = FALSE) {
  # Round LXXX (audit #73): the Seurat floor is checked HERE, at the gate every
  # broken install hits first, rather than left to fail silently at analysis
  # time. Checked before the missing-package branch because a too-old Seurat is
  # not fixed by installing anything from `pkgs`, so reporting it first is the
  # actionable order.
  cv_check_seurat_version()
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) == 0L) {
    return(invisible(TRUE))
  }
  if (install) {
    cli::cli_alert_info("Installing missing agent dependencies: {.pkg {missing}}")
    utils::install.packages(missing)
    still <- missing[!vapply(missing, requireNamespace, logical(1), quietly = TRUE)]
    if (length(still) > 0L) {
      cli::cli_abort(c(
        "Could not install required agent packages: {.pkg {still}}",
        i = "Install them manually with {.code install.packages(c({paste0('\"', still, '\"', collapse = ', ')}))}"
      ))
    }
    return(invisible(TRUE))
  }
  cli::cli_abort(c(
    "The CelliVerse agent needs these packages, which are not installed: {.pkg {missing}}",
    i = "Run {.code celliverse::install_celliverse_agent()} (recommended), or",
    i = "install manually: {.code install.packages(c({paste0('\"', missing, '\"', collapse = ', ')}))}"
  ))
}

# ---- Paths ------------------------------------------------------------------

#' Root directory for CelliVerse agent state (~/.celliverse)
#' @noRd
cv_home_dir <- function() {
  # Allow override for testing / non-standard installs.
  override <- Sys.getenv("CELLIVERSE_HOME", unset = NA)
  if (!is.na(override) && nzchar(override)) {
    return(override)
  }
  file.path(path.expand("~"), ".celliverse")
}

#' Path to the agent config file
#' @noRd
cv_config_path <- function() {
  file.path(cv_home_dir(), "config.json")
}

#' Directory that holds per-session state and artifacts
#' @noRd
cv_sessions_dir <- function() {
  file.path(cv_home_dir(), "sessions")
}

#' Ensure the ~/.celliverse tree exists
#' @noRd
cv_ensure_home <- function() {
  dir.create(cv_home_dir(), recursive = TRUE, showWarnings = FALSE)
  dir.create(cv_sessions_dir(), recursive = TRUE, showWarnings = FALSE)
  invisible(cv_home_dir())
}

# ---- System memory ----------------------------------------------------------

#' Free system memory, in MB, or NA if it cannot be determined.
#'
#' Round XXXIX. Every admission/eviction bound in this package counts THINGS
#' (worker_pool_size, object_store_limit, job_history_limit); none has ever
#' consulted the machine's actual memory. This is the missing primitive.
#'
#' "Available" deliberately means *reclaimable without swapping*, not merely
#' "free": on both platforms a large share of RAM is normally held by caches
#' that the OS will hand back on demand, so a naive free-pages reading would
#' report a machine as starved when it is fine, and the gate would throttle
#' constantly for no reason.
#'
#' - Linux: `MemAvailable` from /proc/meminfo -- the kernel's own estimate,
#'   which already accounts for reclaimable page cache and slab.
#' - macOS: `vm_stat` free + inactive + speculative + purgeable pages, times
#'   the page size reported by the same command (16 KB on Apple Silicon, 4 KB
#'   on Intel -- parsed, never assumed). Note what this deliberately does NOT
#'   count: WIRED pages. GPU-offloaded LLM weights (LM Studio, Ollama with
#'   Metal) are wired and cannot be compressed or paged out, so they are
#'   genuinely unavailable to anyone else -- excluding them is the whole point
#'   on a unified-memory Mac.
#' - Anything else: NA, and every caller must treat NA as "no opinion" and
#'   proceed exactly as it would have before this function existed.
#'
#' Cheap enough for the admission path (a file read on Linux, one short
#' subprocess on macOS) but not free, so callers should only invoke it when
#' they are actually about to make an admission decision.
#' @return numeric MB available, or NA_real_ if undeterminable.
#' @noRd
cv_available_memory_mb <- function() {
  sys <- tolower(Sys.info()[["sysname"]] %||% "")
  out <- tryCatch({
    if (identical(sys, "linux")) {
      f <- "/proc/meminfo"
      if (!file.exists(f)) return(NA_real_)
      l <- readLines(f, warn = FALSE)
      hit <- grep("^MemAvailable:", l, value = TRUE)
      if (!length(hit)) return(NA_real_)
      as.numeric(gsub("[^0-9]", "", hit[1])) / 1024      # kB -> MB
    } else if (identical(sys, "darwin")) {
      vs <- suppressWarnings(system2("vm_stat", stdout = TRUE, stderr = FALSE))
      if (!length(vs)) return(NA_real_)
      # "Mach Virtual Memory Statistics: (page size of 16384 bytes)"
      psz <- suppressWarnings(as.numeric(
        sub(".*page size of ([0-9]+) bytes.*", "\\1", vs[1])))
      if (!is.finite(psz) || psz <= 0) return(NA_real_)
      pages <- function(label) {
        hit <- grep(paste0("^", label, ":"), vs, value = TRUE)
        if (!length(hit)) return(0)
        v <- suppressWarnings(as.numeric(gsub("[^0-9]", "", hit[1])))
        if (is.finite(v)) v else 0
      }
      (pages("Pages free") + pages("Pages inactive") +
        pages("Pages speculative") + pages("Pages purgeable")) * psz / 1048576
    } else {
      NA_real_
    }
  }, error = function(e) NA_real_, warning = function(w) NA_real_)
  if (length(out) != 1L || !is.numeric(out) || !is.finite(out)) return(NA_real_)
  as.numeric(out)
}

# ---- Config -----------------------------------------------------------------

#' Default agent configuration
#' @noRd
cv_default_config <- function() {
  cfg <- list(
    default_provider = "openrouter",
    # Qwen3-30B-A3B-Instruct-2507: strong tool-calling at a low price point;
    # same weights as the recommended Ollama tier (qwen3:30b-instruct).
    default_model    = "qwen/qwen3-30b-a3b-instruct-2507",
    temperature      = 0.2,
    # Round LXXX (audit #92). `top_p`, `top_k` and `seed` appeared ZERO times in
    # the entire adapters file, so every request went out with the provider's
    # default top_p of 1.0 -- which keeps the whole tail of the distribution no
    # matter how low the temperature is. Temperature scales the probabilities;
    # only nucleus sampling actually REMOVES the tail. This repo has a
    # documented history of phantom tool calls and leaked JSON from weak models,
    # which is exactly the failure that tail produces.
    #
    # 0.9 rather than something tighter: below ~0.8 a model starts refusing to
    # paraphrase and repeats stock sentences, and the agent's prose half would
    # get worse to make its tool-call half marginally better.
    top_p            = 0.9,
    # NULL = let the provider decide. A fixed seed makes a run repeatable on the
    # providers that honour it (OpenAI-compatible and Ollama); Anthropic and
    # Gemini have no seed parameter on these endpoints, so it is simply not
    # sent there rather than faked.
    seed             = NULL,
    ollama_host      = "http://localhost:11434",
    # LM Studio local server (OpenAI-compatible). Also accepts any other
    # OpenAI-compatible local endpoint (llama.cpp :8080/v1, Jan :1337/v1).
    lmstudio_host    = .cv_lmstudio_default_host,
    # Ollama tuning: keep the model loaded between agent iterations, and give
    # small local models a context window big enough for the system prompt +
    # tool specs + history (Ollama's 4k default silently truncates).
    ollama_keep_alive = "30m",
    ollama_num_ctx   = 8192L,
    # Server / worker settings.
    port             = 8000L,
    worker_pool_size = 2L,
    max_tool_iters   = 12L,
    tool_timeout_sec = 1800L,
    # Batch 2b fix: cap on TERMINAL (done/error/cancelled) job records kept
    # per session in sess$jobs. Without a cap, every heavy-tool run over a
    # session's entire lifetime accumulated a permanent record (including a
    # live callr process handle) with no eviction -- see
    # cv_job_evict_stale() in agent_worker.R. Running/queued jobs are never
    # evicted regardless of this limit; only old, already-finished ones are.
    job_history_limit = 50L,
    # Batch 3b item 2: three more unbounded-growth siblings of the job-history
    # leak just above, each capped the same way (a generous ceiling that
    # normal use never approaches, oldest-first eviction, anything currently
    # in use is never touched). See cv_object_evict_stale() (agent_object_
    # store.R), cv_history_evict_stale() and cv_sessions_evict_stale() (both
    # agent_session.R) for the actual eviction logic and what "in use" means
    # for each store.
    object_store_limit    = 40L,
    history_message_limit = 400L,
    # Deliberately much more generous than object_store_limit/
    # history_message_limit above: this cap is GLOBAL (every session on the
    # server at once, not one session's own data), so it needs enough
    # headroom that ordinary multi-tab/multi-day usage -- and a full
    # automated test run, which creates a great many short-lived sessions in
    # one long-lived R process -- never bumps into it. 500 idle session
    # shells (env + history list + empty job registry; the actual memory
    # cost of a session is its object store, capped separately) is a trivial
    # amount of memory; the real protection this gives is against a session
    # count that grows genuinely without bound over weeks/months of a server
    # that's never restarted.
    session_registry_limit = 500L,
    # Round XXXIX: the ONLY byte-denominated bound in this config. Every other
    # limit here (worker_pool_size, object_store_limit, job_history_limit,
    # session_registry_limit) counts THINGS; not one of them ever asks the OS
    # how much memory is actually free. A heavy tool's child process was
    # measured at ~650 MB peak RSS on the user's own 2,700-cell dataset, so
    # worker_pool_size = 2 permits a ~1.3 GB concurrent burst on top of
    # whatever else the machine is doing, with nothing checking whether the
    # machine can absorb it. When free system memory is below this many MB, an
    # ADDITIONAL concurrent heavy job is held in the existing admission queue
    # instead of being spawned. See .cv_worker_admit_or_queue()
    # (agent_worker.R) for the deliberate no-deadlock property: this never
    # blocks the FIRST heavy job, so the worst case is serial execution, never
    # a wedge. Set to 0 to disable the gate entirely.
    heavy_job_min_free_mb = 1536L
  )
  # Provider API keys. Empty by default; may be overridden by env vars (see
  # cv_load_config()). Round XXXIV: one empty-string field per provider that
  # needs a key, derived from .cv_provider_registry -- the single source of
  # truth -- instead of one hardcoded line per provider.
  for (field in .cv_provider_key_fields()) cfg[[field]] <- ""
  cfg
}

#' Load agent config, merging file + env-var overrides on top of defaults
#'
#' Env vars (OPENAI_API_KEY, ANTHROPIC_API_KEY, GEMINI_API_KEY / GOOGLE_API_KEY,
#' DEEPSEEK_API_KEY, GROQ_API_KEY, OPENROUTER_API_KEY, CEREBRAS_API_KEY,
#' OLLAMA_HOST, CELLIVERSE_PORT) take precedence over the config file for keys,
#' so users can avoid writing secrets to disk.
#' @return named list config.
#' @noRd
cv_load_config <- function() {
  cfg <- cv_default_config()
  path <- cv_config_path()
  if (file.exists(path)) {
    file_cfg <- tryCatch(
      jsonlite::read_json(path, simplifyVector = TRUE),
      error = function(e) {
        cli::cli_warn("Could not parse config at {.path {path}}: {conditionMessage(e)}. Using defaults.")
        list()
      }
    )
    cfg <- utils::modifyList(cfg, file_cfg)
  }
  # Provider API-key env overrides (only when set and non-empty). Round XXXIV:
  # driven by .cv_provider_registry instead of one hardcoded env_or() call per
  # provider -- see agent_providers.R.
  cfg <- .cv_provider_apply_env_overrides(cfg)
  # Non-key env overrides (hosts, port) are each a single field, not
  # duplicated per-provider elsewhere, so they stay hardcoded here.
  env_or <- function(key, envs) {
    for (e in envs) {
      v <- Sys.getenv(e, unset = NA)
      if (!is.na(v) && nzchar(v)) return(v)
    }
    cfg[[key]]
  }
  cfg$ollama_host    <- env_or("ollama_host",    "OLLAMA_HOST")
  cfg$lmstudio_host  <- env_or("lmstudio_host",  "CELLIVERSE_LMSTUDIO_HOST")
  port_env <- Sys.getenv("CELLIVERSE_PORT", unset = NA)
  if (!is.na(port_env) && nzchar(port_env)) cfg$port <- as.integer(port_env)
  cfg
}

#' Write agent config to disk (creates ~/.celliverse if needed)
#'
#' Note: this persists whatever is passed, including keys. The API layer keeps
#' keys write-only from the client and never returns them in responses.
#' @param cfg named list config.
#' @noRd
cv_save_config <- function(cfg) {
  cv_ensure_home()
  jsonlite::write_json(cfg, cv_config_path(), auto_unbox = TRUE, pretty = TRUE, null = "null")
  # Round LXIV Batch 2a: this file holds the user's API keys in plaintext and
  # was written at the default umask (0644 on most systems), i.e. world-readable
  # on a shared machine or a synced folder. Owner-only from now on. Wrapped
  # because chmod is a no-op on some filesystems (and on Windows), and failing
  # to tighten permissions must never prevent the settings from being saved --
  # that would trade a small exposure for a broken product.
  tryCatch(Sys.chmod(cv_config_path(), mode = "0600"), error = function(e) NULL)
  invisible(cv_config_path())
}

#' Return config safe to send to the browser (keys redacted to booleans)
#' @noRd
cv_config_public <- function(cfg = cv_load_config()) {
  key_flag <- function(x) isTRUE(nzchar(x %||% ""))
  pub <- list(
    default_provider = cfg$default_provider,
    default_model    = cfg$default_model,
    temperature      = cfg$temperature,
    top_p            = cfg$top_p,
    seed             = cfg$seed,
    ollama_host      = cfg$ollama_host,
    lmstudio_host    = cfg$lmstudio_host,
    ollama_keep_alive = cfg$ollama_keep_alive,
    ollama_num_ctx   = cfg$ollama_num_ctx,
    port             = cfg$port,
    worker_pool_size = cfg$worker_pool_size
  )
  # Booleans only - never expose the actual secrets. Round XXXIV: one
  # has_<provider>_key flag per key-needing provider, derived from
  # .cv_provider_registry instead of one hardcoded line per provider. Field
  # names (has_openai_key, has_anthropic_key, ...) are unchanged.
  for (p_name in names(.cv_provider_registry)) {
    key_field <- .cv_provider_registry[[p_name]]$key_field
    if (is.null(key_field)) next
    pub[[paste0("has_", p_name, "_key")]] <- key_flag(cfg[[key_field]])
  }
  pub$providers <- .cv_supported_providers
  # Recommended local (Ollama) model tiers so the UI can offer them.
  pub$ollama_model_tiers <- list(
    light       = .cv_model_tiers$light,
    recommended = .cv_model_tiers$recommended,
    strong      = .cv_model_tiers$strong
  )
  pub$ollama_model_tiers_legacy <- unname(unlist(.cv_model_tiers_legacy))
  pub$ollama_tier_min_ram_gb <- as.list(.cv_tier_min_ram_gb)
  # Hardware-aware local recommendation for onboarding ("Your machine can
  # run X locally (free, private) ... or use the cloud default").
  pub$local_recommendation <- cv_local_recommendation()
  pub
}

# ---- Small helpers ----------------------------------------------------------

#' NULL-coalescing operator
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Generate a short unique id (for sessions, handles, jobs, artifacts)
#' @param prefix character prefix.
#' @noRd
cv_new_id <- function(prefix = "id") {
  ts <- format(Sys.time(), "%H%M%S")
  rnd <- paste(sample(c(0:9, letters), 6, replace = TRUE), collapse = "")
  paste0(prefix, "_", ts, rnd)
}

#' Timestamp helper (ISO-8601)
#' @noRd
cv_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")

#' Recursively drop NULL elements from a (possibly nested) list so they never
#' reach jsonlite::toJSON(). plumber's `unboxedJSON` serializer uses toJSON's
#' default `null = "list"`, which turns a NULL list element into an empty JSON
#' object `{}` on the wire. The React client then receives e.g. `note: {}` and,
#' because `{}` is truthy in JS, tries to render it as a child -> minified React
#' error #31 and a blank page (Round XXII). Dropping the key entirely avoids the
#' `{}` and matches what the client already expects for "field not provided".
#'
#' Only *named or unnamed list elements* that are NULL are removed; atomic
#' vectors, empty lists list(), and non-NULL leaves are returned unchanged. The
#' top-level `x` itself is returned as-is when it is not a list.
#'
#' IMPORTANT: a **data.frame is a list** in R. Recursing into it with lapply()
#' would strip its class, so toJSON() would then emit a column-oriented object
#' (`{"col":[...]}`) instead of an array of row-objects (`[{...},{...}]`). The
#' React TableArtifact calls `rows.map(...)`, so that column object crashed the
#' UI with "t.map is not a function" (Round XXII follow-up). We therefore leave
#' any element that has a class (data.frame, factor, etc.) untouched - toJSON()
#' already serializes a data.frame correctly as an array of rows.
#'
#' @param x any R object (typically a list event/response payload).
#' @return `x` with every NULL list element (recursively) removed.
#' @noRd
cv_json_sanitize <- function(x) {
  # Only recurse into *plain* lists. Anything with an explicit class attribute
  # (data.frame, factor, Date, ...) is left untouched so toJSON() handles it
  # with its class-aware method (a data.frame must stay a data.frame to
  # serialize as an array of row-objects). NOTE: class() returns the IMPLICIT
  # class ("list") for a plain list, so we must test attr(x, "class"), not
  # class(x) - is.null(class(plain_list)) is FALSE and would skip every list.
  if (!is.list(x) || !is.null(attr(x, "class"))) return(x)
  # Drop NULL elements at this level.
  keep <- !vapply(x, is.null, logical(1))
  x <- x[keep]
  # Recurse into remaining plain-list elements only.
  lapply(x, function(el) {
    if (is.list(el) && is.null(attr(el, "class"))) cv_json_sanitize(el) else el
  })
}

# ---- Error / ANSI cleaning --------------------------------------------------

#' Strip ANSI / VT100 escape sequences (colours, styles, hyperlinks) from text.
#'
#' cli/crayon emit SGR colour codes (e.g. \code{\\033[34m}, \code{\\033[39m},
#' \code{\\033[1m}, \code{\\033[38;5;232m}) and OSC-8 hyperlink sequences. When a
#' heavy tool fails inside a callr child that has colours on, those raw codes
#' leak into \code{conditionMessage()} and, ultimately, into the failure text and
#' the model's prose. This removes them so users and the model see clean text.
#'
#' @param x character vector (or coercible). NULL is returned unchanged.
#' @return character vector with escape sequences removed.
#' @noRd
cv_strip_ansi <- function(x) {
  if (is.null(x)) return(NULL)
  x <- as.character(x)
  if (!length(x)) return(x)
  # OSC sequences (e.g. cli hyperlinks): ESC ] ... (BEL | ESC \)
  x <- gsub("\033\\][^\a\033]*(?:\a|\033\\\\)", "", x, perl = TRUE)
  # CSI sequences: ESC [ params intermediates final-byte (covers SGR "...m").
  x <- gsub("\033\\[[0-9;?]*[ -/]*[@-~]", "", x, perl = TRUE)
  # Two-character escape sequences: ESC followed by a single byte.
  x <- gsub("\033[@-Z\\-_]", "", x, perl = TRUE)
  # Any stray control bytes left over (ESC, BEL).
  x <- gsub("[\033\a]", "", x, perl = TRUE)
  x
}

#' Turn a raw (possibly callr-wrapped, ANSI-laden) error into a concise, clean,
#' human-readable message.
#'
#' Steps: strip ANSI; when the string is a callr/base wrapper, keep only the text
#' after the last \dQuote{Caused by error in ...:} or leading \dQuote{Error in ...:}
#' marker (the real cause); drop structural noise lines (callr subprocess banner,
#' backtrace, "Execution halted", rlang trace footer); strip leading rlang bullet
#' glyphs; collapse hard-wrapped whitespace; and truncate very long messages.
#'
#' Safe to apply more than once (idempotent) and to plain messages (returned
#' essentially unchanged), so it can guard every error choke point defensively.
#'
#' @param msg character (or coercible); the raw error text.
#' @param max_chars integer; truncate the cleaned message beyond this length.
#' @return a single cleaned character string ("" when nothing usable remains).
#' @noRd
cv_clean_error <- function(msg, max_chars = 600L) {
  if (is.null(msg) || length(msg) == 0L) return("")
  x <- cv_strip_ansi(paste(as.character(msg), collapse = "\n"))
  if (!nzchar(trimws(x))) return("")

  # Keep the text after the LAST callr/base error marker (the real cause).
  # Anchored to line starts so an incidental "Error in ..." mid-sentence is safe.
  markers <- gregexpr("(?m)(^\\s*Caused by error in [^\n]*:|^\\s*Error in [^:\n]*:)",
                      x, perl = TRUE)[[1]]
  if (markers[1] != -1L) {
    i   <- length(markers)
    end <- markers[i] + attr(markers, "match.length")[i]
    x   <- substring(x, end)
  }

  # Drop structural noise lines; keep the substantive message/hint lines.
  lines <- trimws(unlist(strsplit(x, "\n", fixed = TRUE)))
  lines <- lines[nzchar(lines)]
  noise <- paste0(
    "^(in callr subprocess\\.?$|! in callr subprocess\\.?$|Caused by error",
    "|Execution halted$|In addition:|Backtrace:|Run `?rlang|Warning message",
    "|[0-9]+[:.] |Type \\.Last\\.error)")
  lines <- lines[!grepl(noise, lines, perl = TRUE)]
  # Strip a single leading rlang/cli bullet glyph ("! ", "x ", "i ", unicode).
  lines <- sub("^\\s*(?:[!]|[\u2716\u2717x]|[\u2139i]|[\u2022*>-])\\s+", "", lines, perl = TRUE)
  lines <- lines[nzchar(lines)]
  x <- paste(lines, collapse = " ")

  # Collapse whitespace runs left by hard-wrapping and trim.
  x <- trimws(gsub("\\s+", " ", x, perl = TRUE))
  if (nchar(x) > max_chars) x <- paste0(substr(x, 1L, max_chars - 1L), "\u2026")
  x
}

#' Clean a TOP-LEVEL turn error (LLM connectivity / API failure) for display.
#'
#' This is the message surfaced to the user when a whole chat turn fails before
#' any tool runs - e.g. an unreachable Ollama server or a provider HTTP error.
#' Unlike \code{cv_clean_error} (tuned for callr-wrapped TOOL errors, where the
#' useful text is the innermost "Caused by" cause), a top-level LLM error is a
#' \emph{curated} cli message whose headline and "i"-hint lines ARE the useful
#' text; the only noise is the redundant parent-condition tail
#' (\dQuote{Caused by error: ! <curl detail>}) appended because the abort carries
#' \code{parent = e}. So this cleaner: strips ANSI/OSC; drops everything from the
#' first \dQuote{Caused by error} line onward (the low-level transport detail,
#' which duplicates the headline's "Failed to perform HTTP request"); unwraps
#' cli's hard-wrapping; and keeps the headline + hint lines intact and readable.
#'
#' Idempotent and safe on plain messages. Falls back to \code{cv_clean_error}
#' behaviour (keep innermost cause) when there is no curated headline - i.e. the
#' text STARTS with a "Caused by error" marker (a bare wrapped condition).
#'
#' @param msg character (or coercible); the raw top-level error text.
#' @param max_chars integer; truncate beyond this length.
#' @return a single cleaned character string.
#' @noRd
cv_clean_turn_error <- function(msg, max_chars = 700L) {
  if (is.null(msg) || length(msg) == 0L) return("")
  x <- cv_strip_ansi(paste(as.character(msg), collapse = "\n"))
  if (!nzchar(trimws(x))) return("")

  lines <- unlist(strsplit(x, "\n", fixed = TRUE))
  # Locate the first "Caused by error..." marker line (the parent-condition tail).
  cause_idx <- grep("^\\s*Caused by error", lines, perl = TRUE)[1]
  if (!is.na(cause_idx)) {
    if (cause_idx == 1L) {
      # No curated headline - a bare wrapped condition. Defer to cv_clean_error,
      # which extracts the innermost cause.
      return(cv_clean_error(x, max_chars = max_chars))
    }
    # Drop the parent tail; keep the curated headline + hints above it.
    lines <- lines[seq_len(cause_idx - 1L)]
  }

  # Unwrap cli's hard-wrapping: a line indented by cli is a continuation of the
  # previous logical line (headline wrap or a hint's wrapped body).
  out <- character(0)
  for (ln in lines) {
    if (!nzchar(trimws(ln))) next
    is_continuation <- grepl("^\\s+", ln) && length(out) > 0L &&
      !grepl("^\\s*[\u2139i!]", ln)  # an indented NEW bullet starts a new line
    if (is_continuation) {
      out[length(out)] <- paste0(out[length(out)], " ", trimws(ln))
    } else {
      out <- c(out, trimws(ln))
    }
  }
  # Strip a single leading bullet glyph per logical line; keep the text.
  out <- sub("^\\s*(?:[\u2139i]|[!]|[\u2716\u2717x])\\s+", "", out, perl = TRUE)
  out <- out[nzchar(out)]
  x <- paste(out, collapse = " ")
  x <- trimws(gsub("\\s+", " ", x, perl = TRUE))
  if (nchar(x) > max_chars) x <- paste0(substr(x, 1L, max_chars - 1L), "\u2026")
  x
}

# ---- The structured warnings channel (Round LXIX; audit #23/#24/#25) ---------
#
# WHAT THIS REPLACES. Every caveat a tool wanted to raise was glued onto the end
# of its own summary text with paste():
#
#     if (length(notes)) txt <- paste0(txt, " Note: ", paste(notes, ...), ".")
#     if (nzchar(warn))  txt <- paste(txt, warn)
#     txt <- paste(txt, tool$result_note)
#
# So a caveat that INVALIDATES the result -- "this run was restricted to
# tissue='Brain' and 6 of 8 sets came back without a confident label" -- and a
# caveat that is pure housekeeping -- "C7: no markers matched the filter" --
# arrived as the same undifferentiated run-on sentence, under the same green
# tick, in the same typeface. The user had to read the whole paragraph to
# discover whether the number above it could be trusted.
#
# EXACTLY TWO SEVERITIES, and that is a decision, not a starting point:
#
#   "may_invalidate" -- ignore this and you may draw a WRONG conclusion from
#                       the result shown. The card turns amber and says so.
#   "info"           -- the agent is telling you what it did. The result stands.
#
# A five-level scale is how users learn to ignore all warnings, which is the
# failure this whole feature exists to prevent (audit category 3b, item 5). The
# discriminating question for any new warning is only ever: *would a reader who
# skipped this draw a wrong conclusion from the numbers on screen?*
#
# WHY A COLLECTOR THAT IS PASSED, NOT A GLOBAL. Several warnings are raised
# during argument preparation, well before the result exists -- the log1p
# override, an assay that had to be substituted, a handle that was auto-resolved.
# The obvious implementation is an ambient environment those sites append to.
# This codebase has been there: Round XLI removed the per-session config
# side-channel for exactly this reason, and did it BEFORE the concurrency
# redesign that would have weaponised it. So the collector is an ordinary
# argument, defaulting to NULL, and every site that can raise a warning takes it
# explicitly. A caller that passes nothing gets the old behaviour exactly.

#' The two severities, in the order they must be presented.
#' @noRd
.cv_warn_severities <- c("may_invalidate", "info")

#' A new warnings collector.
#'
#' An environment rather than a list because it is threaded through several
#' functions that would otherwise each have to return it alongside their real
#' result, turning six signatures into six pairs.
#' @noRd
cv_warnings_new <- function() {
  e <- new.env(parent = emptyenv())
  e$items <- list()
  e
}

#' Build one warning record.
#'
#' `code` is a short stable slug (`"log1p_override"`, `"tissue_unproductive"`).
#' It is what a test asserts on: the TEXT is user-facing prose that will be
#' rewritten, and a test that matches prose pins the wording rather than the
#' behaviour. This codebase has been caught by that twice.
#' @noRd
cv_warn <- function(severity, text, code = NULL) {
  severity <- match.arg(severity, .cv_warn_severities)
  text <- trimws(paste(as.character(text), collapse = " "))
  if (!nzchar(text)) return(NULL)
  list(severity = severity, text = text, code = code %||% NA_character_)
}

#' Append a warning to a collector. A NULL collector is a no-op, which is what
#' lets every call site take one without any caller being obliged to.
#' @noRd
cv_warn_add <- function(collector, severity, text, code = NULL) {
  if (is.null(collector)) return(invisible(NULL))
  w <- cv_warn(severity, text, code)
  if (is.null(w)) return(invisible(NULL))
  collector$items <- c(collector$items, list(w))
  invisible(w)
}

#' Normalise anything warning-shaped into a plain list of records.
#'
#' Tolerates a collector, an already-built list, a single record, or NULL, so
#' the merge below does not need every producer to agree on a container.
#' @noRd
cv_warnings_list <- function(x) {
  if (is.null(x)) return(list())
  if (is.environment(x)) return(x$items %||% list())
  if (is.list(x) && !is.null(x$severity) && !is.null(x$text)) return(list(x))
  if (!is.list(x)) return(list())
  Filter(function(w) is.list(w) && !is.null(w$severity) && !is.null(w$text), x)
}

#' Merge warning sets, drop exact duplicates, and sort may_invalidate first.
#'
#' Order is load-bearing rather than cosmetic. A run can raise several warnings
#' at once -- an auto-resolved handle (info), a log1p override (info) and a
#' cross-tissue match (may_invalidate) -- and burying the one that changes the
#' answer under two that do not is the same failure as not raising it. Within a
#' severity, insertion order is preserved, so a tool's own caveats read in the
#' order it produced them.
#'
#' De-duplicated on severity+text because both dispatch paths can legitimately
#' produce the same advisory for one call: the resumed heavy path re-runs
#' argument preparation before re-finding its job.
#' @noRd
cv_warnings_merge <- function(...) {
  all <- unlist(lapply(list(...), cv_warnings_list), recursive = FALSE)
  if (!length(all)) return(list())
  keys <- vapply(all, function(w) paste0(w$severity, "\r", w$text), character(1))
  all <- all[!duplicated(keys)]
  rank <- match(vapply(all, function(w) w$severity, character(1)), .cv_warn_severities)
  rank[is.na(rank)] <- length(.cv_warn_severities) + 1L
  all[order(rank, seq_along(all))]
}

#' Does this warning set contain one that may invalidate the result?
#'
#' The single predicate behind the `done_with_warnings` card state, so the
#' server and the client cannot disagree about what earns amber.
#' @noRd
cv_warnings_invalidating <- function(x) {
  ws <- cv_warnings_list(x)
  any(vapply(ws, function(w) identical(w$severity, "may_invalidate"), logical(1)))
}

#' Attach a merged warning set to a result record, without disturbing anything
#' else about it.
#'
#' Returns the result unchanged when there is nothing to attach, so a tool that
#' raises no warnings carries no `warnings` key at all and every existing
#' consumer sees exactly the payload it saw before.
#' @noRd
cv_result_add_warnings <- function(res, ...) {
  ws <- cv_warnings_merge(...)
  if (!length(ws)) return(res)
  if (!is.list(res)) return(res)
  res$warnings <- cv_warnings_merge(res$warnings, ws)
  res
}

# ---- Package documentation for the model (Round LXXVIII, audit #45) ---------
#
# Reads the INSTALLED Rd database and renders one topic to plain text. `tools`
# and `utils` are base packages, so this adds no dependency -- the standing
# constraint forbids new Imports.
#
# It resolves against celliverse's own help first, and says plainly when a name
# is not one of ours rather than silently returning another package's page: a
# confident answer about the wrong `FindClusters` is exactly the failure this
# item exists to prevent.

#' Render one CelliVerse help topic as plain text.
#'
#' @param fn function name.
#' @param max_chars cap; the text is truncated with an explicit marker, never
#'   quietly.
#' @noRd
cv_function_help_text <- function(fn, max_chars = 6000L) {
  max_chars <- suppressWarnings(as.integer(max_chars))
  if (is.na(max_chars) || max_chars < 500L) max_chars <- 6000L
  db <- tryCatch(tools::Rd_db("celliverse"), error = function(e) NULL)
  if (is.null(db) || !length(db))
    return(sprintf(paste0(
      "The CelliVerse help database is not available in this installation, so I ",
      "cannot read the documentation for '%s' here."), fn))
  topics <- sub("\\.Rd$", "", names(db))
  # An exact topic, then a file whose \name matches, then case-insensitive.
  i <- which(topics == fn)
  if (!length(i)) i <- which(tolower(topics) == tolower(fn))
  if (!length(i)) {
    near <- topics[grepl(fn, topics, ignore.case = TRUE, fixed = FALSE)]
    return(sprintf(paste0(
      "'%s' is not a documented CelliVerse function.%s Ask about one of the ",
      "package's own functions - answering from general single-cell knowledge ",
      "would describe a different method."), fn,
      if (length(near)) sprintf(" Did you mean: %s.", paste(utils::head(near, 5), collapse = ", ")) else ""))
  }
  # `underline_titles = FALSE` because Rd2txt's default renders headings as
  # TERMINAL overstrike -- "_C_l_u_s_t_e_r_i_n_g", literally letter-backspace-
  # underscore. That is meant for a tty and is noise to a model. The gsub is
  # belt and braces for any backspace pair the option does not cover.
  txt <- tryCatch(paste(utils::capture.output(
    tools::Rd2txt(db[[i[1]]], options = list(underline_titles = FALSE))),
    collapse = "\n"), error = function(e) "")
  txt <- gsub(".\010", "", txt, perl = TRUE)
  if (!nzchar(txt))
    return(sprintf("The documentation for '%s' could not be rendered.", fn))
  if (nchar(txt) > max_chars)
    txt <- paste0(substr(txt, 1L, max_chars),
                  sprintf("\n\n[... truncated at %d of %d characters. Ask for a specific section or raise max_chars for the rest.]",
                          max_chars, nchar(txt)))
  txt
}
