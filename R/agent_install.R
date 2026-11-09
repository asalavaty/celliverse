# =============================================================================
# CelliVerse Agent - install / setup entry point
#
# install_celliverse_agent() prepares a machine to run the agent:
#   1. verify the R web-stack deps (from Suggests) are installed
#   2. create the directory returned by tools::R_user_dir("celliverse", "cache") and a default config.json
#   3. detect Node.js (optional - only needed to REBUILD the frontend)
#   4. detect Ollama; if present, pull the default local model
#   5. verify the bundled React app + plumber file are present
#   6. print a readiness summary
#
# It is deliberately conservative: it never force-installs system software
# silently (Ollama/Node install is platform-specific). Instead it detects and
# GUIDES, so the function is safe to run anywhere.
# =============================================================================

#' Prepare this machine to run the CelliVerse agent
#'
#' The agent is \strong{cloud-first}: it runs out of the box with a cloud
#' provider (set an API key in Settings) and needs \emph{no} local model. This
#' installer verifies the R web-stack, creates \code{tools::R_user_dir("celliverse", "cache")} + a default
#' config, and \emph{optionally} sets up a local runtime - it detects Ollama
#' (and pulls a tier model when found) and detects LM Studio (via its \code{lms}
#' CLI). A local model is never required; you can install Ollama and/or LM
#' Studio at any time later and switch providers in Settings. The config
#' \code{default_model} is only pointed at a local Ollama model when Ollama is
#' actually present (or you pass \code{model =} explicitly); otherwise the
#' cloud default is left untouched.
#'
#' @param tier which local model tier(s) to pull when Ollama is present:
#'   `"auto"` (default; detect RAM and pick the best tier that fits), `"light"`
#'   (small, fast), `"recommended"` (MoE, strong tool-calling, needs ~24 GB),
#'   `"strong"` (largest, needs ~36 GB), `"both"` (light+strong), or `"all"`
#'   (all three). Ignored if `model` is given. No effect when Ollama is absent.
#' @param model explicit local Ollama model id to pull. Overrides `tier` when
#'   supplied, and (unlike `tier`) also sets the config `default_model` even if
#'   Ollama is not yet installed. Defaults to the hardware-recommended tier.
#' @param pull_model actually run `ollama pull` if Ollama is found.
#' @return invisibly, a named list summarising what was found/done.
#' @export
install_celliverse_agent <- function(tier = c("auto", "light", "recommended",
                                              "strong", "both", "all"),
                                      model = NULL, pull_model = TRUE) {
  tier <- match.arg(tier)
  cli::cli_h1("CelliVerse agent setup")
  summary <- list()

  # 1. R deps ------------------------------------------------------------------
  missing <- cv_missing_agent_deps()
  
  if (length(missing)) {
    cli::cli_abort(c(
      "Additional R packages required by the CelliVerse agent are not installed: {.pkg {missing}}.",
      i = "Please install them manually and then re-run {.fn install_celliverse_agent}.",
      i = paste0(
        "For example: install.packages(c(",
        paste(sprintf('\"%s\"', missing), collapse = ", "),
        "))"
      )
    ))
  }
  
  cli::cli_alert_success(
    "All required R dependencies are available."
  )
  
  summary$missing_r_deps <- missing

  # 2. Home + config -----------------------------------------------------------
  # Resolve which model id(s) to pull. An explicit `model` wins; otherwise map
  # the requested tier to its id(s). "auto" (the default) picks the best tier
  # that fits this machine's RAM. The FIRST id becomes the config default so a
  # fresh install lands on a working model.
  cv_ensure_home()
  cfg <- cv_load_config()
  rec <- cv_local_recommendation()
  cli::cli_alert_info(rec$headline)
  if (nzchar(rec$legacy_alt %||% ""))
    cli::cli_alert_info("On this machine the lighter legacy model {.val {rec$legacy_alt}} is also a good fit.")
  if (!is.null(model)) {
    pull_models <- model
  } else if (identical(tier, "auto")) {
    pull_models <- rec$model
    cli::cli_alert_info("Auto-selected tier {.val {rec$tier}} ({.val {rec$model}}) for this machine.")
  } else {
    pull_models <- cv_model_tier_id(tier)
    # Explicit tier that does not fit: warn but proceed (user asked for it).
    if (!is.na(rec$ram_gb)) {
      for (t in intersect(tier, names(.cv_tier_min_ram_gb))) {
        need <- .cv_tier_min_ram_gb[[t]]
        if (rec$ram_gb < need) {
          cli::cli_alert_warning(paste0(
            "Tier {.val ", t, "} wants ~", need, " GB RAM but this machine has ~",
            round(rec$ram_gb), " GB - it may run slowly or not load. ",
            "Consider tier = \"auto\" (would pick {.val ", rec$model, "})."))
        }
      }
    }
  }
  # Cloud-first: only write an Ollama model id into the config default when a
  # local runtime is actually available to serve it. The agent runs WITHOUT any
  # local model (default provider is a cloud one); setting default_model to an
  # Ollama id when Ollama is absent would leave a fresh install pointing at a
  # model that cannot run. An explicit `model =` request DOES set the default
  # (the user clearly wants that local model); otherwise we set it only when
  # Ollama is detected below.
  ollama <- cv_detect_binary("ollama")
  default_model <- pull_models[[1]]
  user_forced_model <- !is.null(model)
  if (user_forced_model || !is.null(ollama)) {
    cfg$default_model <- default_model
    cv_save_config(cfg)
  }
  cli::cli_alert_success("Config ready at {.file {cv_config_path()}}")
  if (user_forced_model || !is.null(ollama)) {
    cli::cli_alert_info("Default provider: {.val {cfg$default_provider}} | model: {.val {cfg$default_model}}")
  } else {
    cli::cli_alert_info(paste0(
      "Default provider: {.val ", cfg$default_provider, "} | model: {.val ", cfg$default_model, "} ",
      "(cloud-first: no local model required - the agent runs as-is. To go fully offline, ",
      "install Ollama or LM Studio later; see below.)"))
  }
  if (length(pull_models) > 1L)
    cli::cli_alert_info("Model(s) to pull: {.val {pull_models}}")
  summary$config_path <- cv_config_path()
  summary$models      <- pull_models

  # 3. Node (optional) ---------------------------------------------------------
  node <- cv_detect_binary("node")
  if (!is.null(node)) {
    ver <- tryCatch(system2(node, "--version", stdout = TRUE, stderr = TRUE), error = function(e) "?")
    cli::cli_alert_success("Node.js found ({ver}). You can rebuild the frontend if desired.")
  } else {
    cli::cli_alert_info(paste(
      "Node.js not found. It is OPTIONAL - only needed to rebuild the React UI.",
      "The bundled prebuilt UI works without it."))
  }
  summary$node <- node

  # 4. Local runtimes: Ollama + LM Studio (both OPTIONAL - cloud-first) --------
  # `ollama` was already detected above (to decide whether to set the config
  # default model). Local models are entirely optional: the agent runs on a
  # cloud provider out of the box, and the user can add Ollama and/or LM Studio
  # at any time and switch providers in Settings.
  if (!is.null(ollama)) {
    cli::cli_alert_success("Ollama found at {.path {ollama}}.")
    if (isTRUE(pull_model)) {
      for (m in pull_models) {
        cli::cli_alert_info("Pulling local model {.val {m}} (this can take a while)...")
        code <- tryCatch(system2(ollama, c("pull", m), stdout = "", stderr = ""),
                         error = function(e) 1L)
        if (identical(code, 0L)) cli::cli_alert_success("Model {.val {m}} ready.")
        else cli::cli_alert_warning("Could not pull {.val {m}}. Pull it manually: ollama pull {m}")
      }
    }
    # RAM-based SUGGESTION (never auto-pull): if the pulled set is below the
    # best tier this machine can run, point at the stronger option. Silent when
    # RAM is undetectable or the best tier is already among the pulled models.
    best <- rec$tier
    best_id <- .cv_model_tiers[[best]]
    if (!(best_id %in% pull_models) && best != "light") {
      cli::cli_alert_info(paste0(
        "This machine can run the {.val ", best, "} tier ({.val ", best_id, "}). ",
        "Install it with {.code install_celliverse_agent(tier = \"", best, "\")} ",
        "or {.code ollama pull ", best_id, "}."))
    }
  } else {
    cli::cli_alert_info(paste(
      "Ollama not found (optional). For offline/local models via Ollama, install it from",
      "https://ollama.com, then run: ollama pull {default_model}. To use a cloud model",
      "instead, set an API key in Settings - no local install needed."))
  }
  summary$ollama <- ollama

  # LM Studio: an alternative local runtime (GUI model manager; MLX builds are
  # often fastest on Apple Silicon). Detected via its `lms` CLI. Also optional.
  lms <- cv_detect_binary("lms")
  if (!is.null(lms)) {
    cli::cli_alert_success(paste0(
      "LM Studio found (lms CLI at ", lms, "). Start its server from the app's ",
      "Developer / 'Local Model API' tab (or `lms server start`), then pick provider ",
      "'lmstudio' in Settings."))
  } else {
    cli::cli_alert_info(paste(
      "LM Studio not found (optional alternative to Ollama - a GUI local-model manager;",
      "on Apple Silicon its MLX builds are often the fastest). Install from https://lmstudio.ai,",
      "download a model, start the local server, then choose provider 'lmstudio' in Settings."))
  }
  summary$lmstudio <- lms

  # 5. Bundled assets ----------------------------------------------------------
  www <- system.file("react-app", package = "celliverse")
  pl.f <- system.file("plumber", "plumber.R", package = "celliverse")
  summary$has_frontend <- nzchar(www) && dir.exists(www) && length(list.files(www)) > 0
  summary$has_plumber  <- nzchar(pl.f)
  if (isTRUE(summary$has_frontend)) cli::cli_alert_success("Bundled React UI present.")
  else cli::cli_alert_info("No prebuilt UI bundled yet; API still runs. See setup guide to build it.")
  if (isTRUE(summary$has_plumber)) cli::cli_alert_success("Plumber API definition present.")

  # 6. Readiness ---------------------------------------------------------------
  ready <- length(missing) == 0 && isTRUE(summary$has_plumber)
  if (ready) {
    cli::cli_alert_success("Ready. Launch with: run_celliverse_agent()")
  } else {
    cli::cli_alert_warning("Setup incomplete - see messages above.")
  }
  summary$ready <- ready
  invisible(summary)
}

#' Detect an executable on PATH (returns NULL if absent).
#' @noRd
cv_detect_binary <- function(bin) {
  path <- tryCatch(Sys.which(bin)[[1]], error = function(e) "")
  if (is.na(path) || !nzchar(path)) NULL else unname(path)
}

#' Which runtime R deps are NOT installed?
#' @noRd
cv_missing_agent_deps <- function() {
  pkgs <- .cv_agent_runtime_pkgs
  pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
}
