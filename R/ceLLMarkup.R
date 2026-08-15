# =============================================================================
# ceLLMarkup — LLM-based cell-type annotation from marker genes
#
# Provider-agnostic: works with any LLM the CelliVerse Agent supports
# (Ollama and LM Studio fully offline; OpenAI, Anthropic, Gemini, DeepSeek,
# Groq, OpenRouter, Cerebras in the cloud). The LLM connection is configured
# EXPLICITLY via the `provider`, `model`, `api_key`, and `host` arguments of
# this function — it does not silently inherit the agent's session config.
#
# Three mutually exclusive input modes:
#   1. marker_set_list : named list of marker data.frames (feature + Purity or logFC)
#   2. seuratClusters  : a Seurat FindAllMarkers() data.frame
#   3. panels          : named list of positive (and optionally negative)
#                        marker character vectors — this is what
#                        typoClust(mode = "ceLLMarkup") passes in.
#
# Output: a TypoClust-compatible object (class "TypoClust") whose
# cell_types[[set]] data.frames carry the same columns typoClustVis() and
# addTypoData() expect (CellType, Rank, Combined_Markers, Combined_Score,
# Avg_Pos_Purity, Avg_Neg_Purity, ...).
# =============================================================================

#' LLM-based cell-type annotation of clusters or marker sets (ceLLMarkup)
#'
#' @description
#' Annotates cell clusters, sub-clusters, or arbitrary marker sets by asking a
#' large language model to name the most likely cell type for each set, given
#' its marker genes and an optional tissue/condition/species context. Returns
#' ranked candidate cell types per set in a \code{TypoClust}-compatible object.
#'
#' Exactly one of \code{marker_set_list}, 
#' \code{panels}, or \code{seuratClusters} must be provided.
#'
#' @param sample_source Free-text description of the sample origin shown to the
#'   model (e.g. \code{"human peripheral blood"}). Improves accuracy.
#' @param feature_type Feature type of the markers (e.g. \code{"gene"},
#'   \code{"protein"}). Default \code{"gene"}.
#' @param marker_set_list Named list of marker data.frames, one per set. Each
#'   data.frame should have feature names in the first column and purity or fold changes
#'   in the second. Positive markers (or up-regulated features) are recommended.
#' @param panels Named list of marker panels: either a named list of character
#'   vectors (positive markers per set), or a list with \code{pos_panels}
#'   and/or \code{neg_panels} elements (each a named list of character
#'   vectors). This is the input \code{typoClust(mode = "ceLLMarkup")} uses.
#' @param seuratClusters A data.frame obtained from Seurat's
#'   \code{FindAllMarkers()} (must have \code{avg_log2FC}, \code{p_val_adj},
#'   \code{cluster}, \code{gene} columns). Filtered to up-regulated markers by
#'   \code{padj}/\code{logFC}.
#' @param padj Adjusted p-value threshold for filtering \code{seuratClusters}
#'   (default 0.05; \code{NULL} disables).
#' @param logFC log-fold-change threshold for filtering \code{seuratClusters}
#'   (default \code{NULL} = no extra filter; only up-regulated genes kept).
#' @param tissue Optional tissue context (e.g. \code{"blood"}).
#' @param condition Optional condition context (e.g. \code{"healthy"}).
#' @param species Species for gene-symbol interpretation: \code{"human"},
#'   \code{"mouse"}, or your desired species name.
#' @param provider LLM provider: one of \code{"ollama"}, \code{"lmstudio"},
#'   \code{"openai"}, \code{"anthropic"}, \code{"gemini"}, \code{"deepseek"},
#'   \code{"groq"}, \code{"openrouter"}, \code{"cerebras"}.
#' @param model Model id for the provider (e.g. \code{"qwen3:8b"} for Ollama,
#'   \code{"qwen/qwen3-8b"} for LM Studio, \code{"gpt-4o-mini"} for OpenAI,
#'   \code{"anthropic/claude-3-haiku"} for OpenRouter).
#' @param api_key API key for cloud providers. Not needed for
#'   \code{ollama}/\code{lmstudio}. If \code{NULL}, falls back to the standard
#'   environment variable for the provider (e.g. \code{OPENAI_API_KEY},
#'   \code{OPENROUTER_API_KEY}).
#' @param host Base URL for local providers. Defaults to
#'   \code{http://localhost:11434} (Ollama) or \code{http://localhost:1234/v1}
#'   (LM Studio). Ignored for cloud providers.
#' @param temperature Sampling temperature (default 0.2).
#' @param top_k Number of ranked candidate cell types per set (default 3).
#' @param n_markers Maximum number of markers per set shown to the model
#'   (default 25).
#' @param max_retries Retries per set when the model returns an unparseable or
#'   empty answer (default 1).
#' @param verbose Show progress messages (default \code{TRUE}).
#'
#' @return An object of class \code{TypoClust}: a list with \code{cell_types}
#'   (named list of ranked annotation data.frames) and \code{metadata}.
#' @examples
#' \dontrun{
#' # Annotate marker panels using a locally running Ollama model
#' markers <- list(
#'   Cluster1 = c("CD3D", "CD3E", "TRBC1", "IL7R", "LTB"),
#'   Cluster2 = c("MS4A1", "CD79A", "CD37", "CD74", "HLA-DRA")
#' )
#'
#' annotations <- ceLLMarkup(
#'   panels = markers,
#'   sample_source = "human peripheral blood",
#'   feature_type = "gene",
#'   tissue = "Blood",
#'   condition = "Healthy",
#'   species = "human",
#'   provider = "ollama",
#'   model = "qwen3:8b",
#'   top_k = 3
#' )
#' }
#'   
#' @export

ceLLMarkup <- function(sample_source = NULL,
                       feature_type = "gene",
                       marker_set_list = NULL,
                       panels = NULL,
                       seuratClusters = NULL,
                       padj = 0.05,
                       logFC = NULL,
                       tissue = NULL,
                       condition = NULL,
                       species = c("human", "mouse"),
                       provider = "ollama",
                       model,
                       api_key = NULL,
                       host = NULL,
                       temperature = 0.2,
                       top_k = 3L,
                       n_markers = 25L,
                       max_retries = 1L,
                       verbose = TRUE) {

  species <- species[1]
  if (!is.character(species) || length(species) == 0 || is.na(species)) {
    cli::cli_abort("{.arg species} must be a non-empty character string.")
  }
  
  log_message <- function(...) if (verbose) cli::cli_alert_info(...)

  # ---- 1. Resolve the input into per-set positive/negative marker vectors ----
  n_inputs <- sum(!is.null(marker_set_list), !is.null(seuratClusters), !is.null(panels))
  if (n_inputs != 1L) {
    cli::cli_abort("Exactly one of {.arg marker_set_list}, {.arg seuratClusters}, or {.arg panels} must be provided!")
  }

  pos_panels <- list()
  neg_panels <- list()

  if (!is.null(panels)) {
    # typoClust-style input: list(pos_panels=<named list>, neg_panels=<named list>)
    # or a plain named list of character vectors (positive markers only).
    if (!is.null(names(panels)) && all(c("pos_panels", "neg_panels") %in% names(panels))) {
      pos_panels <- panels$pos_panels %||% list()
      neg_panels <- panels$neg_panels %||% list()
    } else {
      pos_panels <- panels
    }
    pos_panels <- lapply(pos_panels, function(x) utils::head(as.character(x), n_markers))
    neg_panels <- lapply(neg_panels, function(x) utils::head(as.character(x), n_markers))
  } else {
    if (!is.null(seuratClusters)) {
      if (!all(c("avg_log2FC", "p_val_adj", "cluster", "gene") %in% colnames(seuratClusters))) {
        cli::cli_abort(paste0(
          "The {.arg seuratClusters} data.frame does not look like a Seurat ",
          "FindAllMarkers() output: it must have 'avg_log2FC', 'p_val_adj', ",
          "'cluster' and 'gene' columns."))
      }
      sc <- seuratClusters[seuratClusters$avg_log2FC > 0, , drop = FALSE]
      if (!is.null(padj))  sc <- sc[sc$p_val_adj < padj, , drop = FALSE]
      if (!is.null(logFC)) sc <- sc[sc$avg_log2FC >= logFC, , drop = FALSE]
      if (!nrow(sc)) cli::cli_abort("No markers left after filtering {.arg seuratClusters}.")
      marker_set_list <- split(sc[, c("gene", "avg_log2FC"), drop = FALSE], sc$cluster)
      names(marker_set_list) <- paste0("Cluster_", names(marker_set_list))
    }
    # marker_set_list mode: first column = feature, second = fold change.
    for (nm in names(marker_set_list)) {
      df <- marker_set_list[[nm]]
      if (!is.data.frame(df) || ncol(df) < 1L) {
        cli::cli_abort("Each element of {.arg marker_set_list} must be a data.frame (got {.val {nm}}).")
      }
      ord <- if (ncol(df) >= 2L && is.numeric(df[[2]])) order(-df[[2]]) else seq_len(nrow(df))
      feats <- as.character(df[[1]])[ord]
      feats <- feats[!is.na(feats) & nzchar(feats)]
      pos_panels[[nm]] <- utils::head(feats, n_markers)
    }
  }

  set_names <- unique(c(names(pos_panels), names(neg_panels)))
  if (!length(set_names)) cli::cli_abort("No marker sets to annotate.")

  # ---- 2. Build the LLM config (explicit arguments only) --------------------
  config <- .cv_cellmarkup_config(provider = provider, model = model,
                                  api_key = api_key, host = host,
                                  temperature = temperature)

  # ---- 3. Prompt + call ------------------------------------------------------
  markers <- list(clusters = set_names, pos = pos_panels, neg = neg_panels,
                  level = stats::setNames(rep("set", length(set_names)), set_names),
                  degraded = FALSE)
  msgs <- .cv_cellmarkup_prompt(markers, sample_source = sample_source,
                                feature_type = feature_type, tissue = tissue,
                                condition = condition, species = species,
                                top_k = top_k)

  parsed <- NULL
  for (attempt in seq_len(max_retries + 1L)) {
    resp <- cv_chat(msgs, provider = config$default_provider,
                    model = config$default_model, tools = NULL,
                    temperature = config$temperature, stream = FALSE,
                    config = config)
    parsed <- .cv_cellmarkup_parse(resp$content, cluster_names = set_names, top_k = top_k)
    if (!is.null(parsed)) break
    if (attempt <= max_retries) log_message("Model reply was not parseable; retrying ({attempt}/{max_retries})...")
  }
  if (is.null(parsed)) {
    cli::cli_abort("The model did not return a usable annotation after {max_retries + 1L} attempt(s).")
  }

  # ---- 4. Assemble the TypoClust-compatible object ---------------------------
  .cv_cellmarkup_build_typoclust(parsed, markers, tissue = tissue,
                                 condition = condition, species = species,
                                 pos_panels = pos_panels, neg_panels = neg_panels)
}

# ---- Internal helpers ---------------------------------------------------------

#' Build a cv_chat()-compatible config from explicit ceLLMarkup arguments.
#' @keywords internal
.cv_cellmarkup_config <- function(provider, model, api_key, host, temperature) {
  if (missing(model) || is.null(model) || !nzchar(model)) {
    cli::cli_abort(c(
      "Please specify the {.arg model} id for provider {.val {provider}}.",
      i = "e.g. 'qwen3:8b' (ollama), 'qwen/qwen3-8b' (lmstudio), 'gpt-4o-mini' (openai), 'anthropic/claude-3-haiku' (openrouter)."
    ))
  }
  cfg <- cv_default_config()
  cfg$default_provider <- provider
  cfg$default_model    <- model
  cfg$temperature      <- temperature %||% 0.2

  # Host for local providers.
  if (!is.null(host) && nzchar(host)) {
    if (provider == "ollama")        cfg$ollama_host <- host
    else if (provider == "lmstudio") cfg$lmstudio_host <- host
  }

  # API key: explicit argument wins; otherwise the standard env var for the
  # provider. Local providers need no key.
  key_field <- paste0(provider, "_key")
  if (!is.null(api_key) && nzchar(api_key)) {
    if (key_field %in% names(cfg)) cfg[[key_field]] <- api_key
  } else {
    env_map <- list(
      openai = "OPENAI_API_KEY", anthropic = "ANTHROPIC_API_KEY",
      gemini = c("GEMINI_API_KEY", "GOOGLE_API_KEY"),
      deepseek = "DEEPSEEK_API_KEY", groq = "GROQ_API_KEY",
      openrouter = "OPENROUTER_API_KEY", cerebras = "CEREBRAS_API_KEY"
    )
    envs <- env_map[[provider]]
    if (!is.null(envs)) {
      for (e in envs) {
        v <- Sys.getenv(e, unset = NA)
        if (!is.na(v) && nzchar(v)) { cfg[[key_field]] <- v; break }
      }
    }
  }
  cfg
}

#' Build the ceLLMarkup chat messages (strict JSON out).
#' @keywords internal
.cv_cellmarkup_prompt <- function(markers, sample_source, feature_type, tissue,
                                  condition, species, top_k) {
  ctx <- c(
    if (!is.null(sample_source) && nzchar(sample_source)) paste0("Sample source: ", sample_source) else NULL,
    if (!is.null(tissue) && nzchar(tissue)) paste0("Tissue: ", tissue) else NULL,
    if (!is.null(condition) && nzchar(condition)) paste0("Condition: ", condition) else NULL,
    paste0("Species: ", species %||% "human")
  )
  blocks <- vapply(markers$clusters, function(cl) {
    pos <- markers$pos[[cl]] %||% character(0)
    neg <- markers$neg[[cl]] %||% character(0)
    line <- sprintf("Set %s:\n  Positive %ss: %s", cl, feature_type,
                    if (length(pos)) paste(pos, collapse = ", ") else "(none)")
    if (length(neg)) line <- paste0(line, sprintf("\n  Negative %ss: %s", feature_type, paste(neg, collapse = ", ")))
    line
  }, character(1))

  sys <- paste(
    "You are an expert single-cell cell-type annotator (the 'ceLLMarkup' method).",
    "Given each set's marker features and the sample context, assign the most likely",
    "cell type/subtype. Use canonical, specific cell-type names (e.g. 'CD8+ cytotoxic T cell',",
    "'Classical monocyte', 'Naive B cell'). 
    If a specific subtype or state of canonical cell-type name is confidently more accurate give the set markers, suggest the more specific subtype/state. 
    Base decisions on established marker biology.",
    sprintf("For EACH set return up to %d ranked candidate cell types (rank 1 = most likely),", top_k),
    "each with a confidence in [0,1] and a one-line reason citing the key markers.",
    "",
    "Respond with STRICT JSON ONLY (no prose, no markdown fences), of the form:",
    '{"annotations":[{"cluster":"<set id>","candidates":[{"cell_type":"...","confidence":0.0,"reason":"..."}]}]}',
    sep = "\n")

  user <- paste(
    paste(ctx, collapse = "\n"), "",
    "Sets and their markers:", "",
    paste(blocks, collapse = "\n\n"), "",
    sprintf("Return JSON with one entry per set (%d sets), up to %d candidates each.",
            length(markers$clusters), top_k),
    sep = "\n")

  list(list(role = "system", content = sys), list(role = "user", content = user))
}

#' Parse the model's JSON annotation; NULL when unparseable (triggers retry).
#' @keywords internal
.cv_cellmarkup_parse <- function(text, cluster_names, top_k = 3L) {
  text <- text %||% ""
  text <- gsub("```(json)?", "", text)
  m <- regmatches(text, regexpr("(?s)\\{.*\\}", text, perl = TRUE))
  json <- if (length(m) && nzchar(m)) m else text
  parsed <- tryCatch(jsonlite::fromJSON(json, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(parsed) || is.null(parsed$annotations)) return(NULL)

  out <- list()
  for (ann in parsed$annotations) {
    cl <- as.character(ann$cluster)
    out[[cl]] <- .cv_cellmarkup_candidates_df(ann$candidates %||% list(), top_k)
  }
  for (cl in cluster_names) {
    if (is.null(out[[as.character(cl)]])) {
      out[[as.character(cl)]] <- data.frame(
        CellType = "Unknown", Confidence = 0, Rank = 1L,
        Reason = "Model did not return an annotation for this set.",
        stringsAsFactors = FALSE)
    }
  }
  out[as.character(cluster_names)]
}

#' Convert candidate dicts to a ranked data.frame.
#' @keywords internal
.cv_cellmarkup_candidates_df <- function(cands, top_k) {
  if (!length(cands)) {
    return(data.frame(CellType = "Unknown", Confidence = 0, Rank = 1L,
                      Reason = "No candidate returned.", stringsAsFactors = FALSE))
  }
  ct <- vapply(cands, function(c) as.character(c$cell_type %||% "Unknown"), character(1))
  cf <- vapply(cands, function(c) suppressWarnings(as.numeric(c$confidence %||% NA)), numeric(1))
  rs <- vapply(cands, function(c) as.character(c$reason %||% ""), character(1))
  df <- data.frame(CellType = ct, Confidence = cf, Reason = rs, stringsAsFactors = FALSE)
  ord <- order(-ifelse(is.na(df$Confidence), -Inf, df$Confidence))
  df <- utils::head(df[ord, , drop = FALSE], top_k)
  df$Rank <- seq_len(nrow(df))
  rownames(df) <- NULL
  df[, c("CellType", "Confidence", "Rank", "Reason")]
}

#' Assemble a TypoClust-compatible object with the columns typoClustVis() and
#' addTypoData() expect. LLM-specific fields (Confidence, Reason) are kept;
#' markerDB-style evidence columns are filled from the input panels so the
#' visualization functions work unchanged.
#' @keywords internal
.cv_cellmarkup_build_typoclust <- function(parsed, markers, tissue, condition,
                                           species, pos_panels, neg_panels) {
  cell_types <- lapply(names(parsed), function(cl) {
    df <- parsed[[cl]]
    pos <- pos_panels[[cl]] %||% character(0)
    neg <- neg_panels[[cl]] %||% character(0)
    pos_str <- paste(pos, collapse = "|")
    neg_str <- paste(neg, collapse = "|")
    n <- nrow(df)
    data.frame(
      Tissue = if (!is.null(tissue)) tissue else NA_character_,
      Condition = if (!is.null(condition)) condition else NA_character_,
      CellType = df$CellType,
      Pos_Markers = pos_str,
      Neg_Markers = neg_str,
      Combined_Markers = if (nzchar(pos_str) && nzchar(neg_str)) paste(pos_str, neg_str, sep = "|")
                         else if (nzchar(pos_str)) pos_str else neg_str,
      Pos_Count = length(pos),
      Neg_Count = length(neg),
      Combined_Count = length(pos) + length(neg),
      Pos_Occurrence = length(pos),
      Neg_Occurrence = length(neg),
      Avg_Pos_Purity = NA_real_,
      Avg_Neg_Purity = NA_real_,
      Combined_Score = ifelse(is.na(df$Confidence), 0, df$Confidence) * (length(pos) + length(neg)),
      Confidence = df$Confidence,
      Reason = df$Reason,
      Rank = df$Rank,
      stringsAsFactors = FALSE
    )
  })
  names(cell_types) <- names(parsed)

  structure(
    list(
      cell_types = cell_types,
      metadata = list(
        desired_sets = names(parsed),
        ann_method = "ceLLMarkup",
        tissue = tissue %||% NA_character_,
        condition = condition %||% NA_character_,
        species = species %||% "human",
        marker_panels = list(pos_panels = pos_panels, neg_panels = neg_panels),
        marker_symbol_df = NULL
      )
    ),
    class = "TypoClust"
  )
}
