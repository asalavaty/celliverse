# =============================================================================
# CelliVerse Agent — NLU / intent-routing heuristics
#
# Round XXXIII (Batch 3b, item 1): extracted verbatim out of agent_loop.R,
# where these ~9 small, self-contained regex-based detector functions had
# accumulated alongside turn-orchestration code with no file boundary between
# the two concerns. This is a PURE relocation — no logic was changed. R
# resolves functions by namespace, not by file, so moving them has no runtime
# effect; the full pre-existing routing test suite (test-round17-routing.R,
# test-round18-meta-question.R, test-round19-tissue-flow.R,
# test-round21-annotation-options.R, test-annotation-intent-helper-safe.R)
# passes unmodified against this new location.
#
# What lives here: deterministic, provider-agnostic text classifiers that
# decide whether a user message needs a clarification pre-flight (method
# choice, tissue/condition, unspecified marker panel) before any LLM call is
# made. Clarification PAYLOAD BUILDERS (the functions that construct the
# dropdown/chip UI responses) live in the companion file agent_clarification.R
# — kept separate since the two are different concerns doing more good split
# apart than merged (detection vs. presentation).
#
# NOTE: this file previously also carried cv_tissue_condition_payload(),
# cv_parse_tissue_condition(), and cv_parse_n_markers() — all three were
# confirmed dead code with zero production call sites (superseded by Round
# XXI's unified cv_annotation_options_payload(), which the LLM's own tool-call
# construction now uses for tissue=/condition=/n= directly, rather than
# backend regex parsing). They were removed as part of this relocation, along
# with their now-orphaned unit tests in test-round19-tissue-flow.R and
# test-round21-annotation-options.R. See CHANGES.md Round XXXIII for details.
# =============================================================================

# ---- Annotation-method clarification (always ask first) ---------------------

#' Detect an annotation/cell-type request that does NOT name a method.
#'
#' Returns TRUE when the user's message asks to annotate / label / identify the
#' cell type of a cluster/set but never mentions a method keyword (LLM,
#' ceLLMarkup, GPT, MarkerDB, marker database, typoClust). In that case the
#' agent must ASK which method to use (Marker DB vs LLM) rather than picking
#' one. Provider/model-agnostic: this is a deterministic pre-flight check, so a
#' weak model that would otherwise auto-route to annotateCellsLLM is stopped
#' before any LLM call.
#' @keywords internal
#' Detect a META-QUESTION about a prior result rather than a fresh request.
#'
#' Round XVIII: "why did you annotate C5 as NK", "what method did you use",
#' "explain the C5 annotation" are questions ABOUT work already done — the model
#' must answer them from conversation history (rule 0 CONVERSATION FIRST), NOT
#' be intercepted by the method-choice chips OR the no-tool-call recovery.
#' Returns TRUE for interrogative/attribution phrasing about a prior result.
#' @keywords internal
cv_is_meta_question <- function(msg) {
  if (is.null(msg) || !nzchar(msg)) return(FALSE)
  m <- tolower(msg)
  grepl(paste0(
    # Interrogative openers. "what/which" only count as meta when asking about
    # METHOD/REASON/EVIDENCE ("what method did you use") - "what cell type is
    # C2?" is a FRESH annotation request and must still trigger method choice.
    "^\\s*(why|how|explain|tell me|describe|can you explain|could you explain)\\b",
    # Round LXIV (D4): the inner \\b after "type" made this lookahead SUCCEED on
    # the plural. "what cell type is C2?" was correctly treated as a fresh
    # annotation request, but "what cell typeS are C1 and C2?" was classified as
    # a question about previous work -- so the method chips never appeared and
    # the Round XVII guarantee silently regressed for the more natural phrasing.
    # Matching an optional trailing "s" fixes it without loosening anything else.
    "|^\\s*(what|which|when|who)\\b(?!.*\\bcell[ -]?types?\\b)",
    "|\\byou (annotated|labelled|labeled|called|said|did|used|chose|picked)\\b",
    "|\\b(did|do|does) you\\b",
    "|\\byour (annotation|label|prediction|result|call)\\b",
    "|\\b(earlier|previously|before|just now)\\b.*\\b(annotat|label|cell[ -]?type|nk|platelet|predict)\\b",
    "|\\b(annotat|label|predict).*(earlier|previously|before|as nk|as platelet|as \\w+ cell)\\b"),
    m, perl = TRUE)
}

#' Detect a request to WRITE existing cluster/annotation labels INTO a Seurat/SCE
#' object (addClustoData / addTypoData), NOT to annotate. Round XIX: "add the
#' clustocell labels to my seurat obj" was wrongly intercepted by the method-choice
#' chips because it matches "label" + "cluster". These are write-back requests that
#' must fall through to the model (which routes to addClustoData/addTypoData).
#' @keywords internal
cv_is_write_labels_request <- function(msg) {
  if (is.null(msg) || !nzchar(msg)) return(FALSE)
  m <- tolower(msg)
  # A write/transfer verb ...
  write_verb <- grepl("\\b(add|write|put|paste|store|save|embed|transfer|inject|attach|merge|incorporate|insert)\\b", m, perl = TRUE)
  # ... a labels/clusters/annotation noun ...
  label_noun <- grepl("label|cluster|clustocell|typoclust|annotation|cell[ -]?type", m, perl = TRUE)
  # ... and a target object / into-to phrasing (seurat/object/obj/into/to/back).
  target    <- grepl("seurat|object|\\bobj\\b|\\binto\\b|\\bback\\b|\\bto\\b|metadata|meta\\.data", m, perl = TRUE)
  isTRUE(write_verb && label_noun && target)
}

#' Shared annotation-intent test: TRUE when the message has both an
#' annotate/label/cell-type VERB and a cluster/set-like NOUN nearby. Round
#' XXXII (Batch 3, item 9): this verb/noun pair was previously copy-pasted
#' verbatim into `cv_is_unspecified_annotation`, `cv_is_markerdb_annotation`,
#' and `cv_is_llm_annotation`; the three copies had silently drifted (the LLM
#' variant's cluster-word pattern also matched "marker"/"gene", since LLM-path
#' requests often name a marker panel directly, e.g. "annotate using markers
#' CD3E, CD8A"). Extracted to one place so the verb pattern can never drift
#' again, while `extra_cluster_words` preserves each caller's exact prior
#' cluster-word set. Pure refactor: every caller passes the same arguments it
#' always implicitly used, so detection behavior is unchanged.
#' @keywords internal
cv_has_annotation_intent <- function(m, extra_cluster_words = NULL) {
  annotate_verb <- grepl("annotat|label|cell[ -]?type|identify|classif|assign|what (cell|type|is)|name the", m, perl = TRUE)
  cluster_pat <- "cluster|clust|\\bc[0-9]|sub[ -]?clust|set|group|population|c1|c2|c3|c4|c5"
  if (!is.null(extra_cluster_words) && length(extra_cluster_words)) {
    cluster_pat <- paste0(cluster_pat, "|", paste(extra_cluster_words, collapse = "|"))
  }
  cluster_word <- grepl(cluster_pat, m, perl = TRUE)
  isTRUE(annotate_verb && cluster_word)
}

cv_is_unspecified_annotation <- function(msg) {
  if (is.null(msg) || !nzchar(msg)) return(FALSE)
  m <- tolower(msg)
  # Method explicitly named -> the agent routes directly, no clarification.
  method_named <- grepl("llm|cellmarkup|\\bgpt\\b|claude|openai|markerdb|marker[ _-]?db|marker database|typoclust|marker-based|database-based", m, perl = TRUE)
  if (method_named) return(FALSE)
  # META-QUESTION guard: a question ABOUT a prior annotation is NOT a fresh
  # annotate request -> fall through to the model (no method-choice chips).
  if (cv_is_meta_question(msg)) return(FALSE)
  # WRITE-LABELS guard: "add/write the cluster labels into my Seurat obj" is an
  # addClustoData/addTypoData request, NOT an annotate request -> no chips.
  if (cv_is_write_labels_request(msg)) return(FALSE)
  # Annotation intent: an annotate/label/cell-type verb near a cluster/set word.
  cv_has_annotation_intent(m)
}

# ---- Tissue/Condition selection (markerDB path) -----------------------------
#' Detect a request for an analysis CelliVerse does not perform.
#'
#' Round LXVII. Reported from live use: "run differential expression between C1
#' and C2" produced the generic recovery line ("I wasn't able to turn that into
#' an action just now. Please restate...") rather than saying the analysis is out
#' of scope.
#'
#' Round LXVI added a capability-boundary rule to the system prompt, and that
#' rule was not the problem -- the model never got the chance to answer. The
#' request reads as ACTIONABLE (an imperative verb plus cluster ids), so when no
#' tool call followed, `recovery_exhausted` fired and OVERWROTE whatever the
#' model had said with the canned restate-it message. A prompt rule cannot win
#' against a branch that discards its output.
#'
#' So the boundary is enforced deterministically instead, in R, before any LLM
#' call -- the same pattern as the annotation method picker. The model is not
#' asked to decline; the server declines.
#'
#' @param msg The user message.
#' @return `NULL` when in scope; otherwise a list(topic=, alternative=).
#' @keywords internal
cv_out_of_scope_request <- function(msg) {
  if (is.null(msg) || !nzchar(msg)) return(NULL)
  m <- tolower(msg)
  # Each entry: a pattern, what to call it, and the nearest thing CelliVerse
  # genuinely does. The alternative is not optional -- "we cannot do that" ends
  # the conversation, and every one of these has a real neighbour.
  rules <- list(
    list(pat = "differential expression|\\bdifferentially expressed\\b|\\bde analysis\\b|\\bdeg\\b|find de genes",
         topic = "differential expression between groups",
         alt = paste0("For the genes that distinguish a cluster, use getClusterMarkers on the ",
                      "ClustoCell; for a subset you define yourself, get_cluster_cells then ",
                      "markoCell. Both rank markers by EWCSR/Gini rather than a two-group test.")),
    list(pat = "trajectory|pseudotime|\\bmonocle\\b|\\bslingshot\\b",
         topic = "trajectory or pseudotime inference",
         alt = "CelliVerse has no trajectory step; export the object with Results and run it elsewhere."),
    list(pat = "rna velocity|\\bvelocyto\\b|\\bscvelo\\b",
         topic = "RNA velocity",
         alt = "CelliVerse has no velocity step; export the object with Results and run it elsewhere."),
    list(pat = "batch (effect|correction)|integrat(e|ion)|\\bharmony\\b|\\bcca\\b",
         topic = "batch integration",
         alt = "Integrate before loading, then run clustoCell on the integrated object."),
    list(pat = "doublet|\\bscrublet\\b|doubletfinder",
         topic = "doublet detection",
         alt = "Do doublet removal before loading; CelliVerse clusters what it is given."),
    list(pat = "\\bcnv\\b|copy.number|\\binfercnv\\b",
         topic = "copy-number inference",
         alt = "CelliVerse has no CNV step; export the object with Results and run it elsewhere."),
    list(pat = "cell.cell (communication|interaction)|ligand.receptor|\\bcellphonedb\\b|\\bcellchat\\b",
         topic = "cell-cell communication analysis",
         alt = paste0("Annotate cell types first with typoClust or annotateCellsLLM, then export ",
                      "with Results for a communication tool.")),
    list(pat = "deconvolution|spatial decon",
         topic = "spatial deconvolution",
         alt = "CelliVerse has no deconvolution step; export the object with Results and run it elsewhere.")
  )
  for (r in rules) {
    if (grepl(r$pat, m, perl = TRUE)) return(list(topic = r$topic, alternative = r$alt))
  }
  NULL
}

#' The user-facing reply for an out-of-scope request.
#' @keywords internal
cv_out_of_scope_text <- function(hit) {
  paste0(
    "CelliVerse does not do ", hit$topic, ", so I cannot run that here. ",
    hit$alternative)
}

#' Extract an explicit `species=` directive from a request, if present.
#'
#' Round LXIV (Batch 1b). The annotation flow now collects species in its own
#' step before tissue/condition, because the Tissue and Condition vocabularies
#' are per-species. This is how each stage knows whether that step has already
#' been answered.
#'
#' @param msg The user message.
#' @return The species as a lowercase string, or `NULL` when absent/empty.
#' @keywords internal
cv_extract_species <- function(msg) {
  if (is.null(msg) || !nzchar(msg)) return(NULL)
  m <- regmatches(msg, regexpr("(?i)\\bspecies\\s*=\\s*([^,;]*)", msg, perl = TRUE))
  if (!length(m)) return(NULL)
  v <- sub("(?i)^\\s*species\\s*=\\s*", "", m, perl = TRUE)
  # Stop at the next directive keyword so "species=mouse and tissue=Blood"
  # yields "mouse", not "mouse and tissue=Blood".
  v <- sub("(?i)\\s+and\\s+(tissue|condition|n)\\s*=.*$", "", v, perl = TRUE)
  v <- trimws(v)
  # Blank means "use the default", which the caller resolves to human. Returning
  # NULL here would instead re-ask the question the user just answered.
  if (!nzchar(v)) return("human")
  tolower(v)
}



#' Detect a markerDB/typoClust annotation request that has NOT yet specified a
#' tissue/condition -> the agent must ask (dropdowns) before running. Round XIX:
#' an unfiltered cross-tissue search mis-annotated C3 as a Pronephros NK cell, so
#' every markerDB annotation now first collects Tissue + Condition (either may be
#' "All" = no filter). Returns TRUE only when the method is markerDB/typoClust AND
#' no tissue=/condition= directive is present yet.
#' @keywords internal
cv_is_markerdb_annotation <- function(msg) {
  if (is.null(msg) || !nzchar(msg)) return(FALSE)
  m <- tolower(msg)
  # Method must be the Marker DB / typoClust (NOT the LLM path).
  is_markerdb <- grepl("markerdb|marker[ _-]?db|marker database|typoclust", m, perl = TRUE)
  if (!is_markerdb) return(FALSE)
  # If a tissue=/condition= directive is already present, the user has answered
  # the dropdowns -> do NOT ask again (route straight to typoClust).
  has_directive <- grepl("\\btissue\\s*=|\\bcondition\\s*=", m, perl = TRUE)
  if (has_directive) return(FALSE)
  # Annotation intent (same verb/cluster test as the unspecified detector).
  cv_has_annotation_intent(m)
}

#' Detect an LLM-based annotation request (annotateCellsLLM / ceLLMarkup) that
#' has NOT yet specified tissue/condition/n -> the agent must ask (the unified
#' picker) before running. Round XXI: the LLM path now collects the same
#' Tissue/Condition/n as the MarkerDB path. Returns TRUE only when the method is
#' the LLM AND no tissue=/condition=/n= directive is present yet.
#' @keywords internal
cv_is_llm_annotation <- function(msg) {
  if (is.null(msg) || !nzchar(msg)) return(FALSE)
  m <- tolower(msg)
  # Method must be the LLM / ceLLMarkup (NOT the MarkerDB path).
  is_llm <- grepl("annotatecellsllm|cellmarkup|\\bllm\\b|\\bgpt\\b|claude|openai", m, perl = TRUE)
  if (!is_llm) return(FALSE)
  # If a tissue=/condition=/n= directive is already present, the user has answered
  # the picker -> do NOT ask again (route straight to annotateCellsLLM).
  has_directive <- grepl("\\btissue\\s*=|\\bcondition\\s*=|\\bn\\s*=", m, perl = TRUE)
  if (has_directive) return(FALSE)
  # Annotation intent (same verb test as the other detectors; the cluster-word
  # set additionally includes "marker"/"gene" since LLM-path requests often
  # name a marker panel directly, e.g. "annotate using markers CD3E, CD8A").
  cv_has_annotation_intent(m, extra_cluster_words = c("marker", "gene"))
}
# ---- Phrase -> tool aliases (Round LXXVIII, audit #40) -----------------------
#
# `cv_intended_tool()` matched a tool NAME as a substring of the message, and
# nothing else. Its own docstring promised fuzzy matching it never did. Measured
# against 17 phrasings a user would actually type:
#
#   run clustocell on my data   -> clustoCell     (the ONLY one that resolved)
#   cluster my cells            -> <none>
#   find clusters               -> <none>
#   annotate the clusters       -> <none>
#   what cell types are these   -> <none>
#   plot a umap / show a umap   -> <none>
#   markers of C1               -> <none>
#   how pure is CD8A in C1      -> <none>
#   ... 1 of 17.
#
# WHERE THIS IS USED, and why it is not a router. cv_intended_tool() feeds the
# no-tool-call recovery paths: when the model replies with prose instead of
# acting, the loop uses it to work out what the user probably wanted so it can
# nudge or auto-resolve a SINGLE unambiguous call. It never overrides a tool
# call the model actually made.
#
# THE SAFETY RULE, and it is the whole design: resolve ONLY when exactly one
# tool matches. A phrase that matches two tools returns NULL, exactly as an
# unmatched phrase does. Auto-running the wrong analysis is far worse than not
# recovering, so ambiguity fails closed. "annotate ... using the marker
# database" versus the LLM path is decided upstream by
# cv_is_markerdb_annotation()/cv_is_llm_annotation(), which run BEFORE this and
# are unchanged; the aliases here deliberately do not try to re-decide it.
#
# Exact tool-name matching still runs FIRST and still wins, so every phrasing
# that resolved before resolves to the same tool now.

#' Curated phrase -> tool aliases.
#'
#' Regexes, anchored on whole words. Kept in one place so the list can be read
#' as a list rather than reverse-engineered from a matcher.
#' @keywords internal
.cv_tool_aliases <- function() {
  list(
    clustoCell        = c("\\bcluster (my |the )?(cells?|data|object)\\b",
                          "\\bfind (the )?clusters?\\b",
                          "\\bidentify (the )?clusters?\\b",
                          "\\bre-?cluster\\b"),
    markoClust        = c("\\bmarkers? (of|for) (my|the|these) clusters?\\b",
                          "\\bcluster markers?\\b"),
    getClusterMarkers = c("\\bmarkers? (of|for) (c[0-9]+|cluster c?[0-9]+)\\b",
                          "\\btop [0-9]+ markers?\\b"),
    getDatasetMarkers = c("\\bdataset markers?\\b",
                          "\\bmarkers? (of|for|across) (the )?(whole )?(dataset|data set)\\b"),
    markerPurity      = c("\\bhow pure\\b", "\\bpurity of\\b"),
    umapPlot          = c("\\b(plot|draw|show|make|create) (me )?(an? )?umap\\b",
                          "\\bumap plot\\b"),
    featureInspect    = c("\\bis \\w+ a marker\\b", "\\bwhere is \\w+ a marker\\b"),
    clustoCell_TransferLabel = c("\\btransfer (the )?labels?\\b"),
    signatureDotHeatmap = c("\\b(dot ?plot|heat ?map)\\b")
  )
}

#' Which tools does this phrase name, by alias? Zero, one, or several.
#' @keywords internal
.cv_alias_matches <- function(text, reg) {
  t <- tolower(text %||% "")
  if (!nzchar(trimws(t))) return(character(0))
  al <- .cv_tool_aliases()
  hits <- character(0)
  for (tn in names(al)) {
    if (!is.null(reg) && !(tn %in% names(reg))) next   # never name a dead tool
    if (any(vapply(al[[tn]], function(rx) grepl(rx, t, perl = TRUE), logical(1))))
      hits <- c(hits, tn)
  }
  unique(hits)
}


#' Detect a user-supplied marker gene panel in a request (e.g. "annotate using
#' markers CD3E, CD8A, IL7R" or "with markers CD3D CD3E CD8A"). Returns a list
#' with `markers` (character vector, possibly length 0) and `n` (the count).
#' Round XXI: when the user provides their own markers, n is fixed to the list
#' length and the picker's n field is hidden. A "gene-like" token is 2+ chars of
#' uppercase letters/digits (allowing a trailing digit/letter, e.g. CD3E, IL7R,
#' MS4A1, PF4); we require >=2 such tokens to avoid false positives.
#' @keywords internal

cv_extract_user_marker_list <- function(msg) {
  empty <- list(markers = character(0), n = 0L)
  if (is.null(msg) || !nzchar(msg)) return(empty)
  # Only look at the text AFTER a "markers"/"genes" cue, so cluster ids (C1) and
  # method words are not mistaken for genes.
  cue <- regexpr("(?i)\\bmarkers?\\b|\\bgenes?\\b", msg, perl = TRUE)
  if (cue < 0) return(empty)
  tail_txt <- substring(msg, cue + attr(cue, "match.length"))
  # Strip a leading "are"/"of"/":" then split on commas/whitespace.
  tail_txt <- sub("(?i)^\\s*(are|of|:|is)\\s*", "", tail_txt, perl = TRUE)
  toks <- unlist(strsplit(tail_txt, "[,\\s]+", perl = TRUE))
  toks <- trimws(toks)
  toks <- toks[nzchar(toks)]

  # Round LXXIV (audit #10). The old pattern was `^[A-Z][A-Z0-9]{1,}$`, which
  # failed in BOTH directions, measured on real phrasings:
  #
  #   "markers HLA-DRB1, CD3E, MS4A1"          -> HLA-DRB1 DROPPED (hyphen)
  #   "markers cd3e, cd8a, il7r"                -> NOTHING matched (lowercase)
  #   "markers HLA-DRA and HLA-DRB1"            -> NOTHING at all
  #   "markers CD3E, CD8A for tissue PBMC in condition COVID"
  #                                             -> PBMC and COVID COLLECTED AS GENES
  #
  # The last one is the reason widening alone would have made this worse: once
  # lowercase is allowed, "and", "for", "in" are all gene-shaped. The function's
  # own comment already promised the missing half -- "Stop the panel at the first
  # non-gene token run" -- and the code never did it. Both halves ship together.
  #
  # A real symbol may carry hyphens, dots and digits (HLA-DRB1, NKX2-1, H2-Ab1,
  # MT-CO1) but must start with a letter and end alphanumerically. Case is
  # normalised UP on output: the markerDB lookup is an exact `==` against
  # upper-case symbols, and ceLLMarkup only puts the string in a prompt.
  gene_re <- "^[A-Za-z][A-Za-z0-9]*([.-][A-Za-z0-9]+)*$"
  # Words that JOIN a list without being part of it. Anything else ends the run.
  joiner_re <- "^(and|&|plus|\\+|or)$"
  # A token that is gene-SHAPED but is a plain English word is not a symbol.
  # Deliberately tiny: this list only needs to cover words that can appear
  # BETWEEN marker names, because the run stops at the first non-member anyway.
  stop_words <- c("a", "an", "the", "as", "at", "by", "for", "from", "in", "into",
                  "of", "on", "to", "with", "using", "use", "tissue", "condition",
                  "species", "cluster", "clusters", "subcluster", "subclusters",
                  "sub", "cell", "cells", "type", "types", "top", "markers",
                  "marker", "genes", "gene", "please", "then", "annotate", "is",
                  "are", "was", "were", "it", "them", "all", "only", "also")

  is_gene <- function(x) grepl(gene_re, x, perl = TRUE) &&
    !(tolower(x) %in% stop_words) && nchar(x) >= 2L

  genes <- character(0)
  for (tk in toks) {
    # Joiner FIRST. "and" is itself gene-shaped under the widened pattern, so
    # testing for a symbol before testing for a joiner collects it as one --
    # which is exactly what the first version of this loop did.
    if (grepl(joiner_re, tolower(tk), perl = TRUE)) next   # joins, does not end
    if (is_gene(tk)) { genes <- c(genes, toupper(tk)); next }
    break                                                   # the run is over
  }
  genes <- unique(genes)

  if (length(genes) >= 2L) {
    return(list(markers = genes, n = length(genes)))
  }
  empty
}
