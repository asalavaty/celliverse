# =============================================================================
# CelliVerse Agent — clarification payload builders
#
# Round XXXIII (Batch 3b, item 1): extracted verbatim out of agent_loop.R,
# where these 3 payload-construction functions had accumulated alongside
# turn-orchestration code and the NLU-detector heuristics (which now live in
# the companion file agent_nlu_routing.R) with no file boundary between the
# three concerns. This is a PURE relocation — no logic was changed. R resolves
# functions by namespace, not by file, so moving them has no runtime effect.
#
# What lives here: functions that build the actual clarification UI payload
# (clickable chips, dropdowns, numeric inputs, resume_template) once one of
# the detectors in agent_nlu_routing.R has decided a clarification is needed.
# Detection and presentation are kept in separate files since they're
# different concerns: a detector answers "should we ask?", a builder answers
# "what exactly do we show?".
# =============================================================================

#' Build a method-choice clarification payload (Marker DB vs LLM) with two
#' clickable choices, each carrying a `resume_message` the UI auto-sends on
#' click so the task continues WITHOUT the user pressing Send again.
#' @keywords internal
cv_method_clarification_payload <- function(user_message) {
  um <- if (is.null(user_message)) "" else as.character(user_message)[1]
  header <- paste0(
    "I can annotate in two ways. Which method should I use? ",
    "Click one and I'll continue straight away.")
  choices <- list(
    list(
      id = "markerdb",
      label = "Marker DB (typoClust)",
      summary = "Curated CelliVerse Marker DB - fast, deterministic, no API key needed.",
      resume_message = paste0(um, " using MarkerDB (typoClust)")
    ),
    list(
      id = "llm",
      label = "LLM (annotateCellsLLM / ceLLMarkup)",
      summary = "LLM-based annotation from top markers - needs an LLM provider configured.",
      resume_message = paste0(um, " using LLM (annotateCellsLLM / ceLLMarkup)")
    )
  )
  lines <- vapply(choices, function(c) sprintf("- **%s**: %s", c$label, c$summary), character(1))
  text <- paste0(header, "\n\n", paste(lines, collapse = "\n"))
  list(text = text, tool = NA_character_, choices = choices, kind = "method_choice")
}

#' Build the UNIFIED annotation-options picker payload (Round XXI): Tissue +
#' Condition dropdowns (each with "All (no filter)" + full vocab) PLUS a numeric
#' n (top markers) field, shown for BOTH the MarkerDB and LLM methods. When the
#' user supplied their own marker list, the n field is omitted and a note is
#' shown instead (n is fixed to the list length). The `resume_template` carries
#' tissue=/condition=/n= placeholders the UI fills and sends on Continue.
#' Ask which species BEFORE the tissue/condition picker.
#'
#' Round LXIV (Batch 1b). This is a separate step, not another field on the
#' tissue card, and that is a correctness requirement rather than a style
#' choice: the Tissue and Condition vocabularies are looked up per species
#' (`tissueCondition_types[[species]]`), so the tissue list cannot be built
#' until the species is known. Asking both at once would offer human tissues to
#' someone annotating a mouse dataset, and the mismatch would only surface as an
#' abort after the run had started.
#'
#' The two methods get different controls because their domains genuinely
#' differ:
#'
#' - **markerDB (typoClust)**: two chips, Human and Mouse. The curated Marker DB
#'   holds exactly those two, and typoClust aborts for anything else. Chips
#'   auto-send, the same as the method chips one step earlier.
#' - **LLM (ceLLMarkup)**: a free-text field pre-filled `human`. The model is not
#'   restricted to a dictionary, so any species works; leaving it blank means
#'   human.
#'
#' @param user_message The user's original request.
#' @param method `"markerdb"` or `"llm"`.
#' @keywords internal
cv_species_clarification_payload <- function(user_message, method = c("markerdb", "llm")) {
  method <- match.arg(method)
  um <- if (is.null(user_message)) "" else as.character(user_message)[1]
  method_label <- if (identical(method, "llm")) "LLM (annotateCellsLLM / ceLLMarkup)" else "MarkerDB (typoClust)"
  base <- sub("\\s*using\\s+(MarkerDB|markerDB|the marker database|typoClust|LLM|annotateCellsLLM|ceLLMarkup).*$",
              "", um, perl = TRUE)
  if (!nzchar(base)) base <- "annotate the clusters"

  if (identical(method, "markerdb")) {
    mk <- function(sp, label) list(
      label = label,
      value = sp,
      resume_message = paste0(base, " using ", method_label, " with species=", sp))
    return(list(
      text = paste0(
        "Which **species** is this dataset? The curated CelliVerse Marker DB covers ",
        "human and mouse, so pick one and I'll continue straight away."),
      tool = NA_character_,
      choices = list(mk("human", "Human"), mk("mouse", "Mouse")),
      dropdowns = list(), inputs = list(), note = NULL,
      resume_template = NULL, base_request = base,
      kind = "species_choice"))
  }

  list(
    text = paste0(
      "Which **species** is this dataset? The LLM is not limited to a fixed list, ",
      "so you can type any species. Leave it as it is for human, and press Continue."),
    tool = NA_character_,
    choices = list(),
    dropdowns = list(),
    inputs = list(list(id = "species", label = "Species", type = "text",
                       default = "human", placeholder = "human")),
    note = NULL,
    resume_template = paste0(base, " using ", method_label, " with species={species}"),
    base_request = base,
    kind = "species_choice")
}

#' @keywords internal
cv_annotation_options_payload <- function(user_message, method = c("markerdb", "llm"), species = "human") {
  method <- match.arg(method)
  um <- if (is.null(user_message)) "" else as.character(user_message)[1]
  sp <- if (is.null(species) || !nzchar(species)) "human" else tolower(species[1])
  tc <- tryCatch({
    e <- new.env(); utils::data("tissueCondition_types", package = "celliverse", envir = e)
    get("tissueCondition_types", envir = e)
  }, error = function(err) NULL)
  vocab <- if (!is.null(tc) && !is.null(tc[[sp]])) tc[[sp]] else NULL
  tissues    <- if (!is.null(vocab)) vocab$all_tissues    %||% character(0) else character(0)
  conditions <- if (!is.null(vocab)) vocab$all_conditions %||% character(0) else character(0)
  all_opt <- "All (no filter)"

  method_label <- if (identical(method, "llm")) "LLM (annotateCellsLLM / ceLLMarkup)" else "MarkerDB (typoClust)"
  # Strip any trailing method directive from the original request so the resume
  # message is clean; the picker re-adds tissue=/condition=/n=.
  base <- sub("\\s*using\\s+(MarkerDB|markerDB|the marker database|typoClust|LLM|annotateCellsLLM|ceLLMarkup).*$", "", um, perl = TRUE)
  if (!nzchar(base)) base <- "annotate the clusters"

  # User-supplied marker list? Then n is fixed to the list length and hidden.
  ml <- cv_extract_user_marker_list(um)
  has_user_markers <- length(ml$markers) >= 2L

  # Round LXXVIII (audit #38 + #39). PREFILL WHAT THE USER ALREADY SAID.
  #
  # Reproduced first, and the audit's #39 wording turned out to be the symptom
  # backwards. It says "n=20 alone currently suppresses the tissue question
  # entirely"; measured, nothing is suppressed -- the card asks for tissue,
  # condition AND n every time, no matter what the message contained:
  #
  #   "annotate the clusters"                       -> asks tissue,condition,n
  #   "annotate the clusters with n=20"             -> asks tissue,condition,n
  #   "annotate the clusters in blood"              -> asks tissue,condition,n
  #   "annotate ... human blood healthy n=20"       -> asks tissue,condition,n
  #
  # and no dropdown carried a selected value at all. So the real defect is the
  # mirror image of the reported one: the user is made to re-enter, by hand,
  # from a 397-item list, something they already typed in plain English.
  #
  # THE FIELDS STAY ON THE CARD. Audit category 2 item 4 records the settled
  # decision that the METHOD is always asked; the audit's own #38 text asks for
  # prefill "(still requiring Continue)" and notes that tissue/condition/n is a
  # different matter with no decision covering it. Prefilling and keeping the
  # confirmation satisfies both items without reopening the settled one -- and
  # it fails safe: a wrong guess is one visible dropdown away from correction,
  # where a silently-applied guess would not be.
  #
  # MATCHED AGAINST THE REAL VOCABULARY, never free text. `tissues` and
  # `conditions` come from the package's own tissueCondition_types for THIS
  # species, so a prefill can only ever be a value the dropdown already offers.
  pre_t <- .cv_match_vocab(um, tissues)
  pre_c <- .cv_match_vocab(um, conditions)
  pre_n <- .cv_extract_top_n(um)

  dropdowns <- list(
    list(id = "tissue",    label = "Tissue",    options = c(all_opt, tissues),
         value = pre_t),
    list(id = "condition", label = "Condition", options = c(all_opt, conditions),
         value = pre_c)
  )
  # Numeric n input (omitted when the user gave their own markers).
  inputs <- if (has_user_markers) list() else list(
    list(id = "n", label = "Top markers per set (n)",
         default = pre_n %||% 20L, min = 1L)
  )
  prefilled <- c(if (!is.null(pre_t)) "tissue", if (!is.null(pre_c)) "condition",
                 if (!is.null(pre_n)) "n")
  note <- if (has_user_markers) {
    sprintf("Using all %d markers you provided (%s); n is fixed to that count.",
            ml$n, paste(utils::head(ml$markers, 8), collapse = ", "))
  } else NULL
  # Say WHICH fields were taken from the message. A prefill the user cannot see
  # the provenance of is indistinguishable from a default, and a wrong one would
  # then look like the system's opinion rather than a reading of their words.
  if (length(prefilled)) {
    pre_note <- sprintf("Filled in from your message: %s. Change anything that is not what you meant.",
                        paste(prefilled, collapse = ", "))
    note <- if (is.null(note)) pre_note else paste(note, pre_note)
  }

  header <- if (identical(method, "llm")) {
    paste0(
      "Before I annotate with the LLM, which **Tissue** and **Condition** should I use as ",
      "context, and how many **top markers (n)** per set should I show the model? ",
      "Pick all three (or \"All (no filter)\" for tissue/condition to annotate generally) ",
      "and press Continue.")
  } else {
    paste0(
      "Before I annotate with the Marker DB, which **Tissue** and **Condition** should I ",
      "match against, and how many **top markers (n)** per set should I use? Pick all three ",
      "(or \"All (no filter)\" to search the whole database) and press Continue.")
  }

  # resume_template: the UI fills {tissue}/{condition}/{n} and sends on Continue.
  # "All (no filter)" -> the value is dropped (NULL = no filter / general).
  # Round LXIV: carry the already-chosen species forward literally, so the final
  # resume message names all four and the tool call gets the right `species`.
  # Without this the species collected one step earlier would be dropped here
  # and every annotation would silently fall back to human.
  # Round LXXIV (audit #10, rider): the `n={n}` leak.
  #
  # When the user supplies their own marker panel, `inputs` above is EMPTY (n is
  # fixed to the panel length and the field is hidden). The client's compose()
  # only substitutes placeholders whose id appears in `inputs`, so `{n}` was
  # never replaced and the message sent to the model literally read
  # "... and n={n}". That happened on the CORRECT path -- a genuine user panel --
  # not only when the panel was detected in error.
  #
  # Substituted server-side with the panel length, which is the value the hidden
  # field would have carried, so the model reads a real number either way.
  n_clause <- if (has_user_markers) paste0(" and n=", ml$n) else " and n={n}"
  resume_template <- paste0(base, " using ", method_label,
                            " with species=", sp,
                            " and tissue={tissue} and condition={condition}", n_clause)
  list(text = header, tool = NA_character_, choices = list(),
       dropdowns = dropdowns, inputs = inputs, note = note,
       resume_template = resume_template, base_request = base,
       user_markers = if (has_user_markers) ml$markers else NULL,
       kind = "annotation_options_choice")
}

#' Build a markdown bullet list of candidate objects (handle + one-line summary)
#' plus a machine-readable `choices` list, so the UI can render clickable handle
#' chips and the user never has to type a handle. `tool` is the intended tool
#' (may be NULL); `header` is a short lead-in sentence.
#' @keywords internal
cv_clarification_payload <- function(store, header, handles, tool = NULL) {
  handles <- unique(as.character(handles[nzchar(as.character(handles))]))
  descs <- lapply(handles, function(h) {
    d <- tryCatch(cv_object_descriptor(store, h), error = function(e) NULL)
    list(handle = h,
         type = if (!is.null(d)) d$type %||% NA_character_ else NA_character_,
         summary = if (!is.null(d)) d$summary %||% "" else "")
  })
  lines <- vapply(descs, function(d) {
    if (!is.na(d$type) && nzchar(d$summary))
      sprintf("- `%s` [%s]: %s", d$handle, d$type, d$summary)
    else
      sprintf("- `%s`", d$handle)
  }, character(1))
  text <- paste0(header, "\n\n", paste(lines, collapse = "\n"))
  list(text = text, tool = tool %||% NA_character_, choices = descs)
}

# ---- Reading tissue / condition / n out of the request (Round LXXVIII) ------
#
# Both helpers are deliberately CONSERVATIVE: they return NULL unless the match
# is unambiguous, because a wrong prefill on a 397-option dropdown is worse than
# no prefill -- the user scans, sees something plausible, and presses Continue.

#' Longest vocabulary term that appears as a whole phrase in the message.
#'
#' Longest-first so "Bone Marrow" wins over "Bone", and word-boundary anchored
#' so "Lung" does not match "Lunge". Returns NULL when nothing matches or when
#' two DIFFERENT terms of the same length both match (ambiguous -> ask).
#' @keywords internal
.cv_match_vocab <- function(msg, vocab) {
  txt <- tolower(as.character(msg %||% "")[1])
  vocab <- as.character(vocab %||% character(0))
  vocab <- vocab[nzchar(vocab)]
  if (!nzchar(trimws(txt)) || !length(vocab)) return(NULL)
  ord <- vocab[order(nchar(vocab), decreasing = TRUE)]
  hits <- character(0)
  for (v in ord) {
    rx <- paste0("\\b", gsub("([.\\\\|()\\[\\]{}^$*+?])", "\\\\\\1", tolower(v), perl = TRUE), "\\b")
    if (grepl(rx, txt, perl = TRUE)) {
      # Stop at the first (longest) match, but keep looking for a same-length
      # rival so a genuine ambiguity can be reported as none.
      if (!length(hits) || nchar(v) == nchar(hits[1])) hits <- c(hits, v) else break
    }
  }
  if (length(hits) != 1L) return(NULL)
  hits[1]
}

#' The n the user asked for, if they said one.
#'
#' Only forms that NAME the quantity count. A bare number in "annotate C1" or
#' "top 5 markers of C3" must not become n, which is why there is no bare-digit
#' branch -- the same trap Round LXXIV's #11 detector had to be narrowed for.
#' @keywords internal
.cv_extract_top_n <- function(msg) {
  txt <- tolower(as.character(msg %||% "")[1])
  if (!nzchar(trimws(txt))) return(NULL)
  pats <- c("\\bn\\s*=\\s*([0-9]+)\\b",
            "\\bn\\s+of\\s+([0-9]+)\\b",
            "\\btop\\s+([0-9]+)\\s+markers?\\b",
            "\\b([0-9]+)\\s+top\\s+markers?\\b",
            "\\bwith\\s+([0-9]+)\\s+markers?\\s+per\\b")
  for (p in pats) {
    m <- regmatches(txt, regexec(p, txt, perl = TRUE))[[1]]
    if (length(m) >= 2L) {
      n <- suppressWarnings(as.integer(m[2]))
      if (!is.na(n) && n >= 1L && n <= 10000L) return(n)
    }
  }
  NULL
}

# =============================================================================
# Round LXXXV — a heavy dispatch that cannot fit this machine
#
# `cv_run_tool_call()`'s validate hooks have one contract: an abort becomes a
# structured error the MODEL reads and corrects. That is right for a bad
# argument, but wrong for "this call would need more memory than the machine
# has" -- the model cannot see the machine's own numbers, so its next guess is
# exactly that, a guess, paid for with another worker spawn. The condition
# class below lets that decision reach the USER instead, through the same
# machinery agent_loop.R's mid-turn "stall" clarification already uses: raised
# from deep inside `tool$validate()`, re-signalled rather than converted at
# each layer in between (mirrors `cv_job_pending_condition()`, agent_worker.R
# -- the precedent this is built from), caught once in `run_tools()`, and
# turned into a "clarification" event carrying a dropdown/input card.
# =============================================================================

#' A condition meaning the turn cannot proceed without an interactive decision
#' from the user -- raised from a `validate()` hook, and understood by
#' `cv_run_tool_call()` and `run_tools()` alike as something to re-raise
#' rather than convert into a tool result. See `cv_job_pending_condition()`
#' (agent_worker.R) for the identical shape this mirrors.
#' @param payload the clarification payload (`text`/`tool`/`choices`/
#'   `dropdowns`/`inputs`/`note`/`resume_template`/`base_request`/`kind`).
#' @keywords internal
cv_needs_clarification_condition <- function(payload) {
  structure(
    class = c("cv_needs_clarification", "error", "condition"),
    list(message = "needs clarification", call = NULL, payload = payload))
}

#' Build the clarification payload for a heavy dispatch that needs a smaller
#' sketch size than the one it was called with.
#'
#' Only reached when `route$fits` is FALSE and `route$sketch_can_help` is
#' TRUE -- `.cv_assert_heavy_object_fits()` (agent_tools_core.R) handles the
#' "cannot help at all" case as an ordinary abort, since there is nothing left
#' to offer there but an explanation. Here there IS a concrete number, so it
#' is offered as an editable numeric field rather than just stated.
#'
#' Deliberately carries NO "proceed anyway" choice, unlike every other
#' advisory in this codebase (`cv_upload_advice()`, `cv_conversion_advice()`)
#' -- this is the one check that exists specifically so the machine never
#' goes down, and a bypass would remove the one property it is for.
#'
#' The resume message is built from the RESOLVED call's own arguments, not
#' re-parsed from the user's original words: by this point the model has
#' already turned those words into a structured call, and that structure is
#' the more reliable source. Every other argument the call already carried
#' (besides the data handle and the sketch settings) is restated verbatim so
#' Continuing does not silently drop a choice the user already made.
#' @keywords internal
cv_sketch_size_clarification_payload <- function(store, args, tool, route, data_arg = "data") {
  handle    <- args[[data_arg]]
  nc        <- route$n_cells_total
  needs     <- cv_bytes_human(route$needs_mb * 2^20)
  budget    <- cv_bytes_human(route$budget_mb * 2^20)
  suggested <- route$suggested_sketch_ncells
  default_n <- if (is.finite(suggested)) suggested else max(1L, min(5000L, nc - 1L))

  carry_names <- setdiff(names(args), c(data_arg, "sketch", "sketch_ncells"))
  carry <- args[carry_names]
  carry <- carry[!vapply(carry, is.null, logical(1))]
  carry_txt <- if (length(carry)) {
    paste0(" and ", paste(sprintf(
      "%s=%s", names(carry),
      vapply(carry, function(v) paste(as.character(v), collapse = ","), character(1))
    ), collapse = " and "))
  } else ""

  text <- sprintf(paste0(
    "Running %s on %s (%s cells) needs about %s here, and this machine can offer about ",
    "%s right now - even with sketching, this step's own preparation runs on the full ",
    "data before any sketch is taken. A sketch of about %s cells is expected to fit. ",
    "Pick a size and I'll continue."),
    tool$name, handle, format(nc, big.mark = ","), needs, budget,
    format(default_n, big.mark = ","))

  list(
    text = text, tool = tool$name, choices = list(), dropdowns = list(),
    inputs = list(list(id = "sketch_ncells", label = "Sketch size (cells)",
                       default = default_n, min = 1L)),
    note = NULL,
    resume_template = sprintf("run %s on %s with sketch=TRUE and sketch_ncells={sketch_ncells}%s",
                              tool$name, handle, carry_txt),
    base_request = sprintf("run %s on %s", tool$name, handle),
    kind = "sketch_size_choice"
  )
}

#' Build the clarification payload for the SPEED-only sketch offer (Round
#' LXXXVI): a large dataset that fits in memory just fine but may be slow.
#'
#' Unlike `cv_sketch_size_clarification_payload()` above, this one DOES carry
#' a "run on the full dataset" bypass (via `choices`) -- sketching here is a
#' preference, not the only way to avoid a crash, so declining it is a
#' perfectly good answer and must stay one click away. The bypass's
#' `resume_message` is the plain, unmodified request: `.cv_offer_speed_sketch()`
#' (agent_tools_core.R) has already recorded that this handle+tool was asked
#' about on the session before raising this, so the resumed call reaches
#' validate() a second time and is let through silently, however the model
#' happens to phrase "no sketch" -- see that function's own comment for why
#' the argument's resolved value cannot be trusted to carry that distinction.
#'
#' `sketch_kind` picks the field being offered: clustoCell takes a cell COUNT
#' (`sketch_ncells`); markoClust takes a FRACTION per cluster (`sketch_fraction`,
#' only meaningful with `identify_subclusters=TRUE`, which the caller has
#' already checked before reaching here).
#' @keywords internal
cv_speed_sketch_clarification_payload <- function(handle, tool, ncell,
                                                    sketch_kind = c("ncells", "fraction")) {
  sketch_kind <- match.arg(sketch_kind)
  base_request <- sprintf("run %s on %s", tool$name, handle)
  bypass <- list(list(
    id = "run_full", label = "Run on the full dataset",
    summary = "Skip sketching. May take a while on a dataset this size.",
    resume_message = base_request
  ))
  lede <- sprintf(paste0(
    "%s (%s cells) is large enough that %s may take a long time, particularly its ",
    "cell-cell similarity and network-generation steps, even though this looks fine ",
    "memory-wise. "), handle, format(ncell, big.mark = ","), tool$name)

  if (sketch_kind == "ncells") {
    default_n <- as.integer(max(1L, min(5000L, ncell - 1L)))
    text <- paste0(lede,
      "Sketch a smaller representative subset first (labels are transferred back to the ",
      "full dataset automatically), or continue on the full dataset.")
    return(list(
      text = text, tool = tool$name, dropdowns = list(), note = NULL,
      inputs = list(list(id = "sketch_ncells", label = "Sketch size (cells)",
                         default = default_n, min = 1L)),
      resume_template = sprintf("%s with sketch=TRUE and sketch_ncells={sketch_ncells}", base_request),
      base_request = base_request, choices = bypass, kind = "speed_sketch_choice"
    ))
  }

  # fraction: markoClust's own default is 0.5; a large-dataset offer suggests
  # a smaller starting point, but the field stays fully editable.
  default_frac <- 0.1
  text <- paste0(lede,
    "Sketch a smaller fraction of cells per cluster first (labels are transferred back to ",
    "the full dataset automatically), or continue on the full dataset.")
  list(
    text = text, tool = tool$name, dropdowns = list(), note = NULL,
    inputs = list(list(id = "sketch_fraction", label = "Sketch fraction (0-1)",
                       default = default_frac, min = 0.01, max = 0.99)),
    resume_template = sprintf("%s with sketch=TRUE and sketch_fraction={sketch_fraction}", base_request),
    base_request = base_request, choices = bypass, kind = "speed_sketch_choice"
  )
}
