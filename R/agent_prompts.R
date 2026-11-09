# =============================================================================
# CelliVerse Agent — saved prompts (Round LXXXI, enhancement E2)
#
# WHY THIS IS ON THE SERVER AND NOT IN localStorage.
#
# The request was for favourite prompts "restored/accessible in and across all
# sessions". localStorage would satisfy the letter of that in one browser
# profile and break it the moment the user opens the app in a different browser,
# a private window, or from a second machine pointed at the same R server -- and
# it would be silently lost by the same "clear site data" click that people use
# to fix an unrelated glitch. The rest of this package already keeps durable
# user state under the directory returned by tools::R_user_dir("celliverse", "cache") (config.json, sessions/, logs/); prompts belong
# beside them, in the same tree the user can back up, edit by hand, and copy to
# another machine.
#
# THE BUILT-INS ARE NOT COPIED INTO THE FILE. cv_example_prompts() stays the one
# definition of the starter set -- the same list the first screen already shows
# and /api/intro already serves. Removing a built-in records its id in a
# `hidden` list rather than deleting a row, so a later release that improves the
# starter wording improves it for everyone instead of pinning each user to
# whatever text happened to be current the day they first opened the app.
#
# Everything here is pure file I/O over jsonlite, which is already an Import. No
# new dependency, no database, no schema migration: an unreadable or half-written
# file degrades to "no saved prompts" rather than to an error, because a broken
# favourites list must never be able to stop the user from asking a question.
# =============================================================================

#' Path to the saved-prompts file.
#'
#' One file, not one per session: these are the user's own shortcuts and the
#' whole point is that they outlive the conversation that created them.
#' @noRd
cv_prompts_path <- function() file.path(cv_home_dir(), "prompts.json")

#' How many saved prompts one installation may hold.
#'
#' A bound, not a policy limit. The file is read on every page load and rendered
#' into a rail; an unbounded list is the same failure mode the log directory
#' (CV_LOG_KEEP_DAYS) and the job history were given bounds for in earlier
#' rounds. 500 is far above any plausible hand-curated favourites list, so in
#' practice nobody meets it -- it exists so that a runaway client cannot grow the
#' file without limit.
#' @noRd
CV_PROMPTS_MAX <- 500L

#' The category a prompt gets when the user does not choose one.
#' @noRd
CV_PROMPT_DEFAULT_CATEGORY <- "General"

#' Stable, human-readable id for a prompt.
#'
#' Derived from the LABEL rather than minted from a counter or a timestamp, so
#' the same prompt gets the same id on every machine, the file stays diffable,
#' and the tests are deterministic. `prefix` separates the two namespaces:
#' `builtin:` ids may appear in the `hidden` list, `user:` ids may not.
#'
#' Deliberately NOT random: Round LXXX's logging notes make the same argument
#' the other way round, and here determinism is what lets a built-in stay hidden
#' across a restart.
#' @noRd
cv_prompt_slug <- function(label, prefix = "user") {
  s <- tolower(as.character(label %||% ""))
  s <- gsub("[^a-z0-9]+", "-", s)
  s <- gsub("^-+|-+$", "", s)
  if (!nzchar(s)) s <- "prompt"
  paste0(prefix, ":", substr(s, 1L, 48L))
}

#' Coerce whatever is on disk into the shape the rest of this file expects.
#'
#' Defensive by design: this file is in the user's own directory, they are
#' invited to edit it, and a stray character in it must degrade to "no saved
#' prompts" rather than to a 500 on the page that shows the chat.
#' @noRd
.cv_prompts_normalise <- function(x) {
  if (!is.list(x)) x <- list()
  # `hidden` arrives as a character vector when this function is called on a
  # store built in memory, and as a LIST of length-1 strings when it is called
  # on one just read back from JSON with simplifyVector = FALSE.
  #
  # This cost a live bug during Round LXXXI's own smoke test: the first version
  # tested `is.character(hidden)` only, so hiding a built-in starter wrote the
  # id to disk correctly and then silently discarded it on the very next read --
  # the starter came straight back, and the API reported `hidden = 0` while the
  # file said otherwise. unlist() collapses both shapes to the same thing.
  hidden <- unlist(x[["hidden"]], use.names = FALSE)
  hidden <- if (is.character(hidden)) unique(hidden[nzchar(hidden)]) else character(0)
  rows <- x[["prompts"]]
  if (!is.list(rows)) rows <- list()
  out <- list()
  seen <- character(0)
  for (r in rows) {
    if (!is.list(r)) next
    label <- as.character(r[["label"]] %||% "")[1]
    text  <- as.character(r[["text"]]  %||% "")[1]
    if (is.na(label) || is.na(text) || !nzchar(trimws(text))) next
    if (!nzchar(trimws(label))) label <- substr(trimws(text), 1L, 60L)
    cat_ <- as.character(r[["category"]] %||% CV_PROMPT_DEFAULT_CATEGORY)[1]
    if (is.na(cat_) || !nzchar(trimws(cat_))) cat_ <- CV_PROMPT_DEFAULT_CATEGORY
    id <- as.character(r[["id"]] %||% "")[1]
    if (is.na(id) || !nzchar(id) || !startsWith(id, "user:")) id <- cv_prompt_slug(label, "user")
    # Two rows may slug to the same id (two labels differing only in
    # punctuation). Disambiguate rather than dropping one: a favourites list
    # that silently loses an entry is worse than one with an ugly id.
    base <- id; k <- 2L
    while (id %in% seen) { id <- paste0(base, "-", k); k <- k + 1L }
    seen <- c(seen, id)
    out[[length(out) + 1L]] <- list(id = id, label = trimws(label),
                                    text = trimws(text), category = trimws(cat_))
  }
  if (length(out) > CV_PROMPTS_MAX) out <- out[seq_len(CV_PROMPTS_MAX)]
  list(hidden = hidden, prompts = out)
}

#' Read the saved-prompts file.
#'
#' Never throws and never warns. A missing file is the ordinary first-run state;
#' an unparseable one is treated the same way, because the alternative -- an
#' error surfacing on the chat screen -- would let a corrupt favourites list
#' block the product's main job.
#' @noRd
cv_prompts_load <- function(path = cv_prompts_path()) {
  raw <- tryCatch({
    if (!file.exists(path)) NULL
    else jsonlite::fromJSON(path, simplifyVector = FALSE)
  }, error = function(e) NULL, warning = function(w) NULL)
  .cv_prompts_normalise(raw)
}

#' Write the saved-prompts file atomically.
#'
#' Temp file plus rename, so a second browser tab (or a crash mid-write) can
#' never leave a half-written JSON document where the favourites live. Returns
#' the normalised store invisibly.
#' @noRd
cv_prompts_save <- function(store, path = cv_prompts_path()) {
  store <- .cv_prompts_normalise(store)
  cv_ensure_home()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp", Sys.getpid())
  payload <- list(version = 1L,
                  hidden = as.list(store$hidden),
                  prompts = store$prompts)
  jsonlite::write_json(payload, tmp, auto_unbox = TRUE, pretty = TRUE, null = "null")
  ok <- tryCatch(file.rename(tmp, path), error = function(e) FALSE)
  if (!isTRUE(ok)) {
    # Rename across devices can fail; fall back to a copy, then clean up.
    tryCatch({ file.copy(tmp, path, overwrite = TRUE); unlink(tmp) },
             error = function(e) NULL)
  }
  invisible(store)
}

#' The built-in starter prompts, in the shape the rail renders.
#'
#' `cv_example_prompts()` (Round LXXX) stays the single definition of the list;
#' this only attaches the id, the category and the `builtin` flag. Adding a
#' starter prompt there therefore adds it here, to every installation, with no
#' file migration -- which is exactly why the built-ins are not copied into
#' prompts.json.
#' @noRd
cv_prompts_builtin <- function() {
  ex <- cv_example_prompts()
  lapply(ex, function(e) list(
    id = cv_prompt_slug(e$label, "builtin"),
    label = e$label, text = e$text,
    category = "Starters", builtin = TRUE
  ))
}

#' Every prompt the rail should show: built-ins minus hidden, then the user's.
#'
#' Order is deliberate. The starters come first because on a fresh install they
#' are all there is, and a user who has added their own has, by definition,
#' already found the rail.
#' @noRd
cv_prompts_all <- function(store = cv_prompts_load()) {
  bi <- cv_prompts_builtin()
  keep <- vapply(bi, function(p) !(p$id %in% store$hidden), logical(1))
  user <- lapply(store$prompts, function(p) c(p, list(builtin = FALSE)))
  c(bi[keep], user)
}

#' The distinct categories present, starters first.
#' @noRd
cv_prompt_categories <- function(prompts = cv_prompts_all()) {
  cats <- vapply(prompts, function(p) as.character(p$category %||% CV_PROMPT_DEFAULT_CATEGORY),
                 character(1))
  unique(cats)
}

#' Add one saved prompt.
#'
#' Validates and returns the WHOLE list rather than just the new row, so the
#' client never has to merge server state into its own copy -- the same contract
#' the settings endpoint uses, and the reason two open tabs cannot drift.
#'
#' Errors are sentences in the approved voice: what happened, what to do next,
#' no apology and no exclamation mark.
#' @noRd
cv_prompts_add <- function(label, text, category = CV_PROMPT_DEFAULT_CATEGORY,
                           store = cv_prompts_load()) {
  text <- trimws(as.character(text %||% "")[1])
  if (is.na(text) || !nzchar(text)) {
    stop("A saved prompt needs some text. Type the message you want to keep, then add it.",
         call. = FALSE)
  }
  if (nchar(text) > 4000L) {
    stop("That prompt is longer than 4000 characters. Shorten it, or send it as a message instead of saving it.",
         call. = FALSE)
  }
  label <- trimws(as.character(label %||% "")[1])
  if (is.na(label) || !nzchar(label)) label <- substr(text, 1L, 60L)
  if (nchar(label) > 80L) label <- substr(label, 1L, 80L)
  category <- trimws(as.character(category %||% CV_PROMPT_DEFAULT_CATEGORY)[1])
  if (is.na(category) || !nzchar(category)) category <- CV_PROMPT_DEFAULT_CATEGORY
  if (nchar(category) > 40L) category <- substr(category, 1L, 40L)
  if (length(store$prompts) >= CV_PROMPTS_MAX) {
    stop(sprintf("You already have %d saved prompts, which is the most this keeps. Remove one to make room.",
                 CV_PROMPTS_MAX), call. = FALSE)
  }
  store$prompts <- c(store$prompts,
                     list(list(id = cv_prompt_slug(label, "user"), label = label,
                               text = text, category = category)))
  cv_prompts_save(store)
}

#' Remove one prompt by id.
#'
#' A BUILT-IN IS HIDDEN, NOT DELETED -- there is no row to delete, and recording
#' the id means a starter the user does not want stays gone across restarts
#' while the definition of the starter set remains a single list in R. A user
#' prompt is dropped outright.
#'
#' Removing something that is already gone succeeds quietly. The user pressed a
#' button meaning "I do not want to see this", and after the call they do not;
#' erroring on a double click would be reporting a failure that is not one.
#' @noRd
cv_prompts_remove <- function(id, store = cv_prompts_load()) {
  id <- as.character(id %||% "")[1]
  if (is.na(id) || !nzchar(id)) {
    stop("That prompt could not be identified. Reload the page and try again.", call. = FALSE)
  }
  if (startsWith(id, "builtin:")) {
    store$hidden <- unique(c(store$hidden, id))
  } else {
    keep <- vapply(store$prompts, function(p) !identical(p$id, id), logical(1))
    store$prompts <- store$prompts[keep]
  }
  cv_prompts_save(store)
}

#' Put every hidden built-in back.
#'
#' The way out of "I removed the starters and now I want them". Leaves the
#' user's own prompts untouched.
#' @noRd
cv_prompts_restore_builtins <- function(store = cv_prompts_load()) {
  store$hidden <- character(0)
  cv_prompts_save(store)
}
