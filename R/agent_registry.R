# =============================================================================
# CelliVerse Agent — typed tool registry (infrastructure)
#
# The registry is the heart of the agent. Each tool entry encodes:
#   - name, description        : LLM-facing identity
#   - parameters               : JSON-schema-style spec pinned to real formals()
#   - input_object_types       : which object types a handle arg may point to
#   - output_object_type       : type produced (for chaining / DAG validation)
#   - handler                  : R closure (store, args) -> result record
#   - cost                     : "light" (inline) or "heavy" (async worker)
#   - produces                 : "object" | "plot" | "table" | "metadata"
#   - tier                     : "core" (shown to LLM) or "advanced" (hidden)
#
# The typed input/output info lets the agent (a) offer the right handles as
# arguments, (b) REJECT invalid tool chains before executing, and (c) suggest
# the correct next step. This encodes CelliVerse's workflow DAG.
# =============================================================================

#' Construct a single registry tool entry
#' @keywords internal
cv_tool <- function(name, description, parameters = list(),
                    input_object_types = character(0),
                    output_object_type = NA_character_,
                    handler, cost = c("light", "heavy"),
                    produces = "metadata", tier = c("core", "advanced"),
                    next_suggestions = character(0),
                    result_note = NULL,
                    dispatch_cost = NULL, heavy_impl = NULL,
                    prepare = NULL, validate = NULL) {
  cost <- match.arg(cost)
  tier <- match.arg(tier)
  structure(list(
    name = name,
    description = description,
    parameters = parameters,
    input_object_types = input_object_types,
    output_object_type = output_object_type,
    handler = handler,
    cost = cost,
    produces = produces,
    tier = tier,
    next_suggestions = next_suggestions,
    # Optional standing note appended to EVERY successful result text for this
    # tool (both the light-handler path and the heavy cv_result_from_value
    # path). Used e.g. by typoClust to always advertise the LLM alternative.
    result_note = result_note,
    # Round XXXIV (Batch 3b item 3): `cost` above is the STATIC, registry-
    # display classification (cv_registry_metadata() ships it to the client
    # Tool Inspector as a plain string, so it must stay a string, never a
    # function). Some tools have genuinely DATA-DEPENDENT cost -- umapPlot is
    # near-free when a UMAP embedding already exists (just re-draws it) but
    # runs a full NormalizeData+FindVariableFeatures+ScaleData+RunPCA+RunUMAP
    # pipeline when it doesn't. `dispatch_cost`, when set, is a
    # function(store, call_args) -> "light"|"heavy" that cv_make_dispatcher()
    # consults PER CALL to pick the real path, leaving `cost` alone as the
    # display value. `heavy_impl`, when set, is the name of the internal
    # function the heavy worker's child process should invoke instead of
    # `name` -- needed for tools (like umapPlot) whose public tool name has
    # no matching top-level celliverse:: function of the same name, only an
    # inline `handler` closure bound to the session's object store.
    dispatch_cost = dispatch_cost,
    heavy_impl = heavy_impl,
    # Round LXIV (D1): pre-dispatch preparation that BOTH paths must run.
    #
    # cv_make_dispatcher() calls tool$handler() only when the resolved cost is
    # "light"; a heavy tool goes to cv_launch_heavy(), whose child invokes
    # `heavy_impl %||% name` from the celliverse namespace. So anything a
    # handler does BEFORE calling the CelliVerse function -- argument
    # rewriting, store lookups, guards -- simply does not happen for a heavy
    # tool. That is not hypothetical: markoCell and markerPurity are both heavy
    # and both had their CellSet expansion and their subset guard living in the
    # handler, i.e. dead code in production, while every test called
    # tool$handler() directly and therefore could not see it.
    #
    # This is the THIRD instance of the same drift (CHANGES.md:1499, and
    # typoClust in Round XXXIII), so it is fixed declaratively rather than with
    # another `if (identical(tool$name, ...))` branch in cv_launch_heavy.
    #
    # Signature: function(store, args, tool, handle_args) -> list(args=,
    # inherit_from=). It must run in the PARENT, because the object store lives
    # there and a CellSet handle can only be resolved against it. Called by
    # cv_launch_heavy() before materialization and by the handler on the light
    # path -- exactly one of the two runs for any given call, so there is no
    # double application.
    prepare = prepare,
    # Round LXX (audit #12/#13): pre-dispatch VALIDATION, as distinct from
    # pre-dispatch preparation above.
    #
    # `prepare` rewrites arguments and therefore has to run on whichever path
    # will use them -- cv_launch_heavy() for a heavy tool, the handler for a
    # light one. A validator rewrites nothing, so it does not belong on either
    # path: it belongs at the single funnel every call passes through,
    # cv_run_tool_call(), which is also where the Round LXIX warnings collector
    # already lives. That placement buys two things a per-path check could not:
    # it runs exactly once for light and heavy alike, and it runs BEFORE
    # cv_launch_heavy() spawns a child, so a request that cannot succeed costs
    # nothing to refuse.
    #
    # What it is for: conditions the callee already refuses, checked earlier and
    # said better. `cluster_labels = "ClustoCell_Clusters"` on an object without
    # that column reached the worker and came back as Seurat's own
    # "'ClustoCell_Clusters' not found in this Seurat object" -- correct, but
    # from inside a child process and WITHOUT naming the columns that do exist,
    # so the model could not self-correct in one turn. `sketch_ncells = 5000` on
    # a 2,700-cell object likewise: clustoCell refuses it in 0.1 s, after the
    # agent has paid for a spawn to hear so.
    #
    # Signature: function(store, args, tool, warnings). It runs in the PARENT
    # (it reads the object store's descriptors) and may do exactly two things:
    # abort, which cv_run_tool_call() converts into a structured tool error the
    # model can act on; or append to the warnings collector for something that
    # is not worth refusing. It must never return rewritten arguments -- that is
    # `prepare`'s job, and splitting the two is what keeps a validator cheap
    # enough to re-run on every poll of a suspended heavy job.
    validate = validate
  ), class = "cv_tool")
}

#' Construct one parameter spec (JSON-schema-ish)
#' @param type "string" | "integer" | "number" | "boolean" | "array" | "object" | "handle"
#' @param handle_types if type=="handle", the acceptable object types.
#' @param enum optional allowed values (from `c(...)` defaults in formals).
#' @param default default value (matches the real formals default).
#' @param required logical.
#' @param items element type for arrays.
#' @param description human/LLM description.
#' @keywords internal
cv_param <- function(type, description = "", default = NULL, required = FALSE,
                     enum = NULL, handle_types = NULL, items = NULL,
                     min = NULL, max = NULL) {
  out <- list(type = type, description = description, required = required)
  if (!is.null(default)) out$default <- default
  if (!is.null(enum)) out$enum <- enum
  if (!is.null(handle_types)) out$handle_types <- handle_types
  if (!is.null(items)) out$items <- items
  # Round LXIV Batch 2a: numeric bounds, enforced by cv_coerce_scalar() below.
  # Until now a parameter's documented range lived only in its description --
  # gini_thresh says "0-1" and 99 was accepted, leiden_resolution accepted -4,
  # sketch_ncells accepted -1 -- and each was forwarded to a heavy callr job
  # that then failed (or worse, did not) minutes later.
  if (!is.null(min)) out$min <- min
  if (!is.null(max)) out$max <- max
  out
}

# ---- Registry container -----------------------------------------------------

#' Build the full tool registry (named list of cv_tool objects)
#'
#' Defined in agent_tools_*.R via cv_register_core_tools() and
#' cv_register_advanced_tools(). Kept as a function so it is rebuilt fresh per
#' process and easy to extend (add one entry -> auto-surfaces to the LLM).
#' @keywords internal
cv_build_registry <- function() {
  reg <- list()
  reg <- c(reg, cv_register_core_tools())
  reg <- c(reg, cv_register_advanced_tools())
  reg <- c(reg, cv_register_meta_tools())
  reg <- c(reg, cv_register_cellmarkup_tools())
  # name the list by tool name
  names(reg) <- vapply(reg, function(t) t$name, character(1))
  reg
}

# Cache the registry in a package-internal env (rebuilt on first use).
.cv_registry_cache <- new.env(parent = emptyenv())

#' Get (and lazily build/cache) the registry
#' @keywords internal
cv_registry <- function(rebuild = FALSE) {
  if (rebuild || is.null(.cv_registry_cache$reg)) {
    .cv_registry_cache$reg <- cv_build_registry()
  }
  .cv_registry_cache$reg
}

#' Look up a tool by name (tolerant of casing + small typos)
#'
#' Weak local models routinely emit a tool name with the wrong case
#' (`clustocell`) or a small typo. Rather than fail the whole turn on such a
#' near-miss, resolve in three tiers:
#'   1. exact key match;
#'   2. case-insensitive match when it is UNIQUE (e.g. `clustocell`/`CLUSTOCELL`
#'      -> `clustoCell`);
#'   3. a single close fuzzy match (`adist`, small edit distance) when EXACTLY
#'      one registered tool is within range (e.g. `clustcell` -> `clustoCell`).
#' Zero matches, or an ambiguous case-insensitive / fuzzy set, still errors with
#' the list of valid tool names so the caller (loop or user) can correct it. A
#' non-exact resolution emits an info note so the correction stays visible.
#' @param warnings optional collector (cv_warnings_new()). Round LXIX: a
#'   non-exact resolution used to reach a cli console the browser user never
#'   sees, so the model asking for `clustcell` and getting `clustoCell` was a
#'   correction nobody outside the R session could observe. Informational: the
#'   right tool ran, and this says which.
#' @keywords internal
cv_tool_get <- function(name, reg = cv_registry(), warnings = NULL) {
  nms <- names(reg)

  # (1) Exact.
  if (name %in% nms) return(reg[[name]])

  # Guard against a non-string / empty name before any fuzzy work.
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    cli::cli_abort(c("Unknown tool {.val {name}}.",
                     i = "Valid tools: {.val {nms}}."))
  }

  # (2) Case-insensitive, only if unambiguous.
  ci <- nms[tolower(nms) == tolower(name)]
  if (length(ci) == 1L) {
    cli::cli_inform(c(i = "Interpreted tool {.val {name}} as {.val {ci}} (case-insensitive match)."))
    cv_warn_add(warnings, "info", sprintf(
      "Read the requested tool '%s' as '%s' (letter case differed).", name, ci),
      code = "tool_name_case")
    return(reg[[ci]])
  }

  # (3) Single close fuzzy match. Scale the allowed edit distance with the name
  # length (short names get a tighter budget) and require a UNIQUE nearest tool.
  d <- utils::adist(tolower(name), tolower(nms))[1, ]
  max_d <- max(1L, floor(nchar(name) / 4))          # e.g. 10-char name -> up to 2 edits
  cand  <- nms[d <= max_d]
  if (length(cand) == 1L) {
    cli::cli_inform(c(i = "Interpreted tool {.val {name}} as {.val {cand}} (closest match)."))
    cv_warn_add(warnings, "info", sprintf(
      "Read the requested tool '%s' as '%s', the closest name in the registry.", name, cand),
      code = "tool_name_fuzzy")
    return(reg[[cand]])
  }

  cli::cli_abort(c(
    "Unknown tool {.val {name}}.",
    i = if (length(cand) > 1L)
          "Did you mean one of: {.val {cand}}?" else
          "Valid tools: {.val {nms}}."
  ))
}

# ---- JSON-schema export for LLM function-calling ----------------------------

#' Convert a cv_param to a JSON-schema property (for provider tool specs)
#' @keywords internal
cv_param_to_schema <- function(p) {
  # "handle" is presented to the LLM as a string (the object handle name).
  json_type <- switch(p$type,
    "handle" = "string",
    "integer" = "integer",
    "number" = "number",
    "boolean" = "boolean",
    "array" = "array",
    "object" = "object",
    "string"
  )
  sch <- list(type = json_type)
  desc <- p$description
  if (p$type == "handle" && length(p$handle_types)) {
    desc <- paste0(desc, sprintf(" (object handle; must reference a %s object)",
                                 paste(p$handle_types, collapse = " or ")))
  }
  if (nzchar(desc)) sch$description <- desc
  if (!is.null(p$enum)) sch$enum <- p$enum
  if (p$type == "array" && !is.null(p$items)) sch$items <- list(type = p$items)
  sch
}

#' Convert a single tool to a provider-neutral function spec
#' (OpenAI-style; adapters reshape as needed).
#' @keywords internal
cv_tool_to_spec <- function(tool) {
  props <- lapply(tool$parameters, cv_param_to_schema)
  names(props) <- names(tool$parameters)
  # JSON-schema `properties` MUST be an object. A parameter-less tool leaves
  # `props` as a NAMELESS empty list, which jsonlite/httr2 serialize as an
  # ARRAY `[]` -- Ollama's Go JSON parser then fails with
  # "Value looks like object, but can't find closing '}' symbol" and returns
  # HTTP 400. Forcing an empty *named* list makes it serialize as `{}`.
  if (length(props) == 0L) props <- stats::setNames(list(), character(0))
  required <- names(tool$parameters)[vapply(tool$parameters, function(p) isTRUE(p$required), logical(1))]
  list(
    type = "function",
    `function` = list(
      name = tool$name,
      description = tool$description,
      parameters = list(
        type = "object",
        properties = props,
        required = as.list(required)
      )
    )
  )
}

#' Build the tool-spec list for the LLM (core tier by default)
#' @param include_advanced logical; include the advanced EWCSR primitives.
#' @keywords internal
cv_tools_specs <- function(reg = cv_registry(), include_advanced = FALSE) {
  keep <- Filter(function(t) t$tier == "core" || (include_advanced && t$tier == "advanced"), reg)
  lapply(unname(keep), cv_tool_to_spec)
}

#' Registry metadata for the client (Package Browser / Tool Inspector)
#' @keywords internal
cv_registry_metadata <- function(reg = cv_registry()) {
  lapply(unname(reg), function(t) {
    list(
      name = t$name,
      description = t$description,
      cost = t$cost,
      tier = t$tier,
      produces = t$produces,
      input_object_types = as.list(t$input_object_types),
      output_object_type = t$output_object_type,
      parameters = lapply(names(t$parameters), function(nm) {
        p <- t$parameters[[nm]]
        list(name = nm, type = p$type, required = isTRUE(p$required),
             default = p$default %||% NA, enum = p$enum %||% NULL,
             description = p$description)
      })
    )
  })
}

# ---- Argument coercion & handle resolution ----------------------------------

#' Try to recover a handle argument unambiguously.
#'
#' Weak local models frequently (a) omit a required handle, (b) pass the
#' parameter name as a placeholder, or (c) echo a template string such as
#' "your_object_handle"/"seurat_object". This helper recovers a real handle
#' ONLY when the choice is unambiguous, and returns `NULL` otherwise so the
#' caller can raise a clear error and let the model self-correct.
#'
#' Policy (in order):
#'   1. Exactly one loaded object matches the required `handle_types` -> use it.
#'   2. `val` is a recognizable placeholder/template token AND exactly one
#'      object exists overall -> use it.
#' Anything else (in particular: a placeholder when >1 typed candidate exists)
#' -> NULL. We deliberately do NOT pick the "first" typed candidate: when the
#' choice is genuinely ambiguous the loop surfaces the standard "requires a
#' valid handle - available: ..." error to the USER so they can pick, rather
#' than the agent silently guessing (which previously tie-broke alphabetically
#' and could operate on the wrong / older object).
#'
#' @param val the supplied value (use `NA`/`NA_character_` for a MISSING arg).
#' @keywords internal
cv_try_resolve_handle <- function(val, nm, p, store) {
  typed <- cv_objects_of_type(store, p$handle_types)
  # (1) Unambiguous by type — safe for both missing and wrong values.
  if (length(typed) == 1L) return(typed[[1]])

  # A missing arg (NA) only qualifies for the type-unique case above; without a
  # value there is no placeholder to match, so do not guess among many.
  if (length(val) != 1L || is.na(val) || !nzchar(val)) return(NULL)

  placeholder_tokens <- unique(c(
    nm, tolower(nm), "obj", "object", "x", "handle", "input", "data",
    "your_object_handle", "object_handle", "your_handle", "handle_here",
    "seurat", "seurat_object", "sce", "your_object", "the_object",
    p$handle_types, tolower(p$handle_types)))
  lv <- tolower(val)
  looks_placeholder <- lv %in% tolower(placeholder_tokens)

  # Hallucinated-handle hardening: a weak model often invents a plausible-but-
  # fake handle such as "my_clustocell_handle" or "the_seurat_object_handle".
  # These are NOT in the literal token list above, but they are clearly
  # placeholder-ish (they embed "handle"/"object"/a type name and match no real
  # handle). Treat them as placeholders too so we fall through to the
  # ask-when-ambiguous path (NULL) instead of a hard "does not exist" error.
  if (!looks_placeholder) {
    embeds_token <- grepl("handle|object|_obj\\b|clustocell|seurat|input|data", lv, perl = TRUE)
    # A real handle looks like <prefix>_<id> (e.g. clusto_083933z9tcs9); a fake
    # one usually has no valid existing-handle match AND carries a placeholder
    # keyword. Only treat as placeholder when it is NOT an existing handle.
    if (embeds_token && !cv_object_exists(store, val)) looks_placeholder <- TRUE
  }
  if (!looks_placeholder) return(NULL)

  # Placeholder token: only safe to auto-resolve when the WHOLE store has a
  # single object (unambiguous). If >1 typed candidate exists, return NULL so
  # the caller asks the user which handle to use (ask-when-ambiguous).
  all_h <- cv_object_handles(store)
  if (length(all_h) == 1L) return(all_h[[1]])
  NULL
}

#' Resolve & validate call arguments against a tool's parameter spec
#'
#' - fills defaults, checks required, coerces scalar types
#' - for "handle" params, verifies the handle exists AND its object type is in
#'   the allowed handle_types (this is the DAG guardrail)
#' Returns a list with $args (resolved, handles still as strings) and
#' $handle_args (names that are handles) — the handler resolves handles to
#' objects from the store.
#' @param warnings optional collector (cv_warnings_new()). Round LXIX: the three
#'   handle-recovery paths below are real decisions the agent makes on the
#'   user's behalf, and every one of them reported to a cli console the browser
#'   user never sees. Informational rather than invalidating: each fires only
#'   when the choice was UNAMBIGUOUS -- exactly one loaded object of the right
#'   type -- so the run used the object the user meant. What was missing was
#'   saying so.
#' @keywords internal
cv_resolve_args <- function(tool, args, store, warnings = NULL) {
  args <- args %||% list()
  spec <- tool$parameters
  out <- list()
  handle_args <- character(0)

  for (nm in names(spec)) {
    p <- spec[[nm]]
    is_array_handle <- identical(p$type, "array") && length(p$handle_types) > 0L
    has <- nm %in% names(args) && !is.null(args[[nm]])
    # An explicitly-empty array (e.g. objects=[]) counts as "not provided" so
    # the singleton auto-resolution below can still recover the loaded object.
    if (has && is_array_handle && length(unlist(args[[nm]])) == 0L) has <- FALSE

    if (!has) {
      # A MISSING required handle is the single most common weak-model mistake
      # (e.g. clustoCell() with no `data`). If exactly one loaded object matches
      # the required type, recover it silently and unambiguously; only error
      # when the choice is genuinely ambiguous (0 or >1 candidates).
      if (isTRUE(p$required) && identical(p$type, "handle")) {
        resolved <- cv_try_resolve_handle(NA_character_, nm, p, store)
        if (!is.null(resolved)) {
          cli::cli_inform(c(i = "Auto-supplied missing {.arg {nm}} of {.val {tool$name}} with the only loaded {.val {p$handle_types}} object {.val {resolved}}."))
          cv_warn_add(warnings, "info", sprintf(
            "'%s' was not given a %s, so the only one loaded (%s) was used.",
            tool$name, paste(p$handle_types, collapse = "/"), resolved),
            code = "handle_auto_supplied")
          handle_args <- c(handle_args, nm)
          out[[nm]] <- resolved
          next
        }
      }
      # Array-of-handles (e.g. typoClust 'objects'): auto-supply the single
      # loaded object of the right type; emit a clean, ANSI-free message when
      # the choice is ambiguous (>1) or impossible (0 candidates + required).
      if (is_array_handle) {
        cand <- cv_objects_of_type(store, p$handle_types)
        if (length(cand) == 1L) {
          cli::cli_inform(c(i = "Auto-supplied missing {.arg {nm}} of {.val {tool$name}} with the only loaded {.val {p$handle_types}} object {.val {cand}}."))
          cv_warn_add(warnings, "info", sprintf(
            "'%s' was not given a %s, so the only one loaded (%s) was used.",
            tool$name, paste(p$handle_types, collapse = "/"), cand),
            code = "handle_auto_supplied")
          out[[nm]] <- cand
          next
        }
        if (length(cand) > 1L) {
          cli::cli_abort(paste0(
            "'", tool$name, "' found ", length(cand),
            " objects that could be used for '", nm, "': ",
            paste(cand, collapse = ", "),
            ". Say which one to use, e.g. ", nm, "=['", cand[1], "']."))
        }
        if (isTRUE(p$required)) {
          cli::cli_abort(paste0(
            "'", tool$name, "' needs a ", paste(p$handle_types, collapse = "/"),
            " object in '", nm, "', but none is loaded. Run the prerequisite ",
            "step first (e.g. clustoCell), then use its result."))
        }
        next  # optional + absent + none available
      }
      if (isTRUE(p$required)) {
        cli::cli_abort(c(
          "Tool {.val {tool$name}} requires argument {.arg {nm}}.",
          i = if (identical(p$type, "handle"))
                "Pass one of the loaded handles: {.val {cv_object_handles(store)}}." else NULL))
      }
      if (!is.null(p$default)) out[[nm]] <- p$default
      next
    }
    val <- args[[nm]]
    if (p$type == "handle") {
      if (!is.character(val) || length(val) != 1L) {
        cli::cli_abort("Argument {.arg {nm}} of {.val {tool$name}} must be a single object handle (string).")
      }
      # --- Handle auto-resolution (robustness against LLM handle mistakes) ---
      # Models occasionally pass the *parameter name* ("obj", "object", "x") as a
      # placeholder, invent a handle, or echo a template like "your_object_handle"
      # instead of using a real handle from the object list. Recover unambiguously.
      if (!cv_object_exists(store, val)) {
        resolved <- cv_try_resolve_handle(val, nm, p, store)
        if (!is.null(resolved)) {
          cli::cli_inform(c(i = "Auto-resolved {.arg {nm}} of {.val {tool$name}} from {.val {val}} to loaded handle {.val {resolved}}."))
          cv_warn_add(warnings, "info", sprintf(
            "'%s' asked for '%s', which is not a loaded object, so the only matching one (%s) was used.",
            tool$name, val, resolved),
            code = "handle_auto_resolved")
          val <- resolved
        }
      }
      if (!cv_object_exists(store, val)) {
        cli::cli_abort(c(
          "Argument {.arg {nm}} of {.val {tool$name}} references handle {.val {val}}, which does not exist.",
          i = "Load or create it first (available handles: {.val {cv_object_handles(store)}})."
        ))
      }
      obj_type <- cv_object_type(cv_object_get(store, val))
      if (length(p$handle_types) && !(obj_type %in% p$handle_types)) {
        cli::cli_abort(c(
          "Invalid tool chain: {.arg {nm}} of {.val {tool$name}} needs a {.val {p$handle_types}} object, but handle {.val {val}} is a {.val {obj_type}}.",
          i = "Run the prerequisite step first."
        ))
      }
      handle_args <- c(handle_args, nm)
      out[[nm]] <- val
    } else if (is_array_handle) {
      # Validate every element is a real handle of an allowed type; keep as a
      # character vector (materialized to a list of objects later, by spec, in
      # the light-handler and the heavy worker paths alike).
      hv <- as.character(unlist(val, use.names = FALSE))
      hv <- hv[nzchar(hv)]
      for (h in hv) {
        if (!cv_object_exists(store, h)) {
          cli::cli_abort(paste0(
            "'", tool$name, "': '", nm, "' references handle '", h,
            "', which is not loaded. Available handle(s): ",
            paste(cv_object_handles(store), collapse = ", "), "."))
        }
        ot <- cv_object_type(cv_object_get(store, h))
        if (!(ot %in% p$handle_types)) {
          cli::cli_abort(paste0(
            "'", tool$name, "': '", nm, "' needs ",
            paste(p$handle_types, collapse = "/"), " object(s), but handle '",
            h, "' is a ", ot, "."))
        }
      }
      out[[nm]] <- hv
    } else {
      out[[nm]] <- cv_coerce_scalar(val, p)
    }
  }
  list(args = out, handle_args = handle_args)
}

#' Coerce a scalar/array value to the declared parameter type
#' @keywords internal
cv_coerce_scalar <- function(val, p) {
  # Round LXIV Batch 2a: match an enum case-INSENSITIVELY and return the
  # canonical spelling. A model that emits "positive" against
  # c("Positive","Negative") was being rejected outright, which is a pointless
  # failure: the value is unambiguous and the fix is to normalise it, not to
  # make the user re-ask. Ambiguity is impossible here because an enum with two
  # entries differing only by case would be a bug in the enum itself.
  if (!is.null(p$enum) && length(val) == 1L && is.character(val) && !(val %in% p$enum)) {
    hit <- which(tolower(val) == tolower(p$enum))
    if (length(hit) == 1L) val <- p$enum[hit]
  }
  if (!is.null(p$enum) && length(val) == 1L && !(val %in% p$enum)) {
    cli::cli_abort("Value {.val {val}} not allowed; choose one of {.val {p$enum}}.")
  }
  # Round LXIV Batch 2a: a coercion that turns a real value into NA is a
  # SILENT failure, and was the worst of the three gaps here -- gini_thresh =
  # "high" became NA with only an R "NAs introduced by coercion" warning, then
  # travelled into a heavy job. Refuse it, and say what was wanted.
  if (p$type %in% c("integer", "number", "boolean") &&
      length(val) == 1L && !is.na(val)) {
    coerced <- suppressWarnings(switch(p$type,
      "integer" = as.integer(val), "number" = as.numeric(val),
      "boolean" = as.logical(val)))
    if (length(coerced) != 1L || is.na(coerced)) {
      cli::cli_abort(c(
        "{.val {val}} is not a valid {p$type} value.",
        i = "Give a {p$type} (for example {.val {if (p$type == 'boolean') 'true' else '1'}})."
      ))
    }
    # Numeric bounds, when the parameter declares them.
    if (p$type %in% c("integer", "number")) {
      if (!is.null(p$min) && coerced < p$min) {
        cli::cli_abort(c(
          "{.val {val}} is below the allowed minimum for this setting.",
          i = "Use a value of {.val {p$min}} or more."))
      }
      if (!is.null(p$max) && coerced > p$max) {
        cli::cli_abort(c(
          "{.val {val}} is above the allowed maximum for this setting.",
          i = "Use a value of {.val {p$max}} or less."))
      }
    }
  }
  switch(p$type,
    "integer" = as.integer(val),
    "number"  = as.numeric(val),
    "boolean" = as.logical(val),
    # JSON arrays parse to R *lists* (jsonlite). Most CelliVerse functions want
    # an atomic vector (e.g. typoClust's desired_sets MUST be a character
    # vector, not a list). Flatten a list of scalars to an atomic vector; leave
    # ragged/nested lists untouched.
    "array"   = if (is.list(val) && length(val) &&
                    all(vapply(val, function(x) is.atomic(x) && length(x) == 1L, logical(1))))
                  unlist(val, use.names = FALSE) else as.vector(val),
    "object"  = val,
    val  # string / default
  )
}
