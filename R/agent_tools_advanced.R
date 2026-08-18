# =============================================================================
# CelliVerse Agent — ADVANCED tools + META (utility) tools
#
# Advanced = low-level EWCSR primitives. These are hidden from the default LLM
# tool list (tier="advanced") to keep the prompt focused, but are fully
# invokable via the Tool Inspector / /tools/invoke.
#
# The decoupled large-dataset workflow (createDataSketch, clustoCell_TransferLabel)
# lives in this file for the same reason it was written here originally --
# both are scalability-specific, low-frequency steps -- but ROUND LXXXVI made
# both tier="core": the user asked for the agent itself to be able to run the
# decoupled sketch -> cluster -> transfer -> addClustoData chain end to end
# when asked for it in conversation, which needs the LLM to actually see both
# tools. clustoCell_TransferLabel had been advanced/hidden since it was added
# (Round L), so this is a deliberate, named change to a long-standing default,
# not an oversight -- flagged as such in CHANGES.md.
#
# Meta = agent utility tools the LLM CAN see (tier="core"): inspect loaded
# objects, fetch a stored table, read the tissue/condition vocabulary. These let
# the model orient itself without dumping big objects into context.
# =============================================================================

#' Register advanced (low-level EWCSR primitives) + decoupled-workflow tools
#' @noRd
cv_register_advanced_tools <- function() {
  list(

    # ---- createDataSketch [HEAVY] : first half of the decoupled workflow ---
    cv_tool(
      name = "createDataSketch",
      description = paste(
        "Create a smaller, representative SKETCH of a Seurat object by adding a",
        "new assay to it (wraps Seurat's SketchData) -- for the DECOUPLED",
        "large-dataset workflow: sketch first, run clustoCell/markoClust ON THE",
        "SKETCH ONLY (assay=<sketched_assay>), then clustoCell_TransferLabel +",
        "addClustoData to bring the labels back to the full dataset. Prefer",
        "clustoCell's/markoClust's own sketch=TRUE for moderate-to-large",
        "datasets; reach for this when even the FULL dataset's own",
        "preprocessing before sketching would itself be too slow or",
        "memory-heavy. Updates the Seurat object IN PLACE (same handle; no",
        "duplicate object is created) by adding the new assay."),
      parameters = list(
        data = cv_param("handle", "The Seurat object to sketch.", required = TRUE, handle_types = "Seurat"),
        assay = cv_param("string", "Source assay to sketch from (defaults to the object's current default assay)."),
        ncells = cv_param("integer", "Number of cells in the sketch.", default = 5000L, min = 1),
        sketched_assay = cv_param("string", "Name of the new assay the sketch is written to.", default = "sketch"),
        method = cv_param("string", paste(
          "Sketching method. 'Uniform' samples cells uniformly and needs no prior",
          "model fit; 'LeverageScore' weights sampling by each cell's estimated",
          "influence and is Seurat's own default, but expects the object to already",
          "carry the preprocessing that produces one."),
          default = "Uniform", enum = c("Uniform", "LeverageScore")),
        over_write = cv_param("boolean", "Overwrite sketched_assay if it already exists on this object.", default = FALSE),
        seed = cv_param("integer", "Random seed.", default = 121L),
        verbose = cv_param("boolean", "Show progress messages.", default = TRUE)
      ),
      input_object_types = "Seurat",
      output_object_type = "Seurat",
      cost = "heavy", produces = "object", tier = "core",
      next_suggestions = c("clustoCell", "markoClust"),
      # A sketch is a SUBSET of the cells -- ncells not below the actual count
      # is the same shape of mistake .cv_assert_sketch_fits() already catches
      # for clustoCell's own sketch_ncells, so the guard reuses its wording.
      # The second check is Round LXXXVI, added after diagnosing a live test
      # failure: Seurat::SketchData() silently returns every cell, unchanged,
      # when the source assay has no normalized "data" layer yet -- see
      # .cv_assert_createsketch_normalized()'s doc comment (agent_tools_core.R)
      # for the full root cause.
      validate = function(store, args, tool, warnings) {
        .cv_assert_createsketch_fits(store, args)
        .cv_assert_createsketch_normalized(store, args)
      },
      handler = function(store, args) {
        obj_handle <- args$data  # capture the SAME handle before materialization
        a <- .cv_materialize(store, args, attr(args, "handle_args"))
        # Seurat::SketchData's own parameter names use dots (sketched.assay,
        # over.write), not this schema's snake_case -- translated explicitly
        # here rather than via a blind do.call(..., a), which would otherwise
        # send it names it does not recognise.
        sk_args <- list(object = a$data, ncells = a$ncells, sketched.assay = a$sketched_assay,
                        method = a$method, over.write = a$over_write, seed = a$seed,
                        verbose = a$verbose)
        if (!is.null(a$assay)) sk_args$assay <- a$assay
        res <- do.call(Seurat::SketchData, sk_args)
        .cv_result_object_inplace(store, obj_handle, res, source = "createDataSketch()")
      }
    ),

    # ---- clustoCell_TransferLabel [HEAVY] ---------------------------------
    cv_tool(
      name = "clustoCell_TransferLabel",
      description = paste(
        "Transfer major/sub-cluster labels from a ClustoCell object built on a",
        "sketched (subsampled) dataset to the full-resolution dataset. Returns a",
        "full-data ClustoCell object. Used in the decoupled sketch+transfer",
        "workflow for very large datasets."),
      parameters = list(
        clustoCell = cv_param("handle", "ClustoCell object from the sketched data.", required = TRUE, handle_types = "ClustoCell"),
        # Round L: required = TRUE, found by empirically probing every
        # ClustoCell-consuming tool with a degenerate object.
        #
        # clustoCell_TransferLabel(query_ewcsr_mat = NULL, query_expr_mat = NULL)
        # gives BOTH query arguments a default of NULL and expects one of them
        # to be supplied -- a semantic requirement no signature scan can see,
        # since formally both are optional. This tool exposes only
        # `query_expr_mat`, so an agent call that omits it cannot supply the
        # other one either: it reached the function with no query data at all
        # and died on `Not an S4 object.`, an internal R message that tells the
        # user nothing about what was missing.
        #
        # Marking it required is strictly an improvement in all three cases,
        # which is why it carries no risk: with no candidate loaded the user
        # gets the standard "needs a Seurat object in 'query_expr_mat' ... run
        # the prerequisite step first"; with exactly one loaded it is
        # auto-supplied and the call now SUCCEEDS where it used to fail; with
        # several the user is asked which. No legitimate call is lost, because
        # transferring labels to a full dataset that was never provided was
        # never going to work.
        query_expr_mat = cv_param("handle", "The full Seurat/SCE object (or expression matrix) to transfer labels ONTO.",
                                  required = TRUE,
                                  handle_types = c("Seurat", "SingleCellExperiment", "SpatialExperiment", "dgCMatrix", "matrix")),
        assay = cv_param("string", "Assay of the full dataset.", default = "RNA"),
        layer = cv_param("string", "Layer of the full dataset.", default = "counts"),
        method = cv_param("string", paste(
          "How labels are carried from the sketched subset to the full dataset.",
          "'ewcsr-cor' correlates EWCSR profiles (the CelliVerse default);",
          "'seurat-project'/'seurat-knn' use Seurat's own projection and",
          "nearest-neighbour transfer."), default = "ewcsr-cor",
                          enum = c("ewcsr-cor", "seurat-project", "ewcsr-red-cor", "seurat-knn")),
        num_threads = cv_param("integer", "Threads (-1 = all).", default = -1L),
        seed = cv_param("integer", "Random seed.", default = 121L)
      ),
      input_object_types = "ClustoCell",
      output_object_type = "ClustoCell",
      cost = "heavy", produces = "object", tier = "core",
      # Round LXXXVI: promoted from "advanced" -- see the file header comment.
      next_suggestions = "addClustoData",
      handler = function(store, args) {
        inh <- .cv_input_handles(attr(args, "cv_tool"), args, attr(args, "handle_args"))
        a <- .cv_materialize(store, args, attr(args, "handle_args"))
        res <- do.call(celliverse::clustoCell_TransferLabel, a)
        .cv_result_object(store, res, source = "clustoCell_TransferLabel()", inherit_from = inh)
      }
    ),

    # ---- ewcsr.sparse [HEAVY] ---------------------------------------------
    cv_tool(
      name = "ewcsr.sparse",
      description = "Low-level: compute the Expression-Weighted Centered Scaled Rank (EWCSR) transform of a sparse matrix.",
      parameters = list(
        mat = cv_param("handle", "A sparse (dgCMatrix) or dense matrix (genes x cells).", required = TRUE,
                       handle_types = c("dgCMatrix", "matrix"))
      ),
      input_object_types = c("dgCMatrix", "matrix"),
      output_object_type = "dgCMatrix",
      cost = "heavy", produces = "object", tier = "advanced",
      handler = function(store, args) {
        m <- cv_object_get(store, args$mat)
        res <- celliverse::ewcsr.sparse(m)
        .cv_result_object(store, res, source = "ewcsr.sparse()", inherit_from = args$mat)
      }
    ),

    # ---- gini.ewcsr.fs [LIGHT] --------------------------------------------
    cv_tool(
      name = "gini.ewcsr.fs",
      description = "Low-level: Gini-based feature selection on the EWCSR representation.",
      parameters = list(
        mat = cv_param("handle", paste(
          "A features x cells matrix (genes down the rows), sparse or dense. The",
          "orientation matters: a transposed matrix scores cells as if they were",
          "genes and returns a plausible-looking wrong answer rather than an",
          "error."), required = TRUE, handle_types = c("dgCMatrix", "matrix"))
      ),
      input_object_types = c("dgCMatrix", "matrix"),
      output_object_type = NA_character_,
      cost = "light", produces = "table", tier = "advanced",
      handler = function(store, args) {
        m <- cv_object_get(store, args$mat)
        res <- celliverse::gini.ewcsr.fs(m)
        list(kind = "table", table = as.data.frame(res),
             text = "Ranked features by Gini-weighted EWCSR score. The table is on screen and downloadable as CSV.")
      }
    ),

    # ---- gini.rank.fs [LIGHT] ---------------------------------------------
    cv_tool(
      name = "gini.rank.fs",
      description = "Low-level: Gini-based feature selection on ranks.",
      parameters = list(
        mat = cv_param("handle", paste(
          "A features x cells matrix (genes down the rows), sparse or dense. The",
          "orientation matters: a transposed matrix scores cells as if they were",
          "genes and returns a plausible-looking wrong answer rather than an",
          "error."), required = TRUE, handle_types = c("dgCMatrix", "matrix"))
      ),
      input_object_types = c("dgCMatrix", "matrix"),
      output_object_type = NA_character_,
      cost = "light", produces = "table", tier = "advanced",
      handler = function(store, args) {
        m <- cv_object_get(store, args$mat)
        res <- celliverse::gini.rank.fs(m)
        list(kind = "table", table = as.data.frame(res),
             text = "Ranked features by Gini rank score. The table is on screen and downloadable as CSV.")
      }
    ),

    # ---- jaccard.sparse [LIGHT] -------------------------------------------
    cv_tool(
      name = "jaccard.sparse",
      description = "Low-level: pairwise Jaccard similarity on a sparse matrix.",
      parameters = list(
        mat = cv_param("handle", paste(
          "A features x cells sparse matrix (genes down the rows). The",
          "orientation matters - see gini.ewcsr.fs."),
          required = TRUE, handle_types = "dgCMatrix")
      ),
      input_object_types = "dgCMatrix",
      output_object_type = NA_character_,
      cost = "light", produces = "object", tier = "advanced",
      handler = function(store, args) {
        m <- cv_object_get(store, args$mat)
        res <- celliverse::jaccard.sparse(m)
        .cv_result_object(store, res, source = "jaccard.sparse()", inherit_from = args$mat)
      }
    ),

    # ---- mutual.rank [LIGHT] ----------------------------------------------
    cv_tool(
      name = "mutual.rank",
      description = "Low-level: convert a correlation/similarity matrix to mutual ranks.",
      parameters = list(
        mat = cv_param("handle", paste(
          "A features x cells matrix (genes down the rows). The orientation",
          "matters - see gini.ewcsr.fs."),
          required = TRUE, handle_types = c("dgCMatrix", "matrix"))
      ),
      input_object_types = c("dgCMatrix", "matrix"),
      output_object_type = NA_character_,
      cost = "light", produces = "object", tier = "advanced",
      handler = function(store, args) {
        m <- cv_object_get(store, args$mat)
        res <- celliverse::mutual.rank(m)
        .cv_result_object(store, res, source = "mutual.rank()", inherit_from = args$mat)
      }
    )

  )
}

#' Register meta / utility tools (LLM-visible orientation helpers)
#' @noRd
cv_register_meta_tools <- function() {
  list(

    # ---- list_objects ------------------------------------------------------
    cv_tool(
      name = "list_objects",
      description = paste(
        "List all objects currently loaded in this session (their handles,",
        "types, and one-line summaries). Use this to see what data/results are",
        "available before choosing a tool."),
      parameters = list(),
      cost = "light", produces = "metadata", tier = "core",
      handler = function(store, args) {
        descs <- cv_object_descriptors(store)
        if (!length(descs)) return(list(kind = "metadata", text = "No objects are loaded yet."))
        lines <- vapply(descs, function(d) sprintf("- %s [%s]: %s", d$handle, d$type, d$summary), character(1))
        list(kind = "metadata",
             text = paste0("Loaded objects:\n", paste(lines, collapse = "\n")),
             data = descs)
      }
    ),

    # ---- describe_object ---------------------------------------------------
    cv_tool(
      name = "describe_object",
      description = paste(
        "Get the detailed descriptor (dimensions, metadata columns, cluster counts,",
        "etc.) of one loaded object by handle. Also reports how the object was",
        "produced -- the arguments the analysis actually ran with, and the",
        "celliverse/R/Seurat versions -- so use this to answer 'what settings",
        "produced this?' or 'which version made this?'."),
      parameters = list(
        handle = cv_param("handle", "The object handle to describe (any loaded object type).", required = TRUE)
      ),
      cost = "light", produces = "metadata", tier = "core",
      handler = function(store, args) {
        d <- cv_object_descriptor(store, args$handle)
        if (is.null(d)) cli::cli_abort("No object with handle {.val {args$handle}}.")
        # Round LXXV (audit #29): provenance is attached HERE and not on the
        # descriptor itself, because describe_object is an explicit, one-off
        # request. The descriptor travels into the system prompt every turn; a
        # version list there would be a per-turn tax for something the user asks
        # about once, if ever.
        p <- cv_object_provenance(store, args$handle)
        txt <- d$summary
        if (!is.null(p)) {
          pt <- cv_provenance_text(p)
          if (nzchar(pt)) txt <- paste0(txt, "\n", pt)
          d$provenance <- p
        }
        list(kind = "metadata", text = txt, data = d)
      }
    ),

    # ---- get_table ---------------------------------------------------------
    # ---- get_function_help (Round LXXVIII, audit #45) ----------------------
    #
    # THE GAP. A conceptual question -- "how does clustoCell pick k?", "what does
    # gini_thresh actually do?" -- had no route to the package's own
    # documentation. The model answered from pretraining, where the likely
    # substitute is generic Seurat advice about a method CelliVerse does not use.
    # The package ships full Rd for every exported function and the model could
    # not reach a word of it.
    #
    # A TOOL, NOT A PROMPT DUMP. Pasting the manual into the system prompt would
    # cost that on every turn for something asked once a session, which is the
    # trade Round LXXV's #29 already refused. A tool is pull, not push: it costs
    # nothing until the question is asked.
    #
    # BOUNDED WITH AN HONEST MARKER, never quietly smaller -- the standing rule.
    # Rd for clustoCell is long; the cap says how much was cut and how to get
    # the rest.
    cv_tool(
      name = "get_function_help",
      description = paste(
        "Read the CelliVerse package's OWN documentation for one of its functions",
        "(clustoCell, markoClust, typoClust, markerPurity, ...). Use this to answer",
        "conceptual or parameter questions - 'how does clustoCell choose clusters',",
        "'what does gini_thresh do', 'what does this argument mean' - instead of",
        "answering from general single-cell knowledge, which will describe a",
        "different method. Returns the real help text."),
      parameters = list(
        function_name = cv_param("string", "Function to look up, e.g. 'clustoCell'.", required = TRUE),
        max_chars = cv_param("integer", paste(
          "Cap on the returned help text. Truncation is announced with the",
          "character count, never silent; raise this to read a long page in one",
          "call."), default = 6000L)
      ),
      cost = "light", produces = "text", tier = "core",
      handler = function(store, args) {
        fn <- as.character(args$function_name %||% "")[1]
        fn <- sub("\\(\\)$", "", trimws(fn))
        if (!nzchar(fn)) cli::cli_abort("Name a function to look up.")
        list(kind = "text", text = cv_function_help_text(fn, args$max_chars %||% 6000L))
      }
    ),

    cv_tool(
      name = "get_metadata_columns",
      description = "List the cell-level metadata column names of a Seurat/SCE object (useful to find the right cluster_labels column).",
      parameters = list(
        obj = cv_param("handle", "A Seurat/SCE object.", required = TRUE,
                       handle_types = c("Seurat", "SingleCellExperiment", "SpatialExperiment"))
      ),
      cost = "light", produces = "metadata", tier = "core",
      handler = function(store, args) {
        d <- cv_object_descriptor(store, args$obj)
        cols <- d$metadata_cols %||% character(0)
        list(kind = "metadata",
             text = if (length(cols)) paste("Metadata columns:", paste(cols, collapse = ", ")) else "No metadata columns.",
             data = as.list(cols))
      }
    ),

    # ---- tissue_condition_vocab -------------------------------------------
    cv_tool(
      name = "tissue_condition_vocab",
      description = paste(
        "Return the available tissue types and disease/healthy conditions in the",
        "CelliVerse Marker DB for a species, to help set typoClust's tissue and",
        "condition arguments correctly."),
      parameters = list(
        species = cv_param("string", "Species.", default = "human", enum = c("human", "mouse"))
      ),
      cost = "light", produces = "metadata", tier = "core",
      handler = function(store, args) {
        sp <- args$species %||% "human"
        tc <- tryCatch({
          e <- new.env(); utils::data("tissueCondition_types", package = "celliverse", envir = e)
          get("tissueCondition_types", envir = e)
        }, error = function(err) NULL)
        if (is.null(tc) || is.null(tc[[sp]])) {
          return(list(kind = "metadata", text = sprintf("Could not load tissue/condition vocab for %s.", sp)))
        }
        vocab <- tc[[sp]]
        # Surface the ACTUAL tissue/condition values (not just the list names) so
        # the model sees the exact strings to pass to typoClust. Cap long lists.
        tissues    <- vocab$all_tissues    %||% character(0)
        conditions <- vocab$all_conditions %||% character(0)
        cap <- function(x, n = 60L) if (length(x) > n) c(utils::head(x, n), sprintf("... (%d total)", length(x))) else x
        txt <- paste0(
          "Valid tissue values for ", sp, " (pass these EXACTLY to typoClust's 'tissue'):\n",
          paste(cap(tissues), collapse = ", "),
          "\n\nValid condition values for ", sp, " (pass these EXACTLY to 'condition'):\n",
          paste(cap(conditions), collapse = ", "))
        list(kind = "metadata", text = txt,
             data = list(tissues = tissues, conditions = conditions))
      }
    )

  )
}
