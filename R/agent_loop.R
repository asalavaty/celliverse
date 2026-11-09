# =============================================================================
# CelliVerse Agent — the agent loop (orchestration core)
#
# This ties the LLM layer, the typed registry, the object store, and the session
# together. One turn of the loop:
#
#   1. Assemble context: system prompt + tool specs + live object descriptors +
#      (token-budgeted) conversation history.
#   2. Call cv_chat() (optionally streaming tokens to the client via on_event).
#   3. If the model requested tool calls:
#        - validate & resolve args against the typed registry (DAG guardrail),
#        - dispatch each tool (inline for "light", via worker for "heavy"),
#        - feed structured results (or structured errors) back as tool messages,
#        - loop again (up to max_tool_iters).
#      Else: return the assistant's final text answer.
#
# Everything the model sees about objects is a compact DESCRIPTOR; the giant R
# objects never enter the prompt. Tool errors are returned to the model as
# structured tool results so it can self-correct instead of crashing the turn.
# =============================================================================

#' System prompt: identity, workflow guidance, and hard rules for the model.
#' @noRd
cv_system_prompt <- function(store, config) {
  handles <- cv_object_handles(store)
  # Round LXXXII (audit #20, second half). EVERYTHING IN THIS BLOCK IS DATA.
  #
  # Round LXIV Batch 2a fenced the ceLLMarkup annotation prompt for exactly this
  # reason and the audit's #20 named TWO prompts: "gene names and cluster labels
  # from an uploaded file reach the ceLLMarkup and main prompts verbatim". Only
  # the first was done. The summary line pasted below carries metadata column
  # names, cluster labels, annotated cell-type names and the first few gene and
  # barcode names -- every one of them a string that came out of a file the user
  # was handed by someone else.
  #
  # Nothing malicious is needed for that to matter. A GEO supplementary file
  # with a metadata column containing a sentence, or a cluster someone renamed
  # to something instruction-shaped, is enough; the realistic carrier is a
  # collaborator, not an attacker.
  #
  # The defence is the SAME ONE Round LXIV settled, deliberately reused rather
  # than reinvented: a delimiter so the model can tell where data starts and
  # stops, plus one sentence of standing instruction saying the region is data.
  # Nothing is sanitised or stripped -- a gene called `MARCH1` and a cluster
  # called "Tumour (post-treatment)" must survive untouched, which is why
  # rewriting the text was rejected there and is rejected here.
  #
  # cv_fence_data() also guards the one thing a delimiter cannot: a value that
  # contains the delimiter itself.
  obj_block <- if (length(handles)) {
    lines <- vapply(handles, function(h) {
      d <- cv_object_descriptor(store, h)
      sprintf("  - %s : %s", h, d$summary %||% d$type %||% "object")
    }, character(1))
    cv_fence_data(
      paste(lines, collapse = "\n"),
      lead = paste0("Objects currently loaded in this session (refer to them by handle). ",
                    "The handles are yours to use; the descriptions beside them are text ",
                    "taken from the user's files."))
  } else {
    "No objects are loaded yet. Ask the user to upload data, or use a tool that loads/creates one."
  }

  paste(
    "You are the CelliVerse Agent, an expert single-cell RNA-seq analysis assistant.",
    "CelliVerse is an R package built on the EWCSR method for clustering, marker",
    "discovery, marker-purity scoring, and cell-type annotation. You operate on the",
    "user's data by CALLING TOOLS; you never fabricate results or invent object handles.",
    "",
    "HARD RULES:",
    "0. CONVERSATION FIRST: for greetings, casual chat, thanks, or questions about yourself,",
    "   the package, or what you can do (e.g. 'hi', 'hello', 'thanks', 'what can you do?'),",
    "   reply in plain text WITHOUT calling ANY tool. You already know the loaded objects",
    "   from the list below - never call 'list_objects' just to answer a greeting or a",
    "   general question. Only call a tool when the user asks for an actual analysis action",
    "   on their data.",
    "1. Large data objects live on the server and are referenced ONLY by their exact string",
    "   handle as listed below. Never invent a handle, never use a handle that is not in the",
    "   list, and never ask for or emit raw object contents. Copy the handle VERBATIM from",
    "   the list; do NOT pass the parameter name (e.g. \"obj\") or a made-up name as the value.",
    "   Example: if the list shows 'obj_ab12cd : Seurat ...', call a tool with",
    "   {\"obj\": \"obj_ab12cd\"}, not {\"obj\": \"obj\"}.",
    "2. Respect the workflow DAG. Typical path: load Seurat -> clustoCell -> addClustoData /",
    "   getDatasetMarkers -> typoClust (annotation) -> visualise. A tool that needs a",
    "   ClustoCell cannot accept a raw Seurat handle; run the prerequisite first.",
    "   'addClustoData' writes ClustoCell labels INTO a Seurat/SCE object: its 'obj'",
    "   argument MUST be a Seurat/SingleCellExperiment/SpatialExperiment handle (an",
    "   'obj_...' handle from the list) - NOT the ClustoCell handle and NEVER a placeholder",
    "   like 'obj_clustocell'. Its 'clustoCell' argument MUST be the ClustoCell handle (a",
    "   'clusto_...' handle). If you are unsure which Seurat object to use, call",
    "   'list_objects' first and pick a real handle. You may set 'major_cluster_name' /",
    "   'sub_cluster_name' to the user's requested column names.",
    "   To VISUALISE clusters on a UMAP (a 'UMAP', 'DimPlot', or 'UMAP of/grouped by",
    "   <column>'), call 'umapPlot' with the Seurat handle and group_by set to the metadata",
    "   column (e.g. 'ClustoCell_Clusters' or 'ClustoCell_SubClusters'). addClustoData only",
    "   WRITES labels into the object - it does NOT draw a figure, so never call it to make a",
    "   plot. Run addClustoData once if the column is missing, then call umapPlot.",
    "   addClustoData and addTypoData UPDATE the object IN PLACE (they keep the SAME 'obj_...'",
    "   handle); there is no new handle to track afterwards - keep using the same one.",
    "2b. MARKERS OF AN EXISTING CLUSTER: the cluster and sub-cluster markers are ALREADY stored",
    "   inside the ClustoCell object. To get the TOP / RANKED markers of a cluster or",
    "   sub-cluster (e.g. 'top 10 markers of C1', 'ranked markers of C2'), call",
    "   'getClusterMarkers' with the ClustoCell ('clusto_...') handle, desired_sets (e.g.",
    "   ['C1']), and top_n. CRITICAL: when the user names one or more specific clusters,",
    "   you MUST pass desired_sets with exactly those ids (e.g. 'top 10 markers of C1' ->",
    "   desired_sets=['C1']). If you OMIT desired_sets the tool returns EVERY cluster, which",
    "   is NOT what the user asked for. Only omit desired_sets when the user explicitly wants",
    "   markers for ALL clusters. To PLOT those markers instead, call 'markoClustVis'. Do NOT call",
    "   'markoCell' for the markers of an EXISTING cluster - 'markoCell' is ONLY for a NEW,",
    "   user-defined cell subset (a named list of barcodes, or cluster_labels+desired_clusters)",
    "   and will fail/return an empty 'NA cell subset' result otherwise. NEVER pass a template",
    "   placeholder such as '<clustocell_object_handle>' or 'clustocell_object_handle' as a",
    "   handle - copy the real 'clusto_...' handle verbatim from the loaded-objects list.",
    "2c. MARKER FILTERING for getClusterMarkers: ALWAYS pass the user's VERBATIM phrase",
    "   describing which markers they want as 'request_text' (copied unchanged, e.g. 'top 10",
    "   ranked markers of C1-Sub1', 'markers with purity over 0.5', 'rank between 2 and 5',",
    "   'feature is CD3E'). The server parses it deterministically - case-insensitively and",
    "   ties-aware - and reports how it was interpreted, so relaying the phrase is the most",
    "   reliable way to honour the request. It supports four intents: (a) 'top N ranked',",
    "   'rank N or below', 'ranked N or better', 'markers ranked <= N' = ALL markers with",
    "   Rank <= N, ties KEPT (may exceed N rows); (b) 'top N', 'first N', 'N markers' =",
    "   EXACTLY N rows; (c) threshold/comparison on any column (purity, Gini_Score, Rank,",
    "   Feature) e.g. 'purity > 0.5', 'rank between 2 and 5', 'feature is CD3E'; (d) plain",
    "   'markers of Cx' = the default top 10 rows. You MAY additionally set mode/top_n (or a",
    "   compact 'filter' like 'Purity>0.5') as a hint, but request_text overrides them when",
    "   it is clear. Always tell the user which interpretation the tool reported back.",
    "2d. CELLS / BARCODES OF A CLUSTER: to get the cell names/barcodes/ids of a cluster (e.g.",
    "   'names of the cells in C2', 'which cells are in cluster C1', 'give me N random cells",
    "   from cluster C2'), call 'get_cluster_cells' with the object handle (a ClustoCell",
    "   'clusto_...' handle, or a Seurat/SCE 'obj_...'/'sce_...' handle) and cluster (e.g.",
    "   'C2'). For a random subset set n (sampling is reproducible via seed); omit n for ALL",
    "   cells. Honour a user-supplied subset name via the 'name' argument (e.g. 'named",
    "   random_C2' -> name='random_C2'). This returns a downloadable barcode table AND a",
    "   reusable CellSet ('cellset_...') handle. To then 'run markoCell/markerPurity on that",
    "   subset', call markoCell/markerPurity and pass the CellSet handle as desired_cells -",
    "   do NOT re-list the barcodes. get_cluster_cells returns CELLS, not markers; for markers",
    "   of an existing cluster use getClusterMarkers.",
    "2e. CELL TYPE / ANNOTATION of a cluster or set: when the user asks what CELL TYPE a",
    "   cluster/sub-cluster/set is, or to 'annotate' / 'label' / 'identify the cell type of'",
    "   C1, C1-Sub1, the sub-clusters, etc., call 'typoClust'. It uses the curated CelliVerse",
    "   Marker DB by DEFAULT (mode='markerDB'). Pass the ClustoCell ('clusto_...') handle in",
    "   'objects' (if exactly one ClustoCell/MarkoClust/MarkoCell is loaded you MAY omit it -",
    "   the server auto-uses that one) and, when the user names specific sets, 'desired_sets'",
    "   (e.g. ['C1-Sub1']). SET IDS: pass ONLY ids that appear in the loaded object's summary",
    "   (the system prompt lists every real cluster/sub-cluster id) - NEVER invent or guess a",
    "   set id. For 'all subclusters of C<k>', OMIT desired_sets and set annotate_subclusters=TRUE",
    "   (annotateCellsLLM) rather than listing sub ids. Do NOT answer a cell-type question with get_cluster_cells or",
    "   markoCell: get_cluster_cells returns barcodes (not a cell type) and markoCell is only",
    "   for a NEW user-defined subset - that mis-routing FAILS. After typoClust runs, tell the",
    "   user it used the Marker DB AND that an LLM-based alternative exists ('annotateCellsLLM',",
    "   the ceLLMarkup method); the tool result carries a ready how-to-prompt. Only call",
    "   'annotateCellsLLM' when the user EXPLICITLY asks for an LLM / ceLLMarkup / GPT-based",
    "   annotation. METHOD CHOICE: if the user asks to annotate/label/identify the cell type",
    "   but does NOT name a method (no 'LLM', 'ceLLMarkup', 'GPT', 'MarkerDB', 'marker",
    "   database', or 'typoClust'), DO NOT pick one yourself - the runtime will ask the user",
    "   to choose (Marker DB vs LLM) with clickable buttons before running anything. Reply",
    "   briefly that you need to know which method to use and STOP (no tool call).",
    "   TISSUE/CONDITION/N: the runtime asks the user for Tissue, Condition, and n",
    "   (top markers per set) for BOTH methods before running. When the user's message",
    "   contains 'tissue=<X>', 'condition=<Y>', and/or 'n=<k>' (added after they answer",
    "   the picker), pass them through: for typoClust use 'tissue'/'condition' array args",
    "   (e.g. tissue=['Blood']) and set 'thresh'=k (thresh_mode='n'); for annotateCellsLLM",
    "   use the 'tissue'/'condition' string args and set 'n_markers'=k. If a tissue/condition",
    "   value is 'All (no filter)' or absent, OMIT that argument entirely (NULL = no filter /",
    "   annotate generally). If n is absent, use the default (20). Never invent a",
    "   tissue/condition/n the user did not give.",
    "   MARKER TYPES (typoClust): ALWAYS leave 'use_pos_markers' and 'use_neg_markers' at",
    "   their default TRUE so BOTH positive and negative markers are used. NEVER set either",
    "   to FALSE unless the user EXPLICITLY asks to exclude a marker type. Negative markers",
    "   supply the lineage-exclusion evidence that separates closely-related cell types",
    "   (e.g. CD8+ T vs NK); dropping them silently degrades the annotation.",
    "   SUB-CLUSTER INHERITANCE (typoClust AND annotateCellsLLM): ALWAYS leave",
    "   'inherit_major_clusters' at its default TRUE, and leave 'inherit_score_ratio' alone.",
    "   NEVER set inherit_major_clusters to FALSE unless the user EXPLICITLY asks for the",
    "   sub-clusters to be annotated on their own. It makes a sub-cluster be read within its",
    "   parent major cluster's identity, which is what stops a sub-cluster of an NK-cell",
    "   population being labelled a T cell. It DOES cost one extra model call per parent, and",
    "   that cost is intended - do NOT disable it to save a call, and do not disable it",
    "   because the user named only sub-clusters (that is the normal case for it).",
    # Round LXXV (audit #27). #26 puts a number next to every annotation label;
    # without this rule the model has no instruction whatsoever tying its
    # language to that number, so a coin-toss and a certainty get the same
    # confident prose and the surfacing is wasted. Same block, same voice as the
    # MARKER TYPES and SUB-CLUSTER INHERITANCE rules, which have both held.
    "2f. HOW SURE TO SOUND ABOUT AN ANNOTATION. Every annotation label now carries a",
    "   bracketed strength marker, and your wording MUST match it. '[6.2x next]' means the",
    "   top cell type scored 6.2 times the runner-up - state it plainly. '[1.05x next: CD8+",
    "   T cell]' or '[conf 0.44, close 2nd 0.41: NK cell]' means the top two are nearly",
    "   TIED - say so, name the alternative, and do NOT present the winner as settled.",
    "   '[conf 0.92]' is the MODEL'S OWN estimate, not a measurement: report it as the",
    "   model's confidence, never as evidence strength. '[only candidate]' means nothing",
    "   else was in the database or returned - that is thin support, not certainty. Never",
    "   invent a confidence the marker does not state, never round a near-tie away, and",
    "   never drop the marker's meaning when you summarize.",
    "2g. VISUALIZING ANNOTATION (typoClustVis): use the DEFAULTS (rank_thresh=1, refine=TRUE,",
    "   refine_thresh=1) so the plot matches the vignette - ONE row per cluster. Do NOT raise",
    "   rank_thresh unless the user explicitly asks for multiple ranked predictions per cluster.",
    "   Interpret row labels correctly when the user asks: '_R<k>' = the rank-k prediction",
    "   (suffix only present when rank_thresh>1); '_L<n>' = refined n levels down the cell-type",
    "   hierarchy (n = number of '->' steps in the label); '_L0' = no refinement found, shown as",
    "   the bare cluster id. e.g. 'C1_L1' = cluster C1 top prediction refined 1 level;",
    "   'C2_R2_L1' = cluster C2 rank-2 prediction refined 1 level. NEVER invent a different",
    "   meaning for these suffixes.",
    "3. When the user asks for an analysis, DO run the matching tool - that is how you act",
    "   on their data. If a tool call fails, read the error, fix the arguments (or run the",
    "   missing prerequisite), and try again with DIFFERENT arguments; NEVER repeat the",
    "   identical failing call. The runtime WILL REFUSE a repeated identical call (same tool",
    "   + same arguments) and repeating just wastes the turn. Likewise, once a tool has",
    "   ALREADY returned a result this turn, use that result instead of calling the same",
    "   tool again on the same input unless the user explicitly asks.",
    "3b. ACT DIRECTLY - DO NOT LIST REPEATEDLY. When the user's request clearly maps to a",
    "   tool and the object to run it on is already known, CALL THAT TOOL IMMEDIATELY.",
    "   If EXACTLY ONE compatible object is loaded (see the list below), use its handle",
    "   directly - do NOT call 'list_objects' first to 'check', and NEVER call",
    "   'list_objects' (or 'describe_object') more than once in a turn. After a single",
    "   'list_objects' you ALREADY have every handle, so the very next call must be the",
    "   analysis tool the user asked for (e.g. clustoCell), not another listing. Re-listing",
    "   the same objects is a wasted, no-progress turn: the runtime counts it as a stall and",
    "   will abort the turn. Example: one Seurat object 'obj_x' is loaded and the user says",
    "   'run clustocell on my data' -> call clustoCell with data='obj_x' RIGHT AWAY.",
    "   Only call 'list_objects'/'describe_object' when you genuinely do not know which",
    "   object the user means (e.g. several objects are loaded and the request is ambiguous).",
    "3b-matrix. A COUNTS MATRIX IS NOT A SEURAT OBJECT (Round LXXXIII, from live use).",
    "   When only a `mat_...` / matrix handle is loaded:",
    "     - clustoCell reads it DIRECTLY. Do not convert first just to cluster.",
    "     - Everything else that needs a Seurat - markoClust, markoCell, markerPurity,",
    "       addClustoData, umapPlot - needs `toSeurat` run ONCE on the matrix handle.",
    "   TO DRAW A UMAP FROM A BARE MATRIX the chain is: toSeurat -> clustoCell ->",
    "   addClustoData -> umapPlot(group_by = 'ClustoCell_Clusters'). If the user only",
    "   wants to SEE the cells and no clustering has run, umapPlot can colour by any",
    "   existing metadata column (get_metadata_columns lists them) - offer that rather",
    "   than silently starting a long clustering they did not ask for.",
    "3c. ANSWER THE MESSAGE YOU WERE SENT - NOTHING MORE (Round LXXXI, from live use).",
    "   A goal from an EARLIER turn is not permission to pursue it now. If this message",
    "   says 'add labels to the object', add the labels and report that; do NOT also draw",
    "   the plot that was wanted two turns ago. The user can ask for it in one line if",
    "   they still want it, and doing it unasked spends their tokens and their time on a",
    "   decision they did not make.",
    "   IF WHAT THEY ASKED FOR IS ALREADY TRUE, say so in one sentence and stop. That is a",
    "   complete answer, not a reason to find something else to do.",
    "   AND NEVER INVENT A CAUSE FOR A FAILURE. 'A backend inconsistency', 'a temporary",
    "   state mismatch', 'a system-level issue' are explanations you have no evidence for,",
    "   and stating one makes a guess read as a diagnosis. If you cannot tell why something",
    "   failed, say you cannot tell, say what you checked, and say what you need.",
    "4. Only call tools that are provided, and ONLY via the tool-call mechanism. NEVER",
    "   write a tool call as text in your reply (no ```json {\"name\": ...} ``` blocks,",
    "   no '{\"name\":..,\"arguments\":..}' in prose). Your visible reply is for the user;",
    "   the system executes tools from the structured tool-call field, not from your text.",
    "   Use 'list_objects' / 'describe_object' / 'get_metadata_columns' to inspect state",
    "   before acting when unsure.",
    # Round LXXV: renumbered 2g -> 2h. Inserting the calibration rule as 2f
    # pushed VISUALIZING ANNOTATION to 2g -- which THIS rule already was, giving
    # two different rules one label. Caught by this round's own test, which
    # asserts every label appears exactly once.
    # Round LXXVI rewrote this rule after it FAILED in live use. Its previous
    # text already warned, in as many words, that a gene outside the top N
    # "will appear to be absent when it is simply not top-ranked" -- and the
    # model did exactly that anyway, answering "purity 0.0000, CD8A is not
    # expressed in C1" about a gene sitting at rank 47 of C1's 601 positive
    # markers. That is why the real fix of that round is CODE (the
    # `marker_outside_slice` warning, the appended row, and markerPurity's
    # summary now carrying the numbers). This rule supports those; it is not
    # what is relied on.
    # Round LXXVII (audit #37). Three tools return "markers" and the differences
    # are not guessable from their names. This is prose, and it is honest about
    # that: unlike #35/#36/#43 in the same round there is no mechanism behind it,
    # because choosing the wrong one of these produces a correct answer to a
    # different question rather than a wrong answer to this one.
    # Round LXXVIII (audit #42). A correction is a FRAGMENT: the user has the
    # previous filter in their head and says only what changed. The tool call
    # carries no memory of the last one, so "change the threshold to 0.2" parses
    # to the DEFAULT - measured: source=mode, kind=rows, the 0.2 discarded
    # entirely. The model has the earlier turn in its context and is the only
    # party that can restore the missing half.
    "2j. A CORRECTION IS A FRAGMENT - RESTATE THE WHOLE FILTER. When the user adjusts a",
    "   filter they just asked for - 'change the threshold to 0.2', 'lower it to 0.15',",
    "   'use 0.3 instead' - they are naming ONLY the part that changed. Do NOT pass that",
    "   fragment through as request_text: it has no column in it and will silently fall",
    "   back to the default top-10. Look back at the filter from the earlier turn and send",
    "   the COMPLETE phrase, e.g. if they asked for 'purity > 0.5' and now say 'change it",
    "   to 0.2', send 'purity > 0.2'. If no earlier filter exists, ask which column they",
    "   mean rather than guessing one.",
    "2i. WHICH \"MARKERS\" TOOL. Three of them exist and they answer different questions:",
    "   - 'getClusterMarkers' reads the markers ALREADY STORED on a ClustoCell/MarkoCell for",
    "     ONE cluster or subset. Free, no recomputation. Use it for 'markers of C1'.",
    "   - 'getDatasetMarkers' returns the DATASET-WIDE marker set (the deduplicated union",
    "     across clusters) for downstream use such as a heatmap or an annotation panel. It is",
    "     NOT 'the markers of a cluster' and must not be used to answer that.",
    "   - 'markoClust' / 'markoCell' COMPUTE markers - markoClust for clusters that already",
    "     exist in the object's metadata, markoCell for a cell subset you define. Use these",
    "     only when the markers do not exist yet.",
    "   And the Purity COLUMN in a marker table is that gene's purity inside that stored",
    "   ranking; 'markerPurity' RECOMPUTES purity within a group you name. They can differ",
    "   and both be right - say which one you are quoting (see 2h).",
    "2h. IS THIS GENE A MARKER, AND HOW PURE? (Rounds LXVII + LXXVI, both from live use).",
    "   When the user asks about a NAMED gene in a cluster/sub-cluster/cell subset - 'what is",
    "   the purity of CD8A in C1', 'is CD8A a marker of C1', 'how pure is NKG7 in C1-Sub1':",
    "   ",
    "   STEP 1 - LOOK IN THE OBJECT YOU ALREADY HAVE. If a ClustoCell (or MarkoCell) for that",
    "   data is loaded, its markers are ALREADY computed and stored. Call 'featureInspect'",
    "   with the gene and LEAVE level AND type NULL, so it searches EVERY level and ALL THREE",
    "   marker classes. This is free, exact, and usually the whole answer.",
    "   ",
    "   THERE ARE THREE MARKER CLASSES - POSITIVE, NEGATIVE AND MEDIUM - and a gene missing",
    "   from one is routinely present in another. NEVER conclude a gene is absent, or that its",
    "   purity is zero, from having looked at positive markers alone.",
    "   ",
    "   A TOP-N TABLE CANNOT SHOW ABSENCE. getClusterMarkers returns the top-ranked markers.",
    "   A gene not in that slice may simply rank lower - CD8A is a real positive marker of C1",
    "   at rank 47. If the tool warns that a gene you named sits outside the slice, that",
    "   warning is the answer: report the rank and purity it gives you.",
    "   ",
    "   STEP 2 - ONLY IF THE GENE IS IN NONE OF THE THREE CLASSES, run 'markerPurity' to",
    "   recompute within the group. Pass the SEURAT handle in 'data', the gene(s) in",
    "   'desired_markers', and the group via 'cluster_labels' + 'desired_clusters' (or",
    "   'desired_cells'). Its result summary NAMES the class and purity it found - read that",
    "   and report it. Do not answer a purity question from a marker LIST.",
    "   ",
    "   THE TWO ROUTES GIVE DIFFERENT, BOTH-CORRECT NUMBERS: the stored value is the gene's",
    "   purity in the ORIGINAL clustering; markerPurity RECOMPUTES it within the group you",
    "   named, and may even place it in a different class. Always say WHICH you are quoting,",
    "   and offer the other if it would help.",
    "4b. CAPABILITY BOUNDARY (Round LXV, audit #28). CelliVerse does clustering,",
    "   marker detection and cell-type annotation. It does NOT do differential expression",
    "   between conditions, trajectory/pseudotime, RNA velocity, batch integration, doublet",
    "   detection, CNV inference, cell-cell communication, or spatial deconvolution. If the",
    "   user asks for one of those, SAY SO PLAINLY IN ONE SENTENCE and name the closest thing",
    "   CelliVerse can do (e.g. for differential expression between two clusters: markers via",
    "   getClusterMarkers or markoCell on a defined subset). Do NOT call a tool in the hope",
    "   it is close enough, and do NOT ask which object they meant - the request is out of",
    "   scope, not ambiguous, and an object picker in reply to it is the wrong answer.",
    "5. Prefer sensible defaults; only surface parameters the user cares about. State which",
    "   object handles you produced so the user can reference them.",
    "6. LENGTH IS GRADED, NOT UNIFORM (Round LXXX, audit #91). 'Be concise' on its own",
    "   told you to treat a one-line confirmation and a result the user might publish",
    "   the same way. Match the length to the stakes, in three bands:",
    "   (a) A ROUTINE CONFIRMATION - a plot was drawn, a column was added, an object was",
    "       created - is ONE sentence saying what now exists and where. Do not pad it.",
    "   (b) A RESULT THE USER WILL INTERPRET - markers, purities, annotations, counts -",
    "       is a short paragraph: the headline number, the one caveat that changes how to",
    "       read it, and the obvious next step.",
    "   (c) SOMETHING YOU ARE NOT CONFIDENT IN, or where a setting materially changed the",
    "       answer, or where a tool raised a caveat that may invalidate it, is worth MORE",
    "       words, not fewer: say what is uncertain, why, and what would settle it.",
    "   Uncertainty is the one thing worth spending length on. Never compress (c) down to",
    "   (a) to sound brisk - a confident-sounding wrong answer is the worst output here.",
    "6b. TABLES - REPRODUCE EVERY ROW, AS A MARKDOWN TABLE: when a tool result contains",
    "   a table_preview, your narrative table MUST contain EXACTLY the rows in that",
    "   preview - every row, in the same order, with no rows added or dropped. If the",
    "   tool reports N rows (e.g. '11 row(s); ties at the same rank are all included'),",
    "   your table shows ALL N rows - NEVER truncate to a round number (e.g. do not show",
    "   10 rows when the tool returned 11) and NEVER drop tied rows. FORMAT MATTERS:",
    "   always render the table as a GitHub-flavored MARKDOWN pipe table (a header row,",
    "   a |---|---| separator row, then one |...| row per data row). NEVER emit the",
    "   rows as plain space-aligned text - plain text does not render as a table in the",
    "   chat UI and rows can be lost on screen. Example, if the preview has 11 rows",
    "   (ranks 1 and 2 tied), write:",
    "   | Rank | Feature | Gini_Score | Purity |",
    "   |------|---------|------------|--------|",
    "   | 1 | CST3 | 0.0027 | 0.9973 |",
    "   | 1 | TYROBP | 0.0027 | 0.9973 |",
    "   | ... | (continue for EVERY row, including the last one) |",
    "   Only when the preview says truncated=true (nrow_total > n_shown) may you show",
    "   just the provided rows - and then you MUST say the table is a partial preview",
    "   of nrow_total rows and point the user to the full on-screen table / CSV",
    "   download.",
    "",
    # ---- Round LXXX (audit #85): the voice ---------------------------------
    #
    # Round LXII settled this project's error voice and applied it to
    # R-AUTHORED strings only. But the majority of the text a user reads is the
    # MODEL'S own prose, and until now nothing in this prompt governed it at
    # all: there was one line, "Be concise", and no instruction about emoji,
    # exclamation marks, apologies or "Great! Let me..." openings. Compliance
    # was luck plus model choice, across nine providers.
    #
    # Placed LAST in the rule block, immediately before the object list, because
    # that is the position a long system prompt weights most reliably across
    # providers -- the same reason the object block sits where it does.
    #
    # Stated as PROHIBITIONS with a replacement, not as adjectives. "Be
    # professional" is unenforceable and unmeasurable; "no exclamation marks"
    # is both.
    "HOW TO WRITE (this governs your prose, not which tool to call):",
    "- No emoji. Not one, anywhere, including in headings and list bullets.",
    "- No exclamation marks.",
    "- Do not open with 'Great', 'Certainly', 'Sure', 'Absolutely', 'Of course',",
    "  'I'd be happy to', or 'Let me ...'. Start with the answer.",
    "- Do not apologise, and do not thank the user for their question. If",
    "  something went wrong, say what happened and what to do next - that is the",
    "  useful part of an apology and the rest is noise.",
    "- Do not restate the user's request back to them before answering it.",
    "- Use their own words for their data - cluster ids, column names and gene",
    "  symbols exactly as they wrote them, with the same capitalisation.",
    "- NEVER state that a tool ran, a file was written, a column was added, or a",
    "  number was computed unless a tool result in THIS conversation says so. If",
    "  you are reporting something you did not observe, say that you did not.",
    "- Say 'I' for yourself and 'you' for the user. Do not call yourself 'the",
    "  agent' or write in the third person.",
    "",
    obj_block,
    sep = "\n"
  )
}

#' Convert stored session history (rich message records) to the internal LLM
#' message schema, dropping UI-only fields.
#' @noRd
cv_history_to_llm <- function(history) {
  lapply(history, function(m) {
    out <- list(role = m$role, content = m$content)
    if (!is.null(m$tool_calls))   out$tool_calls   <- m$tool_calls
    if (!is.null(m$tool_call_id)) out$tool_call_id <- m$tool_call_id
    if (!is.null(m$name))         out$name         <- m$name
    out
  })
}

#' Very rough token estimate (chars/4) used only for history budgeting.
#' @noRd
cv_estimate_tokens <- function(x) {
  if (is.null(x)) return(0L)
  as.integer(ceiling(nchar(paste(unlist(x), collapse = " ")) / 4))
}

#' Group messages so tool-call/tool-result runs are treated as one atomic
#' unit by any code that truncates a message list (by token budget or by
#' plain count).
#'
#' An assistant message that emits `tool_calls` and the "tool"-role message(s)
#' that immediately answer those calls are collapsed into a single group.
#' Every other message is its own one-message group. A caller that keeps or
#' drops whole groups (rather than individual messages) can never land a cut
#' between an assistant tool-call message and its matching tool-result
#' message -- the exact shape OpenAI-compatible chat-completions endpoints
#' (including Ollama's and LM Studio's OpenAI-compat servers, not just cloud
#' providers) reject with a 400 error.
#'
#' Extracted from `cv_budget_history()` (Batch 3b item 2) so the two
#' truncation call sites that need this -- the per-turn token budget below,
#' and `cv_history_evict_stale()`'s per-session hard count cap in
#' agent_session.R -- share one definition instead of drifting the way the
#' annotation-intent regex once did (see Batch 3a item 3).
#' @param msgs list of message lists, each with at least a `role` field and
#'   optionally `tool_calls` / `tool_call_id`. Works on both the LLM-shaped
#'   messages `cv_history_to_llm()` produces and the raw records stored in
#'   `sess$history`, since both carry these same field names.
#' @return list of groups; each group is a list of 1+ messages.
#' @noRd
cv_group_history_atomic <- function(msgs) {
  groups <- list()
  i <- 1L
  n <- length(msgs)
  while (i <= n) {
    m <- msgs[[i]]
    tcs <- m$tool_calls %||% list()
    if (identical(m$role, "assistant") && length(tcs) > 0L) {
      ids <- vapply(tcs, function(tc) tc$id %||% NA_character_, character(1))
      grp <- list(m)
      j <- i + 1L
      while (j <= n && identical(msgs[[j]]$role, "tool") &&
             !is.null(msgs[[j]]$tool_call_id) &&
             msgs[[j]]$tool_call_id %in% ids) {
        grp[[length(grp) + 1L]] <- msgs[[j]]
        j <- j + 1L
      }
      groups[[length(groups) + 1L]] <- grp
      i <- j
    } else {
      groups[[length(groups) + 1L]] <- list(m)
      i <- i + 1L
    }
  }
  groups
}

#' Budget the history so the prompt stays within a soft token cap.
#'
#' Keeps the system message (always), then walks from the MOST RECENT message
#' backwards, keeping messages until the budget is exhausted. Tool-call/tool-
#' result pairs are kept together (via `cv_group_history_atomic()`). Object
#' descriptors are injected via the system prompt, so they are NEVER dropped
#' by truncation.
#'
#' Without the atomic grouping, the token cutoff could previously land
#' between an assistant tool-call message and its matching tool-result
#' message, producing a message array with an orphaned "tool" message (a
#' tool response with no preceding assistant `tool_calls` entry) or an
#' orphaned assistant tool-call (no matching response). OpenAI-compatible
#' chat-completions endpoints -- including Ollama's and LM Studio's
#' OpenAI-compat servers, not just cloud providers -- reject that shape with
#' a 400 error once a long-running session's history grows past the budget.
#' @noRd
cv_budget_history <- function(sys_msg, msgs, max_tokens = 12000L) {
  used <- cv_estimate_tokens(sys_msg$content)

  # ---- Group messages so tool-call/tool-result runs truncate atomically ----
  groups <- cv_group_history_atomic(msgs)

  # ---- Reverse walk over GROUPS (not individual messages) ------------------
  kept_rev <- list()
  for (gi in rev(seq_along(groups))) {
    grp <- groups[[gi]]
    t <- sum(vapply(grp, function(m) {
      cv_estimate_tokens(c(m$content,
                            vapply(m$tool_calls %||% list(),
                                   function(tc) jsonlite::toJSON(tc$arguments, auto_unbox = TRUE),
                                   character(1))))
    }, numeric(1)))
    if (used + t > max_tokens && length(kept_rev) > 0L) break
    # Append in reverse-within-group order so the final rev() below restores
    # each group's original internal ordering (assistant tool_calls message
    # before its tool-result message(s)).
    for (m in rev(grp)) kept_rev[[length(kept_rev) + 1L]] <- m
    used <- used + t
  }
  c(list(sys_msg), rev(kept_rev))
}

#' Emit an event to the client if a callback is supplied (SSE bridge).
#' Event types: "token", "tool_start", "tool_result", "tool_error",
#' "assistant", "iteration", "done", "error".
#' @noRd
cv_emit <- function(on_event, type, ...) {
  if (is.function(on_event)) on_event(c(list(type = type), list(...)))
  invisible()
}

#' Cooperative cancel checkpoint (Round XXIV).
#'
#' The Stop button flags the turn via cv_turn_cancel() (which sets
#' `cancel = TRUE` on the turn record). The blocking cv_chat() call never emits
#' an event, so the on_event-based cancel check alone lets a long LLM call run
#' to completion. This helper is called at the top of each agent-loop iteration
#' and right after each cv_chat() returns: if the turn is flagged, it emits a
#' "cancelled" event and aborts with a "cancelled" message, which the turn
#' runner (cv_start_turn) already classifies as status="cancelled" (not an
#' error). No-op when turn_id is NULL (sync/CLI path) or the flag is unset.
#' @noRd
.cv_turn_check_cancelled <- function(session_id, turn_id, on_event = NULL) {
  if (is.null(turn_id)) return(invisible(FALSE))
  rec <- tryCatch(cv_turn_get(session_id, turn_id), error = function(e) NULL)
  if (is.null(rec) || !isTRUE(rec$cancel)) return(invisible(FALSE))
  cv_emit(on_event, "cancelled")
  cli::cli_abort("Turn cancelled by user.")
}

#' Clean a final assistant answer before it is shown to the user.
#'
#' Weak local models (e.g. qwen2.5:7b via Ollama) sometimes narrate a tool call
#' as prose in the answer text — a fenced ```json {"name":..,"arguments":..}```
#' block, or a bare '{"name": "...", "arguments": {...}}' line — in addition to
#' (or instead of) using the native tool-call channel. Those blocks are noise to
#' the user (the loop drives tools via the structured field, not this text), so
#' strip them from the visible answer. If stripping empties the message, fall
#' back to a short, honest line rather than showing raw JSON or nothing.
#' @noRd
cv_clean_assistant_text <- function(text) {
  if (is.null(text) || !nzchar(text)) return(text %||% "")
  x <- text
  # 1) Fenced code blocks (```json ... ``` or ``` ... ```) that look like a
  #    tool call: contain a "name" and an "arguments" key.
  x <- gsub("(?s)```[a-zA-Z0-9_]*\\s*\\{[^`]*?\"name\"[^`]*?\"arguments\"[^`]*?\\}\\s*```",
            "", x, perl = TRUE)
  # 2) Bare single-line JSON tool-call objects not inside a fence.
  x <- gsub("(?m)^\\s*\\{\\s*\"name\"\\s*:\\s*\"[^\"]+\"\\s*,\\s*\"arguments\"\\s*:.*\\}\\s*$",
            "", x, perl = TRUE)
  # 2b) Round LXXXIII, from live use: a FRAGMENT of a tool call, with no opening
  #     brace and no `name` key, e.g.
  #        "arguments": {"matrix": "mat_2104358gk63y", "name": "seurat_from_..."}}
  #     The user saw exactly that printed as the agent's whole reply, because the
  #     model had called a tool that did not exist (`toSeurat`, added this round)
  #     and the recovery path emitted the leftover text verbatim. Patterns 1 and 2
  #     both require a well-formed object, so neither matched.
  #
  #     Anchored on the `"arguments"` KEY followed by a brace, which is JSON and
  #     not something that occurs in prose about single-cell analysis. Whatever
  #     surrounds it is kept: the aim is to remove the machinery, not the answer.
  x <- gsub('(?s)"arguments"\\s*:\\s*\\{.*?\\}\\s*\\}?', "", x, perl = TRUE)
  # 3) Collapse the blank lines left behind.
  x <- gsub("(?m)[ \t]+$", "", x, perl = TRUE)
  x <- gsub("\n{3,}", "\n\n", x, perl = TRUE)
  x <- trimws(x)
  if (!nzchar(x)) {
    return("Done. Let me know what you'd like to do next.")
  }
  x
}

#' Execute a single tool call: resolve args (guardrail), dispatch, capture result
#' or a STRUCTURED error (returned, never thrown, so the model can self-correct).
#' @param dispatch function(tool, resolved) -> result record. Defaults to inline
#'   execution; the API layer injects a worker-pool dispatcher for heavy tools.
#' @noRd
cv_run_tool_call <- function(tc, store, reg = cv_registry(), dispatch = NULL) {
  # Round LXIX (audit #23/#24/#25). This function is the single funnel every
  # tool call passes through -- name resolution, argument resolution, the
  # log1p/assay/layer adjustment, then dispatch -- so it is the one place a
  # collector can be created once and drained once. Passed explicitly to each
  # step rather than left ambient: Round XLI removed the last global
  # side-channel in this codebase and did it before the concurrency redesign
  # that would have weaponised it.
  wc <- cv_warnings_new()
  tool <- tryCatch(cv_tool_get(tc$name, reg, warnings = wc), error = function(e) NULL)
  if (is.null(tool)) {
    return(list(ok = FALSE, tool = tc$name,
                error = sprintf("Unknown tool '%s'. It is not in the registry.", tc$name)))
  }
  # Batch 8b: refuse a call whose arguments could not be READ, rather than
  # treating it as a call with no arguments. Without this, cv_resolve_args()
  # below auto-supplies the one required handle and fills every default, so a
  # `clustoCell` whose arguments were truncated mid-stream ran a full
  # multi-minute clustering on parameters the model never chose -- and reported
  # success. Told plainly, the model re-sends; told nothing, it had no idea
  # anything had been lost.
  why <- attr(tc$arguments, "cv_parse_failed", exact = TRUE)
  if (!is.null(why)) {
    return(list(ok = FALSE, tool = tc$name,
                error = sprintf(paste0("I could not read the arguments for '%s': %s. ",
                                       "Send the call again with its arguments as a JSON object."),
                                tc$name, why)))
  }
  resolved <- tryCatch(cv_resolve_args(tool, tc$arguments, store, warnings = wc),
                       error = function(e) structure(conditionMessage(e), class = "cv_arg_error"))
  if (inherits(resolved, "cv_arg_error")) {
    return(list(ok = FALSE, tool = tc$name, error = cv_clean_error(as.character(resolved))))
  }
  # Attach handle_args as attribute (handler contract) and dispatch. The tool
  # spec travels along too so handlers can capture input handles (incl.
  # array-of-handle params) for result-name inheritance.
  call_args <- resolved$args
  attr(call_args, "handle_args") <- resolved$handle_args
  attr(call_args, "cv_tool") <- tool
  # Counts-vs-log + layer adjustment (agent layer, provider/model-agnostic):
  # inspect the resolved input's target layer and, when it looks log-normalized,
  # set log1p=FALSE (unless the model already passed log1p explicitly). Also
  # drop a missing assay/layer back to the tool default so the analysis fn does
  # not hard-error. Runs for BOTH inline and worker dispatch paths.
  call_args <- tryCatch(
    cv_adjust_log1p_layer(tool, call_args, store,
                          model_supplied = names(tc$arguments %||% list()),
                          warnings = wc),
    error = function(e) call_args)
  # Round LXX (audit #12/#13): the tool's declared pre-dispatch validation.
  #
  # Deliberately here rather than inside cv_launch_heavy() or the handler: this
  # is the one place both dispatch paths pass through, so the check cannot be
  # live on one and dead on the other -- the exact failure Round LXIV found in
  # markoCell's handler preamble. And it is BEFORE `runner`, so a call that
  # cannot succeed never spawns a worker to find that out.
  #
  # AFTER cv_adjust_log1p_layer() on purpose: the validator must see the
  # arguments the tool will actually run with, not the ones the model proposed.
  #
  # The abort is converted rather than propagated, matching cv_resolve_args()
  # above -- this function's contract is that a bad call comes back as a
  # structured error the model can read and correct, never as a thrown
  # condition. ONE exception (Round LXXXV): cv_needs_clarification is
  # re-raised rather than converted, in the handler just below -- that
  # condition means the turn cannot proceed without an interactive decision
  # from the user, which a structured error handed back to the MODEL cannot
  # produce.
  # Round LXXVIII (audit #44): announce the scale of a heavy run BEFORE it is
  # dispatched, at the same funnel the validate hooks use. Stated, never gated.
  if (identical(tool$cost, "heavy"))
    tryCatch(.cv_note_heavy_cost(store, call_args, tool, wc), error = function(e) NULL)
  vfail <- tryCatch({
    if (is.function(tool$validate)) tool$validate(store, call_args, tool, wc)
    NULL
  }, error = function(e) {
    # A validator that needs an interactive decision is NOT a bad call: it
    # re-raises here exactly as a pending heavy job does at the dispatch
    # tryCatch a few lines down, so it can unwind past this function to
    # run_tools().
    if (inherits(e, "cv_needs_clarification")) stop(e)
    structure(conditionMessage(e), class = "cv_arg_error")
  })
  if (inherits(vfail, "cv_arg_error")) {
    return(list(ok = FALSE, tool = tc$name, error = cv_clean_error(as.character(vfail))))
  }
  runner <- dispatch %||% function(tool, args, call_id = NULL) tool$handler(store, args)
  # Round XLVI: pass the tool-call id so a NON-BLOCKING dispatcher can key its
  # launched job by it and, on a later tick, re-find that job instead of
  # starting a second one. Older 2-argument dispatchers (the inline default, and
  # the stubs several tests install) are called exactly as before.
  res <- tryCatch(
    if (length(formals(runner)) >= 3L) runner(tool, call_args, tc$id) else runner(tool, call_args),
    error = function(e) {
      # A pending heavy job is NOT a tool failure: re-raise so the step machine
      # can suspend the turn. Swallowing it here would report "tool failed" the
      # instant a background job was launched.
      if (inherits(e, "cv_job_pending")) stop(e)
      structure(conditionMessage(e), class = "cv_run_error")
    })
  if (inherits(res, "cv_run_error")) {
    return(list(ok = FALSE, tool = tc$name, error = cv_clean_error(as.character(res))))
  }
  # Merge the preparation-time decisions with whatever the handler itself
  # raised. cv_result_add_warnings() returns `res` untouched when there is
  # nothing to add, so a clean run carries no `warnings` key at all and every
  # consumer that predates this round sees exactly the payload it saw before.
  #
  # Deliberately AFTER dispatch rather than before: a heavy call that suspends
  # re-raises cv_job_pending above and never reaches here, so a resumed turn
  # collects these once, on the pass that actually produces a result, instead of
  # accumulating a duplicate set per poll. (cv_warnings_merge() de-duplicates
  # anyway; not relying on that is cheaper than explaining it later.)
  res <- cv_result_add_warnings(res, wc)

  # Round LXXV (audit #29): stamp the object with what actually ran. This is the
  # only point in the codebase that holds BOTH the resolved arguments (which the
  # handler received but does not report back) and the handle the handler just
  # created -- the join the audit's "the persisted tool_calls hold the model's
  # partial args" complaint needs. Runs on both dispatch paths because it runs
  # at the funnel, which is the Round LXX lesson reused rather than restated.
  #
  # Best effort by construction (cv_object_set_provenance() swallows its own
  # errors): a provenance record is a nicety and must never fail a call that
  # succeeded.
  # It is stored on the RECORD, not the descriptor -- see
  # cv_object_set_provenance() for why putting it on the descriptor would spend
  # tokens on every turn for something the model cannot use.
  if (!is.null(res$handle))
    cv_object_set_provenance(
      store, res$handle,
      cv_call_provenance(tc$name, call_args, attr(call_args, "handle_args")))

  # Round LXXVII (audit #35): the curated next steps, finally read. Measured
  # before building: NINE tools populate `next_suggestions`, every name is a
  # REAL registered tool (zero typos), and the field is read by nothing --
  # the only other references in R/ are the registry constructor assigning it
  # to itself. Knowledge already written by the person who knows the workflow,
  # and never shown to anyone.
  #
  # Raised at the funnel so both dispatch paths get it, and at INFO because it
  # fires on every successful run of those nine tools -- Round LXIX's rule
  # exactly: a note that appears on ordinary correct work is not a signal.
  # `result_note` is the established precedent for this shape.
  res <- cv_result_add_warnings(res, .cv_next_steps_note(tool, reg))
  list(ok = TRUE, tool = tc$name, result = res)
}

#' Adjust log1p + assay/layer args on a resolved tool call based on the input
#' object's actual data kind.
#'
#' For tools that take a `log1p` arg (clustoCell/markoClust/markoCell/
#' markerPurity): if the target layer of the resolved input looks LOG-normalized
#' (cv_detect_log_transformed) and the model did NOT explicitly pass log1p, set
#' log1p=FALSE and inform the user (the agent layer's "ask before apply" is
#' surfaced as a clear notification; an explicit model/user choice always wins).
#' Also, if the requested assay/layer is absent on a Seurat input, drop it back
#' to the tool default so the analysis function does not hard-error. Returns the
#' (possibly modified) call_args; leaves non-Seurat / no-log1p tools untouched.
#' @param warnings optional collector (cv_warnings_new()). Round LXIX: this
#'   function's own docstring claimed the log1p decision was "surfaced as a
#'   clear notification", and it was not -- it went to cli, i.e. to an R console
#'   the browser user never looks at. Audit #23.
#'
#'   The three decisions here get DIFFERENT severities, and the split is the
#'   substance of the change:
#'     * log1p override -> INFO. It fires on nearly every log-normalized
#'       dataset, which is most of them, and it is the agent doing the right
#'       thing. Marking it as may-invalidate would put an amber card on the
#'       majority of runs, which is precisely how users learn to ignore
#'       warnings -- the failure this whole feature exists to prevent.
#'     * assay / layer substitution -> MAY_INVALIDATE. A different matrix was
#'       analysed than the one asked for. The run succeeds and the numbers are
#'       real; they are numbers about something else.
#' @noRd
cv_adjust_log1p_layer <- function(tool, call_args, store, model_supplied = character(0),
                                  warnings = NULL) {
  spec <- tool$parameters
  has_log1p <- "log1p" %in% names(spec)
  has_layer <- "layer" %in% names(spec)
  has_assay <- "assay" %in% names(spec)
  if (!has_log1p && !has_layer && !has_assay) return(call_args)

  # Find the primary input object (first handle arg that resolves to a Seurat).
  handle_args <- attr(call_args, "handle_args") %||% character(0)
  input <- NULL
  for (nm in handle_args) {
    h <- call_args[[nm]]
    if (is.character(h) && length(h) == 1L && cv_object_exists(store, h)) {
      obj <- tryCatch(cv_object_get(store, h), error = function(e) NULL)
      if (inherits(obj, "Seurat")) { input <- obj; break }
    }
  }
  if (is.null(input)) return(call_args)   # matrix/SCE inputs: no layer probe

  assay <- if (has_assay && !is.null(call_args$assay)) call_args$assay else tryCatch(Seurat::DefaultAssay(input), error = function(e) "RNA")
  layer <- if (has_layer && !is.null(call_args$layer)) call_args$layer else "counts"

  # Layer/assay presence guard: if the requested assay or layer is missing,
  # fall back to the tool default (RNA/counts) rather than erroring downstream.
  assays <- tryCatch(names(input@assays), error = function(e) character(0))
  if (has_assay && length(assays) && !(assay %in% assays)) {
    cli::cli_inform(c(i = "{.val {tool$name}}: assay {.val {assay}} not present (have: {.val {assays}}); using default assay {.val {tryCatch(Seurat::DefaultAssay(input), error=function(e) 'RNA')}}."))
    asked_assay <- assay
    assay <- tryCatch(Seurat::DefaultAssay(input), error = function(e) "RNA")
    cv_warn_add(warnings, "may_invalidate", sprintf(
      paste0("The assay '%s' is not in this object, so '%s' ran on '%s' instead. ",
             "Available assays: %s. If you meant a different one, say which and re-run."),
      asked_assay, tool$name, assay, paste(assays, collapse = ", ")),
      code = "assay_substituted")
    call_args$assay <- assay
  }
  layers <- tryCatch(if (assay %in% assays) SeuratObject::Layers(input[[assay]]) else character(0),
                     error = function(e) character(0))
  if (has_layer && length(layers) && !(layer %in% layers)) {
    cli::cli_inform(c(i = "{.val {tool$name}}: layer {.val {layer}} not present in assay {.val {assay}} (have: {.val {layers}}); using {.val {layers[1]}}."))
    cv_warn_add(warnings, "may_invalidate", sprintf(
      paste0("The layer '%s' is not in assay '%s', so '%s' ran on '%s' instead. ",
             "Available layers: %s. Counts and normalized data give different ",
             "answers, so check this is the one you wanted."),
      layer, assay, tool$name, layers[1], paste(layers, collapse = ", ")),
      code = "layer_substituted")
    layer <- layers[1]
    call_args$layer <- layer
  }

  # log1p: only auto-adjust when the model did NOT explicitly supply it.
  if (has_log1p && !("log1p" %in% model_supplied)) {
    kind <- tryCatch(cv_detect_log_transformed(input, assay = assay, layer = layer),
                     error = function(e) "unknown")
    if (identical(kind, "log") && isTRUE(call_args$log1p %||% TRUE)) {
      call_args$log1p <- FALSE
      cli::cli_inform(c(i = paste0(
        "{.val {tool$name}}: the {.val {layer}} layer of the input looks log-normalized, ",
        "so I set {.arg log1p}=FALSE to avoid a double log-transform. ",
        "(Say so if it is actually raw counts and I'll re-run with log1p=TRUE.)")))
      cv_warn_add(warnings, "info", sprintf(
        paste0("The '%s' layer looks log-normalized, so log1p was set to FALSE to ",
               "avoid a second log-transform. If it is actually raw counts, say so ",
               "and it will re-run with log1p=TRUE."), layer),
        code = "log1p_override")
    }
  }
  call_args
}

#' Maximum number of table rows sent to the model in a tool-result preview.
#'
#' Small result sets (at most this many rows) reach the model IN FULL so its
#' narrative table can reproduce every row — including rank ties that push a
#' "top N" result past N rows (e.g. "top 10 ranked" returning 11 rows). Larger
#' tables are still head-capped at this many rows, but the payload then carries
#' `nrow_total` + `truncated = TRUE` so the model knows rows were omitted and
#' must point the user at the on-screen table / CSV instead of implying the
#' preview is complete.
#' @noRd
.cv_model_table_max_rows <- 25L

#' Turn a tool result (or error) into the compact TEXT the model sees.
#' Keeps it small: the model gets a summary + handle, never the raw object.
#' @noRd
cv_tool_result_for_model <- function(rr) {
  if (!isTRUE(rr$ok)) {
    return(jsonlite::toJSON(list(status = "error", tool = rr$tool,
                                 error = cv_clean_error(rr$error)),
                            auto_unbox = TRUE))
  }
  res <- rr$result
  payload <- list(status = "ok", tool = rr$tool)
  if (!is.null(res$handle))      payload$handle <- res$handle
  if (!is.null(res$descriptor))  payload$object <- res$descriptor
  if (!is.null(res$text))        payload$summary <- res$text

  # Round LXIX (audit #23/#24/#25). The caveats used to be pasted onto the end
  # of `summary`, so the model received "Created X (handle: h). NOTE (tissue may
  # not fit): ... Annotation used the curated CelliVerse Marker DB ..." as one
  # run-on string and had to decide for itself which half mattered.
  #
  # They now travel as a typed list, may_invalidate first, and `status` becomes
  # "ok_with_warnings" when any of them can change the conclusion. That status
  # is what a model actually branches on.
  #
  # THE MODEL IS NOT THE ENFORCEMENT MECHANISM, and this is the Round LXVII
  # lesson applied rather than restated: whether the user SEES a may-invalidate
  # warning does not depend on the model relaying it. The card renders it from
  # the same `result$warnings` on the tool_result event. This payload exists so
  # the model does not write prose that contradicts the card sitting above it.
  # cv_warnings_merge(), not cv_warnings_list(): the ordering guarantee must
  # hold HERE rather than being inherited from whoever built the list. A handler
  # that attaches warnings directly, or a future producer that appends after
  # cv_result_add_warnings() has run, would otherwise hand the model an
  # info-first list and bury the one warning that changes the answer. Found by
  # this round's own test, which built the list in the wrong order on purpose.
  ws <- cv_warnings_merge(res$warnings)
  if (length(ws)) {
    payload$warnings <- lapply(ws, function(w) list(severity = w$severity, text = w$text))
    if (cv_warnings_invalidating(ws)) payload$status <- "ok_with_warnings"
  }

  # Table preview for the model: prefer the FULL raw table when the handler
  # kept one (res$table) so small result sets reach the model with EVERY row —
  # the rendered artifact only carries the first UI page (default 50 rows),
  # which would silently drop rows 51+ of e.g. a 60-row result. Fall back to
  # the artifact's page rows when no raw table is available. Tables larger
  # than the cap are head-capped and explicitly flagged truncated.
  tb <- if (!is.null(res$table) && is.data.frame(res$table)) res$table else NULL
  if (!is.null(res$table_artifact)) {
    ta <- res$table_artifact
    n_total <- ta$nrow %||% (if (!is.null(tb)) nrow(tb) else length(ta$rows))
    n_show  <- min(n_total, .cv_model_table_max_rows)
    rows_src <- if (!is.null(tb)) tb else ta$rows
    payload$table_preview <- list(
      nrow = n_total, ncol = ta$ncol, columns = ta$columns,
      page = ta$page, n_pages = ta$n_pages,
      head = utils::head(rows_src, n_show),
      nrow_total = n_total, n_shown = n_show,
      truncated = n_total > n_show,
      csv_url = ta$csv$url %||% NA
    )
  } else if (!is.null(tb)) {
    n_total <- nrow(tb)
    n_show  <- min(n_total, .cv_model_table_max_rows)
    payload$table_preview <- list(
      ncol = ncol(tb), nrow = n_total,
      columns = colnames(tb), head = utils::head(tb, n_show),
      nrow_total = n_total, n_shown = n_show,
      truncated = n_total > n_show)
  }

  # Plot artifact for the model: just the URLs, never the grob.
  if (!is.null(res$artifact) && identical(res$artifact$kind, "plot") &&
      !is.null(res$artifact$primary)) {
    payload$plot <- list(
      primary_url = res$artifact$primary$url %||% NA,
      formats = vapply(res$artifact$files, function(f) f$format, character(1)))
  } else if (!is.null(res$artifact) && identical(res$artifact$kind, "plot") &&
             !is.null(res$artifact$error)) {
    # Round LXIV (D9): a plot that could not be written to ANY format used to
    # reach here as silence -- cv_render_plot() set artifact$error, and both
    # consumers (this one and Artifacts.tsx) required artifact$primary, so the
    # error was dropped on both sides. The tool still returned status ok with
    # its own summary ("UMAP of N cells colored by ..."), so the model narrated
    # a figure that does not exist. Tell it plainly instead: the analysis may
    # well have succeeded, only the rendering failed, and those are different
    # things the user needs told apart.
    payload$plot <- list(rendered = FALSE, error = res$artifact$error)
  }
  jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", na = "string")
}

# ---- Live-config refresh (hot-swap) -----------------------------------------

#' Config fields that may be hot-swapped onto a live session between turns.
#'
#' A session snapshots its config at creation time. When the user changes
#' Settings mid-session (new provider/model/key/etc.), those writes land in
#' config.json but must also reach an ALREADY-created in-memory session, or the
#' change silently has no effect (this caused the "gemini provider but qwen
#' model" 404). Only LLM-routing / operational fields hot-swap; object store,
#' history and artifacts stay strictly session-scoped and are never swapped.
#'
#' Round XXXIV: a FUNCTION rather than a plain top-level vector, specifically
#' so the provider key fields it includes can be derived from
#' .cv_provider_registry (agent_providers.R) at CALL time. A plain top-level
#' `.cv_hotswap_config_fields <- c(..., .cv_provider_key_fields(), ...)` would
#' evaluate at package-load time, when this file (agent_loop.R) is sourced --
#' and R CMD INSTALL's default (no Collate field) alphabetical file collation
#' sources "agent_loop.R" BEFORE "agent_providers.R", so the registry
#' wouldn't exist yet. Deferring evaluation to call time sidesteps that
#' entirely: every caller below now uses `.cv_hotswap_config_fields()`.
#' @noRd
.cv_hotswap_config_fields <- function() {
  c("default_provider", "default_model", "temperature",
    .cv_provider_key_fields(),
    "ollama_host", "lmstudio_host", "ollama_keep_alive", "ollama_num_ctx",
    "max_tool_iters", "tool_timeout_sec", "history_token_budget",
    "max_repeat_stall")
}

#' Overlay the current on-disk/env config onto a session's snapshot for the
#' hot-swappable fields, returning the effective config to use THIS turn.
#'
#' Re-reads cv_load_config() (defaults + config.json + env overrides) each call
#' so a live Settings change (any provider: ollama/openai/anthropic/gemini/
#' deepseek/groq/openrouter/cerebras) takes effect on the very next turn of an
#' existing session. Non-hot-swappable
#' fields keep their session value.
#' @noRd
cv_effective_config <- function(sess_config) {
  fresh <- tryCatch(cv_load_config(), error = function(e) NULL)
  if (is.null(fresh)) return(sess_config)
  eff <- sess_config
  for (nm in .cv_hotswap_config_fields()) {
    if (!is.null(fresh[[nm]])) eff[[nm]] <- fresh[[nm]]
  }
  eff
}

# ---- Per-turn tool-call ledger (repeat / no-progress guard) -----------------

#' Canonical signature for a tool call = name + arguments with sorted keys.
#'
#' Two calls that request the same tool with the same arguments (regardless of
#' key order) collapse to the same signature, so we can detect exact repeats
#' within a single turn (the runtime backstop for weak models that ignore the
#' "do not repeat" instruction).
#' @noRd
cv_toolcall_signature <- function(tc) {
  args <- tc$arguments %||% list()
  if (length(args) && !is.null(names(args))) args <- args[order(names(args))]
  args_json <- tryCatch(
    jsonlite::toJSON(args, auto_unbox = TRUE, null = "null", na = "string"),
    error = function(e) paste(utils::capture.output(utils::str(args)), collapse = " "))
  paste0(tc$name %||% "<unknown>", "|", args_json)
}

#' Resolve a possibly-miscased / typo'd tool name to its canonical registry name.
#'
#' Weak models emit `clustocell` / `ClustoCell` / small typos. We canonicalize
#' the name ONCE at the top of the per-call loop so the ledger, repeat guard,
#' feedback text and the actual dispatch all agree on a single spelling. Returns
#' the canonical name when it resolves (via cv_tool_get's tolerant matching),
#' else the original name unchanged (so cv_run_tool_call still emits the clean
#' "Unknown tool" error).
#' @noRd
cv_canonical_tool_name <- function(name, reg) {
  tool <- tryCatch(
    suppressMessages(cv_tool_get(name, reg)),   # notes are re-emitted at call time
    error = function(e) NULL)
  if (is.null(tool)) name else tool$name
}

#' Object-producing re-run key = canonical tool name + its INPUT handle set.
#'
#' The exact-signature guard misses the case where a weak model re-runs an
#' object-producing tool (e.g. clustoCell) on the SAME input but the fresh
#' output handle makes the full-arg signature differ. Keying on the tool plus
#' the set of input handle VALUES (sorted) collapses those re-runs so we can
#' refuse them. Returns NULL for tools that do not produce an object or that
#' have no handle arguments (nothing to collapse on).
#' @noRd
cv_toolcall_object_key <- function(tc, tool) {
  if (is.null(tool) || !identical(tool$produces, "object")) return(NULL)
  spec <- tool$parameters
  handle_names <- names(spec)[vapply(spec, function(p) identical(p$type, "handle"),
                                     logical(1))]
  if (!length(handle_names)) return(NULL)
  args <- tc$arguments %||% list()
  vals <- unlist(args[intersect(handle_names, names(args))], use.names = FALSE)
  vals <- sort(as.character(vals[nzchar(as.character(vals))]))
  if (!length(vals)) return(NULL)
  paste0("obj:", tool$name, "|", paste(vals, collapse = ","))
}

#' Does the user's message look like an ACTIONABLE request (vs. chit-chat)?
#'
#' Used to decide whether a no-tool-call model response deserves a one-shot
#' nudge/auto-resolve. Conservative: true only when the text names a registered
#' tool (any casing) OR uses a clear analysis verb. "Hi"/"thanks" stay false so
#' genuine conversational replies pass through untouched.
#' @noRd
cv_looks_actionable <- function(text, reg) {
  t <- tolower(text %||% "")
  if (!nzchar(trimws(t))) return(FALSE)
  # META-QUESTION guard (Round XVIII): a question ABOUT a prior result ("why did
  # you annotate C5 as NK") is conversational, not an action request. Without
  # this the no-tool-call recovery would auto-run a tool and overwrite the
  # model's conversational answer.
  if (cv_is_meta_question(text)) return(FALSE)
  tool_hit <- any(vapply(names(reg), function(n) grepl(tolower(n), t, fixed = TRUE),
                         logical(1)))
  verb_hit <- grepl("\\b(run|cluster|re-?cluster|annotate|marker|markers|plot|umap|heatmap|dotplot|transfer|subcluster|sub-cluster|analy[sz]e|identify|find|show me|compute|call)\\b",
                    t)
  tool_hit || verb_hit
}

#' Best-guess the tool the user named in free text (for auto-resolution).
#'
#' Returns the tool object whose (lowercased) name appears in the message, or
#' whose name is the closest single fuzzy match to a token in the message. NULL
#' when nothing is confidently identifiable (so we fall back to asking).
#' @noRd
cv_intended_tool <- function(text, reg) {
  t <- tolower(text %||% "")
  if (!nzchar(trimws(t))) return(NULL)
  # Direct substring hit on a registered tool name (longest name first so
  # e.g. "clustoCell_TransferLabel" beats "clustoCell").
  nms <- names(reg)[order(nchar(names(reg)), decreasing = TRUE)]
  for (n in nms) if (grepl(tolower(n), t, fixed = TRUE)) return(reg[[n]])
  # Round LXXVIII (audit #40): a curated alias map, tried only after the exact
  # name match above has failed -- so every phrasing that resolved before still
  # resolves to the same tool. It returns NULL unless EXACTLY ONE tool matches:
  # auto-running the wrong analysis is worse than not recovering, so ambiguity
  # fails closed. See .cv_tool_aliases() (agent_nlu_routing.R) for the list and
  # the measurement that motivated it (1 of 17 phrasings resolved).
  hits <- .cv_alias_matches(t, reg)
  if (length(hits) == 1L) return(reg[[hits]])
  NULL
}

#' If a tool's required handle arg has exactly ONE loaded object of the right
#' type, synthesize that argument so a clearly-intended call can proceed without
#' the model. Returns a named list of args (handles as strings) or NULL when the
#' choice is not unambiguous (0 or >1 candidates) — i.e. ask the user instead.
#' Reuses the ask-when-ambiguous policy (cv_objects_of_type / single candidate).
#' @noRd
cv_autoresolve_singleton_args <- function(tool, store) {
  spec <- tool$parameters
  out  <- list()
  for (nm in names(spec)) {
    p <- spec[[nm]]
    if (!identical(p$type, "handle") || !isTRUE(p$required)) next
    typed <- cv_objects_of_type(store, p$handle_types)
    if (length(typed) != 1L) return(NULL)   # ambiguous / none -> ask
    out[[nm]] <- typed[[1]]
  }
  if (length(out)) out else NULL
}

#' Suggest the next ACTION tool(s) the model should call after a read-only /
#' listing tool succeeded, so the repeat feedback can name a concrete next step
#' instead of a bare "stop repeating".
#'
#' WHY: the loop-stall the user hit was a weak local model re-calling
#' `list_objects` over and over because it did not know it could act on the
#' single loaded object directly. When such a read-only tool (produces ==
#' "metadata") is re-called, we look at the loaded objects and the core-tier
#' tools that can consume them and return up to `max` "tool(handle)" hints,
#' most-compatible first. Returns character(0) when nothing actionable is found
#' (caller then falls back to the generic message).
#' @noRd
cv_next_action_hints <- function(store, reg, max = 2L) {
  descs <- tryCatch(cv_object_descriptors(store), error = function(e) list())
  if (!length(descs)) return(character(0))
  loaded_types <- unique(vapply(descs, function(d) d$type %||% NA_character_, character(1)))
  loaded_types <- loaded_types[!is.na(loaded_types)]
  if (!length(loaded_types)) return(character(0))

  # Core-tier, object-consuming tools only (skip the read-only metadata tools
  # themselves and the advanced EWCSR primitives the model rarely calls directly).
  cands <- Filter(function(t) {
    !identical(t$produces, "metadata") &&
      length(t$input_object_types) > 0L &&
      any(t$input_object_types %in% loaded_types)
  }, reg)
  if (!length(cands)) return(character(0))

  # Rank: (1) workflow order — clustoCell is the entry point for a raw
  # Seurat/SCE, so it must outrank downstream plot/annotate tools that merely
  # ACCEPT a Seurat; (2) tools whose input types are ALL currently loaded
  # (runnable now); (3) number of matching input types.
  wf_rank <- vapply(names(cands), function(nm) {
    if (nm == "clustoCell") return(0)
    if (nm %in% c("markoClust", "markoCell", "markerPurity")) return(1)
    if (nm %in% c("addClustoData", "addTypoData", "getDatasetMarkers", "typoClust")) return(2)
    3
  }, numeric(1))
  score <- vapply(cands, function(t) {
    it <- t$input_object_types
    sum(it %in% loaded_types) + 10 * all(it %in% loaded_types)
  }, numeric(1))
  ord <- order(wf_rank, -score)
  cands <- cands[ord]

  # For each candidate, find a compatible loaded handle for its first required
  # handle param so the hint is concrete ("clustoCell(obj_x)").
  out <- character(0)
  for (t in cands) {
    handle <- NA_character_
    spec <- t$parameters
    for (nm in names(spec)) {
      p <- spec[[nm]]
      if (!identical(p$type, "handle") || !isTRUE(p$required)) next
      typed <- tryCatch(cv_objects_of_type(store, p$handle_types), error = function(e) character(0))
      if (length(typed) >= 1L) { handle <- typed[[1]]; break }
    }
    hint <- if (!is.na(handle)) sprintf("%s(%s)", t$name, handle) else t$name
    out <- c(out, hint)
    if (length(out) >= max) break
  }
  unique(out)
}

#' Build an empty per-turn ledger environment.
#' @noRd
cv_ledger_new <- function() {
  new.env(parent = emptyenv())
}

#' Session-scoped ledger registry (an env of per-turn ledgers keyed by turn id).
#'
#' WHY: the dedup ledger must survive a *re-entrant* second invocation of the
#' SAME turn. httpuv is single-threaded, but a heavy tool blocks the turn in a
#' `later::run_now()` wait loop while browser polls (which also pump the loop)
#' can start work; a session-scoped ledger keyed by turn id means a re-entrant
#' duplicate of the same turn sees the first invocation's records and refuses to
#' re-run an object producer, even if the turn lock is somehow bypassed. A
#' genuinely NEW user turn gets a fresh turn id -> fresh ledger, so legitimate
#' sequential turns are unaffected. When no turn id is supplied (sync path /
#' unit tests) we fall back to a private per-call ledger (old behavior).
#' @noRd
cv_ledger_registry <- function(session_id) {
  sess <- cv_session_get(session_id)
  if (is.null(sess$ledgers) || !is.environment(sess$ledgers)) {
    sess$ledgers <- new.env(parent = emptyenv())
    cv_session_set(sess)
  }
  cv_session_get(session_id)$ledgers
}

#' Get (or lazily create) the ledger for a given session + turn id.
#' Falls back to a fresh private ledger when turn_id is NULL.
#' @noRd
cv_ledger_for_turn <- function(session_id, turn_id = NULL) {
  if (is.null(turn_id) || !nzchar(turn_id)) return(cv_ledger_new())
  reg <- cv_ledger_registry(session_id)
  key <- paste0("ledger:", turn_id)
  if (!exists(key, envir = reg, inherits = FALSE)) {
    assign(key, cv_ledger_new(), envir = reg)
  }
  get(key, envir = reg)
}

#' Look up a previously-seen signature in the ledger.
#' Returns NULL if unseen, else a list(status=, error=, handle=, summary=).
#' @noRd
cv_ledger_get <- function(ledger, sig) {
  key <- paste0("sig:", sig)
  if (exists(key, envir = ledger, inherits = FALSE)) get(key, envir = ledger) else NULL
}

#' Record a tool-call outcome under its signature.
#' @noRd
cv_ledger_put <- function(ledger, sig, rr) {
  key <- paste0("sig:", sig)
  rec <- if (isTRUE(rr$ok)) {
    list(status = "ok", handle = rr$result$handle %||% NA_character_,
         summary = rr$result$text %||% NA_character_)
  } else {
    list(status = "error", error = rr$error %||% "")
  }
  assign(key, rec, envir = ledger)
  invisible(rec)
}

#' Forget every FAILED signature in the ledger.
#'
#' Round LXXXI (D1), from live use. THE BUG: a failed tool-call signature was
#' remembered for the WHOLE turn with no invalidation, so an identical call that
#' had since become CORRECT was refused forever, replaying a stale error.
#'
#' The user asked for a UMAP coloured by sub-clusters. `umapPlot` failed --
#' rightly -- because the Seurat object did not carry `ClustoCell_SubClusters`
#' yet. The model then did exactly what the refusal told it to do, and ran
#' `addClustoData`, which SUCCEEDED and wrote the column onto that same object
#' in place. Its retry of the identical `umapPlot` call was then refused with:
#'
#'   "This exact call already FAILED this turn with: `group_by` names
#'    ClustoCell_SubClusters, which is not a metadata column of this object.
#'    Available columns: <the seven from BEFORE addClustoData ran>."
#'
#' The model checked `get_metadata_columns` (the column was there), checked
#' `describe_object` (the column was there), retried (refused again), invented
#' "a backend inconsistency" to explain it, and finally gave up. Every one of
#' those steps was reasonable. The ledger was wrong.
#'
#' WHY CLEAR ALL FAILURES rather than only the ones "about" this object: the
#' error text of the failure that started this does not mention the handle at
#' all -- it names a column and lists the columns that exist. There is no
#' reliable way to tell from a recorded failure which object it concerned, so
#' matching on the handle would have left this exact bug in place. The cost of
#' clearing everything is bounded and small: a genuinely-still-wrong call gets
#' ONE more attempt after each successful state change, fails again, is
#' re-recorded, and is refused again. The stall guard, the terminal-success
#' guard and the iteration cap are all untouched and still bound the turn.
#'
#' SUCCESSES ARE KEPT. `cv_ledger_get()`'s "already succeeded" branch is what
#' stops a model re-running clustoCell four times in one turn; only the
#' `status == "error"` records are dropped.
#' @param ledger a per-turn ledger environment.
#' @return invisibly, the number of failed records forgotten.
#' @noRd
cv_ledger_forget_failures <- function(ledger) {
  if (!is.environment(ledger)) return(invisible(0L))
  keys <- ls(envir = ledger, all.names = TRUE)
  n <- 0L
  for (k in keys) {
    if (!startsWith(k, "sig:")) next
    rec <- tryCatch(get(k, envir = ledger), error = function(e) NULL)
    if (is.list(rec) && identical(rec$status, "error")) {
      rm(list = k, envir = ledger); n <- n + 1L
    }
  }
  invisible(n)
}

#' Did this successful call CHANGE an object the store holds?
#'
#' Round LXXXI (D1). The trigger for forgetting stale failures. A result that
#' carries a handle either created an object or updated one in place
#' (`.cv_result_object_inplace()` returns the SAME handle on purpose), and in
#' both cases the world a previous failure was judged against has moved.
#'
#' A result with no handle -- a plot, a table, `get_metadata_columns` -- changed
#' nothing, and deliberately does NOT clear anything: re-reading the metadata is
#' not a reason to let a genuinely bad call run again.
#' @noRd
cv_call_changed_an_object <- function(rr) {
  isTRUE(rr$ok) && !is.null(rr$result$handle) &&
    is.character(rr$result$handle) && length(rr$result$handle) == 1L &&
    !is.na(rr$result$handle) && nzchar(rr$result$handle)
}

# ---- Terminal-success guard (anti-loop, provider/model-agnostic) ------------
#
# WHY: the loose-loop the user hit was a weak local model re-running an
# object-producing tool (clustoCell) several times in ONE turn, creating
# several ClustoCell handles. The input-handle re-run guard only fires when the
# repeat carries the SAME input handle(s); a model that omits or varies the
# `data` arg dodges it. The terminal-success guard is the stronger, arg-
# invariant rule: once a tool that PRODUCES AN OBJECT returns ok in this turn,
# that tool is DONE for the turn - any later call to the SAME tool (any args)
# is refused with firm feedback. This holds for every provider/model.

#' Record a successful object-producing call as terminal for this turn.
#' @noRd
cv_ledger_mark_terminal <- function(ledger, tool_name, rr) {
  key <- paste0("terminal:", tool_name)
  assign(key, list(handle = rr$result$handle %||% NA_character_,
                   summary = rr$result$text %||% NA_character_),
         envir = ledger)
  invisible(NULL)
}

#' Fetch the terminal-success record for a tool this turn (NULL if none).
#' @noRd
cv_ledger_terminal <- function(ledger, tool_name) {
  key <- paste0("terminal:", tool_name)
  if (exists(key, envir = ledger, inherits = FALSE)) get(key, envir = ledger) else NULL
}


# ---- NLU-routing heuristics + clarification-payload builders ---------------
# Round XXXIII (Batch 3b, item 1): relocated to R/agent_nlu_routing.R and
# R/agent_clarification.R respectively (pure move, no logic changed) — see
# those files' headers and CHANGES.md Round XXXIII for details. The 3
# functions that used to sit here (cv_tissue_condition_payload,
# cv_parse_tissue_condition, cv_parse_n_markers) were confirmed dead code
# (superseded by Round XXI's cv_annotation_options_payload) and removed.


#' Run one full agentic turn for a user message.
#'
#' @param session_id session id.
#' @param user_message the new user text (already validated/non-empty).
#' @param on_event optional callback for streaming events to the client.
#' @param dispatch optional custom tool dispatcher (worker pool). If NULL, tools
#'   run inline in this process.
#' @param stream logical; stream LLM tokens through on_event.
#' @return list(content=<final assistant text>, iterations=, tool_calls=<n>).
#' @noRd
#' @param stepwise when TRUE, do not run the loop here. Return a
#'   `cv_turn_machine` -- a list of `step()` / `finish()` / `exhausted()`
#'   closures over this turn's state -- so the caller can advance the turn ONE
#'   iteration per event-loop tick (Round XLV / Batch B item 4). The synchronous
#'   default is unchanged, and is what every existing caller and test uses.
#'
#'   A turn that finishes during pre-flight (an annotation clarification, say)
#'   never reaches the loop and returns its plain result list even in stepwise
#'   mode, so a caller must check `inherits(x, "cv_turn_machine")` and treat
#'   anything else as an already-completed turn.
#' Run one tool call, timing it and logging its outcome.
#'
#' Round LXXX (audit #69 + #71). `cv_agent_turn()` calls `cv_run_tool_call()`
#' from three places -- the ordinary loop, the no-tool-call recovery's
#' synthesised call, and the stall auto-recovery -- and all three are real
#' production paths. Timing two of them and forgetting the third would produce a
#' `tool_sec` that is quietly wrong on exactly the runs where something already
#' went unusually, which is the worst place to have a wrong measurement.
#'
#' Both the timing and the log line are best-effort and neither can change the
#' result: `cv_log_event()` never throws, and the timer only adds a number.
#' @noRd
.cv_timed_tool_call <- function(tc, store, reg, dispatch, timer, session_id) {
  t0 <- Sys.time()
  rr <- cv_phase_time(timer, "tool", cv_run_tool_call(tc, store, reg, dispatch = dispatch))
  cv_log_event("tool", session = session_id,
               tool = tc$name,
               status = if (isTRUE(rr$ok)) "ok" else "error",
               sec = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 3),
               # The message, not the arguments: a tool call's arguments can
               # contain user gene lists and cluster labels, and this file sits
               # on disk for 30 days. What failed is diagnostic; what it was
               # called with is the user's data.
               error = if (isTRUE(rr$ok)) NULL else as.character(rr$error %||% "")[1])
  rr
}

cv_agent_turn <- function(session_id, user_message, on_event = NULL,
                          dispatch = NULL, stream = TRUE, turn_id = NULL,
                          stepwise = FALSE) {
  # Round LII (Batch 4a): the synchronous path's session-snapshot flush.
  #
  # cv_session_add_message() no longer writes to disk per message; the write is
  # deferred to the end of the turn. On the ASYNC path that end is settle()/
  # fail() in cv_start_turn() (agent_turns.R). This function is the SYNC path
  # (/api/chat/sync, the SSE handler, and most of the test suite), and it has a
  # dozen-plus return points -- pre-flight clarifications, stall exits, the
  # normal ending, and several error paths. on.exit() covers every one of them,
  # including the ones that leave by throwing, which is why it is used here
  # rather than a flush before each `return()`.
  #
  # Gated on !stepwise deliberately: with stepwise = TRUE this function returns
  # a step machine IMMEDIATELY, before any of the turn's messages exist, so an
  # unconditional on.exit would flush an empty turn here and then never again.
  if (!isTRUE(stepwise)) {
    on.exit(tryCatch(cv_session_flush(session_id), error = function(e) NULL),
            add = TRUE)
  }
  sess   <- cv_session_get(session_id)
  # Hot-swap LLM-routing / operational config from the live on-disk config so a
  # Settings change (any provider) made after this session was created takes
  # effect on THIS turn. Persist it back onto the session so /api/session and
  # tool-side LLM calls (option below) see the same effective config.
  config <- cv_effective_config(sess$config)
  sess$config <- config
  cv_session_set(sess)
  store  <- sess$object_store
  reg    <- cv_registry()
  # BATCH1 FIX (rebuilt from scratch): max_tool_iters/max_repeat_stall are
  # hot-swappable config fields (see .cv_hotswap_config_fields) with no floor
  # check anywhere. A config value of 0 (or a negative/non-numeric value)
  # made `seq_len(max_iters)` below never execute, leaving `iter` unbound; the
  # end-of-turn code that references `iter` (iterations = iter) then threw
  # "object 'iter' not found" instead of a clean, actionable message.
  max_iters <- suppressWarnings(as.integer(config$max_tool_iters %||% 12L))
  if (is.na(max_iters) || max_iters < 1L) max_iters <- 12L
  max_stall <- suppressWarnings(as.integer(config$max_repeat_stall %||% 2L))   # consecutive unproductive iters -> stop
  if (is.na(max_stall) || max_stall < 1L) max_stall <- 2L

  # Ledger of executed tool-call signatures (repeat / no-progress + object-run
  # guards). Session+turn-scoped when a turn id is supplied (async server path)
  # so a re-entrant duplicate of the SAME turn shares it; private per-call
  # otherwise (sync path / tests). A new user turn always gets a fresh ledger.
  ledger <- cv_ledger_for_turn(session_id, turn_id)
  # Round LXXX (audit #68/#69/#71): the turn's own measurements. Created here,
  # at the ONE place a turn begins, so the stepwise (async server) driver and
  # the synchronous driver share exactly one timer and one accumulator rather
  # than each keeping their own -- the light/heavy shape of drift, applied to
  # measurement instead of behaviour.
  timer <- cv_phase_timer_new()
  usage_acc <- cv_usage_acc_new()
  stall  <- 0L          # consecutive iterations with no NEW successful tool call
  stall_reason <- NULL  # human-readable blocker for an honest early-exit message

  # Round LV (Batch 5a): the three repeat guards below (same-tool, same-input-
  # handle, and same-signature) each refused a duplicate tool call with an
  # ELEVEN-LINE block that was byte-identical across all three apart from the
  # ledger record it read and the wording of the stall reason. Verified
  # identical by normalising the record variable and the reason string and
  # comparing — not assumed from how they looked.
  #
  # They had not drifted yet. Batch 3a found the copy that HAD drifted, and
  # collapsing three exact copies now is what stops a fourth divergence: a
  # future change to the refusal payload (an extra event field, say) currently
  # has to be made three times, and missing one produces a guard that silently
  # emits a differently-shaped result.
  #
  # `stall_reason <<- ` from here reaches cv_agent_turn()'s own binding, not a
  # global: this closure's enclosing environment IS this frame, which is where
  # `<<-` starts looking. (Checked, not assumed.) `next` deliberately stays at
  # each call site — it belongs to the caller's loop and cannot be moved in.
  refuse_repeat_call <- function(tc, rec, msg, reason) {
    rr_fb <- list(ok = FALSE, tool = tc$name, error = msg)
    cv_emit(on_event, "tool_result", tool = tc$name,
            handle = rec$handle, summary = rec$summary,
            result = list(handle = rec$handle, text = rec$summary,
                          repeated = TRUE))
    cv_session_add_message(session_id, "tool",
                           content = cv_tool_result_for_model(rr_fb),
                           tool_call_id = tc$id, name = tc$name)
    if (is.null(stall_reason)) stall_reason <<- sprintf(reason, tc$name)
    invisible(NULL)
  }
  nudged <- FALSE       # whether we already re-prompted once for a missing tool call

  # Expose this turn's effective config to tools that make their own LLM calls
  # (annotateCellsLLM / ceLLMarkup) so they inherit the same provider, model,
  # keys and temperature without re-plumbing.
  #
  # Round XLI (Batch 3b item 5): this used to be a PROCESS-GLOBAL R option
  # (`options(celliverse.current_config = config)`, restored via on.exit()).
  # It is now recorded on the SESSION -- where per-session state belongs, and
  # which the object store can already resolve via attr(store,
  # "cv_session_id"). See cv_current_config() (agent_tools_cellmarkup.R) for
  # the full rationale: the global was only ever safe because a turn currently
  # runs start-to-finish without yielding, and the items 4/7 step-machine
  # redesign deliberately removes exactly that property -- at which point one
  # session's provider/model/API key becomes readable by another session's
  # tool, silently and depending on request interleaving.
  #
  # No on.exit() restore, deliberately rather than by omission: the value is
  # now scoped to the session that owns it, so a later turn on the SAME session
  # overwrites it with its own effective config and no other session can
  # observe it at all. There is no longer any global state to restore.
  local({
    s <- tryCatch(cv_session_get(session_id), error = function(e) NULL)
    if (!is.null(s)) { s$effective_config <- config; cv_session_set(s) }
  })

  # Record the user's message in durable history.
  cv_session_add_message(session_id, "user", user_message)

  # ---- Annotation-method pre-flight (always ask first) ----------------------
  # If the user asked to annotate/label a cluster/set but did NOT name a method,
  # do NOT let the model pick one (a weak model auto-routes to annotateCellsLLM).
  # Emit a method-choice clarification with clickable Marker DB / LLM buttons and
  # end the turn BEFORE any LLM call. Clicking a button auto-sends a resume
  # message that names the method, so the next turn routes directly.
  if (cv_is_unspecified_annotation(user_message)) {
    payload <- cv_method_clarification_payload(user_message)
    final_text <- payload$text
    cv_session_add_message(session_id, "assistant", final_text)
    cv_emit(on_event, "clarification", text = payload$text,
            tool = payload$tool, choices = payload$choices,
            kind = payload$kind)
    return(list(content = final_text, iterations = 0L, tool_calls = 0L,
                clarification = "method_choice"))
  }

  # ---- Annotation-options pre-flight (BOTH methods) -------------------------
  # The user picked a method (chip or message) but has NOT yet given a
  # tissue/condition/n. Ask via the unified picker (Tissue + Condition dropdowns,
  # each with an "All (no filter)" option, PLUS a numeric "top markers (n)"
  # field, default 20) and end the turn BEFORE any LLM call. Pressing Continue
  # sends a resume message carrying tissue=/condition=/n=, which routes directly
  # to typoClust (MarkerDB) or annotateCellsLLM (LLM). When the user supplied
  # their own marker list, the n field is hidden (n fixed to the list length).
  .emit_options <- function(payload) {
    final_text <- payload$text
    cv_session_add_message(session_id, "assistant", final_text)
    cv_emit(on_event, "clarification", text = payload$text,
            tool = payload$tool, choices = payload$choices,
            dropdowns = payload$dropdowns,
            inputs = payload$inputs, note = payload$note,
            resume_template = payload$resume_template,
            base_request = payload$base_request,
            kind = payload$kind)
    return(list(content = final_text, iterations = 0L, tool_calls = 0L,
                clarification = payload$kind))
  }
  # ---- Species pre-flight, then the tissue/condition/n picker ---------------
  # Round LXIV (Batch 1b). Species is asked FIRST and on its own, because the
  # Tissue and Condition vocabularies are looked up per species
  # (tissueCondition_types[[species]]). Asking both on one card would offer
  # human tissues to someone annotating a mouse dataset, and the mismatch would
  # only surface as an abort once the run had already started.
  #
  # markerDB gets two chips (the Marker DB holds human and mouse, and typoClust
  # aborts for a third); the LLM path gets a free-text field pre-filled `human`,
  # because ceLLMarkup passes the string into the prompt and is not restricted
  # to a dictionary. Blank means human.
  .annotation_preflight <- function(method) {
    sp <- cv_extract_species(user_message)
    if (is.null(sp))
      return(.emit_options(cv_species_clarification_payload(user_message, method = method)))
    .emit_options(cv_annotation_options_payload(user_message, method = method, species = sp))
  }
  # Round LXVII: an out-of-scope request is answered HERE, deterministically,
  # before any LLM call -- not left to a prompt rule. Reported from live use:
  # "run differential expression between C1 and C2" returned the generic
  # "I wasn't able to turn that into an action" line, because the request looks
  # actionable, no tool ran, and `recovery_exhausted` then OVERWROTE the model's
  # answer with that canned text. A prompt rule cannot win against a branch that
  # discards its output, so the server declines instead of asking the model to.
  oos <- cv_out_of_scope_request(user_message)
  if (!is.null(oos)) {
    final_text <- cv_out_of_scope_text(oos)
    cv_session_add_message(session_id, "assistant", final_text)
    cv_emit(on_event, "assistant", text = final_text)
    return(list(content = final_text, iterations = 0L, tool_calls = 0L,
                out_of_scope = oos$topic))
  }
  if (cv_is_markerdb_annotation(user_message)) return(.annotation_preflight("markerdb"))
  if (cv_is_llm_annotation(user_message))      return(.annotation_preflight("llm"))

  tool_specs <- cv_tools_specs(reg)      # core-tier tools only
  total_tool_calls <- 0L
  total_successful_calls <- 0L   # NEW (non-repeat) successful calls this turn
  # Successful calls of tools that actually ADVANCE an analysis (anything but
  # read-only "metadata" listings such as list_objects / describe_object).
  # Used by the conversational stall exit below: a greeting turn where the
  # model only ever listed objects must not end in a "not making progress"
  # clarification, while a genuine analysis loop (a real tool succeeded, then
  # the model spun) still must.
  total_productive_calls <- 0L
  final_text <- NULL
  # ---- Round XLVI (Batch B item 7): suspend/resume state ---------------------
  # A heavy tool no longer blocks; the dispatcher raises `cv_job_pending` and
  # the iteration unwinds. These remember exactly where it was, so the next tick
  # resumes at the same tool call instead of re-asking the model.
  pending_resp     <- NULL   # the LLM response whose tool calls are mid-flight
  pending_tc_index <- 1L     # cursor into resp$tool_calls
  new_success_this_iter <- 0L
  # Monotonic high-water marks so a RETRIED tool call is not counted twice and
  # does not emit a second tool_start. Both reset when a new response arrives.
  entered_upto <- 0L
  emitted_upto <- 0L

  # ---- Round XLV (Batch B, item 4): ONE iteration, as a callable step --------
  #
  # This used to be `for (iter in seq_len(max_iters))`. The body is UNCHANGED
  # apart from three mechanical edits: assignments to turn-scope variables use
  # `<<-` (they now cross a function boundary), and the OUTER loop's `next` /
  # `break` became `return("continue")` / `return("done")`. Every inner
  # `for (tc in resp$tool_calls)` `next` is untouched.
  #
  # WHY: a turn ran start-to-finish inside a single `later` callback, so the
  # poll request that happened to start it could not return until the whole turn
  # had finished -- measured at 5,900 ms for a 3-iteration turn and 26,420 ms
  # with a real heavy tool (test-round44-poll-latency-http-safe.R). Splitting the
  # loop into callable steps lets the async runner do one step, hand the thread
  # back to httpuv, and resume on the next tick.
  #
  # The closure keeps the state; nothing is copied into a record. `step()`
  # returns "continue" (call me again), "waiting" (a heavy job is still running:
  # call me again, but this did NOT consume an iteration) or "done" (the turn
  # produced its answer).
  iter <- 0L
  step <- function() {
    # Round XLVI: a turn suspended by a heavy job resumes in the MIDDLE of an
    # iteration. Falling through to the model here would re-ask it the question
    # it has already answered, and burn an iteration doing so.
    if (!is.null(pending_resp)) return(run_tools())
    iter <<- iter + 1L
    # Cooperative cancel checkpoint (Round XXIV): the Stop button sets the turn's
    # cancel flag via cv_turn_cancel(), but the blocking cv_chat() below never
    # emits an event, so without an explicit check here the flag would only be
    # seen from inside on_event (i.e. between tool steps) and a long LLM call
    # would run to completion. Check at the top of every iteration and right
    # after each cv_chat() returns so Stop takes effect at the next checkpoint.
    .cv_turn_check_cancelled(session_id, turn_id, on_event)
    cv_emit(on_event, "iteration", n = iter)

    # (Re)build the system prompt each iteration so newly created objects show up.
    sess    <<- cv_session_get(session_id)     # refresh (store mutated by tools)
    store   <<- sess$object_store
    sys_msg <- list(role = "system", content = cv_system_prompt(store, config))
    llm_hist <- cv_history_to_llm(sess$history)
    messages <- cv_budget_history(sys_msg, llm_hist,
                                  max_tokens = config$history_token_budget %||% 12000L)
    # Small local models lose the system prompt's operating rules once history
    # grows (and Ollama's default context can truncate it). Restate the three
    # rules they break most as a short trailing reminder each iteration.
    if (identical(config$default_provider, "ollama")) {
      messages[[length(messages) + 1L]] <- list(role = "user", content = paste0(
        "SYSTEM REMINDER: act now by emitting exactly ONE tool call. ",
        "Never re-run a tool that already succeeded this turn, and do not ",
        "re-list objects you already know. If every requested action is done, ",
        "reply in plain text with no tool call."))
    }

    token_cb <- if (isTRUE(stream)) function(ev) {
      if (identical(ev$type, "token")) cv_emit(on_event, "token", text = ev$text)
    } else NULL

    # Round LXXX (audit #69): the model half of the turn's wall clock, measured
    # rather than reconstructed afterwards from a bespoke harness.
    resp <- cv_phase_time(timer, "llm",
      cv_chat(messages, provider = config$default_provider,
              model = config$default_model, tools = tool_specs,
              temperature = config$temperature %||% 0.2,
              stream = isTRUE(stream), on_delta = token_cb, config = config))
    # Round LXXX (audit #68): every adapter has parsed `usage` since it was
    # written and nothing read it. A turn is several round-trips, so the number
    # worth having is the SUM across them.
    cv_usage_acc_add(usage_acc, resp$usage)

    # Checkpoint again right after the (blocking) LLM call returns, so a Stop
    # pressed during the call is honoured before we act on the response.
    .cv_turn_check_cancelled(session_id, turn_id, on_event)

    if (!cv_response_has_tools(resp)) {
      final_text <<- cv_clean_assistant_text(resp$content %||% "")

      # ---- No-tool-call recovery (weak-model hardening) -------------------
      # The model replied with plain text instead of emitting a tool call. If
      # this turn looks like an actionable request and no tool has run yet, do a
      # ONE-SHOT recovery before giving up: (a) re-prompt once with a firm
      # instruction to emit the tool call; then (b) if it still won't, and the
      # clearly-intended tool has exactly one loaded object of the required
      # type, run that call ourselves (ask-when-ambiguous stays in force).
      actionable <- total_tool_calls == 0L && cv_looks_actionable(user_message, reg)
      if (actionable && !nudged) {
        nudged <<- TRUE
        cv_session_add_message(session_id, "user", paste0(
          "SYSTEM: Do not answer in prose. To act you MUST emit a tool call ",
          "(structured function call) for the matching tool now. If the user ",
          "said 'data', 'the seurat object', 'my object' or similar, that refers ",
          "to the loaded object shown in the system message - use its handle. ",
          "Emit the tool call for the requested action."))
        return("continue")   # retry the loop with the nudge in history
      }
      if (actionable && nudged) {
        itool <- cv_intended_tool(user_message, reg)
        aargs <- if (!is.null(itool)) cv_autoresolve_singleton_args(itool, store) else NULL
        if (!is.null(itool) && !is.null(aargs)) {
          synth <- list(id = cv_new_id("call"), name = itool$name, arguments = aargs)
          cv_emit(on_event, "tool_start", tool = synth$name, arguments = synth$arguments)
          rr <- .cv_timed_tool_call(synth, store, reg, dispatch, timer, session_id)
          # Record this auto-run in the SAME per-turn ledger the model-driven
          # dispatch path uses (see below, ~line 1554) BEFORE looping back to
          # the model. Without this, a duplicate call the model issues on a
          # LATER iteration of this same turn (this branch loops via `next`,
          # not `break`, so the model gets another turn) is invisible to the
          # terminal-success / object-rerun guards -- they only know about
          # calls that went through the ledger -- and can run the same
          # object-producing tool a second time, silently creating a
          # duplicate object. Confirmed via live reproduction: a stalled
          # "cluster my data" request produced TWO FakeClusto objects from
          # ONE user turn before this fix.
          sig <- cv_toolcall_signature(synth)
          obj_key <- cv_toolcall_object_key(synth, itool)
          # Round LXXXI (D1): a successful call that CHANGED an object
          # invalidates every remembered failure. Same fix as the ordinary loop
          # below; this is the no-tool-call recovery path, and a fix applied to
          # only one of the two is the light/heavy dispatch drift this codebase
          # has been bitten by four times. See cv_ledger_forget_failures().
          if (cv_call_changed_an_object(rr)) cv_ledger_forget_failures(ledger)
          cv_ledger_put(ledger, sig, rr)
          if (!is.null(obj_key)) cv_ledger_put(ledger, obj_key, rr)
          if (isTRUE(rr$ok) && identical(itool$produces, "object"))
            cv_ledger_mark_terminal(ledger, synth$name, rr)
          if (isTRUE(rr$ok)) {
            rr$result <- tryCatch(
              cv_render_result(rr$result, sess$artifacts_dir, session_id = session_id,
                               basename = paste0(synth$name, "_", cv_new_id("art"))),
              error = function(e) rr$result)
            cv_emit(on_event, "tool_result", tool = synth$name,
                    handle = rr$result$handle %||% NA, summary = rr$result$text %||% NA,
                    result = cv_result_for_browser(rr$result))
            total_tool_calls <<- total_tool_calls + 1L
            # Round LXIV (D8): this branch is reached only under isTRUE(rr$ok),
            # so the call genuinely SUCCEEDED and must be counted as one. It was
            # not, which was harmless while total_successful_calls had no reader
            # -- but the blank-turn branch below now gates on it, and without
            # this line an auto-resolved run that worked would be reported to
            # the user as "That run did not finish". Counted here for the same
            # reason it is counted on the model-driven path; total_productive_
            # calls is deliberately NOT touched, so stall behaviour is unchanged.
            total_successful_calls <<- total_successful_calls + 1L
            cv_session_add_message(session_id, "assistant", NULL,
                                   tool_calls = list(synth))
            cv_session_add_message(session_id, "tool",
                                   content = cv_tool_result_for_model(rr),
                                   tool_call_id = synth$id, name = synth$name)
            store <<- cv_session_get(session_id)$object_store
            return("continue")   # let the model narrate / continue from the real result
          } else {
            cv_emit(on_event, "tool_error", tool = synth$name, error = cv_clean_error(rr$error))
          }
        }
      }

      # Defense-in-depth: never show a blank turn, and never let a weak model's
      # unhelpful non-answer stand for an ACTIONABLE request it failed to act on.
      #   - acted but said nothing        -> confirm the tool finished.
      #   - actionable, recovery exhausted (nudged once, still no tool ran, and
      #     auto-resolution couldn't pick a single object) -> the honest restate
      #     message with a concrete example (this replaces the "I couldn't turn
      #     that into an action" style prose the user complained about).
      #   - otherwise (a genuine conversational reply) -> keep the model's text.
      recovery_exhausted <- actionable && nudged && total_tool_calls == 0L
      # Round LXIV (D8): gate on SUCCESSFUL calls, not attempted ones.
      # total_tool_calls increments on ENTERING a call and is never decremented
      # when that call fails, so the branch below used to fire for a turn whose
      # only tool call errored -- printing "Done - the tool finished" directly
      # above a card reading "didn't finish". Exactly the weak-local-model case
      # this branch exists to serve is the case it was getting wrong.
      # A turn that tried and failed gets an honest line instead: the card above
      # already carries the specific error, so this must not restate or invent
      # one, only stop claiming success.
      if (total_successful_calls > 0L && !nzchar(trimws(final_text))) {
        final_text <<- "Done - the tool finished and its result is shown above."
      } else if (total_tool_calls > 0L && !nzchar(trimws(final_text))) {
        final_text <<- paste0(
          "That run did not finish. The details are on the card above - ",
          "adjust what you asked for and send it again.")
      } else if (recovery_exhausted || (!nzchar(trimws(final_text)) && total_tool_calls == 0L)) {
        final_text <<- paste0(
          "I wasn't able to turn that into an action just now. Please restate ",
          "what you'd like me to do (for example, \"run clustoCell on ",
          "<handle>\"), and I'll run the matching tool.")
      }
      cv_session_add_message(session_id, "assistant", final_text)
      cv_emit(on_event, "assistant", text = final_text)
      return("done")
    }

    # The model wants tools. Record the assistant tool-call message first.
    cv_session_add_message(session_id, "assistant", resp$content, tool_calls = resp$tool_calls)

    pending_resp     <<- resp
    pending_tc_index <<- 1L
    new_success_this_iter <<- 0L
    entered_upto <<- 0L
    emitted_upto <<- 0L
    run_tools()
  }

  # ---- Round XLVI: the tool-dispatch half of an iteration --------------------
  #
  # Split out of step() so a turn suspended by a heavy job RESUMES HERE rather
  # than re-asking the model. Everything it needs is turn-scope state, so the
  # unwind costs nothing: `pending_resp` holds the response being executed and
  # `pending_tc_index` the cursor into its tool calls.
  #
  # Returns "waiting" when a heavy job is still running -- the driver then
  # re-schedules without advancing the iteration count.
  run_tools <- function() {
    tryCatch(
      run_tools_inner(),
      cv_job_pending = function(cond) "waiting",
      # Round LXXXV: a validate() hook decided this call cannot proceed
      # without an interactive decision from the user (a heavy dispatch too
      # big for this machine -- cv_heavy_dispatch_route(), agent_bigdata.R).
      # Ends the turn exactly like the mid-turn "stall" clarification below:
      # the SAME "clarification" event shape, the SAME "done" sentinel both
      # drivers already route to finish() on.
      cv_needs_clarification = function(cond) {
        payload <- cond$payload
        final_text <<- payload$text
        cv_session_add_message(session_id, "assistant", final_text)
        cv_emit(on_event, "clarification", text = payload$text,
                tool = payload$tool, choices = payload$choices,
                dropdowns = payload$dropdowns, inputs = payload$inputs,
                note = payload$note, resume_template = payload$resume_template,
                base_request = payload$base_request, kind = payload$kind)
        "done"
      })
  }

  run_tools_inner <- function() {
    resp <- pending_resp

    # Execute each requested tool call in order. Indexed rather than
    # `for (tc in resp$tool_calls)` so a resumed iteration can skip the calls
    # that already completed and retry only the one that suspended.
    for (.k in seq_along(resp$tool_calls)) {
      if (.k < pending_tc_index) next        # completed on an earlier tick
      pending_tc_index <<- .k
      tc <- resp$tool_calls[[.k]]
      # Count once per call, not once per retry.
      if (.k > entered_upto) {
        total_tool_calls <<- total_tool_calls + 1L
        entered_upto <<- .k
      }

      # Canonicalize the tool name up front (tolerant of casing / small typos)
      # so the ledger, guards, feedback and dispatch all agree on one spelling.
      tc$name <- cv_canonical_tool_name(tc$name, reg)
      tool_obj <- tryCatch(cv_tool_get(tc$name, reg), error = function(e) NULL)

      sig  <- cv_toolcall_signature(tc)
      seen <- cv_ledger_get(ledger, sig)

      # ---- Terminal-success guard (strongest anti-loop layer) ---------------
      # An object-producing tool that already SUCCEEDED this turn is DONE for
      # the turn: refuse any later call to the SAME tool regardless of argument
      # jitter (missing/different handle). This is what collapses the 3x
      # clustoCell loop the user hit - the input-handle guard below only fires
      # when the repeat carries the SAME input handle, which a weak model can
      # dodge by omitting or varying the arg. Provider/model-agnostic.
      if (!is.null(tool_obj) && identical(tool_obj$produces, "object")) {
        term <- cv_ledger_terminal(ledger, tc$name)
        if (!is.null(term)) {
          msg <- paste0(
            "'", tc$name, "' already completed successfully this turn",
            if (!is.na(term$handle) && nzchar(term$handle))
              paste0(" and produced object ", term$handle) else "",
            ". Do NOT run it again - use that result, take the next step in the ",
            "workflow, or give your final answer. If the USER asked for it to be ",
            "re-run (different settings, a fresh seed), say so in plain text and ",
            "tell them to ask again in a NEW message - the guard is per-turn and ",
            "the next message starts a new one.")
          refuse_repeat_call(tc, term, msg, "re-running '%s' after it already succeeded this turn")
          next
        }
      }

      # ---- Object re-run guard: an object-producing tool (e.g. clustoCell) that
      # already SUCCEEDED on the SAME input handle(s) this turn must not run
      # again, even if a fresh output handle would make the full-arg signature
      # differ. This is the loose-loop case the user hit (clustoCell re-ran after
      # finishing). Keyed on tool + input handle set.
      obj_key <- cv_toolcall_object_key(tc, tool_obj)
      if (is.null(seen) && !is.null(obj_key)) {
        prev <- cv_ledger_get(ledger, obj_key)
        if (!is.null(prev) && identical(prev$status, "ok")) {
          msg <- paste0(
            "'", tc$name, "' already ran successfully on this input this turn",
            if (!is.na(prev$handle) && nzchar(prev$handle))
              paste0(" and produced object ", prev$handle) else "",
            ". Do NOT run it again on the same object - use that result, take the ",
            "next step in the workflow, or give your final answer.")
          refuse_repeat_call(tc, prev, msg, "re-running '%s' on an object it already processed")
          next
        }
      }

      # ---- Repeat guard: this exact (tool + args) call already ran this turn.
      if (!is.null(seen)) {
        if (identical(seen$status, "ok")) {
          # Bug-1 collapse: an identical call already SUCCEEDED. Do not re-run;
          # tell the model the result is already available so it moves on.
          msg <- paste0(
            "This exact call already completed successfully this turn",
            if (!is.na(seen$handle) && nzchar(seen$handle))
              paste0(" (result handle: ", seen$handle, ")") else "",
            ". Do NOT call it again with the same arguments - use the existing ",
            "result, take the next step in the workflow, or give your final answer.")
          # Loop-stall hardening: when a READ-ONLY / listing tool (produces ==
          # "metadata", e.g. list_objects / describe_object) is the one being
          # repeated, the model is stuck "checking" instead of acting. Append a
          # concrete NEXT ACTION naming the loaded handle(s) and the analysis
          # tool(s) that can consume them, so the model knows exactly what to
          # call next rather than listing a third time.
          if (!is.null(tool_obj) && identical(tool_obj$produces, "metadata")) {
            hints <- tryCatch(cv_next_action_hints(store, reg), error = function(e) character(0))
            if (length(hints)) {
              msg <- paste0(
                msg,
                " You already listed the loaded object(s), so STOP inspecting and ACT now: ",
                "call the analysis tool the user asked for on the loaded handle - e.g. ",
                paste(hints, collapse = " or "),
                ". Do NOT call '", tc$name, "' again.")
            }
          }
          refuse_repeat_call(tc, seen, msg, "repeatedly re-running the same successful '%s' call")
          next
        } else {
          # Bug-2 collapse: an identical call already FAILED the same way. Do not
          # re-run; feed back a firm, actionable message instead of looping.
          msg <- paste0(
            "This exact call already FAILED this turn with: ", seen$error,
            ". Do NOT repeat it unchanged. Either change the arguments (e.g. pass ",
            "a valid handle of the required type from the loaded-objects list), run ",
            "the missing prerequisite first, or tell the user what you need.")
          rr_fb <- list(ok = FALSE, tool = tc$name, error = msg)
          cv_emit(on_event, "tool_error", tool = tc$name, error = msg)
          cv_session_add_message(session_id, "tool",
                                 content = cv_tool_result_for_model(rr_fb),
                                 tool_call_id = tc$id, name = tc$name)
          if (is.null(stall_reason))
            stall_reason <<- sprintf("repeatedly calling '%s' with arguments that fail: %s",
                                    tc$name, seen$error)
          next
        }
      }

      # ---- Not seen before: run it for real.
      # On a RESUMED call the job is already running and the client has already
      # been told about it; a second tool_start would show the tool twice.
      if (.k > emitted_upto) {
        cv_emit(on_event, "tool_start", tool = tc$name, arguments = tc$arguments)
        emitted_upto <<- .k
      }
      # Round LXXX (audit #69/#71): the tool half of the wall clock, and one log
      # line per call. Wrapped rather than inlined at each of the three
      # cv_run_tool_call() sites in this file, so the measurement cannot be live
      # on one path and missing on another.
      rr <- .cv_timed_tool_call(tc, store, reg, dispatch, timer, session_id)
      if (isTRUE(rr$ok)) {
        # Render any plot/table result into session artifacts (SVG+PNG / CSV+JSON)
        # and attach compact descriptors (paths + browser URLs) for API/LLM.
        rr$result <- tryCatch(
          cv_render_result(rr$result, sess$artifacts_dir, session_id = session_id,
                           basename = paste0(tc$name, "_", cv_new_id("art"))),
          error = function(e) { cli::cli_warn("Rendering failed: {conditionMessage(e)}"); rr$result })
        cv_emit(on_event, "tool_result", tool = tc$name,
                handle = rr$result$handle %||% NA, summary = rr$result$text %||% NA,
                result = cv_result_for_browser(rr$result))
        new_success_this_iter <<- new_success_this_iter + 1L
        total_successful_calls <<- total_successful_calls + 1L
        if (is.null(tool_obj) || !identical(tool_obj$produces, "metadata"))
          total_productive_calls <<- total_productive_calls + 1L
      } else {
        cv_emit(on_event, "tool_error", tool = tc$name, error = cv_clean_error(rr$error))
        if (is.null(stall_reason))
          stall_reason <<- sprintf("the '%s' call keeps failing: %s", tc$name, rr$error %||% "")
      }
      # Round LXXXI (D1): a successful call that CHANGED an object invalidates
      # every remembered failure, because the world those failures were judged
      # against has moved. See cv_ledger_forget_failures() for the live defect.
      #
      # Written BEFORE the put, though today it makes no difference: the clear
      # only fires when rr$ok is TRUE, and cv_ledger_put() then records status
      # "ok", which cv_ledger_forget_failures() never removes. (An earlier
      # comment here claimed the order was load-bearing. It is not, and saying
      # so would have misled the next reader into thinking a real invariant was
      # being maintained.) Kept in this order so that WIDENING the trigger later
      # cannot make a call erase its own record.
      if (cv_call_changed_an_object(rr)) cv_ledger_forget_failures(ledger)
      cv_ledger_put(ledger, sig, rr)
      # Also record under the object-producing key so a later re-run on the SAME
      # input (with a different fresh output handle) is caught by the guard above.
      if (!is.null(obj_key)) cv_ledger_put(ledger, obj_key, rr)
      # Mark a successful object-producing call as TERMINAL for this turn so the
      # guard above refuses any later re-run of the same tool (any args).
      if (isTRUE(rr$ok) && !is.null(tool_obj) && identical(tool_obj$produces, "object"))
        cv_ledger_mark_terminal(ledger, tc$name, rr)
      # Feed a compact tool message back to the model (keyed to the call id).
      cv_session_add_message(session_id, "tool",
                             content = cv_tool_result_for_model(rr),
                             tool_call_id = tc$id, name = tc$name)
      # Refresh store handle (object_store env is mutated in place, but re-get to be safe).
      store <<- cv_session_get(session_id)$object_store
    }

    # Every tool call in this response is done: the iteration is complete.
    pending_resp     <<- NULL
    pending_tc_index <<- 1L

    # ---- No-progress stall breaker ----------------------------------------
    # Count iterations that produced ZERO new successful tool calls (only exact
    # repeats and/or errors). After `max_stall` consecutive unproductive iters,
    # stop early with an honest, actionable message rather than spinning to the
    # max-iters cap (this bounds near-identical repeats that dodge exact-sig
    # matching, e.g. the model tweaking a bad handle each time).
    if (new_success_this_iter == 0L) {
      stall <<- stall + 1L
    } else {
      stall <<- 0L
      stall_reason <<- NULL
    }
    # Non-actionable turns (greetings, casual chat, general questions) must
    # NEVER end in a "not making progress" clarification: if the model wasted
    # iterations on unnecessary read-only calls, that is our prompt's fault,
    # not the user's. Break out with a plain conversational reply instead.
    # Only applies when nothing PRODUCTIVE happened this turn (no successful
    # non-metadata tool call) — read-only listings like list_objects do not
    # count. If a real analysis tool already succeeded, the stall is a genuine
    # loop and must still surface the honest "not making progress" message.
    if (stall >= max_stall && total_productive_calls == 0L &&
        !cv_looks_actionable(user_message, reg)) {
      # Round LXXX (audit #86): the exclamation mark is gone. This is the one
      # string that most defines the persona -- it is what a user gets when the
      # model stalls on a greeting -- and it broke the no-exclamation-marks rule
      # in the same release that added the rule. It also now names what the
      # agent can actually do in enough detail to be an answer rather than a
      # category (audit #60's complaint, in the R-authored half).
      final_text <<- paste0(
        "I'm the CelliVerse Agent. I run single-cell analyses on data you load ",
        "here: clustering with clustoCell, marker discovery and marker-purity ",
        "scoring, cell-type annotation from the curated CelliVerse Marker DB or ",
        "an LLM, and the figures and tables that go with them. Load a Seurat or ",
        "SingleCellExperiment object under Data / Upload, or tell me what you'd ",
        "like to do, and I'll take it from there.")
      cv_session_add_message(session_id, "assistant", final_text)
      cv_emit(on_event, "assistant", text = final_text)
      return("done")
    }
    if (stall >= max_stall) {
      # ---- Auto-recovery before asking the user ---------------------------
      # The model stalled (only repeats/errors for `max_stall` iterations). If
      # the user's message clearly names a registered tool AND every required
      # handle arg of that tool has exactly ONE loaded object of the right
      # type, the intended call is unambiguous: run it ourselves instead of
      # bouncing a clarification back at the user. This is the same
      # singleton-autoresolve policy as the no-tool-call recovery above, and
      # it is what makes "run clustocell on my data" (one loaded Seurat)
      # succeed even when a weak local model never gets past list_objects.
      itool <- tryCatch(cv_intended_tool(user_message, reg), error = function(e) NULL)
      auto_args <- if (!is.null(itool))
        tryCatch(cv_autoresolve_singleton_args(itool, store), error = function(e) NULL)
      # Only auto-run when the intended tool has NOT already succeeded this
      # turn. If it has, the stall is the model re-requesting finished work -
      # the guards already handled that correctly, and running it again here
      # would duplicate the object (the exact bug the terminal guard exists to
      # prevent). In that case fall through to the clarification.
      already_done <- !is.null(itool) &&
        !is.null(cv_ledger_terminal(ledger, itool$name))
      if (!is.null(itool) && !is.null(auto_args) && !already_done) {
        cv_emit(on_event, "tool_start", tool = itool$name, arguments = auto_args)
        auto_tc <- list(id = paste0("auto_", itool$name), name = itool$name,
                        arguments = auto_args)
        auto_rr <- .cv_timed_tool_call(auto_tc, store, reg, dispatch, timer, session_id)
        if (isTRUE(auto_rr$ok)) {
          # Same post-success path as a model-driven call: render artifacts and
          # emit the tool_result event so the UI shows the new object.
          auto_rr$result <- tryCatch(
            cv_render_result(auto_rr$result, sess$artifacts_dir,
                             session_id = session_id,
                             basename = paste0(itool$name, "_", cv_new_id("art"))),
            error = function(e) auto_rr$result)
          cv_emit(on_event, "tool_result", tool = itool$name,
                  handle = auto_rr$result$handle %||% NA,
                  summary = auto_rr$result$text %||% NA,
                  result = cv_result_for_browser(auto_rr$result))
          cv_session_add_message(session_id, "tool",
                                 content = cv_tool_result_for_model(auto_rr),
                                 tool_call_id = auto_tc$id, name = itool$name)
          total_tool_calls <<- total_tool_calls + 1L
          # Round XLVII: user-facing wording. This used to open with "The model
          # stalled before acting", which describes OUR internals and reads as a
          # failure report even though the step succeeded -- and it then repeated
          # the tool's whole summary, which the result card directly above
          # already shows. Say what was done, once.
          final_text <<- paste0(
            "Ran `", itool$name, "` on `", auto_args[[1]], "` for you",
            if (!is.null(auto_rr$result$handle) && !is.na(auto_rr$result$handle))
              paste0(" \u2014 result: `", auto_rr$result$handle, "`") else "",
            ".")
          cv_session_add_message(session_id, "assistant", final_text)
          cv_emit(on_event, "assistant", text = final_text)
          return("done")
        }
        # Auto-run failed: fall through to the clarification below so the user
        # sees the real error context rather than a silent retry loop.
        stall_reason <<- paste0(stall_reason %||% "the turn stalled",
                               "; auto-running '", itool$name, "' failed: ",
                               auto_rr$error %||% "unknown error")
      }

      # Clickable clarification: instead of a bare "tell me the handle" text,
      # emit a structured payload listing the loaded objects as a markdown list
      # (with a machine-readable `choices` array) so the UI renders clickable
      # handle chips and the user never types a handle. The intended tool (best
      # guess from the user's message) lets the UI prefill "run <tool> on <h>".
      handles <- cv_object_handles(store)
      payload <- cv_clarification_payload(
        store,
        header = paste0(
          "I stopped because I was not making progress - ",
          stall_reason %||% "the same tool call kept repeating without advancing",
          ". Which object should I use? Click one below (or correct the request) and I'll continue."),
        handles = handles,
        tool = if (!is.null(itool)) itool$name else NULL)
      final_text <<- payload$text
      cv_session_add_message(session_id, "assistant", final_text)
      cv_emit(on_event, "clarification", text = payload$text,
              tool = payload$tool, choices = payload$choices)
      return("done")
    }

    # Loop again so the model can react to the tool outputs.
    if (iter == max_iters) {
      final_text <<- paste0(
        "I reached the maximum number of tool steps (", max_iters, ") for this turn. ",
        "Here is where things stand; ask me to continue if needed.")
      cv_session_add_message(session_id, "assistant", final_text)
      cv_emit(on_event, "assistant", text = final_text)
    }
    "continue"
  }

  # ---- Drivers ---------------------------------------------------------------
  # finish(): the end-of-turn work that used to sit after the loop, unchanged.
  finish <- function() {

    # Round LIV: INDEX every session object -- record what it could be
    # downloaded as (.rds plus TXT/CSV sidecars) and refresh the results
    # manifest, so the Results tab can list them. The bytes are written later,
    # when a download actually asks for them (cv_api_serve_artifact /
    # cv_api_artifacts_zip in agent_api.R).
    #
    # This call used to be cv_sync_object_artifacts(), which gzip-serialized
    # every changed object right here -- 4,649 ms for a 193 MB object, on the
    # single thread that also answers HTTP, at the end of an ORDINARY turn.
    # Indexing costs the same regardless of object size. See
    # cv_index_object_artifacts() (agent_artifacts.R) for the measurements and
    # for the restart trade this accepts.
    #
    # Best effort: an indexing hiccup must never fail an otherwise-successful
    # turn.
    # Round LXXV (audit #33): now through the shared helper, because the error
    # and cancelled paths call it too. Until this round this was the ONLY call
    # site, so a stopped turn's finished object never reached Results.
    cv_index_artifacts_safe(session_id, when = "turn end")

    # Round LXXX (audit #68/#69): the `done` event now carries what the turn
    # cost, in time and in tokens. Both are best-effort by construction -- a
    # local model reports no usage at all, and `llm_calls_with_usage` says so
    # rather than letting a partial total read as a whole one.
    ph <- cv_phase_timer_get(timer)
    tk <- cv_usage_acc_get(usage_acc)
    cv_emit(on_event, "done", iterations = iter, tool_calls = total_tool_calls,
            timing = ph, usage = tk)
    cv_log_event("turn", session = session_id, status = "done",
                 iterations = iter, tool_calls = total_tool_calls,
                 wall_sec = ph$wall_sec, llm_sec = ph$llm_sec,
                 tool_sec = ph$tool_sec, other_sec = ph$other_sec,
                 prompt_tokens = tk$prompt_tokens,
                 completion_tokens = tk$completion_tokens,
                 total_tokens = tk$total_tokens,
                 llm_calls = tk$llm_calls,
                 llm_calls_with_usage = tk$llm_calls_with_usage,
                 provider = config$default_provider, model = config$default_model)
    list(content = final_text %||% "", iterations = iter, tool_calls = total_tool_calls,
         timing = ph, usage = tk)
  }

  # Stepwise mode (async server path): hand the machine to the caller so it can
  # run one step per event-loop tick. `exhausted()` reproduces the for-loop's own
  # bound, so both drivers stop at exactly the same place.
  if (isTRUE(stepwise)) {
    return(structure(list(step = step, finish = finish,
                          exhausted = function() iter >= max_iters),
                     class = "cv_turn_machine"))
  }

  # Synchronous driver: identical semantics to the original for-loop, so every
  # existing caller and test exercises the same code path as before.
  repeat {
    out <- step()
    if (identical(out, "done")) break
    # "waiting" is only reachable if a caller hands the SYNC driver an async
    # dispatcher. Nothing in the package does, but degrade to a polite wait
    # rather than a busy spin or a mis-counted iteration if anything ever does.
    if (identical(out, "waiting")) { Sys.sleep(0.05); next }
    if (iter >= max_iters) break
  }
  finish()
}
