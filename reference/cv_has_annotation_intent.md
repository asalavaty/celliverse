# Shared annotation-intent test: TRUE when the message has both an annotate/label/cell-type VERB and a cluster/set-like NOUN nearby. Round XXXII (Batch 3, item 9): this verb/noun pair was previously copy-pasted verbatim into \`cv_is_unspecified_annotation\`, \`cv_is_markerdb_annotation\`, and \`cv_is_llm_annotation\`; the three copies had silently drifted (the LLM variant's cluster-word pattern also matched "marker"/"gene", since LLM-path requests often name a marker panel directly, e.g. "annotate using markers CD3E, CD8A"). Extracted to one place so the verb pattern can never drift again, while \`extra_cluster_words\` preserves each caller's exact prior cluster-word set. Pure refactor: every caller passes the same arguments it always implicitly used, so detection behavior is unchanged.

Shared annotation-intent test: TRUE when the message has both an
annotate/label/cell-type VERB and a cluster/set-like NOUN nearby. Round
XXXII (Batch 3, item 9): this verb/noun pair was previously copy-pasted
verbatim into \`cv_is_unspecified_annotation\`,
\`cv_is_markerdb_annotation\`, and \`cv_is_llm_annotation\`; the three
copies had silently drifted (the LLM variant's cluster-word pattern also
matched "marker"/"gene", since LLM-path requests often name a marker
panel directly, e.g. "annotate using markers CD3E, CD8A"). Extracted to
one place so the verb pattern can never drift again, while
\`extra_cluster_words\` preserves each caller's exact prior cluster-word
set. Pure refactor: every caller passes the same arguments it always
implicitly used, so detection behavior is unchanged.

## Usage

``` r
cv_has_annotation_intent(m, extra_cluster_words = NULL)
```
