# Build the UNIFIED annotation-options picker payload (Round XXI): Tissue + Condition dropdowns (each with "All (no filter)" + full vocab) PLUS a numeric n (top markers) field, shown for BOTH the MarkerDB and LLM methods. When the user supplied their own marker list, the n field is omitted and a note is shown instead (n is fixed to the list length). The \`resume_template\` carries tissue=/condition=/n= placeholders the UI fills and sends on Continue. Ask which species BEFORE the tissue/condition picker.

Round LXIV (Batch 1b). This is a separate step, not another field on the
tissue card, and that is a correctness requirement rather than a style
choice: the Tissue and Condition vocabularies are looked up per species
(\`tissueCondition_types\[\[species\]\]\`), so the tissue list cannot be
built until the species is known. Asking both at once would offer human
tissues to someone annotating a mouse dataset, and the mismatch would
only surface as an abort after the run had started.

## Usage

``` r
cv_species_clarification_payload(user_message, method = c("markerdb", "llm"))
```

## Arguments

- user_message:

  The user's original request.

- method:

  \`"markerdb"\` or \`"llm"\`.

## Details

The two methods get different controls because their domains genuinely
differ:

\- \*\*markerDB (typoClust)\*\*: two chips, Human and Mouse. The curated
Marker DB holds exactly those two, and typoClust aborts for anything
else. Chips auto-send, the same as the method chips one step earlier. -
\*\*LLM (ceLLMarkup)\*\*: a free-text field pre-filled \`human\`. The
model is not restricted to a dictionary, so any species works; leaving
it blank means human.
