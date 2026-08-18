# Build a markdown bullet list of candidate objects (handle + one-line summary) plus a machine-readable \`choices\` list, so the UI can render clickable handle chips and the user never has to type a handle. \`tool\` is the intended tool (may be NULL); \`header\` is a short lead-in sentence.

Build a markdown bullet list of candidate objects (handle + one-line
summary) plus a machine-readable \`choices\` list, so the UI can render
clickable handle chips and the user never has to type a handle. \`tool\`
is the intended tool (may be NULL); \`header\` is a short lead-in
sentence.

## Usage

``` r
cv_clarification_payload(store, header, handles, tool = NULL)
```
