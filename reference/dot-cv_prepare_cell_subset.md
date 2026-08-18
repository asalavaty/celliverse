# Pre-dispatch preparation shared by markoCell and markerPurity.

Round LXIV (D1). Everything here used to live inside each tool's
\`handler\`, which cv_make_dispatcher() invokes ONLY for a light tool.
Both of these tools are \`cost = "heavy"\`, so in production none of it
ran: a CellSet handle reached \`celliverse::markoCell\` as the raw
string \`"cellset_ab12cd"\` instead of the named barcode list it
requires, and aborted – breaking the very hand-off the system prompt
instructs the model to use ("pass the CellSet handle as desired_cells -
do NOT re-list the barcodes"). The subset guard, which is the most
useful mis-routing correction in the layer because it names
\`getClusterMarkers\` as the right tool, was dead for the same reason.

## Usage

``` r
.cv_prepare_cell_subset(store, tool, args, handle_args, require_subset = FALSE)
```

## Arguments

- store:

  Session object store.

- tool:

  The \`cv_tool\` being dispatched.

- args:

  Resolved call arguments.

- handle_args:

  Names of the handle-typed arguments.

- require_subset:

  Abort when no cell subset is given (markoCell only; markerPurity can
  legitimately run on whole clusters).

## Value

\`list(args = \<args with desired_cells expanded\>, inherit_from =
\<chr\>)\`.

## Details

It went unnoticed because every test in test-markocell-guard.R calls
\`tool\$handler(store, args)\` directly, so the suite exercised a path
production never takes. Tests that reach through cv_make_dispatcher()
are in test-round64-batch1b-safe.R.

MUST run in the parent process: the object store lives there, and a
CellSet handle cannot be resolved anywhere else.
