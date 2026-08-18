# A condition meaning the turn cannot proceed without an interactive decision from the user – raised from a \`validate()\` hook, and understood by \`cv_run_tool_call()\` and \`run_tools()\` alike as something to re-raise rather than convert into a tool result. See \`cv_job_pending_condition()\` (agent_worker.R) for the identical shape this mirrors.

A condition meaning the turn cannot proceed without an interactive
decision from the user – raised from a \`validate()\` hook, and
understood by \`cv_run_tool_call()\` and \`run_tools()\` alike as
something to re-raise rather than convert into a tool result. See
\`cv_job_pending_condition()\` (agent_worker.R) for the identical shape
this mirrors.

## Usage

``` r
cv_needs_clarification_condition(payload)
```

## Arguments

- payload:

  the clarification payload (\`text\`/\`tool\`/\`choices\`/
  \`dropdowns\`/\`inputs\`/\`note\`/\`resume_template\`/\`base_request\`/\`kind\`).
