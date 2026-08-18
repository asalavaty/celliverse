# Raise a standing note ONCE per session, not once per call.

Round LXXX (audit \#89). \`result_note\` fired on every successful run
of the tool that declares it, and exactly one tool does: typoClust's
note saying the Marker DB was used and that an LLM alternative exists.
Annotating six sub-clusters in a row printed that same paragraph six
times, which is how a reader learns to skip the notes panel entirely –
the same failure Round LXIX invoked when it capped the severity scale at
two levels.

## Usage

``` r
.cv_session_note_once(store, code, text)
```

## Details

The flag lives as an attribute on the STORE, which is the session's own
environment, using the pattern \`cv_object_next_seq()\` already
established for the monotonic counter. A NEW session gets a fresh store
and therefore hears the note again, which is right: it is orientation
for someone who has just arrived, not a caveat about a particular
result.

Keyed on the note's CODE, deliberately, not on its text – so re-wording
the note does not silently make it start repeating again.

Returns \`list(cv_warn(...))\` the first time and \`NULL\` afterwards,
so callers splice it straight into cv_result_add_warnings(). With no
store (unit tests, programmatic callers) it behaves exactly as it always
did.
