CRAN Comments
================
Adrian Salavaty
03 September 2026

## Resubmission changes requested during manual review

This resubmission addresses the manual-review comments:

- Examples were revised so that short, self-contained examples are run
  normally. `\dontrun{}` is retained only where execution genuinely
  requires external software.

- All uses of `options(warn = -1)` were removed.

- Examples and vignettes that create files now use temporary locations.
  Default CelliVerse Agent state is stored in the standard R user cache
  directory rather than directly in the user’s home directory.

- Direct manipulation of `.GlobalEnv` and `.Random.seed` was removed.

- Automatic installation of R packages was removed. Missing optional
  dependencies are now reported to the user with installation
  instructions.

- Fixed random-seed defaults were replaced by optional user-supplied
  seeds.

## Submission

This is the first CRAN submission of `celliverse` (version 0.0.2).

`celliverse` is an R toolkit for single-cell RNA-sequencing analysis,
including clustering and sub-clustering, marker discovery, cell-type
annotation, visualization, and optional LLM-assisted workflows.

## Test environments

The package has been checked on:

- local macOS, R 4.2.2
- Ubuntu, R-hub, R release and R-devel
- win-builder, R release and R-devel
- Windows Server/AppVeyor, R 4.1.0

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE:

- checking CRAN incoming feasibility … NOTE

This is a new submission.

The words reported as possibly misspelled by CRAN incoming checks are
package-specific terminology, software/package names, scientific
abbreviations, or proper names, including terms such as `CelliVerse`,
`ClustoCell`, `MarkoCell`, `TypoPrompt`, `scRNA-seq`, `Seurat`,
`Ollama`, and author names.

## External services and optional Agent functionality

`celliverse` includes optional LLM-assisted functionality and an
optional browser-based CelliVerse Agent.

No external model service, local LLM server, browser application, or
additional software is started automatically during package
installation, package loading, examples, tests, vignettes, or CRAN
checks. Agent installation and launch are explicit user actions through
`install_celliverse_agent()` and `run_celliverse_agent()`.

Examples requiring network access, API credentials, external model
providers, or local LLM software are not executed during CRAN checks.
Functions that use optional external services are designed to be invoked
explicitly by the user.

Computationally intensive vignette steps are also not re-run during CRAN
checks; precomputed results are used where appropriate to keep check
time and package-resource usage reasonable.

## Downstream dependencies

There are currently no downstream dependencies for `celliverse`.
