# Changelog

## celliverse 0.0.2

### Initial CRAN release

This is the first CRAN release of `celliverse`, an R toolkit for
clustering, marker discovery, cell-type annotation, and downstream
analysis of single-cell RNA-sequencing data.

#### Core single-cell analysis

- Added
  [`clustoCell()`](https://asalavaty.github.io/celliverse/reference/clustoCell.md)
  for data-driven identification of major clusters and sub-clusters
  together with ranked positive and negative marker discovery.
- Added the MarkoCell workflow for marker discovery from pre-defined
  clusters and user-selected cell subsets.
- Added functionality for cluster comparison, marker inspection, label
  transfer, integration with Seurat objects, and visualization of
  clustering and marker results.

#### Cell-type annotation

- Added the CelliVerse MarkerDB, distributed with the package as
  `markerDB`, providing harmonized positive and negative cell-type
  marker information for human and mouse.
- Added
  [`typoClust()`](https://asalavaty.github.io/celliverse/reference/typoClust.md)
  for annotation of ClustoCell/MarkoCell results using either the
  curated CelliVerse MarkerDB or LLM-assisted annotation.
- Added
  [`ceLLMarkup()`](https://asalavaty.github.io/celliverse/reference/ceLLMarkup.md)
  for direct LLM-assisted annotation from marker panels, clustering
  results, or compatible marker tables.
- Added
  [`typoPrompt()`](https://asalavaty.github.io/celliverse/reference/typoPrompt.md)
  for generating structured, model-independent cell-type annotation
  prompts that can be used with any preferred chatbot or LLM.
- Added
  [`saveTypoPrompt()`](https://asalavaty.github.io/celliverse/reference/saveTypoPrompt.md)
  for exporting TypoPrompt objects as plain-text or self-contained HTML
  documents.

#### CelliVerse Agent

- Added an optional LLM-powered CelliVerse Agent that provides a
  browser-based natural-language interface to CelliVerse workflows.
- Added
  [`install_celliverse_agent()`](https://asalavaty.github.io/celliverse/reference/install_celliverse_agent.md)
  and
  [`run_celliverse_agent()`](https://asalavaty.github.io/celliverse/reference/run_celliverse_agent.md)
  for Agent setup and launch.
- Added support for common single-cell input formats, including R
  objects, delimited matrices, Matrix Market/10x inputs, zipped 10x
  triplets, and HDF5 inputs when the required optional dependency is
  available.
- Added support for both cloud-based model providers and local model
  backends such as Ollama and LM Studio.

#### Documentation and reproducibility

- Added a comprehensive package vignette covering clustering, marker
  discovery, annotation, visualization, TypoPrompt, and the CelliVerse
  Agent.
- Added expanded README documentation, installation guidance,
  interactive examples, and links to the browser-based ClustoCell
  demonstration.
- Added links to the dedicated CelliVerse-Project reproducibility
  repository containing manuscript analysis scripts and prepared
  MarkerDB resources.
- Computationally intensive and external-service-dependent examples are
  not executed during package checks; reproducible precomputed results
  are used where appropriate in the vignette.
