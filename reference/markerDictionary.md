# Marker gene dictionary

A species-specific dictionary mapping marker identifiers to standardized
gene annotations, including gene symbols, aliases, and database
identifiers. This dataset supports marker harmonization and identifier
resolution within the `celliverse` framework.

## Format

An object of class `CelliVerse_Data`, implemented as a named list with
two elements:

- human:

  A data frame with 20,887 rows and 6 variables:

  Marker

  :   Internal marker identifier

  Symbol

  :   Official gene symbol

  Alias

  :   Alternative gene symbols or aliases

  Entrez

  :   Entrez Gene identifier

  Ensembl

  :   Ensembl gene identifier

  UniProt

  :   UniProt protein identifier

- mouse:

  A data frame with 4,779 rows and 6 variables:

  Marker

  :   Internal marker identifier

  Symbol

  :   Official gene symbol

  Alias

  :   Alternative gene symbols or aliases

  Entrez

  :   Entrez Gene identifier

  Ensembl

  :   Ensembl gene identifier

  UniProt

  :   UniProt protein identifier

## Source

Integrated from public gene annotation resources including Ensembl,
Entrez Gene, and UniProt.

## Details

This dictionary is used to standardize marker gene identifiers across
datasets and species, enabling consistent matching between user-provided
markers and curated cell-type marker databases.

## See also

[`markerDB`](https://asalavaty.github.io/celliverse/reference/markerDB.md),
[`typoClust`](https://asalavaty.github.io/celliverse/reference/typoClust.md)
