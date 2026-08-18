# Cell-type marker database

A curated database of positive and negative cell-type marker genes for
human and mouse, stored in sparse matrix format. This dataset is used
internally by
[`typoClust()`](https://asalavaty.github.io/celliverse/reference/typoClust.md)
for marker-based cell-type annotation.

## Format

An object of class `CelliVerse_Data`, implemented as a named list with
two elements:

- human:

  A list containing human marker databases:

  positive_db

  :   An object of class `CelliVerse_Sparse_Data` wrapping a sparse
      `dgCMatrix` of dimensions 16,899 genes × 9,410 cell types.

  negative_db

  :   An object of class `CelliVerse_Sparse_Data` wrapping a sparse
      `dgCMatrix` of dimensions 329 genes × 93 cell types.

- mouse:

  A list containing mouse marker databases:

  positive_db

  :   An object of class `CelliVerse_Sparse_Data` wrapping a sparse
      `dgCMatrix` of dimensions 4,779 genes × 2,112 cell types.

  negative_db

  :   An object of class `CelliVerse_Sparse_Data` wrapping a sparse
      `dgCMatrix` of dimensions 32 genes × 33 cell types.

## Source

Curated from published cell-type marker resources and expert annotation.

## Details

Rows correspond to marker genes and columns correspond to annotated cell
types. Matrix values encode marker presence or strength as defined
during database construction. Sparse representation is used to minimize
memory usage.

## See also

[`typoClust`](https://asalavaty.github.io/celliverse/reference/typoClust.md),
[`markerDictionary`](https://asalavaty.github.io/celliverse/reference/markerDictionary.md),
[`tissueCondition_types`](https://asalavaty.github.io/celliverse/reference/tissueCondition_types.md)
