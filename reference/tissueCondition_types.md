# Tissue and condition reference catalog

A reference catalog of tissues and disease conditions for human and
mouse, used to contextualize cell-type annotation by tissue and
pathological state.

## Format

An object of class `CelliVerse_Data`, implemented as a named list with
two elements:

- human:

  A list containing:

  all_tissues

  :   Character vector of all supported tissues (length 396)

  healthy_tissue

  :   Character vector of healthy tissues (length 367)

  diseased_tissue

  :   Character vector of diseased tissues (length 103)

  all_conditions

  :   Character vector of all supported conditions (length 265)

  diseased_tissueCondition

  :   A data frame with two variables:

      Tissue

      :   Tissue name

      Condition

      :   Associated disease condition

- mouse:

  A list containing:

  all_tissues

  :   Character vector of all supported tissues (length 110)

  healthy_tissue

  :   Character vector of healthy tissues (length 108)

  diseased_tissue

  :   Character vector of diseased tissues (length 19)

  all_conditions

  :   Character vector of all supported conditions (length 51)

  diseased_tissueCondition

  :   A data frame with two variables:

      Tissue

      :   Tissue name

      Condition

      :   Associated disease condition

## Source

Curated from public tissue ontologies and disease annotation resources.

## Details

This dataset is used by
[`typoClust()`](https://asalavaty.github.io/celliverse/reference/typoClust.md)
to restrict or guide cell-type annotation according to tissue context
and disease state.

## See also

[`typoClust`](https://asalavaty.github.io/celliverse/reference/typoClust.md),
[`markerDB`](https://asalavaty.github.io/celliverse/reference/markerDB.md)
