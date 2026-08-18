# Longest vocabulary term that appears as a whole phrase in the message.

Longest-first so "Bone Marrow" wins over "Bone", and word-boundary
anchored so "Lung" does not match "Lunge". Returns NULL when nothing
matches or when two DIFFERENT terms of the same length both match
(ambiguous -\> ask).

## Usage

``` r
.cv_match_vocab(msg, vocab)
```
