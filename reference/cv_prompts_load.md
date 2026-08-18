# Read the saved-prompts file.

Never throws and never warns. A missing file is the ordinary first-run
state; an unparseable one is treated the same way, because the
alternative – an error surfacing on the chat screen – would let a
corrupt favourites list block the product's main job.

## Usage

``` r
cv_prompts_load(path = cv_prompts_path())
```
