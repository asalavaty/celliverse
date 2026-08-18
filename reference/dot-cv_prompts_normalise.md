# Coerce whatever is on disk into the shape the rest of this file expects.

Defensive by design: this file is in the user's own directory, they are
invited to edit it, and a stray character in it must degrade to "no
saved prompts" rather than to a 500 on the page that shows the chat.

## Usage

``` r
.cv_prompts_normalise(x)
```
