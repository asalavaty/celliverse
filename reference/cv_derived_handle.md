# Derive a result handle that inherits its base name from the input handle(s).

Naming inheritance: a clustoCell run on \`obj_pbmc3k\` should produce
\`clusto_pbmc3k\`, not a random id - the handle then tells the user (and
the model) where the object came from. The base is the FIRST input
handle with its type prefix stripped (\`obj_pbmc3k\` -\> \`pbmc3k\`);
the result prefix comes from the new object's class. If the candidate is
already taken, suffix \`\_2\`, \`\_3\`, ... until free. When
\`inherit_from\` is empty/NULL we fall back to the historical random id.

## Usage

``` r
cv_derived_handle(store, value, inherit_from = NULL)
```

## Arguments

- store:

  object store environment.

- value:

  the new R object (its class picks the prefix).

- inherit_from:

  character vector of input handles (first one wins).

## Value

a unique handle (character).
