# The \`createDataSketch\` counterpart of \`.cv_assert_sketch_fits()\` above – same shape of mistake (a sketch that is not actually smaller than the object), different tool: \`createDataSketch\` always sketches (there is no \`sketch=\` flag to gate on), so this checks \`ncells\` unconditionally rather than only when sketching is enabled.

The \`createDataSketch\` counterpart of \`.cv_assert_sketch_fits()\`
above – same shape of mistake (a sketch that is not actually smaller
than the object), different tool: \`createDataSketch\` always sketches
(there is no \`sketch=\` flag to gate on), so this checks \`ncells\`
unconditionally rather than only when sketching is enabled.

## Usage

``` r
.cv_assert_createsketch_fits(store, args, data_arg = "data")
```
