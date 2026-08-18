# CelliVerse development vignette

This directory contains the **development and reproducibility sources** used to build the static CelliVerse package vignette.

The directory is intentionally excluded from the CRAN source tarball through `.Rbuildignore`. Its contents may remain version-controlled on GitHub.

## Files

- `Vignettes.Rmd`: source R Markdown document used to author the full CelliVerse vignette.
- `celliverse_vignette_results.rda`: precomputed PBMC3K objects used while rendering the development vignette. This is a build-time cache only and is **not package data**.
- `generate-vignette-results.R`: reproduces `celliverse_vignette_results.rda` from the PBMC3K workflow shown in the vignette.
- `celliverseVignette.css`: custom stylesheet used by `Vignettes.Rmd`, if the vignette YAML contains `css: celliverseVignette.css`.

The CRAN-distributed vignette is the pre-rendered static HTML file under `vignettes/`, together with its `.html.asis` metadata file for the `R.rsp::asis` vignette engine.

## Important distinction

`celliverse_vignette_results.rda` exists only to rebuild the vignette locally. Do **not** place it in `data/` or `inst/extdata/`, and do not generate it with `usethis::use_data()`. Doing so would put the large vignette cache back into the package distributed by CRAN.

The corresponding dataset documentation should therefore not be present in `R/data.R`.

## Rebuilding the precomputed results

Run the following from the root directory of the `celliverse` package:

```r
source("dev-vignettes/generate-vignette-results.R")
```

The script loads the PBMC3K dataset through `SeuratData`, runs the same analysis steps required by the vignette, and writes:

```text
dev-vignettes/celliverse_vignette_results.rda
```

The development environment therefore needs `celliverse`, `Seurat`, and `SeuratData`, together with any packages required by those workflows. These development-only requirements do not need to be declared in the CRAN package `DESCRIPTION` solely for this script because `dev-vignettes/` is excluded by `.Rbuildignore`.

## Loading the cache from the source vignette

Because `Vignettes.Rmd` and `celliverse_vignette_results.rda` live in the same directory, the vignette should load the cache directly:

```r
base::load("celliverse_vignette_results.rda")

invisible(
  list2env(
    celliverse_vignette_results,
    envir = knitr::knit_global()
  )
)
```

Do not use `system.file("extdata", ...)` for this cache after it has been removed from `inst/extdata`.

## Building the static HTML vignette

The development source still uses `knitr` and `rmarkdown` locally. From the package root, render it with:

```r
rmarkdown::render(
  input = "dev-vignettes/Vignettes.Rmd",
  output_file = "Vignettes.html",
  output_dir = normalizePath("vignettes", mustWork = TRUE)
)
```

Before rendering, make sure the custom CSS referenced by the YAML header is available at the path expected by `Vignettes.Rmd`. If the YAML contains:

```yaml
css: celliverseVignette.css
```

keep `celliverseVignette.css` in this `dev-vignettes/` directory.

After rendering, open `vignettes/Vignettes.html` directly in a browser and verify that figures, tables, styling, the TypoPrompt preview, and other embedded content render correctly without access to development-only files.

## Static CRAN vignette files

The package-facing `vignettes/` directory should contain the static output and its `R.rsp` metadata file:

```text
vignettes/
├── Vignettes.html
└── Vignettes.html.asis
```

`Vignettes.html.asis` should contain:

```text
%\VignetteIndexEntry{Introduction to CelliVerse}
%\VignetteEngine{R.rsp::asis}
%\VignetteEncoding{UTF-8}
```

The original `.Rmd`, precomputed `.rda`, and generator script remain under `dev-vignettes/` and are excluded from the CRAN source tarball.

## Package build checks

Before submission, build the source package and confirm that the development directory and vignette cache are absent:

```r
pkg <- devtools::build()

tar_files <- untar(pkg, list = TRUE)

grep(
  "dev-vignettes|celliverse_vignette_results",
  tar_files,
  value = TRUE
)
```

The result should be `character(0)`.

Also confirm that the static vignette files are present:

```r
grep(
  "Vignettes\\.html",
  tar_files,
  value = TRUE
)
```

Finally, run a CRAN-style check on the resulting source tarball.
