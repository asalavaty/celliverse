# =============================================================================
# Generate precomputed objects for the CelliVerse development vignette
# =============================================================================
#
# Purpose
# -------
# Rebuild dev-vignettes/celliverse_vignette_results.rda from the same PBMC3K
# workflow shown in the CelliVerse vignette.
#
# This file and the generated .rda are DEVELOPMENT/REPRODUCIBILITY assets.
# They should remain under dev-vignettes/ and be excluded from the CRAN source
# tarball via .Rbuildignore.
#
# Run this script from the root directory of the celliverse package:
#
#   source("dev-vignettes/generate-vignette-results.R")
#
# Do NOT use usethis::use_data() here. use_data() writes package data under
# data/, which would put this vignette-only cache back into the CRAN package.
# =============================================================================

if (!file.exists("DESCRIPTION")) {
  stop(
    "Run this script from the root directory of the celliverse package."
  )
}

description <- read.dcf("DESCRIPTION")
if (!identical(unname(description[1, "Package"]), "celliverse")) {
  stop("The current directory does not appear to be the celliverse package root.")
}

required_packages <- c("celliverse", "Seurat", "SeuratData")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) {
  stop(
    "Install the following development packages before continuing: ",
    paste(missing_packages, collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(celliverse)
  library(Seurat)
})

set.seed(121)

message("Using:")
message("  celliverse ", as.character(utils::packageVersion("celliverse")))
message("  Seurat     ", as.character(utils::packageVersion("Seurat")))
message("  SeuratData ", as.character(utils::packageVersion("SeuratData")))

# -----------------------------------------------------------------------------
# 1. Load PBMC3K
# -----------------------------------------------------------------------------

# Install the SeuratData PBMC3K dataset if needed, then load it.
# InstallData() does not need to reinstall an already installed dataset.
SeuratData::InstallData("pbmc3k")
pbmc3k_so <- SeuratData::LoadData("pbmc3k")

# -----------------------------------------------------------------------------
# 2. ClustoCell clustering and markers
# -----------------------------------------------------------------------------

set.seed(121)
pbmc_clustoCell <- clustoCell(
  data = pbmc3k_so,
  seed = 121,
  verbose = FALSE
)

pbmc3k_so <- addClustoData(
  obj = pbmc3k_so,
  clustoCell = pbmc_clustoCell
)

pbmc3k_markers <- getDatasetMarkers(
  obj = pbmc_clustoCell,
  pos_thresh = 85
)

# -----------------------------------------------------------------------------
# 3. ClustoCell-marker-based dimensional reduction
# -----------------------------------------------------------------------------

pbmc3k_so <- NormalizeData(
  pbmc3k_so,
  normalization.method = "LogNormalize",
  scale.factor = 10000
)

pbmc3k_so <- ScaleData(
  pbmc3k_so,
  features = pbmc3k_markers$combined_markers
)

pbmc3k_so <- RunPCA(
  pbmc3k_so,
  npcs = 10,
  seed.use = 121,
  features = pbmc3k_markers$combined_markers,
  reduction.name = "clustoCell_pca"
)

pbmc3k_so <- RunUMAP(
  pbmc3k_so,
  dims = 1:10,
  seed.use = 121,
  reduction = "clustoCell_pca",
  reduction.name = "clustoCell_umap"
)

# -----------------------------------------------------------------------------
# 4. Standard Seurat clustering used for comparison in the vignette
# -----------------------------------------------------------------------------

pbmc3k_so <- FindVariableFeatures(
  pbmc3k_so,
  selection.method = "vst",
  nfeatures = 2000
)

pbmc3k_so <- ScaleData(pbmc3k_so)

pbmc3k_so <- RunPCA(
  pbmc3k_so,
  npcs = 10,
  seed.use = 121
)

pbmc3k_so <- FindNeighbors(
  pbmc3k_so,
  dims = 1:10,
  verbose = FALSE
)

pbmc3k_so <- FindClusters(
  pbmc3k_so,
  resolution = 0.5,
  random.seed = 121,
  verbose = FALSE
)

pbmc3k_so <- RunUMAP(
  pbmc3k_so,
  dims = 1:10,
  seed.use = 121
)

# -----------------------------------------------------------------------------
# 5. MarkoClust on Seurat clusters
# -----------------------------------------------------------------------------

set.seed(121)
pbmc3k_markoClust <- markoClust(
  data = pbmc3k_so,
  cluster_labels = "seurat_clusters",
  seed = 121,
  verbose = FALSE
)

# -----------------------------------------------------------------------------
# 6. MarkoCell for the Seurat-1 / ClustoCell-C1 discordant subset
# -----------------------------------------------------------------------------

seurat_1_clustoCell_C1 <- colnames(pbmc3k_so)[
  pbmc3k_so$seurat_clusters == 1 &
    pbmc3k_so$ClustoCell_Clusters == "C1"
]

set.seed(121)
pbmc3k_1_C1_markoCell <- markoCell(
  data = pbmc3k_so,
  desired_cells = list(
    s_1_cc_C1 = seurat_1_clustoCell_C1
  ),
  seed = 121,
  verbose = FALSE
)

# -----------------------------------------------------------------------------
# 7. Single-cell-type/subtype example: C1-Sub1 CD8 cells
# -----------------------------------------------------------------------------

pbmc3k_C1_Sub1_CD8_so <- subset(
  pbmc3k_so,
  subset = ClustoCell_SubClusters == "C1-Sub1"
)

set.seed(121)
pbmc3k_C1_Sub1_CD8_clustoCell <- clustoCell(
  pbmc3k_C1_Sub1_CD8_so,
  identify_subclusters = FALSE,
  seed = 121
)

pbmc3k_C1_Sub1_CD8_clustoCell_C1_typoClust <- typoClust(
  desired_pos_markers = list(
    my_panel = c(
      "CD3E", "GZMA", "GZMK", "CCL5", "NKG7",
      pbmc3k_C1_Sub1_CD8_clustoCell$
        markers$
        major_clusters$
        cluster_specific$
        positive_markers$
        C1$
        Feature[1:5]
    )
  ),
  tissue = "Blood",
  mode = "markerDB",
  species = "human",
  verbose = FALSE
)

# -----------------------------------------------------------------------------
# 8. Marker-purity example
# -----------------------------------------------------------------------------

set.seed(121)
pbmc3k_6_NKG7_markerPurity <- markerPurity(
  data = pbmc3k_so,
  desired_markers = "NKG7",
  cluster_labels = "seurat_clusters",
  desired_clusters = "6",
  seed = 121,
  verbose = FALSE
)

# -----------------------------------------------------------------------------
# 9. Marker DB annotation examples
# -----------------------------------------------------------------------------

clustoCell_pbmc_typoClust <- typoClust(
  objects = list(pbmc_clustoCell),
  desired_sets = paste0("C", 1:5),
  tissue = "Blood",
  mode = "markerDB",
  species = "human",
  verbose = FALSE
)

my_panel_typoClust <- typoClust(
  desired_pos_markers = list(
    my_panel = c(
      "NKG7", "CCL5", "GZMA", "CTSW",
      "CST7", "GNLY", "CD99", "GZMB", "PRF1"
    )
  ),
  tissue = "Blood",
  mode = "markerDB",
  species = "human",
  verbose = FALSE
)

# -----------------------------------------------------------------------------
# 10. Sketching examples
#
# The temporary ClustoCell objects below are not stored separately in the
# vignette cache. Their transferred labels are added to pbmc3k_so because the
# vignette evaluates plots/tables using those metadata columns.
# -----------------------------------------------------------------------------

set.seed(121)
pbmc_clustoCell_withSketch <- clustoCell(
  data = pbmc3k_so,
  sketch = TRUE,
  sketch_ncells = 1500,
  identify_subclusters = FALSE,
  seed = 121,
  verbose = FALSE
)

pbmc3k_so <- addClustoData(
  obj = pbmc3k_so,
  clustoCell = pbmc_clustoCell_withSketch,
  major_cluster_name = "ClustoCell_withSketch",
  add_sub_clusters = FALSE
)

set.seed(121)
pbmc_clustoCell_withSketch <- clustoCell(
  data = pbmc3k_so,
  sketch = TRUE,
  sketch_ncells = 1500,
  identify_subclusters = TRUE,
  refine_transferred_subClusters = TRUE,
  seed = 121,
  verbose = FALSE
)

pbmc3k_so <- addClustoData(
  obj = pbmc3k_so,
  clustoCell = pbmc_clustoCell_withSketch,
  sub_cluster_name = "SubClustoCell_withSketch_refined",
  add_major_clusters = FALSE
)

# -----------------------------------------------------------------------------
# 11. Assemble and save the development-only vignette cache
# -----------------------------------------------------------------------------

celliverse_vignette_results <- list(
  pbmc3k_so = pbmc3k_so,
  pbmc_clustoCell = pbmc_clustoCell,
  pbmc3k_markers = pbmc3k_markers,
  pbmc3k_markoClust = pbmc3k_markoClust,
  pbmc3k_1_C1_markoCell = pbmc3k_1_C1_markoCell,
  pbmc3k_6_NKG7_markerPurity = pbmc3k_6_NKG7_markerPurity,
  clustoCell_pbmc_typoClust = clustoCell_pbmc_typoClust,
  my_panel_typoClust = my_panel_typoClust,
  pbmc3k_C1_Sub1_CD8_clustoCell = pbmc3k_C1_Sub1_CD8_clustoCell,
  pbmc3k_C1_Sub1_CD8_clustoCell_C1_typoClust =
    pbmc3k_C1_Sub1_CD8_clustoCell_C1_typoClust
)

output_dir <- file.path("dev-vignettes")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

output_file <- file.path(
  output_dir,
  "celliverse_vignette_results.rda"
)

save(
  celliverse_vignette_results,
  file = output_file,
  compress = "bzip2",
  version = 3
)

message(
  "Saved vignette cache: ",
  normalizePath(output_file, winslash = "/", mustWork = TRUE)
)
message(
  "Compressed size: ",
  format(
    file.info(output_file)$size / 1024^2,
    digits = 3
  ),
  " MiB"
)
