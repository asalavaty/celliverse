# On package attachment

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "\n",
    "============================================================\n",
    "                        C E L L I V E R S E                  \n",
    "============================================================\n",
    "\n",
    "        Welcome to CelliVerse - A Universe of Single-Cell Tools\n",
    paste0("                        Version ", utils::packageVersion("celliverse"), "\n"),
    "\n",
    "   Type citation(\"celliverse\") to learn how to cite this package.\n"
  )
}

#______________________________

# Global imports

#' @importClassesFrom Matrix dgCMatrix
#' @importFrom stats cor median na.omit quantile setNames
#' @importFrom magrittr %>%
#' @importFrom Matrix which

#______________________________

# logMessage Custom Methods

#' @export
print.logMessage <- function(x, ...) {
  cat(x, "\n")
}

#_______________________________

# ClustoCell Custom Methods

#' @export
print.ClustoCell <- function(x, ...) {
  cat("\033[1;3;4;38;5;22mAn object of class ClustoCell\n\033[0m")
  
  # Show slots
  cat("\033[1;3;38;5;18mSlots:\033[0m\n")
  for (nm in names(x)) {
    slot_class <- class(x[[nm]])[1]
    cat("  o ", format(nm, width = 22, justify = "left"), "[", slot_class, "]\n", sep = "")
  }
  
  # Summary
  cat("\033[1;3;38;5;18mSummary:\033[0m\n")
  
  if ("clusters" %in% names(x) && is.list(x$clusters)) {
    if ("major_clusters" %in% names(x$clusters)) {
      major_n <- length(unique(x$clusters$major_clusters))
      cat("  o Major clusters: ", major_n, "\n", sep = "")
    }
    if ("merged_sub_clusters" %in% names(x$clusters)) {
      sub_n <- length(unique(x$clusters$merged_sub_clusters))
      cat("  o Sub-clusters:   ", sub_n, "\n", sep = "")
    }
  }
  
  if ("quiescent_cells" %in% names(x)) {
    n_quiet <- length(x$quiescent_cells)
    cat("  o Quiescent cells: ", n_quiet, "\n", sep = "")
  }
  
  if ("isolated_cells" %in% names(x)) {
    n_iso <- length(x$isolated_cells)
    cat("  o Isolated cells:  ", n_iso, "\n", sep = "")
  }
  
  if ("globally_pure_ranked" %in% names(x)) {
    n_globally_pure_ranked <- length(x$globally_pure_ranked)
    cat("  o Globally Pure Ranked Features:  ", n_globally_pure_ranked, "\n", sep = "")
  }
  
  if ("globally_pure_high" %in% names(x)) {
    n_globally_pure_high <- length(x$globally_pure_high)
    cat("  o Globally Pure High Features:  ", n_globally_pure_high, "\n", sep = "")
  }
  
  if ("globally_pure_medium" %in% names(x)) {
    n_globally_pure_medium <- length(x$globally_pure_medium)
    cat("  o Globally Pure Medium Features:  ", n_globally_pure_medium, "\n", sep = "")
  }
}

#_______________________________

# TypoClust Custom Methods

#' @export
print.TypoClust <- function(x, ...) {
  cat("\033[1;3;4;38;5;22mAn object of class TypoClust\n\033[0m")
  
  # Show slots
  cat("\033[1;3;38;5;18mSlots:\033[0m\n")
  for (nm in names(x)) {
    slot_class <- class(x[[nm]])[1]
    cat("  o ", format(nm, width = 22, justify = "left"), "[", slot_class, "]\n", sep = "")
  }
  
  # Summary
  cat("\033[1;3;38;5;18mSummary:\033[0m\n")
  cat("  o ", "Number of Celltype Tables: ", length(x$metadata$desired_sets), "\n")
  cat("  o ", "Metadata:\n")
  for (nm in names(x$metadata)) {
    slot_class <- class(x$metadata[[nm]])[1]
    cat("    - ", format(nm, width = 22, justify = "left"), "[", slot_class, "]\n", sep = "")
  }

}

#_______________________________

# MarkoCell Custom Methods

#' @export
print.MarkoCell <- function(x, ...) {
  cat("\033[1;3;4;38;5;22mAn object of class MarkoCell\n\033[0m")
  
  # Show slots
  cat("\033[1;3;38;5;18mSlots:\033[0m\n")
  for (nm in names(x)) {
    slot_class <- class(x[[nm]])[1]
    cat("  o ", format(nm, width = 22, justify = "left"), "[", slot_class, "]\n", sep = "")
  }
  
  # Summary
  cat("\033[1;3;38;5;18mSummary:\033[0m\n")
  
  if ("cluster_markers" %in% names(x)) {
      desired_clusters_n <- length(x$cluster_markers$positive_markers)
      desired_clusters_names <- names(x$cluster_markers$positive_markers)
      cat("  o Number of Desired clusters: ", desired_clusters_n, "\n", sep = "")
      cat("  o Desired clusters: ", paste0(desired_clusters_names, collapse = ", "), "\n", sep = "")
  }
  
  if ("cell_subset_markers" %in% names(x)) {
    desired_cell_subset_n <- length(x$cell_subset_markers$positive_markers)
    desired_cell_subset_names <- names(x$cell_subset_markers$positive_markers)
    cat("  o Number of Desired Cell Subsets: ", desired_cell_subset_n, "\n", sep = "")
    cat("  o Desired Cell Subsets: ", paste0(desired_cell_subset_names, collapse = ", "), "\n", sep = "")
  }
  
  if ("quiescent_cells" %in% names(x)) {
    n_quiet <- length(x$quiescent_cells)
    cat("  o Quiescent cells: ", n_quiet, "\n", sep = "")
  }
  
  if ("isolated_cells" %in% names(x)) {
    n_iso <- length(x$isolated_cells)
    cat("  o Isolated cells:  ", n_iso, "\n", sep = "")
  }
  
  if ("globally_pure_ranked" %in% names(x)) {
    n_globally_pure_ranked <- length(x$globally_pure_ranked)
    cat("  o Globally Pure Ranked Features:  ", n_globally_pure_ranked, "\n", sep = "")
  }
  
  if ("globally_pure_high" %in% names(x)) {
    n_globally_pure_high <- length(x$globally_pure_high)
    cat("  o Globally Pure High Features:  ", n_globally_pure_high, "\n", sep = "")
  }
  
  if ("globally_pure_medium" %in% names(x)) {
    n_globally_pure_medium <- length(x$globally_pure_medium)
    cat("  o Globally Pure Medium Features:  ", n_globally_pure_medium, "\n", sep = "")
  }
}

#_______________________________

# MarkerPurity Custom Methods

#' @export
print.MarkerPurity <- function(x, ...) {
  cat("\033[1;3;4;38;5;22mAn object of class MarkerPurity\n\033[0m")
  
  # Show slots
  cat("\033[1;3;38;5;18mSlots:\033[0m\n")
  for (nm in names(x)) {
    slot_class <- class(x[[nm]])[1]
    cat("  o ", format(nm, width = 16, justify = "left"), "[", slot_class, "]\n", sep = "")
  }
  
  # Summary
  cat("\033[1;3;38;5;18mSummary:\033[0m\n")
  
  if ("within_clusters" %in% names(x)) {
    desired_clusters_n <- length(x$within_clusters$positive_markers)
    desired_clusters_names <- names(x$within_clusters$positive_markers)
    cat("  o Number of Desired clusters: ", desired_clusters_n, "\n", sep = "")
    cat("  o Desired clusters: ", paste0(desired_clusters_names, collapse = ", "), "\n", sep = "")
  }
  
  if ("within_cell_subsets" %in% names(x)) {
    desired_cell_subset_n <- length(x$within_cell_subsets$positive_markers)
    desired_cell_subset_names <- names(x$within_cell_subsets$positive_markers)
    cat("  o Number of Desired Cell Subsets: ", desired_cell_subset_n, "\n", sep = "")
    cat("  o Desired Cell Subsets: ", paste0(desired_cell_subset_names, collapse = ", "), "\n", sep = "")
  }
}

#_________________

## For S3 class objects

#' @export
print.CelliVerse_Data <- function(x, ...) {
  cat("\033[1;3;4;38;5;22mAn object of class CelliVerse_Data\n\033[0m")
  
  if(inherits(x, "data.frame")) {
    cat("  o A data frame.", "\n", sep = "")
    cat("    - Number of columns: ", ncol(x), "\n", sep = "")
    cat("    - Number of rows: ", nrow(x), "\n", sep = "")
  } else if(inherits(x, "list")) {
    # Show slots
    cat("\033[1;3;38;5;18mSlots:\033[0m\n")
    for (nm in names(x)) {
      slot_class <- class(x[[nm]])[1]
      cat("  o ", format(nm, width = 16, justify = "left"), "[", slot_class, "]\n", sep = "")
    }
    
    # Summary
    cat("\033[1;3;38;5;18mSummary:\033[0m\n")
    
    if ("human" %in% names(x)) {
      human_dbs_n <- length(x$human)
      human_dbs_names <- names(x$human)
      if(any(grepl("healthy|diseased", names(x$human)))) {
        cat("  o Number of human tissue/condition categories: ", human_dbs_n, "\n", sep = "")
        cat("    - Human tissue/condition types: ", paste0(paste(human_dbs_names[-5], " (#", sapply(x$human[-5], length), ")", sep = ""), collapse = ", "), "\n", sep = "")
        cat("    - Human diseased tissue and condition combinations: ", paste0(paste(human_dbs_names[5], " (#", sapply(x$human[5], nrow), ")", sep = ""), collapse = ", "), "\n", sep = "")
      } else {
        cat("  o Number of human resources: ", human_dbs_n, "\n", sep = "")
        cat("    - Human resources: ", paste0(human_dbs_names, collapse = ", "), "\n", sep = "")
      }
    }
    
    if ("mouse" %in% names(x)) {
      mouse_dbs_n <- length(x$mouse)
      mouse_dbs_names <- names(x$mouse)
      if(any(grepl("healthy|diseased", names(x$mouse)))) {
        cat("  o Number of mouse tissue/condition categories: ", mouse_dbs_n, "\n", sep = "")
        cat("    - Mouse tissue/condition types: ", paste0(paste(mouse_dbs_names[-5], " (#", sapply(x$mouse[-5], length), ")", sep = ""), collapse = ", "), "\n", sep = "")
        cat("    - Mouse diseased tissue and condition combinations: ", paste0(paste(mouse_dbs_names[5], " (#", sapply(x$mouse[5], nrow), ")", sep = ""), collapse = ", "), "\n", sep = "")
      } else {
        cat("  o Number of mouse resources: ", mouse_dbs_n, "\n", sep = "")
        cat("    - Mouse resources: ", paste0(mouse_dbs_names, collapse = ", "), "\n", sep = "")
      }
    }
  }
}

#_______________________________

# DatasetMarkers Custom Methods

#' @export
print.DatasetMarkers <- function(x, ...) {
  cat("\033[1;3;4;38;5;22mAn object of class DatasetMarkers\n\033[0m")
  
  # Show slots
  cat("\033[1;3;38;5;18mSlots:\033[0m\n")
  for (nm in names(x)) {
    slot_class <- class(x[[nm]])[1]
    cat("  o ", format(nm, width = 22, justify = "left"), "[", slot_class, "]\n", sep = "")
  }
  
  # Summary
  cat("\033[1;3;38;5;18mSummary:\033[0m\n")
  for (nm in names(x)) {
    slot_length <- length(x[[nm]])[1]
    cat("  o ", format(paste0(nm, ":"), width = 22, justify = "left"), slot_length, "\n", sep = "")
  }
}
