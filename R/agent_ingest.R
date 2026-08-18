# =============================================================================
# CelliVerse Agent — dataset ingest: read a single-cell dataset in whatever
# format the user actually has (Round XLIX).
#
# SCOPE IS SET BY A HARD CONSTRAINT, NOT BY AMBITION. The user's requirement:
# add NO new entries to DESCRIPTION's Imports. So a format is supported here
# only if it can be read with base R plus what CelliVerse already imports:
#
#   Matrix      -> Matrix Market (.mtx)
#   Seurat      -> Read10X(), ReadMtx(), Read10X_h5(), CreateSeuratObject()
#   data.table  -> fread() for delimited text, including .gz
#   base/utils   -> readRDS(), load(), unzip(), gzfile()
#
# SUPPORTED
#   .rds                         readRDS
#   .rdata / .rda                load() into a private env
#   .csv .tsv .txt .tab (+ .gz)  data.table::fread -> sparse matrix -> Seurat
#   .mtx (+ .gz)                 Matrix Market, with sidecar barcodes/features
#                                when they sit beside it
#   .zip                         a zipped 10x triplet (matrix/barcodes/features)
#                                or a zip wrapping any single supported file
#   .h5                          10x HDF5 — CONDITIONAL: Seurat::Read10X_h5()
#                                needs hdf5r, which is a Seurat *Suggests* and
#                                is not required by CelliVerse. Supported when
#                                the user happens to have it, with an
#                                actionable message when they do not. No
#                                DESCRIPTION change either way.
#
# DELIBERATELY NOT SUPPORTED — each needs a package we are not allowed to add:
#   .h5ad   anndata / zellkonverter / SeuratDisk
#   .loom   SeuratDisk / LoomExperiment
#   .qs .qs2  qs / qs2
#   .zarr   Rarr / pizzarr
#
# These are not silently rejected. Each returns a message naming the format,
# saying why, and giving the one-line conversion that gets the user moving --
# an unsupported format the user can act on beats a supported one they cannot
# reach. Hand-rolling an h5ad/loom reader on top of raw HDF5 was considered and
# rejected: both are versioned layouts, and a parser that silently mis-reads a
# matrix is far worse than one that declines to try.
# =============================================================================

#' Formats this build can actually read, as a user-facing list.
#' @keywords internal
cv_supported_formats <- function() {
  base <- c(".rds", ".rdata/.rda", ".csv/.tsv/.txt (+.gz)", ".mtx (+.gz)",
            ".zip (10x matrix/barcodes/features)")
  if (cv_has_hdf5()) c(base, ".h5 (10x HDF5)") else base
}

#' Is an HDF5 reader available in THIS R installation? `hdf5r` is a Seurat
#' Suggests, so it may or may not be present; CelliVerse never requires it.
#' @keywords internal
cv_has_hdf5 <- function() requireNamespace("hdf5r", quietly = TRUE)

#' Extensions `cv_read_dataset_file()` actually dispatches on.
#'
#' Kept beside that switch deliberately, and asserted against it by a test: a
#' format added to the reader and not to this vector would be REFUSED at upload
#' before the reader ever saw it, which is a far more confusing failure than the
#' one this exists to prevent.
#'
#' `h5` is included even when `hdf5r` is absent. Refusing it here would replace
#' the reader's specific, actionable message ("install hdf5r, or export as 10x
#' MTX") with a generic list of formats -- the pre-check exists to save the user
#' an upload, not to give a worse answer than the code behind it.
#' @keywords internal
CV_UPLOAD_EXTS <- c("rds", "rdata", "rda", "csv", "tsv", "txt", "tab",
                    "mtx", "zip", "h5")

#' Round LXXIX (audit #56): can this filename possibly be read, judging by the
#' extension alone?
#'
#' Returns `NULL` when the upload should proceed, or `list(message=, detail=)`
#' when it should not.
#'
#' WHY IT RUNS BEFORE THE BYTES ARE STAGED. `cv_api_upload_multipart()` writes a
#' full second copy of the payload to `tempfile()` and only then hands it to the
#' reader. On a `.h5ad` -- the single most likely wrong format for this audience,
#' since it is what a Scanpy user has -- every byte was written, read and thrown
#' away to produce a message the FILENAME already determined.
#'
#' THERE IS NO SIZE LIMIT HERE AND MUST NOT BE ONE. The audit pairs this item
#' with "state an upload size limit"; that half is refused under the project's
#' standing no-upload-limits policy, and refusing it is a decision rather than an
#' oversight. A biologist with a 40 GB atlas is the user this tool is for.
#'
#' An empty extension is ACCEPTED, not refused. A programmatic caller can post
#' bytes under a form-field name with no extension at all, and
#' `cv_api_upload_multipart()` has always defaulted that to `.rds`; turning a
#' working path into an error would be a regression dressed as a guard.
#' @param filename the name the browser sent.
#' @keywords internal
cv_upload_extension_problem <- function(filename) {
  ext <- cv_file_ext2(filename %||% "")
  if (!nzchar(ext)) return(NULL)
  ext <- tolower(ext)
  if (ext %in% CV_UPLOAD_EXTS) return(NULL)
  # A format we know by name and deliberately do not read: give the reader's own
  # message, which names the one-line conversion that gets the user moving.
  if (ext %in% c("h5ad", "loom", "qs", "qs2", "zarr")) {
    return(list(message = cv_unsupported_format_msg(ext),
                detail = sprintf("Refused before upload on the file extension '.%s'.", ext)))
  }
  list(
    message = paste0(
      "This build cannot read a `.", ext, "` file. Formats it reads: ",
      paste(cv_supported_formats(), collapse = ", "),
      ". Nothing was uploaded, so pick another file and try again."),
    detail = sprintf("Refused before upload on the file extension '.%s'.", ext))
}

#' Normalised extension, with `.gz` peeled off ("counts.csv.gz" -> "csv").
#' @keywords internal
cv_file_ext2 <- function(path) {
  f <- tolower(basename(path %||% ""))
  f <- sub("\\.gz$", "", f)
  tools::file_ext(f)
}

#' A message for a format we cannot read without adding a dependency.
#' @keywords internal
cv_unsupported_format_msg <- function(ext) {
  how <- switch(
    ext,
    h5ad = paste0("Convert it to a Seurat .rds in Python + R, e.g. write the ",
                  "AnnData to 10x MTX (`scanpy.write` / `anndata`), or in R with ",
                  "zellkonverter::readH5AD() if you install that package yourself."),
    loom = paste0("Convert it with SeuratDisk::Connect()/as.Seurat() and save ",
                  "the result with saveRDS(), if you install SeuratDisk yourself."),
    qs   = ,
    qs2  = paste0("Re-save it as .rds: `obj <- qs2::qs_read(\"file.qs2\"); ",
                  "saveRDS(obj, \"file.rds\")`."),
    zarr = "Export it to 10x MTX or .h5, or convert to a Seurat .rds.",
    NULL)
  paste0(
    "`.", ext, "` files need an R package CelliVerse does not install, so this ",
    "build cannot read one. ",
    if (!is.null(how)) paste0(how, " ") else "",
    "Formats this build reads: ", paste(cv_supported_formats(), collapse = ", "), ".")
}

# ---- Tabular ---------------------------------------------------------------

#' Do these look like cell barcodes rather than gene names?
#'
#' 10x barcodes are long ACGT runs with an optional `-1` lane suffix, which no
#' gene symbol resembles. Used to decide a count table's ORIENTATION, which is
#' the one genuinely ambiguous thing about a CSV of counts.
#' @keywords internal
cv_looks_like_barcodes <- function(x, min_frac = 0.8) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) return(FALSE)
  probe <- utils::head(x, 200L)
  mean(grepl("^[ACGTNacgtn]{8,}(-\\d+)?$", probe)) >= min_frac
}

#' Decide whether a delimited count table is genes x cells (the convention) or
#' cells x genes, and return it genes x cells either way.
#'
#' Barcode-shaped names decide it outright when present. Otherwise the larger
#' dimension is taken to be genes, which is right for essentially every real
#' scRNA-seq matrix. Whatever is chosen is REPORTED to the user rather than
#' assumed silently: a wrong guess here transposes the whole analysis, so it has
#' to be visible and correctable, not buried.
#' @return list(mat = <genes x cells>, note = <chr>)
#' @keywords internal
cv_orient_count_table <- function(m) {
  rn <- rownames(m); cn <- colnames(m)
  if (cv_looks_like_barcodes(cn)) {
    return(list(mat = m, note = "columns look like cell barcodes, so rows were read as genes"))
  }
  if (cv_looks_like_barcodes(rn)) {
    return(list(mat = t(m), note = "rows looked like cell barcodes, so the table was transposed to genes x cells"))
  }
  if (ncol(m) > nrow(m)) {
    return(list(mat = t(m),
                note = sprintf("no barcode-like names, so the larger dimension (%d) was taken to be genes and the table was transposed",
                               ncol(m))))
  }
  list(mat = m, note = sprintf("no barcode-like names, so the larger dimension (%d rows) was taken to be genes",
                               nrow(m)))
}

#' Read a delimited count table (csv/tsv/txt/tab, optionally gzipped) into a
#' sparse genes x cells matrix.
#'
#' `data.table::fread` is already an import, auto-detects the separator, and
#' handles .gz, so no new dependency and no guessing at commas vs tabs. The
#' first column is taken as row names when it is not numeric -- the near
#' universal layout for an exported count table.
#' @keywords internal
cv_read_delim_counts <- function(path) {
  # GZIP MUST BE DECOMPRESSED EXPLICITLY. Verified empirically: handed a
  # "counts.csv.gz", fread did NOT error -- it returned a single-column table of
  # binary garbage, which then failed much later with a confusing message about
  # having no counts. A reader that silently mis-reads is worse than one that
  # refuses, so decompress here rather than trusting the caller. Streamed in
  # chunks through a base gzfile() connection so a large matrix is never held
  # in memory twice, and no new dependency (R.utils) is required.
  if (grepl("\\.gz$", path, ignore.case = TRUE)) {
    plain <- tempfile(fileext = ".txt")
    inc <- gzfile(path, "rb"); outc <- file(plain, "wb")
    # Batch 8a: the decompressed copy is deleted on the way out, on EVERY exit
    # path including the aborts below. It is read into memory by fread (or
    # read.delim) and nothing downstream holds the path, so releasing it here is
    # safe -- and it is the whole uncompressed size of the user's count table,
    # which for the datasets this reader exists to handle is not small.
    on.exit({ try(close(inc), silent = TRUE); try(close(outc), silent = TRUE)
              unlink(plain) }, add = TRUE)
    repeat {
      chunk <- readBin(inc, "raw", n = 1024L * 1024L)
      if (!length(chunk)) break
      writeBin(chunk, outc)
    }
    close(inc); close(outc)
    path <- plain
  }
  # write.csv() emits one fewer header name than there are columns (row names
  # get no header), which fread warns about before doing exactly the right
  # thing. Expected for the most common export there is; not worth alarming the
  # user over.
  dt <- tryCatch(
    suppressWarnings(data.table::fread(path, data.table = FALSE, showProgress = FALSE)),
    error = function(e) NULL)
  if (is.null(dt)) {
    # Fallback for anything fread declines (unusual quoting, odd encodings).
    con <- if (grepl("\\.gz$", path, ignore.case = TRUE)) gzfile(path) else path
    dt <- utils::read.delim(con, check.names = FALSE, stringsAsFactors = FALSE)
  }
  if (!is.data.frame(dt) || !nrow(dt) || !ncol(dt)) {
    # Reached by any text file that is not actually tabular. Say what was
    # expected and what else is accepted, rather than the bare parse result --
    # "empty table" tells a user who uploaded the wrong file nothing about
    # which file they should have uploaded.
    cli::cli_abort(paste0(
      "That file does not contain a readable table, so it cannot be a count ",
      "matrix. Expected genes in rows and cells in columns (or the transpose), ",
      "delimited by commas or tabs. Formats this build reads: ",
      paste(cv_supported_formats(), collapse = ", "), "."))
  }
  # First column as row names when it is not itself data.
  if (!is.numeric(dt[[1]])) {
    rn <- as.character(dt[[1]])
    dt <- dt[, -1, drop = FALSE]
    if (!ncol(dt)) cli::cli_abort("The table has only one column, so there are no counts to read.")
    rn[is.na(rn) | !nzchar(rn)] <- paste0("row", which(is.na(rn) | !nzchar(rn)))
    rownames(dt) <- make.unique(rn)
  }
  num <- vapply(dt, is.numeric, logical(1))
  if (!any(num)) {
    cli::cli_abort(paste0("No numeric columns found, so this does not look like a count table. ",
                          "Expected genes in rows and cells in columns (or the transpose)."))
  }
  if (!all(num)) dt <- dt[, num, drop = FALSE]
  m <- as.matrix(dt)
  storage.mode(m) <- "double"
  or <- cv_orient_count_table(m)
  list(mat = Matrix::Matrix(or$mat, sparse = TRUE), note = or$note)
}

# ---- Matrix Market ---------------------------------------------------------

#' Find a 10x-style sidecar (barcodes / features / genes) beside an .mtx.
#' @keywords internal
cv_find_sidecar <- function(dir, kind) {
  pat <- switch(kind,
                barcodes = "^barcodes\\.tsv(\\.gz)?$",
                features = "^(features|genes)\\.tsv(\\.gz)?$")
  hits <- list.files(dir, pattern = pat, full.names = TRUE, ignore.case = TRUE)
  if (length(hits)) hits[1] else NULL
}

#' Read an .mtx, using its sidecars when they are present.
#' @keywords internal
cv_read_mtx <- function(path) {
  dir <- dirname(path)
  bc <- cv_find_sidecar(dir, "barcodes")
  ft <- cv_find_sidecar(dir, "features")
  if (!is.null(bc) && !is.null(ft)) {
    m <- Seurat::ReadMtx(mtx = path, cells = bc, features = ft)
    return(list(mat = m, note = sprintf("read with %s and %s", basename(ft), basename(bc))))
  }
  m <- Matrix::readMM(path)
  m <- methods::as(m, "CsparseMatrix")
  rownames(m) <- paste0("gene", seq_len(nrow(m)))
  colnames(m) <- paste0("cell", seq_len(ncol(m)))
  list(mat = m, note = paste0(
    "no barcodes.tsv/features.tsv were found beside it, so genes and cells were ",
    "given placeholder names - upload the three 10x files together in a .zip to keep the real ones"))
}

# ---- Zip -------------------------------------------------------------------

#' Refuse a .zip whose entries would extract outside the target directory.
#'
#' Round LXIV (D7). `utils::unzip()` does not sanitise entry names, so an entry
#' called `../../../.Rprofile` is written wherever it points. This is the
#' "zip-slip" class of bug and it is checked here, on the NAME LIST ONLY,
#' before a single byte is extracted -- listing is cheap and cannot itself
#' write anything.
#'
#' Rejected: any absolute path (POSIX `/x`, UNC `\\\\host`, or Windows `C:\\x`)
#' and any entry with a `..` path SEGMENT. Note the check is on segments, not
#' on the substring: a legitimate file called `sample..2.mtx` or `..hidden.tsv`
#' must still be accepted, and is.
#'
#' Backslashes are normalised to `/` first, because a zip written on Windows
#' can carry `..\\..\\x`, which a `/`-only check would wave straight through.
#'
#' @param path Path to the .zip file.
#' @return `TRUE` invisibly when every entry is safe; aborts otherwise.
#' @keywords internal
cv_assert_safe_zip_entries <- function(path) {
  entries <- tryCatch(utils::unzip(path, list = TRUE)$Name,
                      error = function(e) NULL)
  # An unreadable/corrupt archive is NOT this function's error to report: let
  # the real unzip() below produce the message the user can act on, so a broken
  # download does not get described as a security problem.
  if (is.null(entries) || !length(entries)) return(invisible(TRUE))

  # Round LXIV Batch 2a: refuse an archive that would exhaust the machine
  # before it is opened. Ingest runs in the SERVER process, outside the worker
  # pool's memory admission control, so a zip bomb here takes the whole agent
  # down rather than one job. Both limits are far above any real 10x triplet
  # (three files, a few hundred MB uncompressed at the very most).
  info <- tryCatch(utils::unzip(path, list = TRUE), error = function(e) NULL)
  if (!is.null(info)) {
    if (nrow(info) > 10000L) {
      cli::cli_abort(c(
        "That .zip has too many files to open safely.",
        i = "It contains {nrow(info)} entries; CelliVerse opens up to 10,000.",
        i = "Zip only the files you need - for 10x data that is matrix.mtx, barcodes.tsv and features.tsv."))
    }
    total <- suppressWarnings(sum(as.numeric(info$Length), na.rm = TRUE))
    if (is.finite(total) && total > 20e9) {
      cli::cli_abort(c(
        "That .zip would expand to more than CelliVerse will extract.",
        i = "Uncompressed size is about {round(total / 1e9, 1)} GB; the limit is 20 GB.",
        i = "Point the loader at the file on disk instead of uploading an archive this large."))
    }
  }
  norm <- gsub("\\\\", "/", entries)
  is_abs <- grepl("^/", norm) | grepl("^[A-Za-z]:/", norm) | grepl("^//", norm)
  has_dotdot <- vapply(strsplit(norm, "/", fixed = TRUE),
                       function(parts) any(parts == ".."), logical(1))
  bad <- entries[is_abs | has_dotdot]

  if (length(bad)) {
    cli::cli_abort(c(
      "That .zip cannot be opened safely.",
      i = paste0("It contains ", length(bad), " entr",
                 if (length(bad) == 1L) "y" else "ies",
                 " that would be written outside the extraction folder, ",
                 "which CelliVerse does not allow."),
      i = "First one: {.val {bad[1]}}.",
      i = paste0("Re-create the archive from inside the folder that holds the ",
                 "files, so every entry is a plain relative name.")
    ))
  }
  invisible(TRUE)
}

#' Read a .zip: a 10x triplet if one is inside, else the single supported file.
#' @keywords internal
cv_read_zip <- function(path) {
  ex <- file.path(tempdir(), paste0("cvzip_", basename(tempfile(""))))
  dir.create(ex, showWarnings = FALSE, recursive = TRUE)
  # Batch 8a: the extracted tree goes away on the way out, whichever of this
  # function's five exits is taken -- the three `cli_abort`s included, which is
  # exactly why this is an on.exit and not an unlink before each `return`.
  #
  # Every path below returns an IN-MEMORY object: Seurat::ReadMtx() and
  # cv_read_mtx() return a matrix, and cv_read_dataset_file() returns the read
  # object rather than a handle to the file. Verified by round-tripping a real
  # zip and using the result after the directory is gone, because a reader that
  # returned a file-backed object would turn this cleanup into a crash -- the
  # one genuine risk in this change, so it is checked rather than reasoned about.
  on.exit(unlink(ex, recursive = TRUE, force = TRUE), add = TRUE)
  # Round LXIV (D7): refuse a zip-slip archive BEFORE extracting anything.
  # utils::unzip() honours an entry named "../../../x" and writes outside
  # `exdir` -- reproduced on R 4.3.3 with an entry five levels up, which landed
  # on disk and which the on.exit above could not clean up because it was never
  # inside `ex`. A single hostile entry is therefore an arbitrary file write
  # with the user's privileges: dropping ~/.Rprofile makes it code execution on
  # the next R start, and the realistic carrier is an ordinary-looking
  # supplementary .zip from a collaborator or a public repository.
  cv_assert_safe_zip_entries(path)
  utils::unzip(path, exdir = ex)
  all <- list.files(ex, recursive = TRUE, full.names = TRUE)
  if (!length(all)) cli::cli_abort("That .zip is empty.")

  mtx <- grep("(^|/)matrix\\.mtx(\\.gz)?$", all, value = TRUE, ignore.case = TRUE)
  if (!length(mtx)) mtx <- grep("\\.mtx(\\.gz)?$", all, value = TRUE, ignore.case = TRUE)
  if (length(mtx)) {
    d <- dirname(mtx[1])
    bc <- cv_find_sidecar(d, "barcodes"); ft <- cv_find_sidecar(d, "features")
    if (!is.null(bc) && !is.null(ft)) {
      # The canonical 10x layout: let Seurat read the directory as a unit.
      m <- Seurat::ReadMtx(mtx = mtx[1], cells = bc, features = ft)
      return(list(mat = m, note = sprintf("10x triplet from the .zip (%s, %s, %s)",
                                          basename(mtx[1]), basename(ft), basename(bc))))
    }
    return(cv_read_mtx(mtx[1]))
  }

  # No .mtx: fall back to a single supported file inside the archive.
  cand <- all[cv_file_ext2(all) %in% c("rds", "rdata", "rda", "csv", "tsv", "txt", "tab", "h5")]
  cand <- cand[!grepl("(^|/)__MACOSX/", cand)]
  if (!length(cand)) {
    cli::cli_abort(paste0(
      "That .zip has no readable dataset in it. Expected either the three 10x ",
      "files (matrix.mtx, barcodes.tsv, features.tsv) or a single ",
      paste(cv_supported_formats(), collapse = " / "), " file."))
  }
  if (length(cand) > 1L) {
    cli::cli_abort(paste0(
      "That .zip has ", length(cand), " readable files (",
      paste(basename(utils::head(cand, 5)), collapse = ", "),
      "). Zip only one dataset, or upload the file directly."))
  }
  cv_read_dataset_file(cand[1])
}

# ---- Dispatcher ------------------------------------------------------------

#' Read a single-cell dataset from a file in any format this build supports.
#'
#' Returns the loaded R object plus a human-readable note about anything that
#' had to be inferred (matrix orientation, placeholder names). The note is
#' surfaced to the user: an assumption they can see is one they can correct.
#'
#' @param path file to read.
#' @param filename original name, when `path` is a temp file whose extension may
#'   have been lost.
#' @return list(object=, note=<chr or NULL>, format=<chr>)
#' @keywords internal
cv_read_dataset_file <- function(path, filename = NULL) {
  ext <- cv_file_ext2(filename %||% path)
  if (!nzchar(ext)) ext <- cv_file_ext2(path)

  if (ext %in% c("h5ad", "loom", "qs", "qs2", "zarr")) {
    cli::cli_abort(cv_unsupported_format_msg(ext))
  }

  switch(
    ext,
    rds = list(object = readRDS(path), note = NULL, format = "rds"),

    rdata = ,
    rda = {
      env <- new.env(parent = emptyenv())
      nms <- load(path, envir = env)
      if (!length(nms)) cli::cli_abort("That .RData file contains no objects.")
      # Prefer a real single-cell object when the file holds several things.
      pick <- NULL
      for (n in nms) {
        if (cv_object_type(get(n, envir = env)) %in%
            c("Seurat", "SingleCellExperiment", "SpatialExperiment", "dgCMatrix", "matrix")) {
          pick <- n; break
        }
      }
      if (is.null(pick)) pick <- nms[1]
      list(object = get(pick, envir = env),
           note = if (length(nms) > 1L)
             sprintf("that file held %d objects (%s); `%s` was used",
                     length(nms), paste(nms, collapse = ", "), pick) else NULL,
           format = "rdata")
    },

    csv = , tsv = , txt = , tab = {
      r <- cv_read_delim_counts(path)
      list(object = r$mat,
           note = paste0("read as a count table - ", r$note),
           format = "delimited")
    },

    mtx = {
      r <- cv_read_mtx(path)
      list(object = r$mat, note = paste0("Matrix Market - ", r$note), format = "mtx")
    },

    zip = {
      r <- cv_read_zip(path)
      if (!is.null(r$object)) return(r)   # nested dispatch already shaped it
      list(object = r$mat, note = paste0("from .zip - ", r$note), format = "zip")
    },

    h5 = {
      if (!cv_has_hdf5()) {
        cli::cli_abort(paste0(
          "Reading `.h5` needs the `hdf5r` package, which is not installed here. ",
          "CelliVerse does not require it, so this is optional: install it with ",
          "`install.packages(\"hdf5r\")` and re-upload, or export your data as ",
          "10x MTX (matrix/barcodes/features in a .zip) or a Seurat .rds."))
      }
      m <- Seurat::Read10X_h5(path)
      # A multi-modal .h5 comes back as a list of matrices; take Gene Expression.
      if (is.list(m) && !inherits(m, "Matrix")) {
        nm <- if ("Gene Expression" %in% names(m)) "Gene Expression" else names(m)[1]
        note <- sprintf("multi-modal .h5; the %s matrix was used", nm)
        return(list(object = m[[nm]], note = note, format = "h5"))
      }
      list(object = m, note = NULL, format = "h5")
    },

    # Unknown extension: an .rds with the wrong name is common enough to be
    # worth one attempt before giving up.
    {
      obj <- tryCatch(readRDS(path), error = function(e) NULL)
      if (!is.null(obj)) {
        return(list(object = obj, note = "read as an .rds despite the file name", format = "rds"))
      }
      cli::cli_abort(paste0(
        if (nzchar(ext)) paste0("`.", ext, "` is not a format this build reads. ")
        else "That file has no extension, so its format could not be determined. ",
        "Supported: ", paste(cv_supported_formats(), collapse = ", "), "."))
    }
  )
}
