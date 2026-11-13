# the folder holding a premade baseline: `source` is a folder, a .zip file, or a URL (e.g. of a .zip
# on Google Drive), which is downloaded and unpacked into `destinationPath`/baseline
baselineDir <- function(source, destinationPath) {

  if (dir.exists(source)) {
    return(source)
  }

  dir <- file.path(destinationPath, "baseline")

  if (file.exists(source)) {
    # into an emptied folder, so no file of an earlier baseline is read
    unlink(dir, recursive = TRUE)
    utils::unzip(source, exdir = dir)
  } else {
    prepInputs(url = source, destinationPath = dir, fun = NA)
  }

  return(dir)
}

# a premade baseline, as makePDEs() writes it: `pde_<model>.tif` (and `pdeMap_<model>.tif` if
# `normalizePDE`) for each model, anywhere in `dir`; returned as makePDEs() returns it
readBaseline <- function(dir, modelNames, normalizePDE) {

  files <- list.files(dir, pattern = "[.]tif$", recursive = TRUE, full.names = TRUE)

  read <- function(prefix) {
    wanted <- paste0(prefix, "_", modelNames, ".tif")
    found <- match(wanted, basename(files))

    if (anyNA(found)) {
      stop("The premade baseline in ", dir, " has no ", paste(wanted[is.na(found)], collapse = ", "))
    }

    twice <- wanted[wanted %in% basename(files)[duplicated(basename(files))]]
    if (length(twice)) {
      stop("The premade baseline in ", dir, " has more than one ", paste(twice, collapse = ", "))
    }

    setNames(lapply(files[found], terra::rast), modelNames)
  }

  list(pde = read("pde"), pdeMap = if (normalizePDE) read("pdeMap"))
}
