# the regional study areas caribouLocPrep saves as .rds: a named list with one area per region, each
# packed with terra::wrap() or an sf shape. SpatVectors saved without terra::wrap() cannot be used in
# a new R session, so those stop with a message.
readStudyAreaJuris <- function(file) {

  areas <- readRDS(file)

  lapply(areas, function(a) {

    if (inherits(a, "PackedSpatVector")) {
      return(terra::unwrap(a))
    }

    if (inherits(a, c("sf", "sfc"))) {
      return(terra::vect(a))
    }

    if (inherits(a, "SpatVector") && !inherits(try(terra::nrow(a), silent = TRUE), "try-error")) {
      return(a)
    }

    stop(basename(file), " holds study areas saved without terra::wrap(), which cannot be read back. ",
         "Make the file again with caribouLocPrep, which now wraps them, or supply studyArea_juris.")
  })
}
