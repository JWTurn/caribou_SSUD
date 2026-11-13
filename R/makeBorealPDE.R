#' @title make Boreal caribou PDEs across scales and jurisdictions
#' @export
#' @author Julie W. Turner
#' @return a list with `pde` (the normalized PDE, or the raw utility if `normalizePDE` is FALSE) and
#' `pdeMap` (the binned maps, NULL if `normalizePDE` is FALSE), each a list with one raster per model.
#' If `savePath` is given, each model's PDE (and map) is also written there as a .tif, with `key`
#' (e.g. "year2030") at the end of the file name if given.

makeBorealPDE <- function(mods, studyAreaJuris, juris, envLayers, normalizePDE, savePath = NULL, key = NULL) {
  UDlist <- lapply(names(mods), function(mn) {

    sa <- getStudyAreaForModel(
      modelName = mn,
      studyArea_juris = studyAreaJuris,
      jurisdiction = juris
    )

    mod2UD(
      mod = mods[[mn]],
      envlayers = envLayers,
      studyArea = sa,
      normalize = normalizePDE
    )

  })

  names(UDlist) <- names(mods)

  if (normalizePDE) {
    pde <- lapply(UDlist, `[[`, "pde")
    # as.numeric() drops the class labels, which GeoTIFF would keep only in a separate .aux.xml file
    pdeMap <- lapply(UDlist, function(x) as.numeric(x$map))
  } else {
    pde <- lapply(UDlist, `[[`, "utility")
    pdeMap <- NULL
  }

  # save outputs
  if (!is.null(savePath)) {
    jur <- paste(juris, collapse = "")
    suffix <- paste(c(jur, key), collapse = "_")

    for (mn in names(UDlist)) {
      terra::writeRaster(pde[[mn]], file.path(savePath, paste0('pde_', mn, "_", suffix, ".tif")),
                         filetype = "GTiff", overwrite = TRUE)

      if (!is.null(pdeMap)) {
        terra::writeRaster(pdeMap[[mn]], file.path(savePath, paste0('pdeMap_', mn, "_", suffix, ".tif")),
                           filetype = "GTiff", overwrite = TRUE)
      }
    }
  }

  return(list(pde = pde, pdeMap = pdeMap))
}
