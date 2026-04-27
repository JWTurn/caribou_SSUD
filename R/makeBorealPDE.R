#' @title make Boreal caribou PDEs across scales and jurisdictions
#' @export
#' @author Julie W. Turner
#'
#'

makeBorealPDE <- function(mods, studyAreaJuris, juris, envLayers, normalizePDE, simulationScale, isSim = FALSE, key = NULL, savePath = NULL) {
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

  # store outputs
  # if simulation
  if(isSim){
    if (Par$normalizePDE) {
      sim$simPde[[key]] <- lapply(UDlist, `[[`, "pde")
    } else {
      sim$simPde[[key]] <- lapply(UDlist, `[[`, "utility")
    }
    sim$simPdeMap[[key]] <- lapply(UDlist, `[[`, "map")
    # TODO do I need to do the $utility thing below here too?

    if(savePath){
      for (mn in names(UDlist)) {

        pde <- if (normalizePDE) {
          UDlist[[mn]]$pde
        } else {
          UDlist[[mn]]$utility
        }
        jur <- paste(jurisdiction, collapse = "")
        outfile <- file.path(
          savePath,
          paste0('pde_', mn, "_", jur ,"_", key, ".tif")
        )

        terra::writeRaster(pde, outfile, overwrite = TRUE)
    }
  }
  # if not simulation
  else{
    if (normalizePDE) {
      sim$pde    <- lapply(UDlist, `[[`, "pde")

      if (simulationScale != "global") {
        sim$pdeMap <- lapply(UDlist, `[[`, "map")
      } else {
        sim$pdeMap <- NULL
      }

    } else {
      sim$pde <- lapply(UDlist, function(x) x$utility)
      sim$pdeMap <- NULL
    }
  }

  }
}
