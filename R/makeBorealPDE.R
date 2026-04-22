#' @title make Boreal caribou PDEs across scales and jurisdictions
#' @export
#' @author Julie W. Turner
#'
#'

makeBorealPDE <- function(mods, studyAreaJuris, juris, envLayers, normalizePDE, simulationScale) {
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
