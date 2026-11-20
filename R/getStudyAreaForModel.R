# the models to predict, by name: "global" and/or one per region in `jurisdiction`
pdeModelNames <- function(simulationScale, jurisdiction) {

  if (identical(simulationScale, "jurisdictional")) {
    message("simulationScale = 'jurisdictional' is now called 'regional'")
    simulationScale <- "regional"
  }

  if (length(simulationScale) != 1 || !simulationScale %in% c("global", "regional", "both")) {
    stop("simulationScale must be 'global', 'regional' or 'both'")
  }

  c(if (simulationScale %in% c("global", "both")) "global",
    if (simulationScale %in% c("regional", "both")) jurisdiction)
}

# the chosen models, named as in pdeModelNames(), from `iSSAmodels`: a list of models by name (e.g.
# global, NT), each a fitted model (glmmTMB) or a table of its fixed-effect coefficients (see
# coef_tab()). One model or table on its own, or a list of one unnamed, is the global model.
# `iSSAcoefficients` (one table, the global model) is the older way to supply a table, used when
# `iSSAmodels` is not supplied
pdeModels <- function(modelNames, iSSAmodels = NULL, iSSAcoefficients = NULL) {

  if (is.null(iSSAmodels) && !is.null(iSSAcoefficients)) iSSAmodels <- iSSAcoefficients
  if (is.object(iSSAmodels)) iSSAmodels <- list(global = iSSAmodels)  # a fitted model or a table
  if (!is.list(iSSAmodels) || !length(iSSAmodels)) {
    stop("iSSAmodels must be a list of models by name, e.g. list(global = <fitted model or coefficient",
         " table>)", call. = FALSE)
  }
  if (length(iSSAmodels) == 1L && is.null(names(iSSAmodels))) names(iSSAmodels) <- "global"

  found <- match(toupper(modelNames), toupper(names(iSSAmodels)))

  if (anyNA(found)) {
    stop(
      "iSSAmodels has no model for: ", paste(modelNames[is.na(found)], collapse = ", "),
      ". Available: ", paste(names(iSSAmodels), collapse = ", ")
    )
  }

  setNames(iSSAmodels[found], modelNames)
}

# the choices of `pdeArea`, the area each PDE covers: the first two for the global model, the last two
# for regional models
pdeAreaChoices <- list(
  global = c("fullArea", "fullReportingArea"),
  regional = c("regionalArea", "regionalReportingArea")
)

# checks `pdeArea` for the models run (named as in pdeModelNames()): one choice for each scale run, and
# studyAreaReporting when a chosen area needs it (`hasReporting`)
checkPdeArea <- function(pdeArea, modelNames, hasReporting) {

  bad <- setdiff(pdeArea, unlist(pdeAreaChoices))
  if (length(bad)) {
    stop("pdeArea must be among ", paste0("'", unlist(pdeAreaChoices), "'", collapse = ", "), "; not: ",
         paste(bad, collapse = ", "), call. = FALSE)
  }

  scales <- c(if ("global" %in% modelNames) "global", if (any(modelNames != "global")) "regional")
  for (sc in scales) {
    chosen <- intersect(pdeArea, pdeAreaChoices[[sc]])
    if (length(chosen) != 1) {
      stop("pdeArea needs one of ", paste0("'", pdeAreaChoices[[sc]], "'", collapse = " or "),
           " for the ", sc, " model", if (sc == "regional") "s", call. = FALSE)
    }
    if (grepl("Reporting", chosen) && !hasReporting) {
      stop("pdeArea = '", chosen, "' needs studyAreaReporting, which was not supplied", call. = FALSE)
    }
  }

  invisible(TRUE)
}

# match a model to the area its PDE covers, as `pdeArea` picks (see checkPdeArea()): for the global
# model, "fullArea" = studyAreaCaribou, "fullReportingArea" = studyAreaReporting; for a regional
# model, "regionalArea" = the area of the same name in studyArea_juris, "regionalReportingArea" = the
# part of studyAreaReporting inside that area
getStudyAreaForModel <- function(modelName, studyArea_juris, studyAreaCaribou, studyAreaReporting = NULL,
                                 pdeArea = c("fullArea", "regionalArea")) {

  isGlobal <- toupper(modelName) == "GLOBAL"
  checkPdeArea(pdeArea, if (isGlobal) "global" else modelName, !is.null(studyAreaReporting))
  chosen <- intersect(pdeArea, pdeAreaChoices[[if (isGlobal) "global" else "regional"]])

  if (chosen == "fullReportingArea") {
    return(asSpatVector(studyAreaReporting))
  }

  modelArea <- modelOwnArea(modelName, studyArea_juris, studyAreaCaribou)

  if (chosen %in% c("fullArea", "regionalArea")) {
    return(modelArea)
  }

  # regionalReportingArea
  modelArea <- asSpatVector(modelArea)
  reporting <- terra::project(asSpatVector(studyAreaReporting), terra::crs(modelArea))
  # terra warns when there is no overlap; the stop below says so
  overlap <- suppressWarnings(terra::intersect(terra::aggregate(modelArea), terra::aggregate(reporting)))

  if (!nrow(overlap) || all(terra::expanse(overlap) == 0)) {
    stop("The area of model '", modelName, "' does not overlap studyAreaReporting")
  }

  overlap
}

# the area a model was made for: studyAreaCaribou for the global model, and the area of the same name
# in studyArea_juris for a regional model
modelOwnArea <- function(modelName, studyArea_juris, studyAreaCaribou) {

  # global model
  if (toupper(modelName) == "GLOBAL") {

    if (is.null(studyAreaCaribou)) {
      stop("The global model needs studyAreaCaribou")
    }

    return(studyAreaCaribou)
  }

  # regional model
  nm <- toupper(names(studyArea_juris))
  key <- toupper(modelName)

  if (!key %in% nm) {
    stop(
      "No studyArea_juris match for model '", modelName,
      "'. Available: ", paste(names(studyArea_juris), collapse = ", ")
    )
  }

  return(studyArea_juris[[which(nm == key)[1]]])
}

# an area as a SpatVector (e.g. from an sf object)
asSpatVector <- function(x) {
  if (inherits(x, "SpatVector")) x else terra::vect(x)
}
