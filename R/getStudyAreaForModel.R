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

# the chosen models, named as in pdeModelNames(): from iSSAmodels for the boreal ecotype, and the
# coefficients table for the northern mountain ecotype, which has only a global model for now
pdeModels <- function(modelNames, ecotype, iSSAmodels = NULL, iSSAcoefficients = NULL) {

  if (ecotype == "northern_mountain") {
    if (!identical(modelNames, "global")) {
      stop("The northern mountain ecotype has only a global model for now: use simulationScale = 'global'")
    }
    return(list(global = iSSAcoefficients))
  }

  found <- match(toupper(modelNames), toupper(names(iSSAmodels)))

  if (anyNA(found)) {
    stop(
      "iSSAmodels has no model for: ", paste(modelNames[is.na(found)], collapse = ", "),
      ". Available: ", paste(names(iSSAmodels), collapse = ", ")
    )
  }

  setNames(iSSAmodels[found], modelNames)
}

# match a model to the area its PDE covers: the global model covers studyAreaCaribou, and each
# regional model the area of the same name in studyArea_juris
getStudyAreaForModel <- function(modelName, studyArea_juris, studyAreaCaribou) {

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
