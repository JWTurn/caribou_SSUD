# match the jurisdictional model to the study area for that jurisdiction
getStudyAreaForModel <- function(modelName, studyArea_juris, jurisdiction) {

  nm <- toupper(names(studyArea_juris))
  juris_param <- toupper(jurisdiction)

  # global model
  if (toupper(modelName) == "GLOBAL") {

    if (length(juris_param) != 1) {
      stop("Global model requires a single jurisdiction when using jurisdiction-only workflow.")
    }

    if (!juris_param %in% nm) {
      stop("Jurisdiction '", juris_param, "' not found in studyArea_juris")
    }

    return(studyArea_juris[[which(nm == juris_param)[1]]])
  }

  # jurisdiction model
  key <- toupper(modelName)

  if (!key %in% nm) {
    stop(
      "No studyArea_juris match for model '", modelName,
      "'. Available: ", paste(names(studyArea_juris), collapse = ", ")
    )
  }

  return(studyArea_juris[[which(nm == key)[1]]])
}
