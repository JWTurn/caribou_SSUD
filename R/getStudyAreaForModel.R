# match the jurisdictional model to the study area for that jurisdiction
getStudyAreaForModel <- function(modelName, studyArea, studyArea_juris) {
  if (tolower(modelName) %in% c("global", "all")) {
    return(studyArea)
  }

  key <- toupper(modelName)
  nm  <- toupper(names(studyArea_juris))

  if (!key %in% nm) {
    stop(
      "No jurisdiction-specific studyArea for model '", modelName,
      "'. Available: ", paste(names(studyArea_juris), collapse = ", ")
    )
  }

  studyArea_juris[[which(nm == key)[1]]]
}
