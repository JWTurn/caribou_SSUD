# the landscape layers that stay fixed in dynamic-mode forecasts, taken from modelLand and named as
# the forecast reads them; stops naming any layer modelLand lacks
makeFixedSSUD <- function(modelLand, ecotype) {

  # names in the forecast = names in modelLand
  fixedLayers <- if (ecotype == "boreal") {
    c(prop_veg = "prop_veg", prop_wets = "prop_wets", distpaved = "distpaved",
      distunpaved = "distunpaved", distpolys = "distpolys", timeSinceHarvest0 = "timeSinceHarvest")
  } else {
    c(prop_veg_250 = "prop_veg_250", dist_lf_250 = "dist_lf_250", dist_poly_250 = "dist_poly_250")
  }

  missing <- setdiff(fixedLayers, names(modelLand))

  if (length(missing)) {
    stop("Dynamic mode needs these layers, which modelLand lacks: ", paste(missing, collapse = ", "),
         if (ecotype == "northern_mountain") {
           paste0(". The northern mountain forecast layers are those of the earlier model (at 250 m);",
                  " a model with other covariates needs them updated.")
         })
  }

  fixed <- terra::rast(lapply(fixedLayers, function(n) modelLand[[n]]))
  names(fixed) <- names(fixedLayers)

  return(fixed)
}
