# Landscape composition for baseline
composeLandscape <- function(landscapeYearly, landscape5Yearly) {

  # latest annual rasters
  yLatest <- max(as.integer(gsub("\\D", "", names(landscapeYearly))), na.rm = TRUE)
  nat <- landscapeYearly[[paste0("year", yLatest)]]
  if (is.null(nat) || !inherits(nat, "SpatRaster"))
    stop("landscapeYearly[['year", yLatest, "']] is not a SpatRaster")

  # latest 5-year vectors
  iLatest <- max(as.integer(gsub("\\D", "", names(landscape5Yearly))), na.rm = TRUE)
  anth <- landscape5Yearly[[paste0("intYear", iLatest)]]
  if (is.null(anth) || !(inherits(anth, "SpatVectorCollection") || is.list(anth)))
    stop("landscape5Yearly[['intYear", iLatest, "']] is not a SpatVectorCollection or list")

  paved   <- anth[["paved"]]
  unpaved <- anth[["unpaved"]]
  polys   <- anth[["polys"]]

  if (!inherits(paved, "SpatVector"))   stop("Missing/invalid 'paved' SpatVector in intYear", iLatest)
  if (!inherits(unpaved, "SpatVector")) stop("Missing/invalid 'unpaved' SpatVector in intYear", iLatest)
  if (!inherits(polys, "SpatVector"))   stop("Missing/invalid 'polys' SpatVector in intYear", iLatest)

  # combined covariates to match iSSA
  prop_veg         <- nat[["prop_herbs"]] + nat[["prop_shrub"]] + nat[["prop_bryoids"]]
  prop_wets        <- nat[["prop_wetland"]]
  prop_needleleaf  <- nat[["prop_needleleaf"]] + nat[["prop_wet_treed"]]
  prop_mixedforest <- nat[["prop_mixed"]] + nat[["prop_deciduous"]]

  names(prop_veg)         <- "prop_veg"
  names(prop_wets)        <- "prop_wets"
  names(prop_needleleaf)  <- "prop_needleleaf"
  names(prop_mixedforest) <- "prop_mixedforest"

  # distance rasters
  template <- nat[[1]]
  makeDist <- function(template, v) {
    r <- terra::rasterize(v, template, field = 1, touches = TRUE, background = NA)
    d <- terra::distance(r)  # distance to nearest non-NA cell
    d
  }
  distpaved   <- makeDist(template, paved);   names(distpaved)   <- "distpaved"
  distunpaved <- makeDist(template, unpaved); names(distunpaved) <- "distunpaved"
  distpolys   <- makeDist(template, polys);   names(distpolys)   <- "distpolys"

  tsf <- nat[["timeSinceFire"]];    names(tsf) <- "timeSinceFire"
  tsh <- nat[["timeSinceHarvest"]]; names(tsh) <- "timeSinceHarvest"

  land <- c(prop_veg, prop_wets, prop_needleleaf, prop_mixedforest,
            tsf, tsh, distpaved, distunpaved, distpolys)

  # environment for make_pde() eval()
  list2env(setNames(lapply(names(land), \(n) land[[n]]), names(land)), parent = baseenv())

}
