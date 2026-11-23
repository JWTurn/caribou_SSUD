# The forecast (dynamic mode) landscape layers, built from their names. A layer's name says what it
# is: <covariate>_<resolution m>[_adj], e.g. prop_forest_240, ts_fire_1200, dist_paved_240_adj; a
# name with no resolution (e.g. timeSinceFire) is on the sim grid.

# the patterns that send a layer to the module that forecasts it, by ecotype (dynamicLayers = NULL)
defaultDynamicLayers <- function(ecotype) {
  if (ecotype == "boreal") {
    list(forest = "^prop_(needleleaf|mixedforest)$", fire = "^timeSinceFire$",
         harvest = "^timeSinceHarvest$", disturbance = "^dist")
  } else {
    list(forest = "^prop_forest_", fire = "^ts_fire_", harvest = "^ts_harv(est)?_", disturbance = "^dist_")
  }
}

# splits layer names into the covariate (`stem`), the resolution in m (`res`; NA = the sim grid) and
# whether the layer is a capped distance (`adj`)
parseLayerName <- function(x) {
  base <- sub("_adj$", "", x)
  hasRes <- grepl("_[0-9]+$", base)
  data.frame(layer = x, stem = sub("_[0-9]+$", "", base),
             res = ifelse(hasRes, suppressWarnings(as.numeric(sub("^.*_", "", base))), NA_real_),
             adj = grepl("_adj$", x))
}

# the source that forecasts each layer, named by layer: the name of the first `dynamicLayers` pattern
# it matches, or NA (static, from modelLand). Stops for a layer that matches more than one pattern
layerSources <- function(layers, dynamicLayers) {
  if (!length(dynamicLayers)) return(setNames(rep(NA_character_, length(layers)), layers))
  if (is.null(names(dynamicLayers)) || any(!nzchar(names(dynamicLayers))))
    stop("dynamicLayers must be a named list: source = pattern, e.g. list(fire = \"^ts_fire_\")",
         call. = FALSE)

  hits <- vapply(dynamicLayers, function(p) grepl(p, layers), logical(length(layers)))
  hits <- matrix(hits, nrow = length(layers), dimnames = list(layers, names(dynamicLayers)))

  twice <- layers[rowSums(hits) > 1]
  if (length(twice))
    stop("These layers match more than one dynamicLayers pattern: ", paste(twice, collapse = ", "),
         call. = FALSE)

  setNames(apply(hits, 1, function(h) if (any(h)) names(dynamicLayers)[h] else NA_character_), layers)
}

# the moving-window radius (m) for each layer, named by layer; NA = no window. `movingWindow` is a
# table with columns `layers` (a pattern on layer names) and `radius` (m); a radius of NA takes
# `buffer` (as prepTracks makes it). Stops for a layer that matches more than one row, a row with no
# radius and no `buffer`, or a radius that differs from a supplied `buffer`, so the window cannot
# differ from the one other modules in the workflow use
windowRadii <- function(layers, movingWindow, buffer = NULL) {
  out <- setNames(rep(NA_real_, length(layers)), layers)
  if (is.null(movingWindow) || !NROW(movingWindow)) return(out)
  if (!is.data.frame(movingWindow) || !all(c("layers", "radius") %in% names(movingWindow)))
    stop("movingWindow must be a data.frame with columns `layers` (a pattern on layer names) and",
         " `radius` (m), e.g. data.frame(layers = \"^prop_.*_240$\", radius = 500)", call. = FALSE)

  radius <- as.numeric(movingWindow$radius)
  if (!is.null(buffer)) {
    if (length(buffer) != 1 || is.na(buffer))
      stop("buffer must be one number (m)", call. = FALSE)
    differ <- !is.na(radius) & radius != buffer
    if (any(differ))
      stop("movingWindow gives a radius of ", paste(unique(radius[differ]), collapse = ", "),
           " m, but buffer (from the workflow, e.g. prepTracks) is ", buffer, " m. Set the radius to",
           " NA to use buffer, so every module uses the same window.", call. = FALSE)
    radius[is.na(radius)] <- buffer
  } else if (anyNA(radius)) {
    stop("movingWindow has a row with no radius (NA = use buffer), but buffer is not supplied",
         call. = FALSE)
  }

  hits <- vapply(movingWindow$layers, function(p) grepl(p, layers), logical(length(layers)))
  hits <- matrix(hits, nrow = length(layers), dimnames = list(layers, NULL))
  twice <- layers[rowSums(hits) > 1]
  if (length(twice))
    stop("These layers match more than one movingWindow row: ", paste(twice, collapse = ", "),
         call. = FALSE)
  for (i in seq_len(ncol(hits))) out[hits[, i]] <- radius[i]
  out
}

# a grid-cell map at the resolution `res` (m): unchanged if `res` is NA or the grid's own; otherwise
# `fun` (na.rm; default the mean) of the grid cells in each res x res block, copied back onto every
# cell of the block. Blocks start at the coordinate origin, as modelLand's coarse layers do (hsaCEtool), so the
# grid's xmin and ymax must be multiples of `res`
toResolution <- function(x, res, fun = "mean") {
  cell <- terra::res(x)[1]
  if (is.na(res) || isTRUE(all.equal(res, cell))) return(x)

  k <- res / cell
  if (abs(k - round(k)) > 1e-8 || k < 1)
    stop("A ", res, " m layer cannot be built from the ", cell, " m grid: ", res,
         " is not a multiple of the grid's cell size", call. = FALSE)

  e <- terra::ext(x)
  if (abs(e$xmin %% res) > 1e-6 || abs(e$ymax %% res) > 1e-6)
    stop("The grid's corner (xmin ", e$xmin, ", ymax ", e$ymax, ") is not a multiple of ", res,
         " m, so its ", res, " m blocks would not line up with modelLand's", call. = FALSE)

  k <- round(k)
  coarse <- terra::aggregate(x, fact = k, fun = fun, na.rm = TRUE)
  fine <- terra::disagg(coarse, fact = k)
  terra::crop(fine, x, extend = TRUE)
}

# the mean of the cells within `radius` m of each cell (a circle; na.rm); NA cells stay NA
circleMean <- function(x, radius) {
  w <- terra::focalMat(x, radius, type = "circle")
  w[w > 0] <- 1
  terra::focal(x, w = w, fun = "mean", na.rm = TRUE, na.policy = "omit")
}

# a 0/1 grid-cell map of LandR forest: 1 where the cell's stand has more than `minBiomass` g/m2 of
# biomass and its forest type is one of `types` ("conifer", "deciduous", "mixed"); 0 elsewhere,
# including cells with no stand
landRForestCells <- function(cohortData, pixelGroupMap, types, minBiomass = 0,
                             sppEquiv = NULL, sppEquivCol = "LandR") {
  codes <- c(conifer = 210, deciduous = 220, mixed = 230)
  bad <- setdiff(types, names(codes))
  if (length(bad))
    stop("Unknown forest types: ", paste(bad, collapse = ", "), "; use conifer, deciduous or mixed",
         call. = FALSE)

  # reclassifyCohortData() reads the map's values under the name pixelGroup
  pixelGroupMap <- terra::deepcopy(pixelGroupMap)
  names(pixelGroupMap) <- "pixelGroup"
  # a copy: reclassifyCohortData() adds columns to cohortData by reference
  args <- list(cohortData = data.table::copy(data.table::as.data.table(cohortData)),
               sppEquivCol = sppEquivCol, pixelGroupMap = pixelGroupMap)
  if (!is.null(sppEquiv)) args$sppEquiv <- sppEquiv
  stands <- do.call(reclassifyCohortData, args)

  # AGB is in Mg/ha; 1 Mg/ha = 100 g/m2
  forest <- stands[["forest type"]] %in% codes[types] & stands[["AGB (Mg/ha)"]] * 100 > minBiomass
  forest <- terra::ifel(is.na(forest), 0, forest)
  names(forest) <- "forest"
  forest
}

# the forecast of one forest layer (e.g. prop_forest_240) from 0/1 LandR forest cell maps now and at
# the start of the simulation: at its resolution, then averaged in a circle of `radius` m if given (see
# windowRadii()). "change": modelLand's layer + (LandR now - LandR at the start), kept within 0-1;
# "LandR": LandR's value alone
forestLayer <- function(name, nowCells, startCells, modelLand, radius = NA, mode = "change") {
  res <- parseLayerName(name)$res
  share <- function(cells) {
    out <- toResolution(cells, res)
    if (!is.na(radius)) out <- circleMean(out, radius)
    out
  }

  now <- share(nowCells)
  out <- if (mode == "LandR") {
    now
  } else {
    if (is.null(modelLand[[name]]))
      stop("forestForecast 'change' needs ", name, " in modelLand", call. = FALSE)
    terra::clamp(modelLand[[name]] + now - share(startCells), 0, 1, values = TRUE)
  }
  if (!is.null(modelLand[[name]])) out <- terra::mask(out, modelLand[[name]])
  names(out) <- name
  out
}

# the forecast of time-since layers (years since fire or harvest) for `year`. The grid-cell event
# year comes from modelLand's layer of the same covariate on the grid (`baseLayer`; the event year is
# baselineYear minus its value), replaced by the year from `simTimeSince` (e.g. scfm's timeSinceFire:
# years since an event in the simulation, NA = none) where that is more recent and in baselineYear or
# later. Earlier sim years are not events of the simulation: scfmSpread's timeSinceFire may start from
# stand age (as in YT_caribou), which would read every stand as burned when it started. A cell has no event
# where `baseLayer` is NA and either `noEventYear` is NA and the value is `tsElse` or more, or the
# event year is `noEventYear` or earlier (a modelLand that stores "never" as years since a stand-in
# year, e.g. 1900). Coarser layers take, of the grid cells' event years (cells with no event left
# out), the most recent (`coarse` "recent") or the mean ("mean"). Then years since = year - event
# year; a cell with no event gets `tsElse`, or year - noEventYear when that is set
timeSinceLayers <- function(names, baseLayer, year, baselineYear, tsElse, simTimeSince = NULL,
                            coarse = "mean", noEventYear = NA) {
  fun <- switch(coarse, recent = "max", mean = "mean",
                stop("fireCoarse / harvestCoarse must be 'recent' or 'mean'", call. = FALSE))

  eventYear <- baselineYear - baseLayer
  noEvent <- if (is.na(noEventYear)) baseLayer >= tsElse else eventYear <= noEventYear
  eventYear <- terra::ifel(noEvent, NA, eventYear)

  if (!is.null(simTimeSince)) {
    simYear <- year - simTimeSince
    simYear <- terra::ifel(simYear < baselineYear, NA, simYear)
    eventYear <- terra::ifel(!is.na(simYear) & (is.na(eventYear) | simYear > eventYear),
                             simYear, eventYear)
  }

  fill <- if (is.na(noEventYear)) tsElse else year - noEventYear
  out <- lapply(names, function(n) {
    ts <- year - toResolution(eventYear, parseLayerName(n)$res, fun)
    ts <- terra::ifel(is.na(ts), fill, ts)
    names(ts) <- n
    ts
  })
  terra::rast(out)
}

# the modelLand layer a time-since covariate starts from: the one with the same covariate on the grid
# (its resolution = the grid's cell size, or none)
timeSinceBase <- function(stem, modelLand, cellSize) {
  info <- parseLayerName(names(modelLand))
  hit <- info$layer[info$stem == stem & !info$adj & (is.na(info$res) | info$res == cellSize)]
  if (!length(hit))
    stop("Forecasting ", stem, " layers needs ", stem, "_", cellSize, " (or ", stem,
         ") in modelLand: the event years on the grid", call. = FALSE)
  modelLand[[hit[1]]]
}

# the forecast landscape for `year`: every layer in `layers`, by its source (layerSources()). Static
# layers (no source, or a source whose objects the sim lacks) come from modelLand; `objs` holds the
# sim's objects (cohortData, pixelGroupMap, sppEquiv, timeSinceFire, forestBaseline,
# disturbanceList, buffer); `p` the module's parameters. Returns a SpatRaster with one layer per name
buildForecastLayers <- function(layers, sources, year, modelLand, grid, objs, p) {

  info <- parseLayerName(layers)
  cellSize <- terra::res(grid)[1]
  out <- list()

  static <- layers[is.na(sources)]

  # moving windows: only forest forecasts take one so far (fixed layers come from modelLand as built)
  radii <- windowRadii(layers, p$movingWindow, objs$buffer)
  unsupported <- layers[!is.na(radii) & !is.na(sources) & !sources %in% "forest"]
  if (length(unsupported))
    stop("Moving windows are not supported yet for ", paste(unique(sources[unsupported]), collapse = ", "),
         " layers: ", paste(unsupported, collapse = ", "), ". Change movingWindow so it matches only",
         " forest layers.", call. = FALSE)

  # forest, from LandR
  forest <- layers[sources %in% "forest"]
  if (length(forest)) {
    if (is.null(objs$cohortData) || is.null(objs$pixelGroupMap)) {
      message("No cohortData/pixelGroupMap in the sim: these forest layers stay as in modelLand: ",
              paste(forest, collapse = ", "))
      static <- c(static, forest)
    } else {
      checkOnGrid(objs$pixelGroupMap, grid, "pixelGroupMap")
      for (n in forest) {
        stem <- info$stem[info$layer == n]
        nowCells <- landRForestCells(objs$cohortData, objs$pixelGroupMap, forestTypesFor(stem, p$forestTypes),
                                     p$forestMinBiomass, objs$sppEquiv, p$sppEquivCol)
        startCells <- objs$forestBaseline[[stem]]
        if (p$forestForecast == "change" && is.null(startCells))
          stop("forestBaseline lacks ", stem, ", which forestForecast 'change' needs", call. = FALSE)
        out[[n]] <- forestLayer(n, nowCells, startCells, modelLand, radii[[n]], p$forestForecast)
      }
    }
  }

  # time since fire (scfm's timeSinceFire) and harvest (no forecaster yet: ages only)
  for (src in intersect(c("fire", "harvest"), sources)) {
    these <- layers[sources %in% src]
    simTs <- if (src == "fire") objs$timeSinceFire
    if (is.null(simTs)) {
      message("No ", if (src == "fire") "timeSinceFire (scfm)" else "harvest forecast",
              " in the sim: these layers only age (no new ", src, "s): ", paste(these, collapse = ", "))
    } else {
      checkOnGrid(simTs, grid, "timeSinceFire")
    }
    for (stem in unique(info$stem[info$layer %in% these])) {
      ns <- these[info$stem[match(these, info$layer)] == stem]
      out[ns] <- as.list(timeSinceLayers(ns, timeSinceBase(stem, modelLand, cellSize), year,
                                         p$baselineYear, p$ts_else, simTs,
                                         coarseRule(src, p),
                                         noEventYearFor(src, p$noEventYear)))
    }
  }

  # distances to human disturbance (anthroDisturbance_Generator's disturbanceList)
  dist <- layers[sources %in% "disturbance"]
  if (length(dist)) {
    if (is.null(objs$disturbanceList)) {
      message("No disturbanceList in the sim: these distance layers stay as in modelLand: ",
              paste(dist, collapse = ", "))
      static <- c(static, dist)
    } else {
      classes <- p$disturbanceClasses
      if (is.null(classes)) classes <- defaultDisturbanceClasses(p$ecotype)
      unlinked <- dist[!info$stem[match(dist, info$layer)] %in% names(classes)]
      if (length(unlinked)) {
        message("No disturbanceClasses for these distance layers, which stay as in modelLand: ",
                paste(unlinked, collapse = ", "))
        static <- c(static, unlinked)
      }
      for (stem in unique(info$stem[info$layer %in% setdiff(dist, unlinked)])) {
        ns <- dist[info$stem[match(dist, info$layer)] == stem]
        missing <- setdiff(ns, names(modelLand))
        if (length(missing))
          stop("modelLand lacks these distance layers the forecast starts from: ",
               paste(missing, collapse = ", "), call. = FALSE)
        d <- featureDistance(disturbanceFeatures(objs$disturbanceList, classes[[stem]]), grid,
                             p$distanceCellSize)
        out[ns] <- lapply(ns, function(n) distanceLayer(n, modelLand[[n]], d, p$distanceCap))
      }
    }
  }

  # sources this module cannot build yet
  other <- layers[!is.na(sources) & !sources %in% c("forest", "fire", "harvest", "disturbance")]
  if (length(other)) {
    message("These layers stay as in modelLand (no forecast for their source yet): ",
            paste(other, collapse = ", "))
    static <- c(static, other)
  }

  missing <- setdiff(static, names(modelLand))
  if (length(missing))
    stop("modelLand lacks these layers the forecast needs: ", paste(missing, collapse = ", "), call. = FALSE)
  out[static] <- modelLand[static]

  out <- terra::rast(unname(out[layers]))
  names(out) <- layers
  out
}

# how a source's ("fire", "harvest") coarser layers combine the grid cells' event years: the
# fireCoarse / harvestCoarse parameter, "mean" if not set
coarseRule <- function(src, p) {
  rule <- p[[paste0(src, "Coarse")]]
  if (is.null(rule)) "mean" else rule
}

# the stand-in year meaning "no event" in modelLand's time-since layers of a source ("fire",
# "harvest"), from the noEventYear parameter; NA if not set
noEventYearFor <- function(src, noEventYear) {
  y <- noEventYear[[src]]
  if (is.null(y)) NA_real_ else as.numeric(y)
}

# the LandR forest types counted for a forest covariate (e.g. prop_forest), from forestTypes
forestTypesFor <- function(stem, forestTypes) {
  types <- forestTypes[[stem]]
  if (is.null(types))
    stop("forestTypes does not say which LandR forest types count for ", stem, call. = FALSE)
  types
}

# stops if `x` is not on the forecast grid
checkOnGrid <- function(x, grid, what) {
  if (!isTRUE(terra::compareGeom(x, grid, stopOnError = FALSE, messages = FALSE)))
    stop(what, " is not on the forecast grid (rasterToMatch, or modelLand's): extent, rows and",
         " columns, cell size or coordinate system differ", call. = FALSE)
  invisible(TRUE)
}

# the LandR forest cell maps at the start of the simulation, one per forest covariate the forecast
# builds (e.g. prop_forest), for forestForecast "change"
makeForestBaseline <- function(stems, cohortData, pixelGroupMap, p, sppEquiv = NULL) {
  setNames(lapply(stems, function(s) {
    landRForestCells(cohortData, pixelGroupMap, forestTypesFor(s, p$forestTypes), p$forestMinBiomass,
                     sppEquiv, p$sppEquivCol)
  }), stems)
}

# the model covariates' anthroDisturbance_Generator classes ("sector/class" in disturbanceList), by
# ecotype (disturbanceClasses = NULL). Northern mountain: the Yukon disturbance categories the model
# was fit on, as YT_caribou links them to the Generator's classes; dist_lfother and dist_otherpoly
# have no Generator class. Boreal: none yet
defaultDisturbanceClasses <- function(ecotype) {
  if (ecotype == "boreal") return(list())
  list(dist_paved = "roads/paved", dist_unpaved = "roads/unpaved", dist_placer = "mining/placer",
       dist_quartz = "mining/quartz", dist_othermines = "mining/otherMines")
}

# the features of `classes` ("sector/class", one or more) in disturbanceList, as one SpatVector; NULL
# if there are none. Stops for a class disturbanceList lacks, or one that is not a vector layer
disturbanceFeatures <- function(disturbanceList, classes) {
  parts <- strsplit(classes, "/", fixed = TRUE)
  bad <- classes[lengths(parts) != 2]
  if (length(bad))
    stop("disturbanceClasses must name each class as 'sector/class' (e.g. 'roads/paved'), not: ",
         paste(bad, collapse = ", "), call. = FALSE)

  feats <- lapply(seq_along(classes), function(k) {
    x <- disturbanceList[[parts[[k]][1]]][[parts[[k]][2]]]
    if (is.null(x))
      stop("disturbanceList has no class ", classes[k], call. = FALSE)
    if (inherits(x, "sf")) x <- terra::vect(x)
    if (!inherits(x, "SpatVector"))
      stop("disturbanceList's ", classes[k], " is a ", class(x)[1], ", not a vector layer (SpatVector",
           " or sf): distances are measured to features", call. = FALSE)
    if (nrow(x)) x[, 0] else NULL
  })
  feats <- Filter(Negate(is.null), feats)
  if (!length(feats)) return(NULL)
  if (length(feats) > 1) {
    crs <- terra::crs(feats[[1]])
    feats <- lapply(feats, function(f) if (terra::same.crs(f, feats[[1]])) f else terra::project(f, crs))
  }
  do.call(rbind, unname(feats))
}

# the distance (m) from each grid cell to `features`, as hsaCEtool measures it: on a grid of
# `cellSize` m cells (the grid's cells split; the grid's own cells if `cellSize` is theirs), the
# distance from each cell to the nearest cell a feature touches, then the mean over each grid cell's
# fine cells. NA outside the grid's cells; NULL if there are no features
featureDistance <- function(features, grid, cellSize = 30) {
  if (is.null(features)) return(NULL)
  cell <- terra::res(grid)[1]
  k <- cell / cellSize
  if (k < 1 || abs(k - round(k)) > 1e-8)
    stop("distanceCellSize (", cellSize, " m) must divide the grid's ", cell, " m cells evenly",
         call. = FALSE)
  k <- round(k)

  if (!terra::same.crs(features, grid)) features <- terra::project(features, grid)
  fine <- if (k > 1) terra::disagg(grid[[1]], fact = k) else grid[[1]]
  hit <- terra::rasterize(features, terra::rast(fine), field = 1, touches = TRUE)
  if (all(is.na(terra::global(hit, "max", na.rm = TRUE)[[1]]))) return(NULL)
  d <- terra::distance(hit)
  # a grid with no values has no cells left out
  if (terra::hasValues(fine)) d <- terra::mask(d, fine)
  if (k > 1) d <- terra::aggregate(d, fact = k, fun = "mean", na.rm = TRUE)
  names(d) <- "distance"
  d
}

# the forecast of one distance layer (e.g. dist_paved_240_adj): the smaller of modelLand's layer
# (`base`) and the distance to the sim's features (`d`, from featureDistance(); NULL = none), at the
# layer's resolution; then, for an `_adj` layer, more than cap$beyond_km gives cap$set_to_km. Features
# are only ever added, so the nearest one is either modelLand's or one of the sim's
distanceLayer <- function(name, base, d, cap) {
  info <- parseLayerName(name)
  out <- base
  if (!is.null(d)) {
    simD <- terra::mask(toResolution(d, info$res), base)
    out <- terra::ifel(!is.na(simD) & simD < base, simD, base)
  }
  if (info$adj) out <- capDistance(out, cap)
  names(out) <- name
  out
}

# capped distances: more than cap$beyond_km km gives cap$set_to_km km
capDistance <- function(x, cap) {
  if (is.null(cap$beyond_km) || is.null(cap$set_to_km))
    stop("distanceCap must be a list with beyond_km and set_to_km", call. = FALSE)
  terra::ifel(x > cap$beyond_km * 1000, cap$set_to_km * 1000, x)
}

# at the start of a simulation, the sim's features of each category should all be in modelLand's,
# so their distances should be no shorter than modelLand's. Returns, for each distance layer where
# they are more than `tolerance` m shorter in some cells, the share of cells (named by layer)
disturbanceMismatch <- function(layers, modelLand, grid, disturbanceList, classes, p,
                                tolerance = terra::res(grid)[1]) {
  info <- parseLayerName(layers)
  out <- c()
  for (stem in intersect(unique(info$stem), names(classes))) {
    d <- featureDistance(disturbanceFeatures(disturbanceList, classes[[stem]]), grid, p$distanceCellSize)
    if (is.null(d)) next
    for (n in info$layer[info$stem == stem & info$layer %in% names(modelLand)]) {
      simD <- toResolution(d, parseLayerName(n)$res)
      if (parseLayerName(n)$adj) simD <- capDistance(simD, p$distanceCap)
      shorter <- terra::global(simD < modelLand[[n]] - tolerance, "mean", na.rm = TRUE)[[1]]
      if (isTRUE(shorter > 0)) out[n] <- shorter
    }
  }
  out
}
