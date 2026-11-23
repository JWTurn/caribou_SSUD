testthat::local_edition(3)

## simInit() reads the module's metadata and runs .inputObjects(), not the module's events.

## A module path holding this module, temporary SpaDES paths, and, at the end of the calling test,
## the packages, options and terra temporary folder simInit() changes put back as they were.
local_spades_sandbox <- function(env = parent.frame()) {
  attached <- search()
  withr::defer(
    for (p in setdiff(search(), attached)) detach(p, character.only = TRUE),
    envir = env
  )
  opts <- options()
  withr::defer(
    {
      added <- setdiff(names(options()), names(opts))
      options(opts)
      options(stats::setNames(vector("list", length(added)), added))
    },
    envir = env
  )
  terraTmp <- terra::terraOptions(print = FALSE)$tempdir
  withr::defer(terra::terraOptions(tempdir = terraTmp), envir = env)
  options(spades.useRequire = FALSE, reproducible.useCache = FALSE, spades.moduleCodeChecks = FALSE)

  mods <- withr::local_tempdir(.local_envir = env)
  file.symlink(normalizePath(testthat::test_path("..", "..")), file.path(mods, "caribou_SSUD"))
  list(modulePath = mods, other = withr::local_tempdir(.local_envir = env))
}

## Module code finds prepInputs() in the attached reproducible package, so the mock goes there. It
## takes its arguments by name: prepInputs() never evaluates `fun` (e.g. load(targetFile)) itself.
local_prepInputs_mock <- function(mock, env = parent.frame()) {
  if (!"package:reproducible" %in% search()) {
    suppressPackageStartupMessages(library(reproducible))
  }
  pkg <- as.environment("package:reproducible")
  real <- get("prepInputs", envir = pkg)
  unlockBinding("prepInputs", pkg)
  assign("prepInputs", mock, envir = pkg)
  withr::defer(
    {
      assign("prepInputs", real, envir = pkg)
      lockBinding("prepInputs", pkg)
    },
    envir = env
  )
}

run_simInit <- function(paths, objects, params = list(), ..., times = list(start = 2025, end = 2025)) {
  suppressMessages(SpaDES.core::simInit(
    ...,
    times = times,
    params = list(caribou_SSUD = utils::modifyList(
      list(ecotype = "northern_mountain", .studyAreaName = "test", .useCache = FALSE), params
    )),
    modules = "caribou_SSUD",
    paths = list(
      modulePath = paths$modulePath,
      inputPath = paths$other,
      outputPath = paths$other,
      cachePath = paths$other,
      scratchPath = paths$other
    ),
    objects = objects
  ))
}

test_that("the module's metadata parses, with the inputs .inputObjects() fetches", {
  skip_if_not_installed("SpaDES.core")
  paths <- local_spades_sandbox()

  md <- SpaDES.core::moduleMetadata(module = "caribou_SSUD", path = paths$modulePath)
  expect_contains(md$inputObjects$objectName, c("iSSAmodels", "modelLand"))
  expect_contains(md$parameters$paramName, c("ecotype", "outputFolderID", "normalizePDE", "pdeArea"))
  expect_contains(md$inputObjects$objectName, c("rasterToMatch", "studyAreaReporting"))
  expect_false("rasterToMatch_SSUD" %in% md$inputObjects$objectName)
  expect_equal(md$parameters$default[[which(md$parameters$paramName == "ts_else")]], 200)
})

test_that("simInit() takes supplied inputs as they are and downloads nothing", {
  skip_if_not_installed("SpaDES.core")
  paths <- local_spades_sandbox()
  calls <- character()
  local_prepInputs_mock(function(targetFile = NULL, url = NULL, ...) {
    calls <<- c(calls, targetFile)
    stop("download attempted: ", targetFile)
  })
  fit <- fixture_fit()
  land <- as.list(fixture_envlayers())[c("x", "y")]
  juris <- list(NTYT = fixture_area())

  sim <- run_simInit(paths, list(iSSAmodels = list(fit), modelLand = land, studyArea_juris = juris,
                                 studyAreaCaribou = fixture_area()))
  expect_length(calls, 0)
  expect_identical(sim$iSSAmodels, list(fit))
  expect_named(sim$modelLand, c("x", "y"))
  expect_equal(terra::values(sim$modelLand$x), terra::values(land$x))
  expect_named(sim$studyArea_juris, "NTYT")
})

test_that("simInit() downloads only a missing input, from outputFolderID", {
  skip_if_not_installed("SpaDES.core")
  paths <- local_spades_sandbox()
  calls <- list()
  local_prepInputs_mock(function(targetFile = NULL, url = NULL, ...) {
    calls[[length(calls) + 1L]] <<- list(targetFile = targetFile, url = url)
    if (!identical(targetFile, "modelLand.tif")) stop("unexpected download: ", targetFile)
    r <- fixture_envlayers()
    stats::setNames(c(r$x, r$y), c("x", "y"))
  })
  folder <- "https://drive.google.com/drive/folders/aTestFolder"

  sim <- run_simInit(
    paths,
    list(iSSAmodels = list(fixture_fit()), studyArea_juris = list(NTYT = fixture_area()),
         studyAreaCaribou = fixture_area()),
    params = list(outputFolderID = folder)
  )
  expect_identical(calls, list(list(targetFile = "modelLand.tif", url = folder)))
  expect_named(sim$modelLand, c("x", "y"))
  expect_equal(as.vector(terra::values(sim$modelLand[[2]])), c(0, 1, 0, 1))
})

## the inputs a northern mountain run needs, all supplied
nm_objects <- function() {
  list(iSSAcoefficients = coef_tab(fixture_coefs()), modelLand = as.list(fixture_envlayers())[c("x", "y")],
       studyAreaCaribou = fixture_area())
}

test_that("simInit() makes the baseline when not supplied, and writes no files (init saves it)", {
  skip_if_not_installed("SpaDES.core")
  paths <- local_spades_sandbox()

  sim <- run_simInit(paths, nm_objects())
  made <- makePDEs(list(global = fixture_coefs()), fixture_envlayers(), studyAreaCaribou = fixture_area())
  expect_named(sim$pde, "global")
  expect_equal(terra::values(sim$pde$global), terra::values(made$pde$global))
  expect_equal(terra::values(sim$pdeMap$global), terra::values(made$pdeMap$global))
  ## a cache hit for .inputObjects() would not write them
  expect_false(dir.exists(file.path(paths$other, "pde_test")))
})

test_that("simInit() takes a supplied baseline as it is, and makes and saves nothing", {
  skip_if_not_installed("SpaDES.core")
  paths <- local_spades_sandbox()
  pde <- list(global = terra::rast(fixture_envlayers()$x, vals = 0.25))

  sim <- run_simInit(paths, c(nm_objects(), list(pde = pde)))
  expect_equal(terra::values(sim$pde$global), terra::values(pde$global))
  expect_null(sim$pdeMap)
  expect_false(dir.exists(file.path(paths$other, "pde_test")))
})

test_that("simInit() reads a premade baseline from a URL in baselineSource, and saves nothing", {
  skip_if_not_installed("SpaDES.core")
  paths <- local_spades_sandbox()
  premade <- withr::local_tempdir()
  makePDEs(list(global = fixture_coefs()), fixture_envlayers(), studyAreaCaribou = fixture_area(),
           savePath = premade)
  url <- "https://drive.google.com/file/d/aTestBaseline"
  calls <- list()
  local_prepInputs_mock(function(targetFile = NULL, url = NULL, destinationPath = NULL, fun = NULL, ...) {
    calls[[length(calls) + 1L]] <<- list(url = url, fun = fun)
    dir.create(destinationPath, recursive = TRUE, showWarnings = FALSE)
    file.copy(list.files(premade, full.names = TRUE), destinationPath)
    NULL
  })

  sim <- run_simInit(paths, nm_objects(), params = list(baselineSource = url))
  expect_identical(calls, list(list(url = url, fun = NA)))
  expect_equal(terra::values(sim$pde$global), terra::values(terra::rast(file.path(premade, "pde_global.tif"))))
  expect_false(dir.exists(file.path(paths$other, "pde_test")))
})

## a dynamic northern mountain run's inputs on a 240 m grid of 10 x 10 cells: a model reading forest,
## fire and vegetation layers; LandR's stands (one conifer stand everywhere but the first cell) and
## scfm's time since fire (cell 2 burned 2 years before)
dyn_objects <- function() {
  g <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 2400, ymin = 0, ymax = 2400, crs = "EPSG:3154")
  ts <- terra::rast(g, vals = NA_real_)
  ts[1] <- 10
  pgm <- terra::rast(g, vals = 1L)
  pgm[1] <- NA
  tsf <- terra::rast(g, vals = NA_real_)
  tsf[2] <- 2
  list(
    iSSAcoefficients = data.table::data.table(term = c("prop_forest_240_end", "ts_fire_240_end", "prop_veg_240_end"),
                                              estimate = c(1, -0.01, 0.5)),
    modelLand = list(prop_forest_240 = terra::rast(g, vals = 0.5), ts_fire_240 = ts,
                     prop_veg_240 = terra::rast(g, vals = 0.3)),
    studyAreaCaribou = terra::as.polygons(terra::ext(g), crs = terra::crs(g)),
    rasterToMatch = g,
    cohortData = data.table::data.table(pixelGroup = 1L, speciesCode = "Pice_mar", B = 100L, age = 50L),
    pixelGroupMap = pgm,
    sppEquiv = data.table::data.table(LandR = c("Pice_mar", "Popu_tre"), Type = c("Conifer", "Deciduous")),
    timeSinceFire = tsf
  )
}

test_that("in dynamic mode, simInit() keeps fixed the layers no dynamicLayers pattern matches", {
  skip_if_not_installed("SpaDES.core")
  paths <- local_spades_sandbox()

  sim <- run_simInit(paths, dyn_objects(), params = list(simulationProcess = "dynamic"))
  expect_named(sim$fixedSSUD, "prop_veg_240")

  sim <- run_simInit(paths, dyn_objects(), params = list(simulationProcess = "dynamic", dynamicLayers = list()))
  expect_named(sim$fixedSSUD, c("prop_forest_240", "ts_fire_240", "prop_veg_240"))
})

test_that("a dynamic run forecasts the forest and fire layers from LandR and scfm", {
  skip_if_not_installed("SpaDES.core")
  paths <- local_spades_sandbox()

  sim <- suppressMessages(SpaDES.core::spades(
    run_simInit(paths, dyn_objects(), params = list(simulationProcess = "dynamic", predictStartYear = 2030,
                                                    predictionInterval = 5),
                times = list(start = 2025, end = 2030)),
    debug = FALSE
  ))
  expect_named(sim$forestBaseline, "prop_forest")
  expect_named(sim$simPde, "year2030")
  expect_named(sim$simPde$year2030, "global")

  land <- terra::rast(file.path(paths$other, "pde_test", "sims", "pdeLayers_test_year2030.tif"))
  expect_named(land, c("prop_forest_240", "ts_fire_240", "prop_veg_240"))
  ## no change in LandR's stands since the start: modelLand's forest share
  expect_equal(land$prop_forest_240[1][[1]], 0.5)
  ## burned in 2015 (modelLand), 2028 (scfm), never
  expect_equal(unlist(land$ts_fire_240[1:3]), c(15, 2, 200), ignore_attr = TRUE)
  ## the file holds 32-bit floats
  expect_equal(land$prop_veg_240[1][[1]], 0.3, tolerance = 1e-6)
})

test_that("a dynamic run measures distances to disturbanceList's features, and warns of a mismatch at the start", {
  skip_if_not_installed("SpaDES.core")
  paths <- local_spades_sandbox()
  objs <- dyn_objects()
  objs$iSSAcoefficients <- rbind(objs$iSSAcoefficients,
                                 data.table::data.table(term = "dist_paved_240_adj_end", estimate = -0.0001))
  objs$modelLand$dist_paved_240_adj <- terra::rast(objs$rasterToMatch, vals = 21000)
  ## a paved road at x = 100 m, which modelLand does not have: the start-of-run check warns
  objs$disturbanceList <- list(roads = list(paved = terra::vect("LINESTRING (100 0, 100 2400)", crs = "EPSG:3154")))

  expect_warning(
    sim <- suppressMessages(SpaDES.core::spades(
      run_simInit(paths, objs, params = list(simulationProcess = "dynamic", predictStartYear = 2030),
                  times = list(start = 2025, end = 2030)),
      debug = FALSE
    )),
    "shorter than modelLand's.*dist_paved_240_adj"
  )
  land <- terra::rast(file.path(paths$other, "pde_test", "sims", "pdeLayers_test_year2030.tif"))
  ## on 30 m cells the road touches the 4th of column 1's eight: their mean distance is 60 m, and
  ## column 2's (the 5th to 12th from the road) 255 m
  expect_equal(unlist(land$dist_paved_240_adj[1, 1:2]), c(60, 255), ignore_attr = TRUE)
})

test_that("simInit() says what to supply when outputFolderID lacks modelLand.tif", {
  skip_if_not_installed("SpaDES.core")
  paths <- local_spades_sandbox()
  local_prepInputs_mock(function(...) stop("File not found"))

  objs <- nm_objects()
  objs$modelLand <- NULL
  expect_error(run_simInit(paths, objs), "Could not download modelLand.tif.*File not found.*Supply modelLand")
})

test_that("a dynamic run with a season model makes the season and full-year maps, for the baseline and forecasts", {
  skip_if_not_installed("SpaDES.core")
  paths <- local_spades_sandbox()
  objs <- dyn_objects()
  objs$iSSAcoefficients <- data.table::data.table(
    term = c("seasonwinter:prop_forest_240_end", "seasonsummer:prop_forest_240_end", "ts_fire_240_end",
             "seasonwinter:prop_veg_240_end", "seasonsummer:prop_veg_240_end"),
    estimate = c(1, -1, -0.01, 0.5, 0.2)
  )
  objs$iSSAseasons <- data.table::data.table(season = c("winter", "summer"), days = c(200L, 165L))

  sim <- suppressMessages(SpaDES.core::spades(
    run_simInit(paths, objs, params = list(simulationProcess = "dynamic", predictStartYear = 2030,
                                           predictionInterval = 5, seasonMaps = "winter"),
                times = list(start = 2025, end = 2030)),
    debug = FALSE
  ))
  expect_named(sim$pde, c("global", "global_winter"))
  expect_named(sim$simPde$year2030, c("global", "global_winter"))
  expect_true(all(file.exists(file.path(paths$other, "pde_test", "sims",
                                        paste0("pde_", c("global", "global_winter"), "_year2030.tif")))))
})

test_that("a dynamic run takes modelLand as one multi-layer SpatRaster, as a list of its layers", {
  skip_if_not_installed("SpaDES.core")
  paths <- local_spades_sandbox()
  objs <- dyn_objects()
  objs$modelLand <- terra::rast(objs$modelLand)

  sim <- suppressMessages(SpaDES.core::spades(
    run_simInit(paths, objs, params = list(simulationProcess = "dynamic", predictStartYear = 2030,
                                           predictionInterval = 5),
                times = list(start = 2025, end = 2030)),
    debug = FALSE
  ))
  expect_type(sim$modelLand, "list")
  expect_named(sim$modelLand, c("prop_forest_240", "ts_fire_240", "prop_veg_240"))
  expect_named(sim$fixedSSUD, "prop_veg_240")
  expect_named(sim$simPde$year2030, "global")
})

test_that("a static run makes only the baseline, and schedules no forecast", {
  skip_if_not_installed("SpaDES.core")
  paths <- local_spades_sandbox()

  sim <- suppressMessages(SpaDES.core::spades(
    run_simInit(paths, nm_objects(), params = list(simulationProcess = "static")),
    debug = FALSE
  ))
  done <- SpaDES.core::completed(sim)
  expect_identical(setdiff(done[moduleName == "caribou_SSUD", eventType], ".inputObjects"), "init")
  expect_named(sim$pde, "global")
  expect_null(sim$simPde)
})

test_that("simInit() stops when pdeArea needs studyAreaReporting and none is supplied", {
  skip_if_not_installed("SpaDES.core")
  paths <- local_spades_sandbox()

  expect_error(run_simInit(paths, nm_objects(), params = list(pdeArea = "fullReportingArea")),
               "needs studyAreaReporting")
})

test_that("simInit() makes the baseline over the area pdeArea picks", {
  skip_if_not_installed("SpaDES.core")
  paths <- local_spades_sandbox()

  sim <- run_simInit(paths, c(nm_objects(), list(studyAreaReporting = fixture_area(xmin = 1))),
                     params = list(pdeArea = "fullReportingArea"))
  expect_equal(as.vector(is.na(terra::values(sim$pde$global))), c(TRUE, FALSE, TRUE, FALSE))
})

test_that("a static run stops naming a layer the model reads that modelLand lacks", {
  skip_if_not_installed("SpaDES.core")
  paths <- local_spades_sandbox()
  objs <- nm_objects()
  objs$modelLand$y <- NULL

  expect_error(run_simInit(paths, objs), "'global'.*: y\\. Supply the modelLand")
})

test_that("a static run registers the baseline files it wrote as saved outputs, even with simInit() outputs", {
  skip_if_not_installed("SpaDES.core")
  paths <- local_spades_sandbox()

  # simInit() replaces outputs(sim) with its `outputs` after .inputObjects(), where the baseline is made
  sim <- suppressMessages(SpaDES.core::spades(
    run_simInit(paths, nm_objects(), params = list(simulationProcess = "static"),
                outputs = data.frame(objectName = "iSSAcoefficients", saveTime = 2025)),
    debug = FALSE
  ))
  out <- SpaDES.core::outputs(sim)
  base <- out[out$objectName %in% c("pde", "pdeMap"), ]

  expect_setequal(basename(base$file), c("pde_global.tif", "pdeMap_global.tif"))
  expect_identical(base$objectName[basename(base$file) == "pde_global.tif"], "pde")
  expect_true(all(base$saved))
  expect_true(all(file.exists(base$file)))
  expect_identical(unique(base$package), "terra")
  expect_identical(unique(base$fun), "writeRaster")
})

test_that("a supplied baseline registers no baseline files", {
  skip_if_not_installed("SpaDES.core")
  paths <- local_spades_sandbox()
  pde <- list(global = terra::rast(fixture_envlayers()$x, vals = 0.25))

  sim <- suppressMessages(SpaDES.core::spades(
    run_simInit(paths, c(nm_objects(), list(pde = pde)), params = list(simulationProcess = "static")),
    debug = FALSE
  ))
  expect_false(any(SpaDES.core::outputs(sim)$objectName %in% c("pde", "pdeMap")))
  expect_false(dir.exists(file.path(paths$other, "pde_test")))
})

test_that("a premade baseline (baselineSource) is not saved or registered", {
  skip_if_not_installed("SpaDES.core")
  paths <- local_spades_sandbox()
  premade <- withr::local_tempdir()
  makePDEs(list(global = fixture_coefs()), fixture_envlayers(), studyAreaCaribou = fixture_area(),
           savePath = premade)
  local_prepInputs_mock(function(destinationPath = NULL, ...) {
    dir.create(destinationPath, recursive = TRUE, showWarnings = FALSE)
    file.copy(list.files(premade, full.names = TRUE), destinationPath)
    NULL
  })

  sim <- suppressMessages(SpaDES.core::spades(
    run_simInit(paths, nm_objects(), params = list(simulationProcess = "static",
                                                    baselineSource = "https://drive.google.com/file/d/aTestBaseline")),
    debug = FALSE
  ))
  expect_false(any(SpaDES.core::outputs(sim)$objectName %in% c("pde", "pdeMap")))
  expect_false(dir.exists(file.path(paths$other, "pde_test")))
})

test_that("with .inputObjects() cached, a cache hit still saves and registers the baseline in the run's outputPath", {
  skip_if_not_installed("SpaDES.core")
  paths <- local_spades_sandbox()
  withr::local_options(reproducible.useCache = TRUE)
  params <- list(simulationProcess = "static", .useCache = ".inputObjects")
  run <- function() {
    suppressMessages(SpaDES.core::spades(run_simInit(paths, nm_objects(), params = params), debug = FALSE))
  }
  entries <- function() suppressMessages(data.table::uniqueN(reproducible::showCache(paths$other)$cacheId))

  run()
  n <- entries()
  ## the baseline files gone, as for a run with a new outputPath that shares the cache
  unlink(file.path(paths$other, "pde_test"), recursive = TRUE)
  sim <- run()

  ## a cache hit: .inputObjects() did not run again
  expect_gt(n, 0)
  expect_identical(entries(), n)
  out <- SpaDES.core::outputs(sim)
  base <- out[out$objectName %in% c("pde", "pdeMap"), ]
  expect_setequal(base$file, file.path(SpaDES.core::outputPath(sim), "pde_test",
                                       c("pde_global.tif", "pdeMap_global.tif")))
  expect_true(all(file.exists(base$file)))
})

test_that("registerPdeFiles() lists the files in outputs(sim) as saved, under the objects named", {
  skip_if_not_installed("SpaDES.core")
  paths <- local_spades_sandbox()
  sim <- run_simInit(paths, nm_objects())
  files <- list(pde = c(global = file.path(paths$other, "pde_global_year2030.tif")),
                pdeMap = c(global = file.path(paths$other, "pdeMap_global_year2030.tif")))

  sim <- registerPdeFiles(sim, files, objectNames = c(pde = "simPde", pdeMap = "simPdeMap"))
  out <- SpaDES.core::outputs(sim)
  out <- out[out$objectName %in% c("simPde", "simPdeMap"), ]

  expect_identical(out$objectName, c("simPde", "simPdeMap"))
  expect_identical(basename(out$file), c("pde_global_year2030.tif", "pdeMap_global_year2030.tif"))
  expect_true(all(out$saved))
  expect_identical(unique(out$package), "terra")
})
