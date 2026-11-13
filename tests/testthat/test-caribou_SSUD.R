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

run_simInit <- function(paths, objects, params = list()) {
  suppressMessages(SpaDES.core::simInit(
    times = list(start = 2025, end = 2025),
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
  expect_contains(md$parameters$paramName, c("ecotype", "outputFolderID", "normalizePDE"))
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

test_that("simInit() makes the baseline and saves it, when not supplied", {
  skip_if_not_installed("SpaDES.core")
  paths <- local_spades_sandbox()

  sim <- run_simInit(paths, nm_objects())
  made <- makePDEs(list(global = fixture_coefs()), fixture_envlayers(), studyAreaCaribou = fixture_area())
  expect_named(sim$pde, "global")
  expect_equal(terra::values(sim$pde$global), terra::values(made$pde$global))
  expect_equal(terra::values(sim$pdeMap$global), terra::values(made$pdeMap$global))
  expect_setequal(list.files(file.path(paths$other, "pde_test")), c("pde_global.tif", "pdeMap_global.tif"))
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
