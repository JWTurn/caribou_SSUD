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
    params = list(caribou_SSUD = c(list(ecotype = "northern_mountain", .useCache = FALSE), params)),
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
  land <- list(x = fixture_envlayers()$x)
  juris <- list(NTYT = fixture_area())

  sim <- run_simInit(paths, list(iSSAmodels = list(fit), modelLand = land, studyArea_juris = juris))
  expect_length(calls, 0)
  expect_identical(sim$iSSAmodels, list(fit))
  expect_named(sim$modelLand, "x")
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
    c(r$x, r$y)
  })
  folder <- "https://drive.google.com/drive/folders/aTestFolder"

  sim <- run_simInit(
    paths,
    list(iSSAmodels = list(fixture_fit()), studyArea_juris = list(NTYT = fixture_area())),
    params = list(outputFolderID = folder)
  )
  expect_identical(calls, list(list(targetFile = "modelLand.tif", url = folder)))
  expect_length(sim$modelLand, 2)
  expect_equal(as.vector(terra::values(sim$modelLand[[2]])), c(0, 1, 0, 1))
})
