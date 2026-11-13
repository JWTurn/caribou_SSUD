testthat::local_edition(3)

## a baseline as makePDEs() writes it, for the global model and BC, in a new folder
write_baseline <- function(normalizePDE = TRUE) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  makePDEs(mods = list(global = fixture_coefs(), BC = fixture_coefs()), envLayers = fixture_envlayers(),
           studyAreaCaribou = fixture_area(), studyArea_juris = list(BC = fixture_area(xmax = 1)),
           normalizePDE = normalizePDE, savePath = dir)
  dir
}

test_that("readBaseline() reads back the baseline makePDEs() wrote, for the chosen models", {
  dir <- write_baseline()
  made <- makePDEs(mods = list(BC = fixture_coefs()), envLayers = fixture_envlayers(),
                   studyArea_juris = list(BC = fixture_area(xmax = 1)))
  ud <- readBaseline(dir, "BC", normalizePDE = TRUE)

  expect_named(ud$pde, "BC")
  expect_named(ud$pdeMap, "BC")
  expect_equal(terra::values(ud$pde$BC), terra::values(made$pde$BC))
  expect_equal(terra::values(ud$pdeMap$BC), terra::values(made$pdeMap$BC))
  expect_null(readBaseline(dir, "global", normalizePDE = FALSE)$pdeMap)
})

test_that("readBaseline() stops when a chosen model's file is missing", {
  dir <- write_baseline(normalizePDE = FALSE)

  expect_error(readBaseline(dir, c("global", "SK"), normalizePDE = FALSE), "has no pde_SK.tif")
  expect_error(readBaseline(dir, "global", normalizePDE = TRUE), "has no pdeMap_global.tif")
})

test_that("baselineDir() takes a folder as it is and unpacks a .zip, even with the files in a subfolder", {
  dir <- write_baseline()
  expect_identical(baselineDir(dir, "unused"), dir)

  zip <- file.path(withr::local_tempdir(), "baseline.zip")
  withr::with_dir(dirname(dir), utils::zip(zip, basename(dir), flags = "-rq"))
  dest <- withr::local_tempdir()
  unpacked <- baselineDir(zip, dest)

  expect_identical(unpacked, file.path(dest, "baseline"))
  expect_named(readBaseline(unpacked, c("global", "BC"), normalizePDE = TRUE)$pde, c("global", "BC"))
})
