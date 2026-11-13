testthat::local_edition(3)

test_that("makePDEs() returns each model's PDE and map: global over studyAreaCaribou, regional over its own area", {
  coefs <- fixture_coefs()
  areas <- list(BC = fixture_area(xmax = 1), SK = fixture_area(xmin = 1))
  ud <- makePDEs(mods = list(global = coefs, BC = coefs, SK = coefs), envLayers = fixture_envlayers(),
                 studyAreaCaribou = fixture_area(), studyArea_juris = areas, normalizePDE = TRUE)

  expect_named(ud, c("pde", "pdeMap"))
  expect_named(ud$pde, c("global", "BC", "SK"))
  expect_named(ud$pdeMap, c("global", "BC", "SK"))
  expect_s4_class(ud$pde$BC, "SpatRaster")
  # each PDE sums to 1 over its own area and is empty outside it
  expect_equal(terra::global(ud$pde$BC, "sum", na.rm = TRUE)[[1]], 1)
  expect_equal(as.vector(is.na(terra::values(ud$pde$global))), c(FALSE, FALSE, FALSE, FALSE))
  expect_equal(as.vector(is.na(terra::values(ud$pde$BC))), c(FALSE, TRUE, FALSE, TRUE))
  expect_equal(as.vector(is.na(terra::values(ud$pde$SK))), c(TRUE, FALSE, TRUE, FALSE))
})

test_that("makePDEs() returns the raw utility and no map when not normalizing", {
  ud <- makePDEs(mods = list(global = fixture_coefs()), envLayers = fixture_envlayers(),
                 studyAreaCaribou = fixture_area(), normalizePDE = FALSE)

  expect_named(ud$pde, "global")
  expect_null(ud$pdeMap)
})

test_that("makePDEs() stops for a global model without studyAreaCaribou", {
  expect_error(
    makePDEs(mods = list(global = fixture_coefs()), envLayers = fixture_envlayers(),
             studyArea_juris = list(BC = fixture_area())),
    "needs studyAreaCaribou"
  )
})

test_that("makePDEs() writes each model's PDE and map as one GeoTIFF each, with the key in the file name", {
  coefs <- fixture_coefs()
  dir <- withr::local_tempdir()
  makePDEs(mods = list(BC = coefs), envLayers = fixture_envlayers(), studyArea_juris = list(BC = fixture_area()),
           normalizePDE = TRUE, savePath = dir, key = "year2030")
  makePDEs(mods = list(global = coefs), envLayers = fixture_envlayers(), studyAreaCaribou = fixture_area(),
           normalizePDE = FALSE, savePath = dir)

  files <- list.files(dir, full.names = TRUE)
  expect_setequal(basename(files), c("pde_BC_year2030.tif", "pdeMap_BC_year2030.tif", "pde_global.tif"))
  # each file is a GeoTIFF that keeps the landscape's CRS and grid
  for (f in files) {
    expect_true(any(grepl("Driver: GTiff/GeoTIFF", terra::describe(f), fixed = TRUE)))
    expect_true(terra::compareGeom(terra::rast(f), fixture_envlayers()$x, stopOnError = FALSE))
  }
})
