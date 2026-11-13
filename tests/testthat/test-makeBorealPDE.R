testthat::local_edition(3)

test_that("makeBorealPDE() returns each jurisdiction model's PDE and map, on its own area", {
  coefs <- fixture_coefs()
  areas <- list(BC = fixture_area(xmax = 1), SK = fixture_area(xmin = 1))
  ud <- makeBorealPDE(mods = list(BC = coefs, SK = coefs), studyAreaJuris = areas, juris = c("BC", "SK"),
                      envLayers = fixture_envlayers(), normalizePDE = TRUE)

  expect_named(ud, c("pde", "pdeMap"))
  expect_named(ud$pde, c("BC", "SK"))
  expect_named(ud$pdeMap, c("BC", "SK"))
  expect_s4_class(ud$pde$BC, "SpatRaster")
  # each PDE sums to 1 over its own area and is empty outside it
  expect_equal(terra::global(ud$pde$BC, "sum", na.rm = TRUE)[[1]], 1)
  expect_equal(as.vector(is.na(terra::values(ud$pde$BC))), c(FALSE, TRUE, FALSE, TRUE))
  expect_equal(as.vector(is.na(terra::values(ud$pde$SK))), c(TRUE, FALSE, TRUE, FALSE))
})

test_that("makeBorealPDE() returns the raw utility and no map when not normalizing", {
  coefs <- fixture_coefs()
  ud <- makeBorealPDE(mods = list(global = coefs), studyAreaJuris = list(BC = fixture_area()), juris = "BC",
                      envLayers = fixture_envlayers(), normalizePDE = FALSE)

  expect_named(ud$pde, "global")
  expect_null(ud$pdeMap)
})

test_that("makeBorealPDE() writes each model's PDE and map as GeoTIFFs, with the key in the file name", {
  coefs <- fixture_coefs()
  dir <- withr::local_tempdir()
  makeBorealPDE(mods = list(BC = coefs), studyAreaJuris = list(BC = fixture_area()), juris = c("BC", "SK"),
                envLayers = fixture_envlayers(), normalizePDE = TRUE, savePath = dir, key = "year2030")
  makeBorealPDE(mods = list(BC = coefs), studyAreaJuris = list(BC = fixture_area()), juris = "BC",
                envLayers = fixture_envlayers(), normalizePDE = FALSE, savePath = dir)

  files <- list.files(dir, full.names = TRUE)
  expect_setequal(basename(files), c("pde_BC_BCSK_year2030.tif", "pdeMap_BC_BCSK_year2030.tif", "pde_BC_BC.tif"))
  # each file is a GeoTIFF that keeps the landscape's CRS and grid
  for (f in files) {
    expect_true(any(grepl("Driver: GTiff/GeoTIFF", terra::describe(f), fixed = TRUE)))
    expect_true(terra::compareGeom(terra::rast(f), fixture_envlayers()$x, stopOnError = FALSE))
  }
})
