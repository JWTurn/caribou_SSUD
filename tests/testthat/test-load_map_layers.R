testthat::local_edition(3)

test_that("load_map_layers() builds the nine model layers on the land-cover grid", {
  grid <- function(vals, ncols = 2) {
    terra::rast(nrows = 2, ncols = ncols, xmin = 0, xmax = ncols, ymin = 0, ymax = 2, vals = vals)
  }
  lc <- c(needleleaf = 0.1, deciduous = 0.2, mixed = 0.05, wet_treed = 0.05, shrub = 0.1, bryoids = 0.2, herbs = 0.1, wetland = 0.2)
  propLC <- terra::rast(lapply(lc, function(p) grid(rep(p, 4))))
  names(propLC) <- names(lc)
  ## distance to paved roads covers one more column than the land cover: it is cut to the land cover
  lfPaved <- grid(c(0, 10, 99, 20, 30, 99), ncols = 3)
  lfOther <- grid(c(5, 15, 25, 35))
  disturbOther <- grid(c(0, 1, 0, 1))
  ## year burned and year harvested; NA where never, which counts as ts_else years ago
  fire <- grid(c(2000, NA, 2015, NA))
  harv <- grid(c(NA, 1990, NA, 2010))

  utils::capture.output(
    land <- load_map_layers(propLC, lfOther, lfPaved, disturbOther, fire, harv, year = 2020, ts_else = 100)
  )
  expect_named(land, c(
    "prop_veg", "prop_needleleaf", "prop_mixforest", "prop_wets", "log_tsf", "log_tsh",
    "log_distlf", "log_distlfother", "disturb"
  ))
  expect_equal(as.vector(terra::ext(land)), as.vector(terra::ext(propLC)))
  v <- function(layer) as.vector(terra::values(land[[layer]]))
  expect_equal(v("prop_veg"), rep(0.4, 4))
  expect_equal(v("prop_needleleaf"), rep(0.1, 4))
  expect_equal(v("prop_mixforest"), rep(0.3, 4))
  expect_equal(v("prop_wets"), rep(0.2, 4))
  expect_equal(v("log_tsf"), log(c(20, 100, 5, 100) + 1))
  expect_equal(v("log_tsh"), log(c(100, 30, 100, 10) + 1))
  expect_equal(v("log_distlf"), log(c(0, 10, 20, 30) + 1))
  expect_equal(v("log_distlfother"), log(c(5, 15, 25, 35) + 1))
  expect_equal(v("disturb"), c(0, 1, 0, 1))
})
