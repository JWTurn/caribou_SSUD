testthat::local_edition(3)

## a modelLand as the module keeps it: a list of SpatRasters named by layer
fixture_land <- function(layers) {
  r <- fixture_envlayers()$x
  stats::setNames(lapply(layers, function(n) r), layers)
}

test_that("makeFixedSSUD() takes the fixed layers from modelLand, named as the forecast reads them", {
  boreal <- makeFixedSSUD(
    fixture_land(c("prop_veg", "prop_wets", "distpaved", "distunpaved", "distpolys", "timeSinceHarvest",
                   "prop_needleleaf")),
    "boreal"
  )
  expect_identical(names(boreal),
                   c("prop_veg", "prop_wets", "distpaved", "distunpaved", "distpolys", "timeSinceHarvest0"))

  nm <- makeFixedSSUD(fixture_land(c("prop_veg_250", "dist_lf_250", "dist_poly_250", "prop_forest_250")),
                      "northern_mountain")
  expect_identical(names(nm), c("prop_veg_250", "dist_lf_250", "dist_poly_250"))
})

test_that("makeFixedSSUD() names the layers modelLand lacks", {
  expect_error(
    makeFixedSSUD(fixture_land(c("prop_veg", "prop_wets", "distpaved", "distunpaved", "distpolys")), "boreal"),
    "lacks: timeSinceHarvest"
  )
  expect_error(
    makeFixedSSUD(fixture_land(c("prop_veg_240", "dist_paved_240")), "northern_mountain"),
    "lacks: prop_veg_250, dist_lf_250, dist_poly_250"
  )
})
