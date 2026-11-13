testthat::local_edition(3)

test_that("make_pde_map() bins the PDE into deciles, 0 (lowest) to 9", {
  set.seed(2)
  v <- sample(100)
  pde <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 10, vals = v / sum(v))

  map <- make_pde_map(pde)
  expect_equal(as.vector(terra::values(map)), (v - 1) %/% 10)
})

test_that("make_pde_map() leaves cells outside the study area empty", {
  pde <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 10, vals = c(NA, 1:99))

  map <- make_pde_map(pde)
  expect_true(is.na(terra::values(map)[1]))
  expect_false(anyNA(terra::values(map)[-1]))
})
