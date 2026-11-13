testthat::local_edition(3)

## make_pde() and mod2UD() (R/pde_fns.R), with a fitted model.

test_that("make_pde() is exp(2 x the end-of-step selection), normalized over the study area", {
  fit <- fixture_fit()
  b <- glmmTMB::fixef(fit)$cond
  x <- 1:4
  y <- c(0, 1, 0, 1)
  ## only the _end terms, I() and interactions included; the intercept and x_start are left out
  u <- exp(2 * (b[["x_end"]] * x + b[["I(x_end^2)"]] * x^2 + b[["x_end:y_end"]] * x * y))

  pde <- make_pde(fit, fixture_envlayers(), fixture_area(), normalize = FALSE)
  expect_equal(as.vector(terra::values(pde)), u)

  ## a study area over the left column (cells 1 and 3): the others are masked out, and the PDE sums
  ## to 1 over the two inside
  pde <- make_pde(fit, fixture_envlayers(), fixture_area(xmax = 1))
  expect_equal(as.vector(terra::values(pde)), c(u[1], NA, u[3], NA) / (u[1] + u[3]))
})

test_that("mod2UD() returns the PDE and its decile map, or the raw utility", {
  fit <- fixture_fit()

  ud <- mod2UD(fit, fixture_envlayers(), fixture_area())
  expect_named(ud, c("pde", "map"))
  expect_equal(terra::global(ud$pde, "sum")[[1]], 1)
  expect_equal(terra::values(ud$map), terra::values(make_pde_map(ud$pde)))

  raw <- mod2UD(fit, fixture_envlayers(), fixture_area(), normalize = FALSE)
  expect_named(raw, "utility")
  expect_equal(
    terra::values(raw$utility),
    terra::values(make_pde(fit, fixture_envlayers(), fixture_area(), normalize = FALSE))
  )
})

test_that("mod2UD() reads a model saved as an .rds file", {
  fit <- fixture_fit()
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(fit, path)

  expect_equal(
    terra::values(mod2UD(path, fixture_envlayers(), fixture_area())$pde),
    terra::values(mod2UD(fit, fixture_envlayers(), fixture_area())$pde)
  )
})
