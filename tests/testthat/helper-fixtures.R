## Small made-up fixtures for the tests: no caribou data, no downloads.

## A 2 x 2 landscape with two covariates, x and y, as make_pde() reads them: an environment whose
## names are the model's covariates without their "_end" suffix.
fixture_envlayers <- function() {
  x <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2, vals = 1:4)
  y <- terra::rast(x, vals = c(0, 1, 0, 1))
  list2env(list(x = x, y = y), parent = baseenv())
}

## A polygon over the given extent, in the fixture landscape's CRS.
fixture_area <- function(xmin = 0, xmax = 2, ymin = 0, ymax = 2) {
  terra::as.polygons(terra::ext(xmin, xmax, ymin, ymax), crs = terra::crs(fixture_envlayers()$x))
}

## A small iSSA-like glmmTMB fit with a start-of-step term (not used for the PDE), an end-of-step
## term, a squared term written with I(), and an interaction.
fixture_fit <- function() {
  set.seed(1)
  steps <- data.frame(
    x_start = stats::runif(300),
    x_end = stats::runif(300),
    y_end = stats::rbinom(300, 1, 0.5)
  )
  steps$case_ <- stats::rbinom(300, 1, stats::plogis(-1 + 1.5 * steps$x_end - 0.5 * steps$x_end * steps$y_end))
  glmmTMB::glmmTMB(
    case_ ~ x_start + x_end + I(x_end^2) + x_end:y_end,
    family = stats::binomial(),
    data = steps
  )
}
