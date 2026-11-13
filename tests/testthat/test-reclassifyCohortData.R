testthat::local_edition(3)

## A conifer and a broadleaf species, in LandR's species-equivalence layout.
fixture_spp <- function() {
  data.table::data.table(LandR = c("Pice_mar", "Popu_tre"), Type = c("Conifer", "Deciduous"))
}

## A pixelGroupMap of five pixels: pixel groups 1, 2, (none), 3 and 2 again.
fixture_pixelGroupMap <- function() {
  pgm <- terra::rast(nrows = 1, ncols = 5, xmin = 0, xmax = 5, ymin = 0, ymax = 1, vals = c(1L, 2L, NA, 3L, 2L))
  names(pgm) <- "pixelGroup"
  pgm
}

test_that("reclassifyCohortData() maps forest type, biomass-weighted age and biomass by pixel", {
  ## pixel group 1 all conifer, 2 half and half, 3 all broadleaf; B in g/m2
  cohortData <- data.table::data.table(
    pixelGroup = c(1L, 2L, 2L, 3L),
    speciesCode = c("Pice_mar", "Pice_mar", "Popu_tre", "Popu_tre"),
    B = c(100L, 50L, 50L, 300L),
    age = c(50L, 40L, 80L, 30L)
  )

  out <- reclassifyCohortData(cohortData, "LandR", fixture_spp(), fixture_pixelGroupMap())
  expect_named(out, c("stand age", "forest type", "AGB (Mg/ha)"))
  ## conifer (210), mixed (230), no forest, broadleaf (220), mixed
  expect_equal(as.vector(terra::values(out[["forest type"]])), c(210, 230, NA, 220, 230))
  expect_equal(as.vector(terra::values(out[["stand age"]])), c(50, 60, NA, 30, 60))
  expect_equal(as.vector(terra::values(out[["AGB (Mg/ha)"]])), c(1, 1, NA, 3, 1))
})

test_that("reclassifyCohortData() classes a stand by its conifer share, whatever the order of its cohorts", {
  ## pixel group 1 is 80% conifer, 2 is 80% broadleaf, 3 is half and half
  cohortData <- data.table::data.table(
    pixelGroup = c(1L, 1L, 2L, 2L, 3L, 3L),
    speciesCode = c("Pice_mar", "Popu_tre", "Pice_mar", "Popu_tre", "Pice_mar", "Popu_tre"),
    B = c(80L, 20L, 20L, 80L, 50L, 50L),
    age = 50L
  )
  pgm <- terra::rast(nrows = 1, ncols = 3, xmin = 0, xmax = 3, ymin = 0, ymax = 1, vals = 1:3)
  names(pgm) <- "pixelGroup"

  ## conifer (210), broadleaf (220), mixed (230), with the cohorts in either order
  for (cd in list(cohortData, cohortData[rev(seq_len(nrow(cohortData)))])) {
    out <- reclassifyCohortData(data.table::copy(cd), "LandR", fixture_spp(), pgm)
    expect_equal(as.vector(terra::values(out[["forest type"]])), c(210, 220, 230))
  }
})

test_that("reclassifyCohortData() compares the conifer share, not the broadleaf share, with the mixed band", {
  ## 30% conifer is below a band of (0.55, 0.9), so broadleaf, though the 70% broadleaf is inside it
  cohortData <- data.table::data.table(
    pixelGroup = c(2L, 2L),
    speciesCode = c("Pice_mar", "Popu_tre"),
    B = c(30L, 70L),
    age = c(40L, 80L)
  )
  pgm <- terra::rast(nrows = 1, ncols = 1, xmin = 0, xmax = 1, ymin = 0, ymax = 1, vals = 2L)
  names(pgm) <- "pixelGroup"

  out <- reclassifyCohortData(cohortData, "LandR", fixture_spp(), pgm, mixedForestCutoffs = c(0.55, 0.9))
  expect_equal(terra::values(out[["forest type"]])[[1]], 220)
})

test_that("reclassifyCohortData() takes the mixed-forest band from mixedForestCutoffs", {
  cohortData <- data.table::data.table(
    pixelGroup = c(2L, 2L),
    speciesCode = c("Pice_mar", "Popu_tre"),
    B = c(50L, 50L),
    age = c(40L, 80L)
  )
  pgm <- terra::rast(nrows = 1, ncols = 1, xmin = 0, xmax = 1, ymin = 0, ymax = 1, vals = 2L)
  names(pgm) <- "pixelGroup"

  ## half conifer is mixed under the default band (0.33, 0.66), broadleaf above it
  out <- reclassifyCohortData(data.table::copy(cohortData), "LandR", fixture_spp(), pgm)
  expect_equal(terra::values(out[["forest type"]])[[1]], 230)
  out <- reclassifyCohortData(data.table::copy(cohortData), "LandR", fixture_spp(), pgm, mixedForestCutoffs = c(0.55, 0.9))
  expect_equal(terra::values(out[["forest type"]])[[1]], 220)
})
