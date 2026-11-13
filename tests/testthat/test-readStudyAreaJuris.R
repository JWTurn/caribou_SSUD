testthat::local_edition(3)

## the regional areas saved as an .rds file, each area made into a list element by `pack`
save_areas <- function(pack) {
  file <- withr::local_tempfile(fileext = ".rds", .local_envir = parent.frame())
  areas <- list(BC = fixture_area(xmax = 1), SK = fixture_area(xmin = 1))
  saveRDS(lapply(areas, pack), file)
  file
}

test_that("readStudyAreaJuris() reads areas packed with terra::wrap()", {
  areas <- readStudyAreaJuris(save_areas(terra::wrap))

  expect_named(areas, c("BC", "SK"))
  expect_s4_class(areas$BC, "SpatVector")
  expect_equal(as.vector(terra::ext(areas$SK)), as.vector(terra::ext(fixture_area(xmin = 1))))
  expect_true(terra::same.crs(areas$BC, fixture_area()))
})

test_that("readStudyAreaJuris() reads sf areas, as SpatVectors", {
  areas <- readStudyAreaJuris(save_areas(sf::st_as_sf))

  expect_named(areas, c("BC", "SK"))
  expect_s4_class(areas$BC, "SpatVector")
  expect_equal(as.vector(terra::ext(areas$BC)), as.vector(terra::ext(fixture_area(xmax = 1))))
})

test_that("readStudyAreaJuris() stops for SpatVectors saved without terra::wrap()", {
  expect_error(readStudyAreaJuris(save_areas(identity)), "saved without terra::wrap()")
})
