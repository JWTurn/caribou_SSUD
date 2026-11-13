testthat::local_edition(3)

test_that("getStudyAreaForModel() matches a jurisdiction model to its area, ignoring case", {
  areas <- list(BC = "the BC area", NTYT = "the NT and YT area")

  expect_identical(getStudyAreaForModel("bc", areas, jurisdiction = c("BC", "NTYT")), "the BC area")
  expect_identical(getStudyAreaForModel("NTYT", areas, jurisdiction = "BC"), "the NT and YT area")
  expect_error(getStudyAreaForModel("SK", areas, jurisdiction = "SK"), "Available: BC, NTYT")
})

test_that("getStudyAreaForModel() gives a global model the area of its one jurisdiction", {
  areas <- list(BC = "the BC area", NTYT = "the NT and YT area")

  expect_identical(getStudyAreaForModel("global", areas, jurisdiction = "ntyt"), "the NT and YT area")
  expect_error(getStudyAreaForModel("GLOBAL", areas, jurisdiction = c("BC", "NTYT")), "single jurisdiction")
  expect_error(getStudyAreaForModel("GLOBAL", areas, jurisdiction = "SK"), "'SK' not found")
})
