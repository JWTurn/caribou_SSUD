testthat::local_edition(3)

test_that("pdeModelNames() picks the global model, one per region, or both", {
  expect_identical(pdeModelNames("global", c("BC", "NT")), "global")
  expect_identical(pdeModelNames("regional", c("BC", "NT")), c("BC", "NT"))
  expect_identical(pdeModelNames("both", "NT"), c("global", "NT"))
  expect_message(expect_identical(pdeModelNames("jurisdictional", "NT"), "NT"), "now called 'regional'")
  expect_error(pdeModelNames("herd", "NT"), "'global', 'regional' or 'both'")
})

test_that("pdeModels() takes the chosen models from iSSAmodels, ignoring case, and names them as chosen", {
  models <- list(global = "the global model", NT = "the NT model", BC = "the BC model")

  expect_identical(pdeModels(c("global", "nt"), "boreal", iSSAmodels = models),
                   list(global = "the global model", nt = "the NT model"))
  expect_error(pdeModels(c("SK", "NT"), "boreal", iSSAmodels = models), "no model for: SK. Available: global, NT, BC")
})

test_that("pdeModels() gives the northern mountain ecotype its one global model", {
  expect_identical(pdeModels("global", "northern_mountain", iSSAcoefficients = "the table"),
                   list(global = "the table"))
  expect_error(pdeModels("YT", "northern_mountain", iSSAcoefficients = "the table"), "only a global model")
})

test_that("getStudyAreaForModel() matches a regional model to its area, ignoring case", {
  areas <- list(BC = "the BC area", NTYT = "the NT and YT area")

  expect_identical(getStudyAreaForModel("bc", areas, NULL), "the BC area")
  expect_identical(getStudyAreaForModel("NTYT", areas, "the caribou area"), "the NT and YT area")
  expect_error(getStudyAreaForModel("SK", areas, NULL), "Available: BC, NTYT")
})

test_that("getStudyAreaForModel() gives the global model studyAreaCaribou", {
  areas <- list(BC = "the BC area")

  expect_identical(getStudyAreaForModel("GLOBAL", areas, "the caribou area"), "the caribou area")
  expect_error(getStudyAreaForModel("global", areas, NULL), "needs studyAreaCaribou")
})
