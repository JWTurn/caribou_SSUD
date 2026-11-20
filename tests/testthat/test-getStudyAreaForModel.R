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

  expect_identical(pdeModels(c("global", "nt"), iSSAmodels = models),
                   list(global = "the global model", nt = "the NT model"))
  expect_error(pdeModels(c("SK", "NT"), iSSAmodels = models), "no model for: SK. Available: global, NT, BC")
})

test_that("pdeModels() takes fitted models and coefficient tables alike, for any model", {
  tab <- data.table::data.table(term = "x_end", estimate = 1)
  fit <- structure(list(), class = "glmmTMB")
  models <- list(global = tab, YT = fit)

  expect_identical(pdeModels(c("global", "YT"), iSSAmodels = models), models)
  ## one model or table on its own, or a list of one unnamed, is the global model
  expect_identical(pdeModels("global", iSSAmodels = tab), list(global = tab))
  expect_identical(pdeModels("global", iSSAmodels = fit), list(global = fit))
  expect_identical(pdeModels("global", iSSAmodels = list(fit)), list(global = fit))
  expect_error(pdeModels("YT", iSSAmodels = tab), "no model for: YT. Available: global")
})

test_that("pdeModels() takes iSSAcoefficients as the global model when iSSAmodels is not supplied", {
  tab <- data.table::data.table(term = "x_end", estimate = 1)

  expect_identical(pdeModels("global", iSSAcoefficients = tab), list(global = tab))
  expect_identical(pdeModels("global", iSSAmodels = list(global = "a model"), iSSAcoefficients = tab),
                   list(global = "a model"))
  expect_error(pdeModels("global"), "must be a list of models by name")
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

test_that("getStudyAreaForModel() gives the global model studyAreaReporting with pdeArea 'fullReportingArea'", {
  rep <- fixture_area(xmax = 1)
  juris <- list(BC = fixture_area(xmin = 1))
  areas <- c("fullReportingArea", "regionalArea")

  expect_equal(as.vector(terra::ext(getStudyAreaForModel("global", juris, fixture_area(), rep, areas))),
               as.vector(terra::ext(rep)))
  # a regional model keeps its region
  expect_equal(as.vector(terra::ext(getStudyAreaForModel("BC", juris, fixture_area(), rep, areas))),
               as.vector(terra::ext(juris$BC)))
  # an sf area is taken too
  expect_s4_class(getStudyAreaForModel("global", juris, fixture_area(), sf::st_as_sf(rep), areas), "SpatVector")
})

test_that("getStudyAreaForModel() gives a regional model the part of studyAreaReporting in its region with 'regionalReportingArea'", {
  rep <- fixture_area(xmin = 1)
  juris <- list(BC = fixture_area(ymax = 1))
  areas <- c("fullArea", "regionalReportingArea")

  expect_equal(as.vector(terra::ext(getStudyAreaForModel("BC", juris, fixture_area(), rep, areas))),
               c(xmin = 1, xmax = 2, ymin = 0, ymax = 1))
  # the global model keeps studyAreaCaribou
  expect_equal(as.vector(terra::ext(getStudyAreaForModel("global", juris, fixture_area(), rep, areas))),
               c(xmin = 0, xmax = 2, ymin = 0, ymax = 2))
  expect_error(getStudyAreaForModel("BC", list(BC = fixture_area(xmax = 1)), NULL, fixture_area(xmin = 1.5), areas),
               "does not overlap")
})

test_that("checkPdeArea() wants one known choice for each scale run, and studyAreaReporting for a 'Reporting' one", {
  expect_true(checkPdeArea(c("fullArea", "regionalArea"), c("global", "BC"), FALSE))
  expect_true(checkPdeArea("fullArea", "global", FALSE))
  expect_error(checkPdeArea("fullArea", c("global", "BC"), FALSE), "'regionalArea' or 'regionalReportingArea' for the regional models")
  expect_error(checkPdeArea(c("fullArea", "fullReportingArea"), "global", TRUE), "one of 'fullArea' or 'fullReportingArea'")
  expect_error(checkPdeArea("herd", "global", FALSE), "not: herd")
  expect_error(checkPdeArea("fullReportingArea", "global", FALSE), "needs studyAreaReporting")
  # a choice for a scale not run is not used, so needs nothing
  expect_true(checkPdeArea(c("fullArea", "regionalReportingArea"), "global", FALSE))
})
