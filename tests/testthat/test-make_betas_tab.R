testthat::local_edition(3)

test_that("make_betas_tab() tabulates a model's fixed effects as term and estimate", {
  fit <- fixture_fit()

  tab <- make_betas_tab(fit)
  expect_s3_class(tab, "data.table")
  expect_named(tab, c("term", "estimate"))
  expect_identical(tab$term, names(glmmTMB::fixef(fit)$cond))
  expect_equal(tab$estimate, unname(glmmTMB::fixef(fit)$cond))
})
