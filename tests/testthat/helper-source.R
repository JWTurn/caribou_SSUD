## Load the packages the module attaches (reqdPkgs) and the module's R/ functions.
for (pkg in c("data.table", "terra", "glmmTMB")) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}
for (f in list.files(testthat::test_path("..", "..", "R"), pattern = "[.]R$", full.names = TRUE)) {
  source(f, local = environment())
}
