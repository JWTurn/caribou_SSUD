## Run the module's tests, from the module root: Rscript tests/run-tests.R [file filter]
## tests/testthat/helper-source.R attaches the packages the module's R/ functions call and sources
## those functions. Used by .github/workflows/test-module.yaml.
## Tests use at most 2 cores, as CRAN asks; TEST_CORES raises that, to 4 at most, for a longer run.
## R starts OpenMP and BLAS before this script runs, so the limits must be in the environment R
## starts with: without them, the script runs itself again in a new R process that has them.
asked <- suppressWarnings(as.integer(Sys.getenv("TEST_CORES")))
cores <- if (is.na(asked)) 2L else min(4L, max(1L, asked))
limit <- suppressWarnings(as.integer(Sys.getenv("OMP_THREAD_LIMIT")))
if (is.na(limit) || limit > cores) {
  status <- system2(
    file.path(R.home("bin"), "R"),
    shQuote(commandArgs(trailingOnly = FALSE)[-1L]),
    env = paste0(
      c("OMP_THREAD_LIMIT=", "OMP_NUM_THREADS=", "OPENBLAS_NUM_THREADS=", "MKL_NUM_THREADS="),
      cores
    )
  )
  quit(save = "no", status = status)
}
data.table::setDTthreads(cores)
args <- commandArgs(trailingOnly = TRUE)
testthat::test_dir(
  file.path("tests", "testthat"),
  filter = if (length(args)) args[[1L]] else NULL,
  reporter = "summary",
  stop_on_failure = TRUE
)
