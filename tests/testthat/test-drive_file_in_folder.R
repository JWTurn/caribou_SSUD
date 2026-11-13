test_that("drive_file_in_folder() finds one file by name in a folder, across all drives", {
  seen <- NULL
  local_mocked_bindings(
    drive_find = function(q = NULL, corpus = NULL, ...) {
      seen <<- list(q = q, corpus = corpus)
      data.frame(name = "NMC_model_full_to2020.zip", id = "abc123")
    },
    .package = "googledrive"
  )
  url <- drive_file_in_folder("https://drive.google.com/drive/folders/f1", "NMC_model_full_to2020.zip")
  expect_identical(url, "https://drive.google.com/file/d/abc123")
  expect_identical(seen$corpus, "allDrives")
  expect_true(all(c("'f1' in parents", "name = 'NMC_model_full_to2020.zip'") %in% seen$q))
})

test_that("drive_file_in_folder() stops unless exactly one file has the name", {
  local_mocked_bindings(
    drive_find = function(...) data.frame(name = character(), id = character()),
    .package = "googledrive"
  )
  expect_error(drive_file_in_folder("f1", "a.zip"), "0 files named 'a.zip'")
  local_mocked_bindings(
    drive_find = function(...) data.frame(name = c("a.zip", "a.zip"), id = c("x", "y")),
    .package = "googledrive"
  )
  expect_error(drive_file_in_folder("f1", "a.zip"), "2 files named 'a.zip'")
})
