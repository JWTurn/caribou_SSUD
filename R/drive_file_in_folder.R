#' Google Drive URL of the one file named `name` in the Drive folder `folder`
#' Given a folder url, reproducible lists the folder through its shared drive, which an account that
#' is not a member of that drive cannot do, and then downloads every file in it. This finds the one
#' file with a search across all drives instead; drop it once reproducible fetches only the named file.
drive_file_in_folder <- function(folder, name) {
  hit <- googledrive::drive_find(
    q = c(sprintf("'%s' in parents", googledrive::as_id(folder)), sprintf("name = '%s'", name), "trashed = false"),
    corpus = "allDrives"
  )
  if (nrow(hit) != 1L) {
    stop(nrow(hit), " files named '", name, "' in Drive folder ", folder, "; expected 1", call. = FALSE)
  }
  paste0("https://drive.google.com/file/d/", hit$id)
}
