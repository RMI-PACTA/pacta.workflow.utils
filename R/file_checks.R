#' check_dir_writable
#'
#' Check if a directory is writable. Check is performed by attempting to write
#' a 0-byte file (`.test`) to the directory (and removing afterwards). Emits a
#' warning on windows, since file permissions on Windows systems are difficult
#' to test, but _should_ be accurate.
#'
#' @param dir directory to check writiblity
#' @return logical, can a file be written to the directory?
#' @export
check_dir_writable <- function(dir) {
  log_trace("Checking if directory is writable: ", dir)
  if (tolower(Sys.info()["sysname"]) == "windows") {
    log_warn("Function check_dir_writable is not tested on Windows.")
    warning("check_dir_writable may return incorrect results on Windows.")
  }
  if (dir.exists(dir)) {
    log_trace("Directory exists.")
    test_path <- file.path(dir, ".test")
    dir_is_writable <- file.create(test_path, showWarnings = FALSE)
    if (dir_is_writable) {
      file.remove(test_path)
    }
  } else {
    log_error("Directory \"{dir}\" does not exist.")
    stop("Directory does not exist.")
  }
  return(dir_is_writable)
}
