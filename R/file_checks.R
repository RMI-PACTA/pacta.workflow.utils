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
  if (tolower(Sys.info()[["sysname"]]) == "windows") {
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

check_file <- function(filepath) {
  log_trace("Checking if file exists: ", filepath)
  pass <- FALSE
  info <- file.info(filepath, extra_cols = FALSE)
  if (all(is.na(info))) {
    log_error("File \"{filepath}\" does not exist.")
    warning("File does not exist.")
  } else if (is.na(info[["isdir"]]) || info[["isdir"]]) {
    log_error("File \"{filepath}\" is a directory.")
    warning("File is a directory.")
  } else if (info[["size"]] == 0L || is.na(info[["size"]])) {
    log_warn("File \"{filepath}\" is empty.")
    warning("File is empty.")
  } else {
    log_trace("File exists and is non-zero size.")
    pass <- TRUE
  }
  return(invisible(pass))
}
