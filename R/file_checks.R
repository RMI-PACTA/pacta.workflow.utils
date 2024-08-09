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
