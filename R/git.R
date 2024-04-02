get_git_info <- function(repo) {
  log_trace("checking that directory \"{repo}\"exists.")
  if (dir.exists(repo)) {
    if (is_git_path(repo)) {
    } else {
      log_warn("directory \"{repo}\" is not a git repository.")
      warning("Directory \"{repo}\" is not a git repository")
      out <- list()
    }
  } else {
    # dir does not exist
    log_warn("directory \"{repo}\" does not exist.")
    warning("Cannot find git information for directory which does not exist")
    out <- list()
  }
  return(out)
}

is_git_path <- function(path) {
  log_trace("checking that path \"{path}\" exists.")
  if (file.exists(path)) {
    out <- tryCatch({
      git_path <- gert::git_find(path = path)
      dir.exists(git_path)
    }, error = function(e) {
      FALSE
    })
  } else {
    # dir does not exist
    log_error("path \"{path}\" does not exist.")
    stop("Cannot find git information for path which does not exist.")
  }
  return(out)
}
