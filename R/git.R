get_git_info <- function(repo) {
  log_trace("checking that directory \"{repo}\"exists.")
  if (is_git_path(repo)) {
    git_repo <- gert::git_find(path = repo)
    info <- gert::git_info(repo = git_repo)
    latest_commit <- info[["commit"]]
    if (is.na(latest_commit)) {
      log_debug("No commits found in repo.")
      latest_commit <- NULL
    }
    out <- list(
      repo = normalizePath(info[["path"]]),
      is_git = TRUE,
      branch = git_branch_info(repo = repo),
      commit = latest_commit
    )
  } else {
    log_warn("Directory \"{repo}\" is not a git repository.")
    warning("Specified path is not in a git repository.")
    out <- NULL
  }
  return(out)
}

is_git_path <- function(path) {
  log_trace("checking that path \"{path}\" is in a git repository.")
  if (file.exists(path)) {
    log_trace("path \"{path}\" exists.")
    git_path <- tryCatch({
      gert::git_find(path = path)
    }, error = function(e) {
      log_trace("error while finding git repo in parent tree for \"{path}\".")
      NULL
    })
    if (is.null(git_path)) {
      log_trace("no git repo found in parent tree for \"{path}\".")
      is_git_path <- FALSE
    } else {
      log_trace("git repo found in parent tree for \"{path}\".")
      is_git_path <- dir.exists(git_path)
    }
  } else {
    # dir does not exist
    log_error("path \"{path}\" does not exist.")
    stop("Cannot find git information for path which does not exist.")
  }
  return(is_git_path)
}

git_branch_info <- function(repo) {
  log_trace("checking branch information for repo \"{repo}\".")
  if (is_git_path(repo)) {
    git_repo <- gert::git_find(path = repo)
    active_branch <- gert::git_branch(repo = git_repo)
    if (is.null(active_branch)) {
      log_debug("No active branch found.")
      return(NULL)
    }
    log_debug("active branch: \"{active_branch}\".")
    branch_list <- gert::git_branch_list(repo = git_repo)
    active_index <- which(branch_list[["name"]] == active_branch)
    active_commit <- branch_list[[active_index, "commit"]]
    active_upstream <- branch_list[[active_index, "upstream"]]
    if (is.na(active_upstream)) {
      log_debug("Branch \"{active_branch}\" has no upstream.")
      active_upstream <- NULL
      up_to_date <- NULL
      upstream_commit <- NULL
      remote_url = NULL
    } else {
      log_trace("Branch \"{active_branch}\" has an upstream: \"{active_upstream}\".")
      active_upstream <- gsub("refs/heads/", "", active_upstream)
      upstream_index <- which(branch_list[["ref"]] == active_upstream)
      upstream_commit <- branch_list[[upstream_index, "commit"]]
      up_to_date <- active_commit == upstream_commit
      remote_list <- gert::git_remote_list(repo = git_repo)
      # refs/remotes/origin/branch
      remote_name <- strsplit(x = active_upstream, split = "/")[[1]][3]
      remote_info <- gert::git_remote_info(repo = git_repo, remote = remote_name)
      remote_url <- remote_info[["url"]]
    }
    out <- list(
      name = active_branch,
      commit = active_commit,
      upstream = active_upstream,
      remote_url = remote_url,
      up_to_date = up_to_date,
      upstream_commit = upstream_commit
    )
  } else {
    log_warn("Directory \"{repo}\" is not a git repository.")
    warning("Specified path is not in a git repository.")
    out <- NULL
  }
  return(out)
}

# TODO: Conflicted Repos https://github.com/r-lib/gert/pull/40
