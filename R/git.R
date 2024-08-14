#' get_git_info
#'
#' Get git information for a repository.
#'
#' @param repo (Local directory) path to a git repository.
#' @return list of git information. Keys include:
#'   * repo: path to the repository.
#'   * is_git: logical indicating if the path is in a git repository.
#'   * commit: latest commit hash.
#'   * clean: logical indicating if the repository is clean.
#'   * branch: list of branch information. See `git_branch_info`.
#'   * changed_files: list of changed files. See `git_changed_files`.
#'   * tags: list of tags. See `git_tag_info`.
#' @export
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
    changed_files <- git_changed_files(repo = git_repo)
    # cleaning path for older versions of R on windows
    repo_path <- gsub(
      x = normalizePath(info[["path"]]),
      pattern = "[\\]+$", # nolint: nonportable_path_linter
      replacement = ""
    )
    out <- list(
      repo = repo_path,
      is_git = TRUE,
      commit = latest_commit,
      clean = (length(changed_files) == 0L),
      branch = git_branch_info(repo = git_repo),
      changed_files = changed_files,
      tags = git_tag_info(repo = git_repo)
    )
  } else {
    log_warn("Directory \"{repo}\" is not a git repository.")
    warning("Specified path is not in a git repository.")
    out <- NULL
  }
  return(out)
}

#' is_git_path
#'
#' Check if a path is in a git repository.
#'
#' @param path (Local directory) path to check.
#' @return logical indicating if the path is in a git repository.
#' @export
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

#' git_branch_info
#'
#' Get branch information for a repository.
#'
#' @param repo (Local directory) path to a git repository.
#' @return list of branch information. Keys include:
#'   * name: name of the active branch.
#'   * commit: commit hash of the active branch.
#'   * upstream: name of the upstream branch.
#'   * remote_url: URL of the remote repository.
#'   * up_to_date: logical indicating if the active branch is up to date with
#'   remote
#'   * upstream_commit: commit hash of the upstream branch.
#' @export
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
      remote_url <- NULL
    } else {
      log_trace(
        "Branch \"{active_branch}\" has an upstream: \"{active_upstream}\"."
      )
      active_upstream <- gsub(
        pattern = "refs/heads/", # nolint: nonportable_path_linter
        replacement = "",
        x = active_upstream
      )
      upstream_index <- which(branch_list[["ref"]] == active_upstream)
      upstream_commit <- branch_list[[upstream_index, "commit"]]
      up_to_date <- active_commit == upstream_commit
      # format of remote ref: refs/remotes/origin/branch
      remote_name <- strsplit(
        x = active_upstream,
        split = "/",
        fixed = TRUE
      )[[1L]][[3L]]
      remote_info <- gert::git_remote_info(
        repo = git_repo,
        remote = remote_name
      )
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

#' git_changed_files
#'
#' Get changed files in a repository.
#'
#' @param repo (Local directory) path to a git repository.
#' @return list of changed files. Keys are file paths, values are status.
#' @export
git_changed_files <- function(repo) {
  log_trace("checking for changed files in repo \"{repo}\".")
  if (is_git_path(repo)) {
    git_repo <- gert::git_find(path = repo)
    status <- gert::git_status(repo = git_repo)
    changed_files <- list()
    for (f in status[["file"]]) {
      changed_files[[f]] <- status[["status"]][status[["file"]] == f]
    }
    return(changed_files)
  } else {
    log_debug("Specified path is not in a git repository.")
    return(NULL)
  }
}

#' git_tag_info
#'
#' Get tag information for a repository.
#'
#' @param repo (Local directory) path to a git repository.
#' @return list of tag information. Keys are tag names, values are lists with
#' keys:
#'   * name: tag name.
#'   * commit: commit hash.
#'   * points_to: commit hash that the tag points to.
#' @export
git_tag_info <- function(repo) {
  log_trace("checking for tags in repo \"{repo}\".")
  if (is_git_path(repo)) {
    git_repo <- gert::git_find(path = repo)
    tags_df <- gert::git_tag_list(repo = git_repo)
    tags <- list()
    for (i in seq_along(tags_df[["name"]])) {
      tag_name <- tags_df[["name"]][i]
      tag_commit <- tags_df[["commit"]][i]
      tag_pointer <- gert::git_commit_info(repo = git_repo, ref = tag_commit)
      tags[[tag_name]] <- list(
        name = tag_name,
        commit = tag_commit,
        points_to = tag_pointer[["id"]]
      )
    }
    return(tags)
  } else {
    log_debug("Specified path is not in a git repository.")
    return(NULL)
  }
}
