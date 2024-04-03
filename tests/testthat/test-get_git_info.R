## save current settings so that we can reset later
threshold <- logger::log_threshold()
appender  <- logger::log_appender()
layout    <- logger::log_layout()
on.exit({
  ## reset logger settings
  logger::log_threshold(threshold)
  logger::log_layout(layout)
  logger::log_appender(appender)
})

logger::log_appender(logger::appender_stdout)
logger::log_threshold(logger::FATAL)
logger::log_layout(logger::layout_simple)

testing_git_config <- function(repo) {
  gert::git_config_set(repo = repo, name = "user.name", value = "testthat")
  gert::git_config_set(
    repo = repo,
    name = "user.email",
    value = "PACTATesting@rmi.org"
  )
}

# TESTS BEGIN
test_that("get_git_info processes non-git-repo correctly", {
  test_dir <- withr::local_tempdir()
  expect_warning(
    object = {
      metadata <- get_git_info(repo = test_dir)
    },
    regexp = "^Specified path is not in a git repository.$"
  )
  expect_null(metadata)
})

test_that("get_git_info processes fresh git repo correctly", {
  test_dir <- withr::local_tempdir()
  gert::git_init(path = test_dir)
  metadata <- get_git_info(repo = test_dir)
  expect_identical(
    metadata,
    list(
      repo = normalizePath(test_dir),
      is_git = TRUE,
      commit = NULL,
      clean = TRUE,
      branch = NULL,
      changed_files = list(),
      tags = list()
    )
  )
})

test_that("get_git_info processes fresh git repo with new file correctly", {
  test_dir <- withr::local_tempdir()
  gert::git_init(path = test_dir)
  test_file <- file.path(test_dir, "foo.txt")
  writeLines("Hello, world!", con = test_file)
  metadata <- get_git_info(repo = test_dir)
  expect_identical(
    metadata,
    list(
      repo = normalizePath(test_dir),
      is_git = TRUE,
      commit = NULL,
      clean = FALSE,
      branch = NULL,
      changed_files = list(
        foo.txt = "new"
      ),
      tags = list()
    )
  )
})

test_that("get_git_info processes git repo with a single commit correctly", {
  test_dir <- withr::local_tempdir()
  test_file <- file.path(test_dir, "foo.txt")
  writeLines("Hello, world!", con = test_file)
  gert::git_init(path = test_dir)
  testing_git_config(repo = test_dir)
  gert::git_add(files = basename(test_file), repo = normalizePath(test_dir))
  commit_sha <- gert::git_commit(repo = test_dir, message = "Initial commit")
  metadata <- get_git_info(repo = test_dir)
  expect_identical(
    metadata,
    list(
      repo = normalizePath(test_dir),
      is_git = TRUE,
      commit = commit_sha,
      clean = TRUE,
      branch = list(
        name = "master",
        commit = commit_sha,
        upstream = NULL,
        remote_url = NULL,
        up_to_date = NULL,
        upstream_commit = NULL
      ),
      changed_files = list(),
      tags = list()
    )
  )
})

test_that("get_git_info processes git repo with dirty index correctly", {
  test_dir <- withr::local_tempdir()
  test_file <- file.path(test_dir, "foo.txt")
  writeLines("Hello, world!", con = test_file)
  gert::git_init(path = test_dir)
  testing_git_config(repo = test_dir)
  gert::git_add(files = basename(test_file), repo = normalizePath(test_dir))
  commit_sha <- gert::git_commit(repo = test_dir, message = "Initial commit")
  writeLines("Hello, Testing!", con = test_file)
  metadata <- get_git_info(repo = test_dir)
  expect_identical(
    metadata,
    list(
      repo = normalizePath(test_dir),
      is_git = TRUE,
      commit = commit_sha,
      clean = FALSE,
      branch = list(
        name = "master",
        commit = commit_sha,
        upstream = NULL,
        remote_url = NULL,
        up_to_date = NULL,
        upstream_commit = NULL
      ),
      changed_files = list(
        foo.txt = "modified"
      ),
      tags = list()
    )
  )
})

test_that("get_git_info processes git repo with conflicts correctly", {
  test_dir <- withr::local_tempdir()
  test_file <- file.path(test_dir, "foo.txt")
  writeLines("Hello, world!", con = test_file)
  gert::git_init(path = test_dir)
  testing_git_config(repo = test_dir)
  gert::git_add(files = basename(test_file), repo = normalizePath(test_dir))
  gert::git_commit(repo = test_dir, message = "Initial commit")

  gert::git_branch_create(repo = test_dir, branch = "feature")
  writeLines("Hello, feature!", con = test_file)
  gert::git_add(files = basename(test_file), repo = normalizePath(test_dir))
  gert::git_commit(repo = test_dir, message = "Feature commit")

  gert::git_branch_checkout(repo = test_dir, branch = "master")
  writeLines("Hello, Testing!", con = test_file)
  gert::git_add(files = basename(test_file), repo = normalizePath(test_dir))
  commit_sha <- gert::git_commit(repo = test_dir, message = "Master commit")

  suppressMessages(
    gert::git_merge(repo = test_dir, ref = "feature")
  )

  metadata <- get_git_info(repo = test_dir)
  expect_identical(
    metadata,
    list(
      repo = normalizePath(test_dir),
      is_git = TRUE,
      commit = commit_sha,
      clean = FALSE,
      branch = list(
        name = "master",
        commit = commit_sha,
        upstream = NULL,
        remote_url = NULL,
        up_to_date = NULL,
        upstream_commit = NULL
      ),
      changed_files = list(
        foo.txt = "conflicted"
      ),
      tags = list()

    )
  )
})

test_that("get_git_info processes git repo with tags correctly", {
  test_dir <- withr::local_tempdir()
  test_file <- file.path(test_dir, "foo.txt")
  writeLines("Hello, world!", con = test_file)
  gert::git_init(path = test_dir)
  testing_git_config(repo = test_dir)
  gert::git_add(files = basename(test_file), repo = normalizePath(test_dir))
  commit_sha <- gert::git_commit(repo = test_dir, message = "Initial commit")
  foo_sha <- gert::git_tag_create(
    repo = test_dir,
    name = "foo",
    message = "foo",
    ref = commit_sha
  )
  bar_sha <- gert::git_tag_create(
    repo = test_dir,
    name = "bar",
    message = "bar",
    ref = commit_sha
  )
  metadata <- get_git_info(repo = test_dir)
  expect_identical(
    metadata,
    list(
      repo = normalizePath(test_dir),
      is_git = TRUE,
      commit = commit_sha,
      clean = TRUE,
      branch = list(
        name = "master",
        commit = commit_sha,
        upstream = NULL,
        remote_url = NULL,
        up_to_date = NULL,
        upstream_commit = NULL
      ),
      changed_files = list(),
      tags = list(
        bar = list(
          name = "bar",
          commit = bar_sha,
          points_to = commit_sha
        ),
        foo = list(
          name = "foo",
          commit = foo_sha,
          points_to = commit_sha
        )
      )
    )
  )
})

test_that("get_git_info processes cloned git repo", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  test_dir <- normalizePath(withr::local_tempdir())
  dl <- gert::git_clone(
    url = "https://github.com/yihui/rmini.git", #nolint: nonportable_path_linter
    path = test_dir,
    verbose = FALSE
  )
  metadata <- get_git_info(repo = test_dir)
  expect_identical(
    metadata,
    list(
      repo = normalizePath(test_dir),
      is_git = TRUE,
      commit = "f839b7327c4cb422705b9f3b7c5ffc87555d98e2",
      clean = TRUE,
      branch = list(
        name = "master",
        commit = "f839b7327c4cb422705b9f3b7c5ffc87555d98e2",
        upstream = "refs/remotes/origin/master", #nolint: nonportable_path_linter
        remote_url = "https://github.com/yihui/rmini.git",
        up_to_date = TRUE,
        upstream_commit = "f839b7327c4cb422705b9f3b7c5ffc87555d98e2"
      ),
      changed_files = list(),
      tags = list()
    )
  )
})

test_that("get_git_info processes cloned git repo with local dirty", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  test_dir <- normalizePath(withr::local_tempdir())
  dl <- gert::git_clone(
    url = "https://github.com/yihui/rmini.git", #nolint: nonportable_path_linter
    path = test_dir,
    verbose = FALSE
  )
  test_file <- file.path(test_dir, "foo.txt")
  writeLines("Hello, world!", con = test_file)
  metadata <- get_git_info(repo = test_dir)
  expect_identical(
    metadata,
    list(
      repo = normalizePath(test_dir),
      is_git = TRUE,
      commit = "f839b7327c4cb422705b9f3b7c5ffc87555d98e2",
      clean = FALSE,
      branch = list(
        name = "master",
        commit = "f839b7327c4cb422705b9f3b7c5ffc87555d98e2",
        upstream = "refs/remotes/origin/master", #nolint: nonportable_path_linter
        remote_url = "https://github.com/yihui/rmini.git",
        up_to_date = TRUE,
        upstream_commit = "f839b7327c4cb422705b9f3b7c5ffc87555d98e2"
      ),
      changed_files = list(
        foo.txt = "new"
      ),
      tags = list()
    )
  )
})

test_that("get_git_info processes cloned git repo with local commit", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  test_dir <- normalizePath(withr::local_tempdir())
  dl <- gert::git_clone(
    url = "https://github.com/yihui/rmini.git", #nolint: nonportable_path_linter
    path = test_dir,
    verbose = FALSE
  )
  testing_git_config(repo = test_dir)
  test_file <- file.path(test_dir, "foo.txt")
  writeLines("Hello, world!", con = test_file)
  gert::git_add(files = basename(test_file), repo = normalizePath(test_dir))
  commit_sha <- gert::git_commit(repo = test_dir, message = "Initial commit")
  metadata <- get_git_info(repo = test_dir)
  expect_identical(
    metadata,
    list(
      repo = normalizePath(test_dir),
      is_git = TRUE,
      commit = commit_sha,
      clean = TRUE,
      branch = list(
        name = "master",
        commit = commit_sha,
        upstream = "refs/remotes/origin/master", #nolint: nonportable_path_linter
        remote_url = "https://github.com/yihui/rmini.git",
        up_to_date = FALSE,
        upstream_commit = "f839b7327c4cb422705b9f3b7c5ffc87555d98e2"
      ),
      changed_files = list(),
      tags = list()
    )
  )
})
