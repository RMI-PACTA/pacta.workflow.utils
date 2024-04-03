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
      changed_files = list()
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
      )
    )
  )
})

test_that("get_git_info processes git repo with a single commit correctly", {
  test_dir <- withr::local_tempdir()
  test_file <- file.path(test_dir, "foo.txt")
  writeLines("Hello, world!", con = test_file)
  gert::git_init(path = test_dir)
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
      changed_files = list()
    )
  )
})

test_that("get_git_info processes git repo with dirty index correctly", {
  test_dir <- withr::local_tempdir()
  test_file <- file.path(test_dir, "foo.txt")
  writeLines("Hello, world!", con = test_file)
  gert::git_init(path = test_dir)
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
      )
    )
  )
})

# TODO: dirty/clean
# TODO: Diffs

# moved file
