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
      branch = NULL,
      commit = NULL
    )
  )
})

test_that("get_git_info processes git repo with a single commit correctly", {
  test_dir <- withr::local_tempdir()
  test_file <- withr::local_tempfile(tmpdir = test_dir, fileext = ".txt")
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
      branch = list(
        name = "master",
        commit = commit_sha,
        upstream = NULL,
        remote_url = NULL,
        up_to_date = NULL,
        upstream_commit = NULL
      ),
      commit = commit_sha
    )
  )
})

# TODO: dirty/clean
# TODO: Diffs
