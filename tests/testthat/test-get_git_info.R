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
    url = remote_package[["url"]], #nolint: nonportable_path_linter
    path = test_dir,
    verbose = FALSE
  )
  metadata <- get_git_info(repo = test_dir)
  expect_identical(
    metadata,
    list(
      repo = normalizePath(test_dir),
      is_git = TRUE,
      commit = remote_package[["sha"]],
      clean = TRUE,
      branch = list(
        name = remote_package[["branch"]],
        commit = remote_package[["sha"]],
        upstream = remote_package[["upstream"]],
        remote_url = remote_package[["url"]],
        up_to_date = TRUE,
        upstream_commit = remote_package[["sha"]]
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
    url = remote_package[["url"]], #nolint: nonportable_path_linter
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
      commit = remote_package[["sha"]],
      clean = FALSE,
      branch = list(
        name = remote_package[["branch"]],
        commit = remote_package[["sha"]],
        upstream = remote_package[["upstream"]],
        remote_url = remote_package[["url"]],
        up_to_date = TRUE,
        upstream_commit = remote_package[["sha"]]
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
    url = remote_package[["url"]], #nolint: nonportable_path_linter
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
        name = remote_package[["branch"]],
        commit = commit_sha,
        upstream = remote_package[["upstream"]],
        remote_url = remote_package[["url"]],
        up_to_date = FALSE,
        upstream_commit = remote_package[["sha"]]
      ),
      changed_files = list(),
      tags = list()
    )
  )
})
