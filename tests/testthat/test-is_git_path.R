# # TESTS BEGIN
test_that("is_git_path processes non-existing directory correctly", {
  test_dir <- withr::local_tempdir()
  test_dir_child <- file.path(test_dir, "child")
  expect_error(
    object = is_git_path(path = test_dir_child),
    regexp = "^Cannot find git information for path which does not exist.$"
  )
})

test_that("is_git_path processes non-existing file correctly", {
  test_dir <- withr::local_tempdir()
  test_file <- withr::local_tempfile(tmpdir = test_dir, fileext = ".rds")
  expect_error(
    object = is_git_path(path = test_file),
    regexp = "^Cannot find git information for path which does not exist.$"
  )
})

test_that("is_git_path processes non-git-repo correctly", {
  test_dir <- withr::local_tempdir()
  expect_false(is_git_path(path = test_dir))
})

test_that("is_git_path processes file in non-git-repo correctly", {
  test_dir <- withr::local_tempdir()
  test_file <- withr::local_tempfile(tmpdir = test_dir, fileext = ".rds")
  saveRDS(mtcars, test_file)
  expect_false(is_git_path(path = test_file))
})

test_that("is_git_path processes git-repo correctly", {
  test_dir <- withr::local_tempdir()
  gert::git_init(path = test_dir)
  expect_true(is_git_path(path = test_dir))
})

test_that("is_git_path processes file in git-repo correctly", {
  test_dir <- withr::local_tempdir()
  gert::git_init(path = test_dir)
  test_file <- withr::local_tempfile(tmpdir = test_dir, fileext = ".rds")
  saveRDS(mtcars, test_file)
  expect_true(is_git_path(path = test_file))
})
