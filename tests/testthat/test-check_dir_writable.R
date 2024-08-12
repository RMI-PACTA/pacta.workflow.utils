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

test_that("check_dir_writable correctly registers writable directory", {
  skip_on_os("windows")
  test_dir <- withr::local_tempdir()
  expect_true(check_dir_writable(test_dir))
})

test_that("check_dir_writable correctly errors on missing directory", {
  test_dir <- withr::local_tempdir()
  dne_dir <- file.path(test_dir, "does_not_exist")
  expect_warning(
    check_dir_writable(dne_dir),
    regexp = "^Directory does not exist.$"
  )
})

test_that("check_dir_writable correctly registers un-writable directory", {
  skip_on_os("windows")
  test_dir <- withr::local_tempdir()
  Sys.chmod(test_dir, mode = "000")
  expect_false(check_dir_writable(test_dir))
})

test_that("check_dir_writable emits warning on windows.", {
  skip_on_os(c("mac", "linux", "solaris"))
  test_dir <- withr::local_tempdir()
  expect_warning(
    check_dir_writable(test_dir),
    regexp = "^check_dir_writable may return incorrect results on Windows.$"
  )
})
