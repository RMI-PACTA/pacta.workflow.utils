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
  test_dir <- withr::local_tempdir()
  expect_true(check_dir_writable(test_dir))
})

# TESTS BEGIN
test_that("check_dir_writable correctly registers un-writable directory", {
  test_dir <- withr::local_tempdir()
  Sys.chmod(test_dir, mode = "000")
  expect_false(check_dir_writable(test_dir))
})

