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

test_that("check_dir_writable correctly identifies missing files", {
  test_file <- withr::local_tempfile()
  expect_warning(
    {results <- check_file(test_file)},
    regexp = "^File does not exist.$"
  )
  expect_false(results)
})

test_that("check_dir_writable correctly identifies missing files", {
  test_file <- withr::local_tempdir()
  expect_warning(
    {results <- check_file(test_file)},
    regexp = "^File is a directory.$"
  )
  expect_false(results)
})

test_that("check_dir_writable correctly identifies 0-byte files", {
  test_file <- withr::local_tempfile()
  file.create(test_file)
  expect_warning(
    {results <- check_file(test_file)},
    regexp = "^File is empty.$"
  )
  expect_false(results)
})

test_that("check_dir_writable correctly identifies extant, non-empty files", {
  test_file <- withr::local_tempfile()
  saveRDS(object = mtcars, file = test_file)
  results <- check_file(test_file)
  expect_true(results)
})
