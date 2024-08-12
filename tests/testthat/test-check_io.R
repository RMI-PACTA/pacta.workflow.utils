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


missing_file <- withr::local_tempfile()
file_is_dir <- withr::local_tempdir()
empty_file <- withr::local_tempfile()
file.create(empty_file)
good_file <- withr::local_tempfile()
saveRDS(object = mtcars, file = good_file)
good_file2 <- withr::local_tempfile()
saveRDS(object = mtcars, file = good_file2)

test_that("check_io correctly flags missing files", {
  expect_error(
    # suppressWarnings from check_file
    suppressWarnings(check_io(input_files = missing_file)),
    regexp = "^IO checks failed.$"
  )
})

test_that("check_io correctly flags directories as files", {
  expect_error(
    # suppressWarnings from check_file
    suppressWarnings(check_io(input_files = file_is_dir)),
    regexp = "^IO checks failed.$"
  )
})

test_that("check_io correctly flags empty files", {
  expect_error(
    # suppressWarnings from check_file
    suppressWarnings(check_io(input_files = empty_file)),
    regexp = "^IO checks failed.$"
  )
})

test_that("check_io correctly flags single good file", {
  expect_true(check_io(input_files = good_file))
})

test_that("check_io correctly flags multiple good files", {
  input_files <- c(good_file, good_file2)
  expect_true(check_io(input_files = input_files))
})

test_that("check_io correctly flags incorrect files in vector", {
  input_files <- c(
    missing_file,
    file_is_dir,
    empty_file,
    good_file,
    good_file2
  )
  expect_error(
    # suppressWarnings from check_file
    suppressWarnings(check_io(input_files = input_files)),
    regexp = "^IO checks failed.$"
  )
})

test_dir <- withr::local_tempdir()
test_dir2 <- withr::local_tempdir()
nopermissions_dir <- withr::local_tempdir()
Sys.chmod(nopermissions_dir, mode = "000")
dne_dir <- file.path(test_dir, "does_not_exist")

test_that("check_io correctly flags single good dir", {
  expect_true(check_io(output_dirs = test_dir))
})

test_that("check_io correctly flags multiple good dirs", {
  expect_true(check_io(output_dirs = c(test_dir, test_dir2)))
})

test_that("check_io correctly flags dir with no permissions", {
  expect_error(
    check_io(output_dirs = nopermissions_dir),
    regexp = "^IO checks failed.$"
  )
})

test_that("check_io correctly flags dir witdoes not exist", {
  expect_error(
    check_io(output_dirs = dne_dir),
    regexp = "^IO checks failed.$"
  )
})

