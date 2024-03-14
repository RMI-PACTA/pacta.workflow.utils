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

test_that("get_single_file_metadata processes CSV tables correctly", {
  testfile <- withr::local_tempfile(fileext = ".csv")
  write.csv(mtcars, testfile, row.names = FALSE)
  test_time <- as.POSIXct("2020-01-01T12:34:56+00:00")
  Sys.setFileTime(testfile, test_time)
  metadata <- get_single_file_metadata(testfile)
  expect_identical(
    metadata,
    list(
      file_name = basename(testfile),
      file_extension = "csv",
      file_path = testfile,
      file_size = as.integer(file.size(testfile)),
      file_last_modified = format(
        as.POSIXlt(test_time, tz = "UTC"),
        "%Y-%m-%dT%H:%M:%S+00:00"
      ),
      file_md5 = digest::digest(testfile, algo = "md5", file = TRUE),
      summary_info = list(
        nrow = 32L,
        colnames = colnames(mtcars),
        class = "data.frame"
      )
    )
  )
})

test_that("get_single_file_metadata processes RDS tables correctly", {
  testfile <- withr::local_tempfile(fileext = ".rds")
  saveRDS(mtcars, testfile)
  test_time <- as.POSIXct("2020-01-01T12:34:56+00:00")
  Sys.setFileTime(testfile, test_time)
  metadata <- get_single_file_metadata(testfile)
  expect_identical(
    metadata,
    list(
      file_name = basename(testfile),
      file_extension = "rds",
      file_path = testfile,
      file_size = as.integer(file.size(testfile)),
      file_last_modified = format(
        as.POSIXlt(test_time, tz = "UTC"),
        "%Y-%m-%dT%H:%M:%S+00:00"
      ),
      file_md5 = digest::digest(testfile, algo = "md5", file = TRUE),
      summary_info = list(
        nrow = 32L,
        colnames = colnames(mtcars),
        class = "data.frame"
      )
    )
  )
})

test_that("get_single_file_metadata processes RDS non-tables correctly", {
  testfile <- withr::local_tempfile(fileext = ".rds")
  saveRDS("This is a string", testfile)
  test_time <- as.POSIXct("2020-01-01T12:34:56+00:00")
  Sys.setFileTime(testfile, test_time)
  metadata <- get_single_file_metadata(testfile)
  expect_identical(
    metadata,
    list(
      file_name = basename(testfile),
      file_extension = "rds",
      file_path = testfile,
      file_size = as.integer(file.size(testfile)),
      file_last_modified = format(
        as.POSIXlt(test_time, tz = "UTC"),
        "%Y-%m-%dT%H:%M:%S+00:00"
      ),
      file_md5 = digest::digest(testfile, algo = "md5", file = TRUE),
      summary_info = list(
        class = "character"
      )
    )
  )
})

test_that("get_single_file_metadata processes txt files correctly", {
  testfile <- withr::local_tempfile(fileext = ".txt")
  writeLines("This is a string", testfile)
  test_time <- as.POSIXct("2020-01-01T12:34:56+00:00")
  Sys.setFileTime(testfile, test_time)
  metadata <- get_single_file_metadata(testfile)
  expect_identical(
    metadata,
    list(
      file_name = basename(testfile),
      file_extension = "txt",
      file_path = testfile,
      file_size = as.integer(file.size(testfile)),
      file_last_modified = format(
        as.POSIXlt(test_time, tz = "UTC"),
        "%Y-%m-%dT%H:%M:%S+00:00"
      ),
      file_md5 = digest::digest(testfile, algo = "md5", file = TRUE)
      # No summary info
    )
  )
})

test_that("get_single_file_metadata processes lists RDS correctly", {
  testfile <- withr::local_tempfile(fileext = ".rds")
  test_list <- list(
    a = 1L,
    b = "two",
    c = list(
      d = 3.4,
      e = "four"
    )
  )
  saveRDS(test_list, testfile)
  test_time <- as.POSIXct("2020-01-01T12:34:56+00:00")
  Sys.setFileTime(testfile, test_time)
  metadata <- get_single_file_metadata(testfile)
  expect_identical(
    metadata,
    list(
      file_name = basename(testfile),
      file_extension = "rds",
      file_path = testfile,
      file_size = as.integer(file.size(testfile)),
      file_last_modified = format(
        as.POSIXlt(test_time, tz = "UTC"),
        "%Y-%m-%dT%H:%M:%S+00:00"
      ),
      file_md5 = digest::digest(testfile, algo = "md5", file = TRUE),
      summary_info = list(
        length = 3L,
        names = c("a", "b", "c"),
        class = "list"
      )
    )
  )
})

test_that("get_single_file_metadata processes named JSON list correctly", {
  testfile <- withr::local_tempfile(fileext = ".JSON")
  test_list <- list(
    a = 1L,
    b = "two",
    c = list(
      d = 3.4,
      e = "four"
    )
  )
  jsonlite::write_json(test_list, testfile, auto_unbox = TRUE)
  test_time <- as.POSIXct("2020-01-01T12:34:56+00:00")
  Sys.setFileTime(testfile, test_time)
  metadata <- get_single_file_metadata(testfile)
  expect_identical(
    metadata,
    list(
      file_name = basename(testfile),
      file_extension = "JSON",
      file_path = testfile,
      file_size = as.integer(file.size(testfile)),
      file_last_modified = format(
        as.POSIXlt(test_time, tz = "UTC"),
        "%Y-%m-%dT%H:%M:%S+00:00"
      ),
      file_md5 = digest::digest(testfile, algo = "md5", file = TRUE),
      summary_info = list(
        length = 3L,
        names = c("a", "b", "c"),
        class = "list"
      )
    )
  )
})

test_that("get_single_file_metadata processes unnamed JSON list correctly", {
  testfile <- withr::local_tempfile(fileext = ".JSON")
  test_list <- list(
    1L,
    "two",
    list(
      d = 3.4,
      e = "four"
    )
  )
  jsonlite::write_json(test_list, testfile, auto_unbox = TRUE)
  test_time <- as.POSIXct("2020-01-01T12:34:56+00:00")
  Sys.setFileTime(testfile, test_time)
  metadata <- get_single_file_metadata(testfile)
  expect_identical(
    metadata,
    list(
      file_name = basename(testfile),
      file_extension = "JSON",
      file_path = testfile,
      file_size = as.integer(file.size(testfile)),
      file_last_modified = format(
        as.POSIXlt(test_time, tz = "UTC"),
        "%Y-%m-%dT%H:%M:%S+00:00"
      ),
      file_md5 = digest::digest(testfile, algo = "md5", file = TRUE),
      summary_info = list(
        length = 3L,
        names = NULL,
        class = "list"
      )
    )
  )
})

test_that("get_single_file_metadata processes partially named JSON", {
  testfile <- withr::local_tempfile(fileext = ".JSON")
  test_list <- list(
    1L,
    b = "two",
    list(
      d = 3.4,
      e = "four"
    )
  )
  jsonlite::write_json(test_list, testfile, auto_unbox = TRUE)
  test_time <- as.POSIXct("2020-01-01T12:34:56+00:00")
  Sys.setFileTime(testfile, test_time)
  metadata <- get_single_file_metadata(testfile)
  expect_identical(
    metadata,
    list(
      file_name = basename(testfile),
      file_extension = "JSON",
      file_path = testfile,
      file_size = as.integer(file.size(testfile)),
      file_last_modified = format(
        as.POSIXlt(test_time, tz = "UTC"),
        "%Y-%m-%dT%H:%M:%S+00:00"
      ),
      file_md5 = digest::digest(testfile, algo = "md5", file = TRUE),
      summary_info = list(
        length = 3L,
        names = c("1", "b", "3"),
        class = "list"
      )
    )
  )
})

test_that("get_single_file_metadata processes JSON table correctly", {
  testfile <- withr::local_tempfile(fileext = ".JSON")
  jsonlite::write_json(mtcars, testfile, auto_unbox = TRUE)
  test_time <- as.POSIXct("2020-01-01T12:34:56+00:00")
  Sys.setFileTime(testfile, test_time)
  metadata <- get_single_file_metadata(testfile)
  expect_identical(
    metadata,
    list(
      file_name = basename(testfile),
      file_extension = "JSON",
      file_path = testfile,
      file_size = as.integer(file.size(testfile)),
      file_last_modified = format(
        as.POSIXlt(test_time, tz = "UTC"),
        "%Y-%m-%dT%H:%M:%S+00:00"
      ),
      file_md5 = digest::digest(testfile, algo = "md5", file = TRUE),
      summary_info = list(
        nrow = 32L,
        colnames = colnames(mtcars),
        class = "data.frame"
      )
    )
  )
})

test_that("missing files raise an error", {
  missing_file <- withr::local_tempfile(fileext = ".csv")
  expect_error(
    object = get_single_file_metadata(missing_file),
    regexp = "File does not exist."
  )
})

test_that("get_single_file_metadata without argument raises an error", {
  expect_error(
    object = get_single_file_metadata()
  )
})

test_that("get_single_file_metadata with multiple files raises an error", {
  first_file <- withr::local_tempfile(fileext = ".csv")
  second_file <- withr::local_tempfile(fileext = ".csv")
  expect_error(
    object = get_single_file_metadata(c(first_file, second_file)),
    regexp = "Only one file path can be passed to get_single_file_metadata."
  )
})
