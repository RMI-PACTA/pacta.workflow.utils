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
  csv_file <- withr::local_tempfile(fileext = ".csv")
  write.csv(mtcars, csv_file, row.names = FALSE)
  test_time <- as.POSIXct("2020-01-01T12:34:56+00:00")
  Sys.setFileTime(csv_file, test_time)
  metadata <- get_single_file_metadata(csv_file)
  expect_identical(
    metadata,
    list(
      file_name = basename(csv_file),
      file_extension = "csv",
      file_path = csv_file,
      file_size = 1303L,
      file_last_modified = format(
        as.POSIXlt(test_time, tz = "UTC"),
        "%Y-%m-%dT%H:%M:%S+00:00"
      ),
      file_md5 = digest::digest(csv_file, algo = "md5", file = TRUE),
      summary_info = list(
        nrow = 32L,
        colnames = colnames(mtcars),
        class = "data.frame"
      )
    )
  )
})

test_that("get_single_file_metadata processes RDS tables correctly", {
  rds_file <- withr::local_tempfile(fileext = ".rds")
  saveRDS(mtcars, rds_file)
  test_time <- as.POSIXct("2020-01-01T12:34:56+00:00")
  Sys.setFileTime(rds_file, test_time)
  metadata <- get_single_file_metadata(rds_file)
  expect_identical(
    metadata,
    list(
      file_name = basename(rds_file),
      file_extension = "rds",
      file_path = rds_file,
      file_size = 1225L,
      file_last_modified = format(
        as.POSIXlt(test_time, tz = "UTC"),
        "%Y-%m-%dT%H:%M:%S+00:00"
      ),
      file_md5 = digest::digest(rds_file, algo = "md5", file = TRUE),
      summary_info = list(
        nrow = 32L,
        colnames = colnames(mtcars),
        class = "data.frame"
      )
    )
  )
})

test_that("get_single_file_metadata processes RDS non-tables correctly", {
  rds_file <- withr::local_tempfile(fileext = ".rds")
  saveRDS("This is a string", rds_file)
  test_time <- as.POSIXct("2020-01-01T12:34:56+00:00")
  Sys.setFileTime(rds_file, test_time)
  metadata <- get_single_file_metadata(rds_file)
  expect_identical(
    metadata,
    list(
      file_name = basename(rds_file),
      file_extension = "rds",
      file_path = rds_file,
      file_size = 67L,
      file_last_modified = format(
        as.POSIXlt(test_time, tz = "UTC"),
        "%Y-%m-%dT%H:%M:%S+00:00"
      ),
      file_md5 = digest::digest(rds_file, algo = "md5", file = TRUE),
      summary_info = list(
        class = "character"
      )
    )
  )
})

test_that("get_single_file_metadata processes txt files correctly", {
  txt_file <- withr::local_tempfile(fileext = ".txt")
  writeLines("This is a string", txt_file)
  test_time <- as.POSIXct("2020-01-01T12:34:56+00:00")
  Sys.setFileTime(txt_file, test_time)
  metadata <- get_single_file_metadata(txt_file)
  expect_identical(
    metadata,
    list(
      file_name = basename(txt_file),
      file_extension = "txt",
      file_path = txt_file,
      file_size = 17L,
      file_last_modified = format(
        as.POSIXlt(test_time, tz = "UTC"),
        "%Y-%m-%dT%H:%M:%S+00:00"
      ),
      file_md5 = digest::digest(txt_file, algo = "md5", file = TRUE)
      # No summary info
    )
  )
})

test_that("get_single_file_metadata processes lists RDS correctly", {
  rds_file <- withr::local_tempfile(fileext = ".rds")
  test_list <- list(
    a = 1L,
    b = "two",
    c = list(
      d = 3.4,
      e = "four"
    )
  )
  saveRDS(test_list, rds_file)
  test_time <- as.POSIXct("2020-01-01T12:34:56+00:00")
  Sys.setFileTime(rds_file, test_time)
  metadata <- get_single_file_metadata(rds_file)
  expect_identical(
    metadata,
    list(
      file_name = basename(rds_file),
      file_extension = "rds",
      file_path = rds_file,
      file_size = 124L,
      file_last_modified = format(
        as.POSIXlt(test_time, tz = "UTC"),
        "%Y-%m-%dT%H:%M:%S+00:00"
      ),
      file_md5 = digest::digest(rds_file, algo = "md5", file = TRUE),
      summary_info = list(
        length = 3L,
        names = c("a", "b", "c"),
        class = "list"
      )
    )
  )
})

test_that("get_single_file_metadata processes named JSON list correctly", {
  json_file <- withr::local_tempfile(fileext = ".JSON")
  test_list <- list(
    a = 1L,
    b = "two",
    c = list(
      d = 3.4,
      e = "four"
    )
  )
  jsonlite::write_json(test_list, json_file, auto_unbox = TRUE)
  test_time <- as.POSIXct("2020-01-01T12:34:56+00:00")
  Sys.setFileTime(json_file, test_time)
  metadata <- get_single_file_metadata(json_file)
  expect_identical(
    metadata,
    list(
      file_name = basename(json_file),
      file_extension = "JSON",
      file_path = json_file,
      file_size = 43L,
      file_last_modified = format(
        as.POSIXlt(test_time, tz = "UTC"),
        "%Y-%m-%dT%H:%M:%S+00:00"
      ),
      file_md5 = digest::digest(json_file, algo = "md5", file = TRUE),
      summary_info = list(
        length = 3L,
        names = c("a", "b", "c"),
        class = "list"
      )
    )
  )
})

test_that("get_single_file_metadata processes unnamed JSON list correctly", {
  json_file <- withr::local_tempfile(fileext = ".JSON")
  test_list <- list(
    1L,
    "two",
    list(
      d = 3.4,
      e = "four"
    )
  )
  jsonlite::write_json(test_list, json_file, auto_unbox = TRUE)
  test_time <- as.POSIXct("2020-01-01T12:34:56+00:00")
  Sys.setFileTime(json_file, test_time)
  metadata <- get_single_file_metadata(json_file)
  expect_identical(
    metadata,
    list(
      file_name = basename(json_file),
      file_extension = "JSON",
      file_path = json_file,
      file_size = 31L,
      file_last_modified = format(
        as.POSIXlt(test_time, tz = "UTC"),
        "%Y-%m-%dT%H:%M:%S+00:00"
      ),
      file_md5 = digest::digest(json_file, algo = "md5", file = TRUE),
      summary_info = list(
        length = 3L,
        names = NULL,
        class = "list"
      )
    )
  )
})

test_that("get_single_file_metadata processes partially named JSON", {
  json_file <- withr::local_tempfile(fileext = ".JSON")
  test_list <- list(
    1L,
    b = "two",
    list(
      d = 3.4,
      e = "four"
    )
  )
  jsonlite::write_json(test_list, json_file, auto_unbox = TRUE)
  test_time <- as.POSIXct("2020-01-01T12:34:56+00:00")
  Sys.setFileTime(json_file, test_time)
  metadata <- get_single_file_metadata(json_file)
  expect_identical(
    metadata,
    list(
      file_name = basename(json_file),
      file_extension = "JSON",
      file_path = json_file,
      file_size = 43L,
      file_last_modified = format(
        as.POSIXlt(test_time, tz = "UTC"),
        "%Y-%m-%dT%H:%M:%S+00:00"
      ),
      file_md5 = digest::digest(json_file, algo = "md5", file = TRUE),
      summary_info = list(
        length = 3L,
        names = c("1", "b", "3"),
        class = "list"
      )
    )
  )
})

test_that("get_single_file_metadata processes JSON table correctly", {
  json_file <- withr::local_tempfile(fileext = ".JSON")
  jsonlite::write_json(mtcars, json_file, auto_unbox = TRUE)
  test_time <- as.POSIXct("2020-01-01T12:34:56+00:00")
  Sys.setFileTime(json_file, test_time)
  metadata <- get_single_file_metadata(json_file)
  expect_identical(
    metadata,
    list(
      file_name = basename(json_file),
      file_extension = "JSON",
      file_path = json_file,
      file_size = 4147L,
      file_last_modified = format(
        as.POSIXlt(test_time, tz = "UTC"),
        "%Y-%m-%dT%H:%M:%S+00:00"
      ),
      file_md5 = digest::digest(json_file, algo = "md5", file = TRUE),
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

