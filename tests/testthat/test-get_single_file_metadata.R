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
      file_md5 = "5143f7b8ed70e91698d432d721c11a63",
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
      file_md5 = "23d45331b5667757134959aa333240ae",
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
      file_md5 = "9dbb0d8e2235c70d2d8a77b1409f1597",
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
      file_md5 = "9ac4dbbc3c0ad2429e61d0df5dc28add"
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
      file_md5 = "856a49f702cfc3e12a6eb947fff06cf3",
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
      file_md5 = "6ae81bbeea491d418cfa4e3a082d7a2e",
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
      file_md5 = "ae45a51f7004c0dde31d21078020588b",
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
      file_md5 = "4ef0b7bf858cbc635cb2d826b23849f0",
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
      file_md5 = "a76f3e6fe44afa6a2533725659ba35a1",
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
