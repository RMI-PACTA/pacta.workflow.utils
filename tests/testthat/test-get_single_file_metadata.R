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
