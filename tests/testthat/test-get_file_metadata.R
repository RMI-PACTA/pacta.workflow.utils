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

# setup
test_time <- as.POSIXct("2020-01-01T12:34:56+00:00")

csv_file <- withr::local_tempfile(fileext = ".csv")
write.csv(mtcars, csv_file, row.names = FALSE)
Sys.setFileTime(csv_file, test_time)
csv_metadata <- list(
  file_name = basename(csv_file),
  file_extension = "csv",
  file_path = csv_file,
  file_size_human = format(
    structure(as.integer(file.size(csv_file)), class = "object_size"), # nolint: undesirable_function_linter
    units = "auto",
    standard = "SI"
  ),
  file_size = as.integer(file.size(csv_file)),
  file_last_modified = format(
    as.POSIXlt(test_time, tz = "UTC"),
    "%Y-%m-%dT%H:%M:%S+00:00"
  ),
  file_md5 = digest::digest(csv_file, algo = "md5", file = TRUE)
)
csv_metadata_summary <- c(
  csv_metadata,
  list(
    summary_info = list(
      nrow = 32L,
      colnames = colnames(mtcars),
      class = "data.frame"
    )
  )
)

rds_file <- withr::local_tempfile(fileext = ".rds")
saveRDS(mtcars, rds_file)
Sys.setFileTime(rds_file, test_time)
rds_metadata <- list(
  file_name = basename(rds_file),
  file_extension = "rds",
  file_path = rds_file,
  file_size_human = format(
    structure(as.integer(file.size(rds_file)), class = "object_size"),# nolint: undesirable_function_linter
    units = "auto",
    standard = "SI"
  ),
  file_size = as.integer(file.size(rds_file)),
  file_last_modified = format(
    as.POSIXlt(test_time, tz = "UTC"),
    "%Y-%m-%dT%H:%M:%S+00:00"
  ),
  file_md5 = digest::digest(rds_file, algo = "md5", file = TRUE)
)
rds_metadata_summary <- c(
  rds_metadata,
  list(
    summary_info = list(
      nrow = 32L,
      colnames = colnames(mtcars),
      class = "data.frame"
    )
  )
)

# TESTS BEGIN
test_that("get_file_metadata processes single files correctly", {
  metadata <- get_file_metadata(csv_file)
  expect_identical(
    metadata,
    list(
      csv_metadata
    )
  )
})

test_that("get_file_metadata processes a vector of files correctly", {
  metadata <- get_file_metadata(c(csv_file, rds_file))
  expect_identical(
    metadata,
    list(
      csv_metadata,
      rds_metadata
    )
  )
})

test_that("get_file_metadata processes a list of files correctly", {
  metadata <- get_file_metadata(list(csv_file, rds_file))
  expect_identical(
    metadata,
    list(
      csv_metadata,
      rds_metadata
    )
  )
})

test_that("get_file_metadata respects input order", {
  metadata <- get_file_metadata(c(rds_file, csv_file))
  expect_identical(
    metadata,
    list(
      rds_metadata,
      csv_metadata
    )
  )
})

test_that("get_file_metadata returns an empty list on empty input", {
  metadata <- get_file_metadata(list())
  expect_identical(
    metadata,
    list()
  )
})

test_that("get_file_metadata returns an empty list on NULL input", {
  metadata <- get_file_metadata(NULL)
  expect_identical(
    metadata,
    list()
  )
})

missing_file <- withr::local_tempfile(fileext = ".foo")
test_that("missing files raise an error", {
  expect_error(
    object = get_file_metadata(missing_file),
    regexp = "File does not exist."
  )
})

test_that("get_single_file_metadata without argument raises an error", {
  expect_error(
    object = get_file_metadata()
  )
})
