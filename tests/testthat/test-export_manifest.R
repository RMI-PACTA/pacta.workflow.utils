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

test_that("export_manifest with minimal arguments", {
  manifest_file <- withr::local_tempfile(fileext = ".json")
  suppressWarnings({
    manifest <- export_manifest(
      manifest_path = manifest_file,
      input_files = NULL,
      output_files = NULL,
      params = list()
    )
  })

  expect_true(file.exists(manifest_file))
  expect_gt(file.size(manifest_file), 0L)
  manifest_content <- jsonlite::fromJSON(txt = manifest_file)
  creation_time <- as.POSIXct(Sys.time(), tz = "UTC")
  attr(creation_time, "tzone") <- "UTC"
  expect_type(manifest_content, "list")
  expect_named(
    object = manifest_content,
    expected = c(
      "input_files",
      "output_files",
      "envirionment",
      "manifest_creation_datetime",
      "params"
    )
  )
  expect_identical(
    object = manifest_content[["input_files"]],
    expected = list()
  )
  expect_identical(
    object = manifest_content[["output_files"]],
    expected = list()
  )
  expect_equal(
    object = as.POSIXct(
      manifest_content[["manifest_creation_datetime"]],
      tz = "UTC"
    ),
    expected = creation_time,
    tolerance = 1L
  )
  expect_identical(
    object = manifest_content[["params"]],
    expected = list()
  )
  # loaded packages can change during testthat testing
  expected_environment_info <- suppressWarnings({
    get_manifest_envirionment_info()
  })
  expected_environment_info[["packages"]][["loaded"]] <- list()
  manifest_content[["envirionment"]][["packages"]][["loaded"]] <- list()
  expect_identical(
    object = manifest_content[["envirionment"]],
    expected = expected_environment_info
  )
})

test_that("export_manifest with minimal arguments and file summary info", {
  manifest_file <- withr::local_tempfile(fileext = ".json")
  suppressWarnings({
    manifest <- export_manifest(
      manifest_path = manifest_file,
      input_files = NULL,
      output_files = NULL,
      params = list(),
      file_summary_info = TRUE
    )
  })

  expect_true(file.exists(manifest_file))
  expect_gt(file.size(manifest_file), 0L)
  manifest_content <- jsonlite::fromJSON(txt = manifest_file)
  creation_time <- as.POSIXct(Sys.time(), tz = "UTC")
  attr(creation_time, "tzone") <- "UTC"
  expect_type(manifest_content, "list")
  expect_named(
    object = manifest_content,
    expected = c(
      "input_files",
      "output_files",
      "envirionment",
      "manifest_creation_datetime",
      "params"
    )
  )
  expect_identical(
    object = manifest_content[["input_files"]],
    expected = list()
  )
  expect_identical(
    object = manifest_content[["output_files"]],
    expected = list()
  )
  expect_equal(
    object = as.POSIXct(
      manifest_content[["manifest_creation_datetime"]],
      tz = "UTC"
    ),
    expected = creation_time,
    tolerance = 1L
  )
  expect_identical(
    object = manifest_content[["params"]],
    expected = list()
  )
  # loaded packages can change during testthat testing
  expected_environment_info <- suppressWarnings({
    get_manifest_envirionment_info()
  })
  expected_environment_info[["packages"]][["loaded"]] <- list()
  manifest_content[["envirionment"]][["packages"]][["loaded"]] <- list()
  expect_identical(
    object = manifest_content[["envirionment"]],
    expected = expected_environment_info
  )
})

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

test_that("export_manifest with files with summary info", {
  manifest_file <- withr::local_tempfile(fileext = ".json")
  suppressWarnings({
    manifest <- export_manifest(
      manifest_path = manifest_file,
      input_files = csv_file,
      output_files = rds_file,
      params = list(),
      file_summary_info = TRUE
    )
  })

  expect_true(file.exists(manifest_file))
  expect_gt(file.size(manifest_file), 0L)
  manifest_content <- jsonlite::fromJSON(txt = manifest_file, simplifyDataFrame = FALSE)
  creation_time <- as.POSIXct(Sys.time(), tz = "UTC")
  attr(creation_time, "tzone") <- "UTC"
  expect_type(manifest_content, "list")
  expect_named(
    object = manifest_content,
    expected = c(
      "input_files",
      "output_files",
      "envirionment",
      "manifest_creation_datetime",
      "params"
    )
  )
  expect_identical(
    object = manifest_content[["input_files"]][[1L]],
    expected = csv_metadata_summary
  )
  expect_identical(
    object = manifest_content[["output_files"]][[1L]],
    expected = rds_metadata_summary
  )
  expect_equal(
    object = as.POSIXct(
      manifest_content[["manifest_creation_datetime"]],
      tz = "UTC"
    ),
    expected = creation_time,
    tolerance = 1L
  )
  expect_identical(
    object = manifest_content[["params"]],
    expected = list()
  )
  # loaded packages can change during testthat testing
  expected_environment_info <- suppressWarnings({
    get_manifest_envirionment_info()
  })
  expected_environment_info[["packages"]][["loaded"]] <- list()
  manifest_content[["envirionment"]][["packages"]][["loaded"]] <- list()
  expect_identical(
    object = manifest_content[["envirionment"]],
    expected = expected_environment_info
  )
})

test_that("export_manifest with files", {
  manifest_file <- withr::local_tempfile(fileext = ".json")
  suppressWarnings({
    manifest <- export_manifest(
      manifest_path = manifest_file,
      input_files = csv_file,
      output_files = rds_file,
      params = list()
    )
  })

  expect_true(file.exists(manifest_file))
  expect_gt(file.size(manifest_file), 0L)
  manifest_content <- jsonlite::fromJSON(txt = manifest_file)
  creation_time <- as.POSIXct(Sys.time(), tz = "UTC")
  attr(creation_time, "tzone") <- "UTC"
  expect_type(manifest_content, "list")
  expect_named(
    object = manifest_content,
    expected = c(
      "input_files",
      "output_files",
      "envirionment",
      "manifest_creation_datetime",
      "params"
    )
  )
  expect_identical(
    object = as.list(manifest_content[["input_files"]]),
    expected = csv_metadata
  )
  expect_identical(
    object = as.list(manifest_content[["output_files"]]),
    expected = rds_metadata
  )
  expect_equal(
    object = as.POSIXct(
      manifest_content[["manifest_creation_datetime"]],
      tz = "UTC"
    ),
    expected = creation_time,
    tolerance = 1L
  )
  expect_identical(
    object = manifest_content[["params"]],
    expected = list()
  )
  # loaded packages can change during testthat testing
  expected_environment_info <- suppressWarnings({
    get_manifest_envirionment_info()
  })
  expected_environment_info[["packages"]][["loaded"]] <- list()
  manifest_content[["envirionment"]][["packages"]][["loaded"]] <- list()
  expect_identical(
    object = manifest_content[["envirionment"]],
    expected = expected_environment_info
  )
})

