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
    expected = as.POSIXct(Sys.time(), tz = "UTC"),
    tolerance = 5L
  )
  expect_identical(
    object = manifest_content[["params"]],
    expected = list()
  )
  expected_environment_info <- suppressWarnings({
      get_manifest_envirionment_info()
    })
  expected_environment_info[["packages"]][["loaded"]] <- NULL
  manifest_content[["envirionment"]][["packages"]][["loaded"]] <- NULL
  expect_identical(
    object = manifest_content[["envirionment"]],
    expected = expected_environment_info
  )
})
