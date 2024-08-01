# ## save current settings so that we can reset later
# threshold <- logger::log_threshold()
# appender  <- logger::log_appender()
# layout    <- logger::log_layout()
# on.exit({
#   ## reset logger settings
#   logger::log_threshold(threshold)
#   logger::log_layout(layout)
#   logger::log_appender(appender)
# })

# logger::log_appender(logger::appender_stdout)
# logger::log_threshold(logger::FATAL)
# logger::log_layout(logger::layout_simple)

test_that("create_manifest with minimal arguments", {
  suppressWarnings({
    manifest <- create_manifest(
      input_files = NULL,
      output_files = NULL,
      file_summary_info = TRUE
    )
    expected_environment_info <- get_manifest_envirionment_info()
  })

  expect_type(manifest, "list")
  expect_named(
    object = manifest,
    expected = c(
      "input_files",
      "output_files",
      "envirionment",
      "manifest_creation_datetime"
    )
  )
  expect_identical(
    object = manifest[["input_files"]],
    expected = list()
  )
  expect_identical(
    object = manifest[["output_files"]],
    expected = list()
  )
  # loaded packages can change during testthat testing
  expected_environment_info <- suppressWarnings({
    get_manifest_envirionment_info()
  })
  expected_environment_info[["packages"]][["loaded"]] <- list()
  manifest[["envirionment"]][["packages"]][["loaded"]] <- list()
  expect_identical(
    object = manifest[["envirionment"]],
    expected = expected_environment_info
  )
  expect_equal(
    object = as.numeric(
      as.POSIXct(manifest[["manifest_creation_datetime"]], tz = "UTC")
    ),
    expected = as.numeric(as.POSIXct(Sys.time(), tz = "UTC")),
    tolerance = 5L
  )
})

test_that("create_manifest with works with simple file arguments", {
  csv_file <- withr::local_tempfile(fileext = ".csv")
  write.csv(mtcars, csv_file, row.names = FALSE)
  csv_info <- list(
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
      as.POSIXlt(file.mtime(csv_file), tz = "UTC"),
      "%Y-%m-%dT%H:%M:%S+00:00"
    ),
    file_md5 = digest::digest(file = csv_file, algo = "md5")
  )

  suppressWarnings({
    manifest <- create_manifest(
      input_files = csv_file,
      output_files = csv_file
    )
  })

  expect_type(manifest, "list")
  expect_named(
    object = manifest,
    expected = c(
      "input_files",
      "output_files",
      "envirionment",
      "manifest_creation_datetime"
    )
  )
  expect_identical(
    object = manifest[["input_files"]],
    expected = list(csv_info)
  )
  expect_identical(
    object = manifest[["output_files"]],
    expected = list(csv_info)
  )
})

test_that("create_manifest with works with simple file arguments and summary", {
  csv_file <- withr::local_tempfile(fileext = ".csv")
  write.csv(mtcars, csv_file, row.names = FALSE)
  csv_info <- list(
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
      as.POSIXlt(file.mtime(csv_file), tz = "UTC"),
      "%Y-%m-%dT%H:%M:%S+00:00"
    ),
    file_md5 = digest::digest(file = csv_file, algo = "md5"),
    summary_info = list(
      nrow = nrow(mtcars),
      colnames = colnames(mtcars),
      class = "data.frame"
    )
  )

  suppressWarnings({
    manifest <- create_manifest(
      input_files = csv_file,
      output_files = csv_file,
      file_summary_info = TRUE
    )
  })

  expect_type(manifest, "list")
  expect_named(
    object = manifest,
    expected = c(
      "input_files",
      "output_files",
      "envirionment",
      "manifest_creation_datetime"
    )
  )
  expect_identical(
    object = manifest[["input_files"]],
    expected = list(csv_info)
  )
  expect_identical(
    object = manifest[["output_files"]],
    expected = list(csv_info)
  )
})

test_that("create_manifest with works with vector file arguments", {
  csv_file <- withr::local_tempfile(fileext = ".csv")
  write.csv(mtcars, csv_file, row.names = FALSE)
  csv_info <- list(
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
      as.POSIXlt(file.mtime(csv_file), tz = "UTC"),
      "%Y-%m-%dT%H:%M:%S+00:00"
    ),
    file_md5 = digest::digest(file = csv_file, algo = "md5"),
    summary_info = list(
      nrow = nrow(mtcars),
      colnames = colnames(mtcars),
      class = "data.frame"
    )
  )

  rds_file <- withr::local_tempfile(fileext = ".rds")
  saveRDS(mtcars, rds_file)
  rds_info <- list(
    file_name = basename(rds_file),
    file_extension = "rds",
    file_path = rds_file,
    file_size_human = format(
      structure(as.integer(file.size(rds_file)), class = "object_size"), # nolint: undesirable_function_linter
      units = "auto",
      standard = "SI"
    ),
    file_size = as.integer(file.size(rds_file)),
    file_last_modified = format(
      as.POSIXlt(file.mtime(rds_file), tz = "UTC"),
      "%Y-%m-%dT%H:%M:%S+00:00"
    ),
    file_md5 = digest::digest(file = rds_file, algo = "md5"),
    summary_info = list(
      nrow = nrow(mtcars),
      colnames = colnames(mtcars),
      class = "data.frame"
    )
  )

  suppressWarnings({
    manifest <- create_manifest(
      input_files = c(csv_file, rds_file),
      output_files = c(csv_file, rds_file),
      file_summary_info = TRUE
    )
  })

  expect_type(manifest, "list")
  expect_named(
    object = manifest,
    expected = c(
      "input_files",
      "output_files",
      "envirionment",
      "manifest_creation_datetime"
    )
  )
  expect_identical(
    object = manifest[["input_files"]],
    expected = list(csv_info, rds_info)
  )
  expect_identical(
    object = manifest[["output_files"]],
    expected = list(csv_info, rds_info)
  )
})

test_that("create_manifest with works with named vector file arguments", {
  csv_file <- withr::local_tempfile(fileext = ".csv")
  write.csv(mtcars, csv_file, row.names = FALSE)
  csv_info <- list(
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
      as.POSIXlt(file.mtime(csv_file), tz = "UTC"),
      "%Y-%m-%dT%H:%M:%S+00:00"
    ),
    file_md5 = digest::digest(file = csv_file, algo = "md5"),
    summary_info = list(
      nrow = nrow(mtcars),
      colnames = colnames(mtcars),
      class = "data.frame"
    )
  )

  rds_file <- withr::local_tempfile(fileext = ".rds")
  saveRDS(mtcars, rds_file)
  rds_info <- list(
    file_name = basename(rds_file),
    file_extension = "rds",
    file_path = rds_file,
    file_size_human = format(
      structure(as.integer(file.size(rds_file)), class = "object_size"), # nolint: undesirable_function_linter
      units = "auto",
      standard = "SI"
    ),
    file_size = as.integer(file.size(rds_file)),
    file_last_modified = format(
      as.POSIXlt(file.mtime(rds_file), tz = "UTC"),
      "%Y-%m-%dT%H:%M:%S+00:00"
    ),
    file_md5 = digest::digest(file = rds_file, algo = "md5"),
    summary_info = list(
      nrow = nrow(mtcars),
      colnames = colnames(mtcars),
      class = "data.frame"
    )
  )

  suppressWarnings({
    manifest <- create_manifest(
      input_files = c(foo = csv_file, bar = rds_file),
      output_files = NULL,
      file_summary_info = TRUE
    )
  })

  expect_type(manifest, "list")
  expect_named(
    object = manifest,
    expected = c(
      "input_files",
      "output_files",
      "envirionment",
      "manifest_creation_datetime"
    )
  )
  expect_identical(
    object = manifest[["input_files"]],
    expected = list(foo = csv_info, bar = rds_info)
  )
  expect_identical(
    object = manifest[["output_files"]],
    expected = list()
  )
})

test_that("create_manifest with works with named list file arguments", {
  csv_file <- withr::local_tempfile(fileext = ".csv")
  write.csv(mtcars, csv_file, row.names = FALSE)
  csv_info <- list(
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
      as.POSIXlt(file.mtime(csv_file), tz = "UTC"),
      "%Y-%m-%dT%H:%M:%S+00:00"
    ),
    file_md5 = digest::digest(file = csv_file, algo = "md5"),
    summary_info = list(
      nrow = nrow(mtcars),
      colnames = colnames(mtcars),
      class = "data.frame"
    )
  )

  rds_file <- withr::local_tempfile(fileext = ".rds")
  saveRDS(mtcars, rds_file)
  rds_info <- list(
    file_name = basename(rds_file),
    file_extension = "rds",
    file_path = rds_file,
    file_size_human = format(
      structure(as.integer(file.size(rds_file)), class = "object_size"), # nolint: undesirable_function_linter
      units = "auto",
      standard = "SI"
    ),
    file_size = as.integer(file.size(rds_file)),
    file_last_modified = format(
      as.POSIXlt(file.mtime(rds_file), tz = "UTC"),
      "%Y-%m-%dT%H:%M:%S+00:00"
    ),
    file_md5 = digest::digest(file = rds_file, algo = "md5"),
    summary_info = list(
      nrow = nrow(mtcars),
      colnames = colnames(mtcars),
      class = "data.frame"
    )
  )

  suppressWarnings({
    manifest <- create_manifest(
      input_files = list(foo = csv_file, bar = rds_file),
      output_files = NULL,
      file_summary_info = TRUE
    )
  })

  expect_type(manifest, "list")
  expect_named(
    object = manifest,
    expected = c(
      "input_files",
      "output_files",
      "envirionment",
      "manifest_creation_datetime"
    )
  )
  expect_identical(
    object = manifest[["input_files"]],
    expected = list(foo = csv_info, bar = rds_info)
  )
  expect_identical(
    object = manifest[["output_files"]],
    expected = list()
  )
})

test_that("create_manifest works with simple ... arguments", {
  suppressWarnings({
    manifest <- create_manifest(
      input_files = NULL,
      output_files = NULL,
      params = list(foo = "bar"),
      file_summary_info = TRUE
    )
  })
  expect_type(manifest, "list")
  expect_named(
    object = manifest,
    expected = c(
      "input_files",
      "output_files",
      "envirionment",
      "manifest_creation_datetime",
      "params"
    )
  )
  expect_identical(
    object = manifest[["params"]],
    expected = list(foo = "bar")
  )
})

test_that("create_manifest works with nested ... arguments", {
  suppressWarnings({
    manifest <- create_manifest(
      input_files = NULL,
      output_files = NULL,
      params = list(foo = "bar"),
      foo = list(
        bar = list(
          baz = seq(1L, 5L),
          quux = 3.14159
        ),
        grault = "garply"
      ),
      file_summary_info = TRUE
    )
  })
  expect_type(manifest, "list")
  expect_named(
    object = manifest,
    expected = c(
      "input_files",
      "output_files",
      "envirionment",
      "manifest_creation_datetime",
      "params",
      "foo"
    )
  )
  expect_identical(
    object = manifest[["params"]],
    expected = list(foo = "bar")
  )
  expect_identical(
    object = manifest[["foo"]],
    expected = list(
      bar = list(
        baz = seq(1L, 5L),
        quux = 3.14159
      ),
      grault = "garply"
    )
  )
})

test_that("create_manifest fails with unnamed ... arguments", {
  expect_error(
    object = create_manifest(
      input_files = NULL,
      output_files = NULL,
      params = list(foo = "bar"),
      "Hello"
    )
  )
})

test_that("create_manifest fails with unnamed in nesting of ... arguments", {
  expect_error(
    object = create_manifest(
      input_files = NULL,
      output_files = NULL,
      params = list(foo = "bar"),
      foo = list(
        bar = list(
          "Hello",
          "world"
        )
      )
    )
  )
})
