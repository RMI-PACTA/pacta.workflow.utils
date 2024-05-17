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
logger::log_threshold(logger::TRACE)
logger::log_layout(logger::layout_simple)

test_that("No inheritence", {
  params <- list(
    foo = 1L,
    string = "simple params"
  )
  results <- inherit_params(params)
  expect_identical(results, params)
})

test_that("Simple inheritence works", {
  params <- list(
    foo = 1L,
    string = "simple params",
    inherit = "test01"
  )
  param_dir <- withr::local_tempdir()
  writeLines(
    '{
      "inherited_key": 2,
      "some_other_key": "test01",
      "string": "we should not see this"
    }',
    file.path(param_dir, "test01.json")
  )
  results <- inherit_params(
    params = params,
    inheritence_search_paths = param_dir
  )
  expect_identical(
    object = results,
    expected = list(
      inherited_key = 2L,
      some_other_key = "test01",
      string = "simple params",
      foo = 1L
    )
    )
})

test_that("Simple inheritence picks the correct file", {
  params <- list(
    foo = 1L,
    string = "simple params",
    inherit = "test02"
  )
  param_dir <- withr::local_tempdir()
  writeLines(
    '{
      "inherited_key": 2,
      "some_other_key": "test01",
      "string": "we should not see this"
    }',
    file.path(param_dir, "test01.json")
  )
  writeLines(
    '{
      "inherited_key": 3,
      "some_other_key": "test02",
      "string": "we should not see this either"
    }',
    file.path(param_dir, "test02.json")
  )
  # Note that we're inheriting from test02.json
  results <- inherit_params(
    params = params,
    inheritence_search_paths = param_dir
  )
  expect_identical(
    object = results,
    expected = list(
      inherited_key = 3L,
      some_other_key = "test02",
      string = "simple params",
      foo = 1L
    )
    )
})

test_that("Nested inheritence works", {
  params <- list(
    foo = 1L,
    string = "simple params",
    inherit = "test01"
  )
  param_dir <- withr::local_tempdir()
  writeLines(
    '{
      "inherited_key": 2,
      "some_other_key": "test01",
      "string": "we should not see this",
      "test01": true,
      "inherit": "test02"
    }',
    file.path(param_dir, "test01.json")
  )
  writeLines(
    '{
      "inherited_key": 3,
      "some_other_key": "test02",
      "string": "we should not see this either",
      "test02": true
    }',
    file.path(param_dir, "test02.json")
  )
  results <- inherit_params(
    params = params,
    inheritence_search_paths = param_dir
  )
  expect_identical(
    object = results,
    expected = list(
      inherited_key = 2L,
      some_other_key = "test01",
      string = "simple params",
      test02 = TRUE,
      test01 = TRUE,
      foo = 1L
    )
    )
})

# TODO: test not inheriting
# TODO: test not inheriting (through keyname)
# TODO: test inheriting with multiple levels
# TODO: circular inheritance (do not allow!)
# TODO: multiple directories
# TODO: Missing inheritence file
# TODO: multiple named inheritence file
# TODO: multiple "inherit" keys should fail
