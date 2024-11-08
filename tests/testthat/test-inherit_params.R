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

test_that("Only inheritence works", {
  params <- list(
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
      string = "we should not see this"
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

test_that("Missing inheritence file throws error", {
  params <- list(
    foo = 1L,
    string = "simple params",
    inherit = "test01"
  )
  param_dir <- withr::local_tempdir()
  testthat::expect_error(
    inherit_params(
      params = params,
      inheritence_search_paths = param_dir
    ),
    regexp = "^Inheritence file not found.$"
  )
})

test_that("Circular inheritence throws error", {
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
      "inherit": "test02"
    }',
    file.path(param_dir, "test01.json")
  )
  writeLines(
    '{
      "inherited_key": 3,
      "some_other_key": "test02",
      "string": "we should not see this either",
      "inherit": "test01"
    }',
    file.path(param_dir, "test02.json")
  )
  testthat::expect_error(
    inherit_params(
      params = params,
      inheritence_search_paths = param_dir
    ),
    regexp = "^Inheritence loop detected.$"
  )
})

test_that("Searching across multiple directories works", {
  params <- list(
    foo = 1L,
    string = "simple params",
    inherit = "test01"
  )
  first_dir <- withr::local_tempdir()
  writeLines(
    '{
      "inherited_key": 2,
      "dir": "first",
      "some_other_key": "test01",
      "string": "we should not see this",
      "test01": true,
      "inherit": "test02"
    }',
    file.path(first_dir, "test01.json")
  )
  second_dir <- withr::local_tempdir()
  writeLines(
    '{
      "inherited_key": 3,
      "dir": "second",
      "some_other_key": "test02",
      "string": "we should not see this either",
      "test02": true
    }',
    file.path(second_dir, "test02.json")
  )
  results <- inherit_params(
    params = params,
    inheritence_search_paths = c(first_dir, second_dir)
  )
  expect_identical(
    object = results,
    expected = list(
      inherited_key = 2L,
      dir = "first", # inheriting from first_dir/test01.json
      some_other_key = "test01",
      string = "simple params",
      test02 = TRUE,
      test01 = TRUE,
      foo = 1L
    )
  )
})

test_that("Searching across multiple directories works", {
  params <- list(
    foo = 1L,
    string = "simple params",
    inherit = "test01"
  )
  first_dir <- withr::local_tempdir()
  writeLines(
    '{
      "inherited_key": 2,
      "dir": "first",
      "some_other_key": "test01",
      "string": "we should not see this",
      "test01": true
    }',
    file.path(first_dir, "test01.json")
  )
  second_dir <- withr::local_tempdir()
  writeLines(
    '{
      "inherited_key": 3,
      "dir": "second",
      "some_other_key": "secret bonus file",
      "string": "we should not see this either",
      "test02": true
    }',
    file.path(second_dir, "test01.json")
  )
  testthat::expect_warning(
    {
      results <- inherit_params(
        params = params,
        inheritence_search_paths = c(first_dir, second_dir)
      )
    },
    regexp = "^Multiple inheritence files found.$"
  )
  expect_identical(
    object = results,
    expected = list(
      inherited_key = 2L,
      dir = "first", # inheriting from first_dir/test01.json
      some_other_key = "test01",
      string = "simple params",
      test01 = TRUE,
      foo = 1L
    )
  )
})

test_that("Inheritence only looks at top level keys (not nested)", {
  params <- list(
    foo = 1L,
    bar = list(
      baz = 2L,
      inherit = "test01"
    )
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
    expected = params
  )
})

