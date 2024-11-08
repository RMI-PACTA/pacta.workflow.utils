test_that("Multiple values in inherit key inherits from multiple files", {
  params <- list(
    foo = 1L,
    string = "simple params",
    inherit = c("test01", "test02")
  )
  param_dir <- withr::local_tempdir()
  writeLines(
    '{
      "inherited_key": 2,
      "some_other_key": "test01"
    }',
    file.path(param_dir, "test01.json")
  )
  writeLines(
    '{
      "other_inherited_key": 3,
      "some_third_key": "test02",
      "string2": "this is a string"
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
      other_inherited_key = 3L,
      some_third_key = "test02",
      string2 = "this is a string",
      inherited_key = 2L,
      some_other_key = "test01",
      foo  = 1L,
      string = "simple params"
    )
  )
})

test_that("Multiple values in inherit key work with nested inheritence", {
  params <- list(
    foo = 1L,
    inherit = c("test10", "test20")
  )
  param_dir <- withr::local_tempdir()
  writeLines(
    '{
      "key01": 10,
      "key10": 10,
      "inherit": "test11"
    }',
    file.path(param_dir, "test10.json")
  )
  writeLines(
    '{
      "key01": 11,
      "key10": 11,
      "key11": 11
    }',
    file.path(param_dir, "test11.json")
  )
  writeLines(
    '{
      "key01": 20,
      "key20": 20,
      "inherit": "test21"
    }',
    file.path(param_dir, "test20.json")
  )
  writeLines(
    '{
      "key01": 21,
      "key20": 21,
      "key21": 21
    }',
    file.path(param_dir, "test21.json")
  )
  results <- inherit_params(
    params = params,
    inheritence_search_paths = param_dir
  )
  expect_identical(
    object = results,
    expected = list(
      key01 = 10L,
      key10 = 10L,
      key11 = 11L,
      key20 = 20L,
      key21 = 21L,
      foo  = 1L
    )
  )
})

test_that("Identical inheritence paths throws error", {
  params <- list(
    foo = 1L,
    string = "simple params",
    inherit = c("test01", "test01")
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
    }',
    file.path(param_dir, "test02.json")
  )
  testthat::expect_error(
    inherit_params(
      params = params,
      inheritence_search_paths = param_dir
    ),
    regexp = "^Duplicate values found in inheritence key.$"
  )
})

test_that("Overwriting inherited properties in decreasing precendence", {
  params <- list(
    foo = 1L,
    string = "simple params",
    inherit = c("test01", "test02", "test03")
  )
  param_dir <- withr::local_tempdir()
  writeLines(
    '{
      "foo": 2,
      "introcuded_in_test01": "test01",
      "string": "We should not see this."
    }',
    file.path(param_dir, "test01.json")
  )
  writeLines(
    '{
      "foo": 3,
      "introcuded_in_test01": "test02",
      "introcuded_in_test02": "test02",
      "string": "We should not see this."
    }',
    file.path(param_dir, "test02.json")
  )
  writeLines(
    '{
      "foo": 4,
      "introcuded_in_test01": "test03",
      "introcuded_in_test02": "test03",
      "introcuded_in_test03": "test03",
      "string": "We should not see this."
    }',
    file.path(param_dir, "test03.json")
  )
  results <- inherit_params(
    params = params,
    inheritence_search_paths = param_dir
  )
  expect_identical(
    object = results,
    expected = list(
      foo = 1L,
      introcuded_in_test01 = "test01",
      introcuded_in_test02 = "test02",
      introcuded_in_test03 = "test03",
      string = "simple params"
    )
  )
})


