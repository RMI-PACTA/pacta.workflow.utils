test_that("Multiple values in inherit key inherits from multiple files", {
  params <- list(
    foo = 1L,
    inherit = c("test01", "test02")
  )
  param_dir <- withr::local_tempdir()
  writeLines(
    '{
      "inherited_key": "test01"
    }',
    file.path(param_dir, "test01.json")
  )
  writeLines(
    '{
      "other_inherited_key": "test02"
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
      other_inherited_key = "test02",
      inherited_key = "test01",
      foo  = 1L
    )
  )
})

test_that("Identical inheritence paths throws error", {
  params <- list(
    foo = 1L,
    inherit = c("test01", "test01")
  )
  param_dir <- withr::local_tempdir()
  writeLines(
    '{
      "string": "we should not see this"
    }',
    file.path(param_dir, "test01.json")
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
    inherit = c("test01", "test02", "test03")
  )
  param_dir <- withr::local_tempdir()
  writeLines(
    '{
      "foo": 2,
      "introcuded_in_test01": "test01"
    }',
    file.path(param_dir, "test01.json")
  )
  writeLines(
    '{
      "foo": 3,
      "introcuded_in_test01": "test02",
      "introcuded_in_test02": "test02"
    }',
    file.path(param_dir, "test02.json")
  )
  writeLines(
    '{
      "foo": 4,
      "introcuded_in_test01": "test03",
      "introcuded_in_test02": "test03",
      "introcuded_in_test03": "test03"
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
      introcuded_in_test03 = "test03"
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

test_that("Nested inheritence works with multiple inheritence", {
  params <- list(
    foo = 1L,
    inherit = c("test10", "test20")
  )
  param_dir <- withr::local_tempdir()
  writeLines(
    '{
      "key01": 10,
      "key10": 10,
      "inherit": ["test11", "test12"]
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
      "key01": 12,
      "key10": 12,
      "key12": 12
    }',
    file.path(param_dir, "test12.json")
  )
  writeLines(
    '{
      "key01": 20,
      "key20": 20,
      "inherit": ["test21", "test22"]
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
  writeLines(
    '{
      "key01": 22,
      "key20": 22,
      "key22": 22
    }',
    file.path(param_dir, "test22.json")
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
      key12 = 12L,
      key11 = 11L,
      key20 = 20L,
      key22 = 22L,
      key21 = 21L,
      foo  = 1L
    )
  )
})

