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

test_that("modify_list_element correctly modifies a flat list", {
  test_list <- list(a = 1L, b = 2.2, c = "a")
  results <- modify_list_element(
    x = test_list,
    positions = "b",
    function_to_apply = I
  )
  expect_identical(
    object = results,
    expected = list(a = 1L, b = I(2.2), c = "a")
  )
})

test_that("modify_list_element accepts different functions", {
  test_list <- list(a = 1L, b = 2.2, c = "a")
  results <- modify_list_element(
    x = test_list,
    positions = "b",
    function_to_apply = as.integer
  )
  expect_identical(
    object = results,
    expected = list(a = 1L, b = 2L, c = "a")
  )
})

test_that("modify_list_element correctly modifies a nested list", {
  test_list <- list(a = 1L, b = list(ba = 2L, bb = 2.2), c = "a")
  results <- modify_list_element(
    x = test_list,
    positions = c("b", "bb"),
    function_to_apply = I
  )
  expect_identical(
    object = results,
    expected = list(a = 1L, b = list(ba = 2L, bb = I(2.2)), c = "a")
  )
})

test_that("modify_single_list_element modifies multiple elements", {
  test_list <- list(a = 1L, b = list(ba = 2L, bb = 2.2), c = "a")
  results <- modify_list_element(
    x = test_list,
    position = list(
      c("b", "bb"),
      "a"
    ),
    function_to_apply = I
  )
  expect_identical(
    object = results,
    expected = list(a = I(1L), b = list(ba = 2L, bb = I(2.2)), c = "a")
  )
})

test_that("modify_list_element modifies list elements of nested list", {
  test_list <- list(a = 1L, b = list(ba = 2L, bb = 2.2), c = "a")
  results <- modify_list_element(
    x = test_list,
    positions = "b",
    function_to_apply = I
  )
  expect_identical(
    object = results,
    expected = list(a = 1L, b = I(list(ba = 2L, bb = 2.2)), c = "a")
  )
})

test_that("modify_list_element correctly modifies an deep nested list", {
  test_list <- list(
    a = 1L,
    b = list(
      c = 2L,
      d = list(
        e = 3L,
        f = list(
          g = 4L,
          h = list(
            i = 5L,
            j = 6L
          )
        )
      )
    )
  )
  results <- modify_list_element(
    x = test_list,
    positions = c("b", "d", "f", "h", "i"),
    function_to_apply = I
  )
  expect_identical(
    object = results,
    expected = list(
      a = 1L,
      b = list(
        c = 2L,
        d = list(
          e = 3L,
          f = list(
            g = 4L,
            h = list(
              i = I(5L),
              j = 6L
            )
          )
        )
      )
    )
  )
})

test_that("modify_list_element can accept positional arguments", {
  test_list <- list(
    a = 1L,
    b = list(
      c = 2L,
      d = list(
        e = 3L,
        f = list(
          g = 4L,
          h = list(
            i = 5L,
            j = 6L
          )
        )
      )
    )
  )
  results <- modify_list_element(
    x = test_list,
    positions = c(2L, 2L, 2L, 2L, 1L),
    function_to_apply = I
  )
  expect_identical(
    object = results,
    expected = list(
      a = 1L,
      b = list(
        c = 2L,
        d = list(
          e = 3L,
          f = list(
            g = 4L,
            h = list(
              i = I(5L),
              j = 6L
            )
          )
        )
      )
    )
  )
})

test_that("modify_list_element can modify multiple nested list elements", {
  test_list <- list(
    a = 1L,
    b = list(
      c = 2L,
      d = list(
        e = 3L,
        f = list(
          g = 4L,
          h = list(
            i = 5L,
            j = 6L
          )
        )
      )
    )
  )
  results <- modify_list_element(
    x = test_list,
    positions = list(
      c("b", "d", "f", "h", "i"),
      "a",
      c("b", "c"),
      c("b", "d", "f", "g")
    ),
    function_to_apply = I
  )
  expect_identical(
    object = results,
    expected = list(
      a = I(1L),
      b = list(
        c = I(2L),
        d = list(
          e = 3L,
          f = list(
            g = I(4L),
            h = list(
              i = I(5L),
              j = 6L
            )
          )
        )
      )
    )
  )
})
