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

test_that("un_asis does not alter objects without AsIs class", {
  x <- rnorm(10L)
  results <- un_asis(x)
  expect_identical(results, x)
})

test_that("un_asis removes AsIs class", {
  x <- rnorm(10L)
  x_asis <- I(x)
  results <- un_asis(x_asis)
  expect_identical(results, x)
})
