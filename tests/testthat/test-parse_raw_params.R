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

test_that("No inheritence, no raw param validation, pass as string", {
  json_string <- '{
    "id": 1,
    "name": "A green door",
    "price": 12.50,
    "tags": ["home", "green"]
  }'
  results <- parse_raw_params(json_string)
  expect_identical(
    object = results,
    expected = list(
      id = 1L,
      name = "A green door",
      price = 12.5,
      tags = c("home", "green")
    )
  )
})
