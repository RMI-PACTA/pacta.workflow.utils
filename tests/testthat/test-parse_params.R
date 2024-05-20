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


test_that("No inheritence, pass as string", {
  json_string <- '{
    "id": 1,
    "name": "A green door",
    "price": 12.50,
    "tags": ["home", "green"]
  }'
  results <- parse_params(json_string)
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

test_that("No inheritence, pass as file", {
  json_string <- '{
    "id": 1,
    "name": "A green door",
    "price": 12.50,
    "tags": ["home", "green"]
  }'
  json_file <- withr::local_tempfile(fileext = ".json")
  writeLines(json_string, json_file)
  results <- parse_params(json_file)
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

test_that("No inheritence, pass oneline string to command args", {
  json_string <- '{
      "id": 1,
      "name": "A green door",
      "price": 12.50,
      "tags": ["home", "green"]
    }' |>
    gsub(pattern = "\\s{2,}", replacement = "", x = _) # Note _ instead of . for base pipe
  results <- run_with_cmd_args(
      code = "pacta.workflow.utils:::parse_params(commandArgs())",
      cmdargs = json_string
  )
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

test_that("No inheritence, pass filepath to command args", {
  json_string <- '{
      "id": 1,
      "name": "A green door",
      "price": 12.50,
      "tags": ["home", "green"]
  }'
  json_file <- withr::local_tempfile(fileext = ".json")
  writeLines(json_string, json_file)
  results <- run_with_cmd_args(
      code = "pacta.workflow.utils:::parse_params(commandArgs())",
      cmdargs = json_file
  )
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

test_that("No inheritence, pass multiline string to command args", {
  json_string <- '{
      "id": 1,
      "name": "A green door",
      "price": 12.50,
      "tags": ["home", "green"]
  }'
  results <- run_with_cmd_args(
      code = "pacta.workflow.utils:::parse_params(commandArgs())",
      cmdargs = json_string
  )
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

base_params_dir <- withr::local_tempdir()
base_01_string <- '{
    "name": "A green door",
    "tags": ["home", "green"],
    "supplier": "ACME Doors"
  }'
writeLines(
  base_01_string,
  file.path(base_params_dir, "base01.json")
)

test_that("Simple inheritence, pass as string", {
  json_string <- '{
    "id": 1,
    "price": 12.50,
    "inherit": "base01"
  }'
  results <- parse_params(
    JSON = json_string,
    inheritence_search_paths = base_params_dir
  )
  expect_identical(
    object = results,
    expected = list(
      name = "A green door",
      tags = c("home", "green"),
      supplier = "ACME Doors",
      id = 1L,
      price = 12.5
    )
  )
})

test_that("Simple inheritence, pass as file", {
  json_string <- '{
    "id": 1,
    "price": 12.50,
    "inherit": "base01"
  }'
  json_file <- withr::local_tempfile(fileext = ".json")
  writeLines(json_string, json_file)
  results <- parse_params(
    JSON = json_file,
    inheritence_search_paths = base_params_dir
  )
  expect_identical(
    object = results,
    expected = list(
      name = "A green door",
      tags = c("home", "green"),
      supplier = "ACME Doors",
      id = 1L,
      price = 12.5
    )
  )
})
