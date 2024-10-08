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
    json = json_string,
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
    json = json_file,
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

product_schema <- '{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "Product",
  "description": "A product from Acme\'s catalog",
  "type": "object",
  "properties": {
    "id": {
      "description": "The unique identifier for a product",
      "type": "integer"
    },
    "name": {
      "description": "Name of the product",
      "type": "string"
    },
    "price": {
      "type": "number",
      "minimum": 0,
      "exclusiveMinimum": true
    },
    "tags": {
      "type": "array",
      "items": {
        "type": "string"
      },
      "minItems": 1,
      "uniqueItems": true
    }
  },
  "required": ["id", "name", "price"]
}'
schema_dir <- withr::local_tempdir()
schema_file <- file.path(schema_dir, "product.json")
writeLines(product_schema, schema_file)

test_that("No inheritence, pass as string, validation works", {
  json_string <- '{
    "id": 1,
    "name": "A green door",
    "price": 12.50,
    "tags": ["home", "green"]
  }'
  results <- parse_params(
    json = json_string,
    schema_file = schema_file
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

test_that("No inheritence, pass as string, failing validation works", {
  json_string <- '{
    "id": 1.5,
    "price": 12.50,
    "tags": ["home", "green"]
  }'
  testthat::expect_error(
    object = {
      parse_params(
        json = json_string,
        schema_file = schema_file
      )
    },
    regexp = "^JSON Validation failed.$"
  )
})

test_that("simple inheritence, pass as string, validation works", {
  json_string <- '{
    "id": 1,
    "price": 12.50,
    "inherit": "base01"
  }'
  results <- parse_params(
    json = json_string,
    inheritence_search_paths = base_params_dir,
    schema_file = schema_file
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

test_that("Forcing scalar to array works", {
  json_string <- '{
    "id": 1,
    "name": "A green door",
    "price": 12.50,
    "tags": ["home"],
    "supplier": "ACME Doors"
  }'
  results <- parse_params(
    json = json_string,
    inheritence_search_paths = base_params_dir,
    schema_file = schema_file,
    force_array = "tags"
  )
  expect_identical(
    object = results,
    expected = list(
      id = 1L,
      name = "A green door",
      price = 12.5,
      tags = I("home"),
      supplier = "ACME Doors"
    )
  )
})
