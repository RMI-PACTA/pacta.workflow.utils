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

test_that("Empty param string fails", {
  json_string <- ""
  expect_error(
    parse_raw_params(json = json_string),
    "No parameters specified."
  )
})

test_that("NULL param string fails", {
  json_string <- NULL
  expect_error(
    parse_raw_params(json = json_string),
    "No parameters specified."
  )
})

test_that("multiple empty param string fails", {
  json_string <- rep("", 3L)
  expect_error(
    parse_raw_params(json = json_string),
    "No parameters specified."
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

test_that("No inheritence, schema validation works", {
  json_string <- '{
    "id": 1,
    "name": "A green door",
    "price": 12.50,
    "tags": ["home", "green"]
  }'
  results <- parse_raw_params(
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
base_02_string <- '{
    "name": "A blue door",
    "tags": ["home", "blue"],
    "supplier": "FooBar Doors"
  }'
writeLines(
  base_02_string,
  file.path(base_params_dir, "base02.json")
)

test_that("simple inheritence, schema validation works", {
  json_string <- '{
    "id": 1,
    "price": 12.50,
    "inherit": "base01"
  }'
  results <- parse_raw_params(
    json = json_string,
    schema_file = schema_file,
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

test_that("simple inheritence 2, schema validation works", {
  json_string <- '{
    "id": 1,
    "price": 12.50,
    "inherit": "base02"
  }'
  results <- parse_raw_params(
    json = json_string,
    schema_file = schema_file,
    inheritence_search_paths = base_params_dir
  )
  expect_identical(
    object = results,
    expected = list(
      name = "A blue door",
      tags = c("home", "blue"),
      supplier = "FooBar Doors",
      id = 1L,
      price = 12.5
    )
  )
})

raw_schema <- '{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "rawProduct",
  "description": "Valid Input params for product",
  "type": "object",
  "properties": {
    "inherit": {
      "description": "Valid keys for inheritence",
      "type": "string",
      "enum": [
        "base01",
        "base02"
      ]
    }
  },
  "anyOf": [
    {
      "required": [
        "inherit"
      ]
    },
    {
      "$ref": "product.json"
    }
  ]
}'
raw_schema_file <- file.path(schema_dir, "rawProduct.json")
writeLines(raw_schema, raw_schema_file)

test_that("No inheritence, raw validation works", {
  json_string <- '{
    "id": 1,
    "name": "A green door",
    "price": 12.50,
    "tags": ["home", "green"]
  }'
  results <- parse_raw_params(
    json = json_string,
    schema_file = schema_file,
    raw_schema_file = raw_schema_file
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

test_that("simple inheritence, raw validation works - inherit", {
  json_string <- '{
    "id": 1,
    "price": 12.50,
    "inherit": "base01"
  }'
  results <- parse_raw_params(
    json = json_string,
    schema_file = schema_file,
    inheritence_search_paths = base_params_dir,
    raw_schema_file = raw_schema_file
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

test_that("raw validation errors correctly, no inherit field", {
  json_string <- '{
    "id": 1,
    "price": 12.50
  }'
  expect_error(
    parse_raw_params(
      json = json_string,
      schema_file = schema_file,
      inheritence_search_paths = base_params_dir,
      raw_schema_file = raw_schema_file
    ),
    "Invalid raw input parameters."
  )
})

test_that("raw validation errors correctly, invalid inherit field", {
  json_string <- '{
    "id": 1,
    "price": 12.50,
    "inherit": "base03"
  }'
  expect_error(
    parse_raw_params(
      json = json_string,
      schema_file = schema_file,
      inheritence_search_paths = base_params_dir,
      raw_schema_file = raw_schema_file
    ),
    "Invalid raw input parameters."
  )
})
