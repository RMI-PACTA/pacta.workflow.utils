#' parse_raw_params
#'
#' Parse and validate JSON parameters, including optionally validation raw
#' parameters prior to inheriting.
#'
#' @param json JSON string
#' @param inheritence_search_paths path to directory in which to search for
#' inheritance files. See `parse_params` for details.
#' @param schema_file JSON Schema file to validate parameters against. See
#' `parse_params` for details.
#' @param raw_schema_file JSON Schema file to validate raw parameters against.
#' See `jsonvalidate::json_validate` for details.
#' @return list of parameters
#' @examples

#' product_schema <- '{
#'   "$schema": "http://json-schema.org/draft-04/schema#",
#'   "title": "Product",
#'   "description": "A product from Acme\'s catalog",
#'   "type": "object",
#'   "properties": {
#'     "id": {
#'       "description": "The unique identifier for a product",
#'       "type": "integer"
#'     },
#'     "name": {
#'       "description": "Name of the product",
#'       "type": "string"
#'     },
#'     "price": {
#'       "type": "number",
#'       "minimum": 0,
#'       "exclusiveMinimum": true
#'     },
#'     "tags": {
#'       "type": "array",
#'       "items": {
#'         "type": "string"
#'       },
#'       "minItems": 1,
#'       "uniqueItems": true
#'     }
#'   },
#'   "required": ["id", "name", "price"]
#' }'
#' schema_dir <- withr::local_tempdir()
#' schema_file <- file.path(schema_dir, "product.json")
#' writeLines(product_schema, schema_file)
#' raw_schema <- '{
#'   "$schema": "http://json-schema.org/draft-04/schema#",
#'   "title": "rawProduct",
#'   "description": "Valid Input params for product",
#'   "type": "object",
#'   "properties": {
#'     "inherit": {
#'       "description": "Valid keys for inheritence",
#'       "type": "string",
#'       "enum": [
#'         "base01",
#'         "base02"
#'       ]
#'     }
#'   },
#'   "anyOf": [
#'     {
#'       "required": [
#'         "inherit"
#'       ]
#'     },
#'     {
#'       "$ref": "product.json"
#'     }
#'   ]
#' }'
#' raw_schema_file <- file.path(schema_dir, "rawProduct.json")
#' writeLines(raw_schema, raw_schema_file)
#' json_string <- '{
#'  "id": 1,
#'   "price": 12.50,
#'   "inherit": "base01"
#' }  '
#' results <- parse_raw_params(
#'   json = json_string,
#'   schema_file = schema_file,
#'   inheritence_search_paths = base_params_dir,
#'   raw_schema_file = raw_schema_file
#' )  
#' results
#' @export
parse_raw_params <- function(
  json,
  inheritence_search_paths = NULL,
  schema_file = NULL,
  raw_schema_file = NULL
) {
  # Read Params
  log_trace("Processing input parameters.")
  if (length(json) == 0L || all(json == "")) {
    log_error("No parameters specified.")
    stop("No parameters specified.")
  }

  if (is.null(raw_schema_file)) {
    log_debug(
      "No raw schema file specified. ",
      "Skipping raw parameter validation."
    )
  } else {
    log_trace("Validating raw input parameters.")
    raw_input_validation_results <- jsonvalidate::json_validate(
      json = json,
      schema = raw_schema_file,
      verbose = TRUE,
      greedy = FALSE,
      engine = "ajv"
    )
    if (raw_input_validation_results) {
      log_trace("Raw input parameters are valid.")
    } else {
      log_error(
        "Invalid raw input parameters. ",
        "Must include \"inherit\" key, or match full schema.",
        "See schema for details.",
        raw_schema_file
      )
      stop("Invalid raw input parameters.")
    }
  }

  params <- parse_params(
    json = json,
    inheritence_search_paths = inheritence_search_paths,
    schema_file = schema_file
  )

  return(params)
}
