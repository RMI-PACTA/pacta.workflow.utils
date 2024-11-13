#' parse_params
#'
#' Parse parameters from JSON string or file.
#'
#' @param json JSON string or file path.
#' @param inheritence_search_paths Paths to search for inherited parameters.
#' See `inherit_params`.
#' @param schema_file Path to JSON Schema file for validation.
#' @param force_array Path in params list to force casting as JSON array.
#' (Default empty)
#' @return Parsed parameters as a standard R list.
#' @export
parse_params <- function(
  json,
  inheritence_search_paths = NULL,
  schema_file = NULL,
  force_array = list()
) {
  log_trace("Parsing params.")
  if (length(json) == 1L && file.exists(json)) {
    log_trace("Reading params from file: {json}.")
  } else {
    log_trace("Reading params from string.")
  }
  raw_params <- jsonlite::fromJSON(json)
  full_params <- inherit_params(
    raw_params,
    inheritence_search_paths
  )

  # force array
  full_params <- modify_list_element(
    x = full_params,
    positions = force_array,
    function_to_apply = I
  )

  if (!is.null(schema_file)) {
    if (requireNamespace("jsonvalidate", quietly = TRUE)) {
      log_trace("Validating parameters.")
      validation_results <- jsonvalidate::json_validate(
        json = jsonlite::toJSON(full_params, auto_unbox = TRUE),
        schema = schema_file,
        verbose = TRUE,
        engine = "ajv"
      )
      if (validation_results) {
        log_trace("Validation successful.")
      } else {
        log_error("Validation against JSON Schema failed.")
        log_error("Schema file: {schema_file}")
        pretty_log_jsonvalidate_errors(validation_results)
        stop("JSON Validation failed.")
      }
    } else {
      log_error("jsonvalidate package not found.")
      stop("jsonvalidate package not found.")
    }
  } else {
    log_trace("No JSON Schema provided. Skipping validation.")
  }

  return(full_params)
}

pretty_log_jsonvalidate_errors <- function(
  validation_object,
  logging_function = log_error
) {
  errors <- attr(validation_object, "errors")
  if (length(errors) == 0L) {
    return(NULL)
  }
  for (row in seq(1L, nrow(errors))) {
    logging_function("JSON Validation ({row} / {nrow(errors)}):")
    logging_function("  Keyword: {errors[[row, 'keyword']]}")
    logging_function("  instancePath: {errors[[row, 'instancePath']]}")
    logging_function("  schemaPath: {errors[[row, 'schemaPath']]}")
    logging_function("  Message: {errors[[row, 'message']]}")
  }
  return(errors)
}
