#' parse_raw_params
#'
#' Parse and validate JSON parameters, including optionally validation raw
#' parameters prior to inheriting.
#'
#' @param json JSON string
#' @param inheritence_search_paths value
#' @param schema_file value
#' @param raw_schema_file value
#' @return returndes
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
