parse_params <- function(
  json,
  inheritence_search_paths = NULL,
  schema_file = NULL
) {
  log_trace("Parsing params.")
  if (file.exists(json)) {
    log_trace("Reading params from file: {json}.}")
  } else {
    log_trace("Reading params from string.")
  }
  raw_params <- jsonlite::fromJSON(json)
  full_params <- inherit_params(
    raw_params,
    inheritence_search_paths
  )

  if (!is.null(schema_file)) {
    if (requireNamespace("jsonvalidate", quietly = TRUE)) {
      log_trace("Validating parameters.")
      validation_results <- jsonvalidate::json_validate(
        json = jsonlite::toJSON(full_params, auto_unbox = TRUE),
        schema = schema_file,
        verbose = TRUE
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

inherit_params <- function(
  params,
  inheritence_search_paths
) {
  inherit_key <- "inherit"

  inherited_files <- NULL
  while (inherit_key %in% names(params)) {

    # check for multiple inheritence keys
    if (sum(names(params) == inherit_key) > 1L) {
      log_error("Multiple inheritence keys found.")
      stop("Multiple inheritence keys found.")
    }

    log_trace(
      "Key \"{inherit_key}\" found in parameters. Inheriting parameters."
    )

    to_inherit <- params[[inherit_key]]
    if (length(to_inherit) > 1L) {
      log_error("Multiple values in inherit key.")
      stop("Multiple values in inherit key.")
    }
    params[[inherit_key]] <- NULL # remove inherit key

    possible_paths <- file.path(
      inheritence_search_paths,
      paste0(to_inherit, ".json")
    )
    candidate_file <- possible_paths[file.exists(possible_paths)]
    if (length(candidate_file) == 0L) {
      log_error("Inheritence file not found: {possible_paths}.")
      stop("Inheritence file not found.")
    } else {
      if (length(candidate_file) > 1L) {
        log_warn("Multiple files matching inheritence pattern found:")
        log_warn("{candidate_file}.")
        warning("Multiple inheritence files found.")
        candidate_file <- candidate_file[[1L]]
        log_warn("Using first file: {candidate_file}.")
      }
    }
    if (candidate_file %in% inherited_files) {
      log_error(
        "Inheritence loop detected while inheriting from {candidate_file}."
      )
      log_error("Inherited file: {inherited_files}.")
      stop("Inheritence loop detected.")
    }
    inherited_files <- c(inherited_files, candidate_file)
    log_trace("Inheriting parameters from file: {candidate_file}.")
    inherit_params <- jsonlite::fromJSON(candidate_file)
    params <- merge_lists(
      base_list = inherit_params,
      overlay_list = params
    )
  }

  log_trace("No inheritence key (\"{inherit_key}\") found.")
  return(params)
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
