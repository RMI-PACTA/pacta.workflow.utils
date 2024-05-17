parse_params <- function(
  params,
  inheritence_search_paths
) {
  log_trace("Parsing params.")
  if (file.exists(params)) {
    log_trace("Reading params from file: {params}.}")
  } else {
    log_trace("Reading params from string.")
  }
  raw_params <- jsonlite::fromJSON(params)
  full_params <- inherit_params(
    raw_params,
    inheritence_search_paths
  )
  return(full_params)
}

inherit_params <- function(
  params,
  inheritence_search_paths
) {
  inherit_key <- "inherit"

  while (inherit_key %in% names(params)) {
    log_trace(
      "Key \"{inherit_key}\" found in parameters. Inheriting parameters."
    )

    to_inherit <- params[[inherit_key]]
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
        browser()
        candidate_file <- candidate_file[[1L]]
        log_warn("Using first file: {candidate_file}.")
      }
    }
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
