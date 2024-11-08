#' inherit_params
#'
#' Inherit parameters from a JSON file. This function will search for a file
#' (in `inheritence_search_paths`) with the name of the value of the `inherit`
#' key in the `params` list. If found, the parameters in that file will be
#' overlaid onto the `params` list. This process will continue until no
#' `inherit` key is found. It is possible to specify multiple values in the
#' `inherit` key; doing so will cause the parameters from each file to be
#' overlaid onto the `params` list in the order specified. If inherit is
#' `list("file01", "file02")`, then the `params` list will remain as-is, any
#' _new_ values from `file01` will be added, and then any new values that
#' aren't in `params` or `file01` will be added from `file02`.
#'
#' @param params list of parameters, which may include an inheritence key
#' (`inherit`).
#' @param inheritence_search_paths Paths to search for inherited parameters.
#' @return merged list of parameters, with inherited parameters overlaid onto
#' `params`.
#' @export
inherit_params <- function(
  params,
  inheritence_search_paths,
  inherited_files = NULL
) {
  inherit_key <- "inherit"

  if (inherit_key %in% names(params)) {

    # check for multiple inheritence keys
    if (sum(names(params) == inherit_key) > 1L) {
      log_error("Multiple inheritence keys found.")
      stop("Multiple inheritence keys found.")
    }

    log_trace(
      "Key \"{inherit_key}\" found in parameters. Inheriting parameters."
    )

    to_inherit_vec <- params[[inherit_key]]
    if (anyDuplicated(to_inherit_vec) != 0L) {
      log_error("Duplicate values found in inheritence key.")
      stop("Duplicate values found in inheritence key.")
    }
    params[[inherit_key]] <- NULL # remove inherit key

    for (to_inherit in to_inherit_vec) {

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
      params_to_inherit <- inherit_params(
        params = jsonlite::fromJSON(candidate_file),
        inheritence_search_paths = inheritence_search_paths,
        inherited_files = inherited_files
      )
      params <- merge_lists(
        base_list = params_to_inherit,
        overlay_list = params
      )
    }
  }

  log_trace("No inheritence key (\"{inherit_key}\") found.")
  return(params)
}
