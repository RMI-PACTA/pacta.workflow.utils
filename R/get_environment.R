#' Get Environment information for manifest
#'
#' This function takes no arguments and returns a nested list, suitable for
#' inclusion in manifest export.
#'
#' @return nested list of file details, length the same as the input vector.
get_manifest_envirionment_info <- function() {
  #: Envvars
  return(invisible(NULL))
}

get_r_session_info <- function() {
  return(
    list(
      R.version = utils::sessionInfo()[["R.version"]],
      platform = utils::sessionInfo()[["platform"]],
      running = utils::sessionInfo()[["running"]],
      locale = utils::sessionInfo()[["locale"]],
      tzone = utils::sessionInfo()[["tzone"]],
      libPaths = .libPaths() # nolint: undesirable_function_linter
    )
  )
}
