#' Get Environment information for manifest
#'
#' This function takes no arguments and returns a nested list, suitable for
#' inclusion in manifest export.
#'
#' @return nested list of file details, length the same as the input vector.
get_manifest_envirionment_info <- function() {

  # TODO: Envvars
  # TODO: Session
  # Todo: packages


  return(invisible(FALSE))
}

get_r_session_info <- function() {
  return(
    list(
        R.version = utils::sessionInfo()[["R.version"]],
        platform = utils::sessionInfo()[["platform"]],
        running = utils::sessionInfo()[["running"]],
        locale = utils::sessionInfo()[["locale"]],
        tzone = utils::sessionInfo()[["tzone"]],
    )
  )
}

get_package_info <- function(
  base = utils::sessionInfo()[["basePkgs"]],
  attached = names(utils::sessionInfo()[["otherPkgs"]]),
  loaded = names(utils::sessionInfo()[["loadedOnly"]])
) {
  log_debug("Getting package info.")
  log_trace("Base packages: {base}")
  base_pkgs <- vapply(
    X = base,
    FUN = get_individual_package_info,
    FUN.VALUE = list(10L),
    USE.NAMES = TRUE
  )
  log_trace("Attached packages: {attached}")
  attached_pkgs <- vapply(
    X = attached,
    FUN = get_individual_package_info,
    FUN.VALUE = list(1L),
    USE.NAMES = TRUE
  )
  log_trace("Loaded packages: {loaded}")
  loaded_pkgs <- vapply(
    X = loaded,
    FUN = get_individual_package_info,
    FUN.VALUE = list(10L),
    USE.NAMES = TRUE
  )
  log_debug("Done fetching package info.")
  return(
    list(
      base = base_pkgs,
      attached = attached_pkgs,
      loaded = loaded_pkgs
    )
  )
}

get_individual_package_info <- function(packagename) {
  log_trace("Getting package info for {packagename}.")
  pkg_details <- as.list(
      pak::pkg_status(packagename)[
        c(
          "package",
          "version",
          "library",
          "repository",
          "platform",
          "built",
          "remotetype",
          "remotepkgref",
          "remoteref",
          "remotesha"
        )
      ]
    )
  output <- list()
  output[[packagename]] <- pkg_details
  return(output)
}
