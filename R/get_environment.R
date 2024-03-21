#' Get Environment information for manifest
#'
#' This function takes no arguments and returns a nested list, suitable for
#' inclusion in manifest export.
#'
#' @return nested list of file details, length the same as the input vector.
get_manifest_envirionment_info <- function() {

  #: Envvars


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
      libPaths = .libPaths() # nolint: undesirable_function_linter
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
  if (length(packagename) != 1L || !is.character(packagename)) {
    log_error("packagename must be a single string.")
    stop("packagename must be a single string.")
  } else {
    if (packagename %in% installed.packages()[, "Package"]) {
      installed_index <- which(installed.packages()[, "Package"] == packagename)
      installed_path <- installed.packages()[installed_index, "LibPath"]
      if (length(installed_path) > 1L) {
        log_warn(
          "Multiple installations of package \"{packagename}\" found: ",
          "{installed_path}"
        )
        log_warn("Using installation first on the search path.")
        warning("Multiple installations of package found.")
      }
      lib_index <- min(which(.libPaths() == installed_path)) #nolint: undesirable_function_linter
      lib <- .libPaths()[lib_index] #nolint: undesirable_function_linter
      log_trace("Package \"{packagename}\" is installed at {lib}")
    } else {
      log_error("Package \"{packagename}\" is not installed.")
      stop("Package is not installed.")
    }
  }
  log_trace("Getting package info for {packagename}.")
  pkg_details <- as.list(
    pkgdepends::lib_status(
      library = lib,
      packages = packagename
    )
  )
  details_list <- list(
    package = pkg_details[["package"]],
    version = pkg_details[["version"]],
    library = pkg_details[["library"]],
    library_index = lib_index,
    repository = pkg_details[["repository"]],
    platform = pkg_details[["platform"]],
    built = pkg_details[["built"]],
    remotetype = pkg_details[["remotetype"]],
    remotepkgref = pkg_details[["remotepkgref"]],
    remoteref = pkg_details[["remoteref"]],
    remotesha = pkg_details[["remotesha"]]
  )
  clean_details_list <- lapply(
    X = details_list,
    FUN = function(x) {
      ifelse(is.null(x), NA_character_, x)
    }
  )
  output <- list()
  output[[packagename]] <- clean_details_list
  return(output)
}
