#' Get package information for active packages
#'
#' This function takes 3 vectors of package names and returns a nested list of
#' package details, suitable for inclusion in manifest export.
#'
#' @param base vector of package names. Best left as default, which includes the loaded base packages.
#' @param attached vector of package names. Best left as default, which includes the attached packages.
#' @param loaded vector of package names. Best left as default, which includes the loaded packages.
#'
#' @return nested list of file details, length 3, with top level keys being `base`, `attached`, and `loaded`.
#' Underneath those keys are lists of package details, with the package names as keys, and further details as returned by [get_individual_package_info()].
#' @seealso [get_individual_package_info()]
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

#' Get package information for a package
#'
#' This function takes a single package name and returns a list of package
#' details, suitable for inclusion in manifest export.
#'
#' @param Singular charater string of package name
#'
#' @return nested list of file details, length 1, with the top-level key being
#' the `packagename` passed as an argument. underneath that key are:
#' - `package`: The name of the package
#' - `version`: The version of the package
#' - `library`: The path of the library the package is installed in
#' - `library_index`: The index of the library in the `.libPaths()` vector
#' - `repository`: The repository the package was pulled from
#' - `platform`: The platform the package was built for
#' - `built`: Information about the packages build (relevant for binary packages)
#' - `remotetype`: The type of remote repository the package was pulled from
#' - `remotepkgref`: The reference used by `pak` to install the package
#' - `remoteref`: The reference of the package when it was pulled from REPO
#' - `remotesha`: the SHA-1 hash of the reference (if applicable)
#' @examples
#' get_individual_package_info("digest")
get_individual_package_info <- function(packagename) {
  if (length(packagename) != 1L || !is.character(packagename)) {
    log_error("packagename must be a single string.")
    stop("packagename must be a single string.")
  } else {
    if (packagename %in% utils::installed.packages()[, "Package"]) {
      installed_index <- which(
        utils::installed.packages()[, "Package"] == packagename
      )
      installed_path <- utils::installed.packages()[installed_index, "LibPath"]
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
