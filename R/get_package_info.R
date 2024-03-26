#' Get package information for active packages
#'
#' This function takes a vector or (possibly nested) list of package names and
#' returns a nested list of package details, suitable for inclusion in manifest
#' export.
#'
#' @param packagelist vector or list of package names. Best left as default,
#' which includes the currently loaded and attached namespaces separated into
#' useful categories.
#'
#' @return nested list of file details, length 3, with top level keys being
#' `base`, `attached`, and `loaded`. Underneath those keys are lists of package
#' details, with the package names as keys, and further details as returned by
#' [get_individual_package_info()].
#' @seealso [get_individual_package_info()]
get_package_info <- function(
  packagelist = list(
    base = utils::sessionInfo()[["basePkgs"]],
    attached = names(utils::sessionInfo()[["otherPkgs"]]),
    loaded = names(utils::sessionInfo()[["loadedOnly"]])
  )
) {
  log_debug("Getting package info.")
  if (inherits(packagelist, "character")) {
    out <- vapply(
      X = packagelist,
      FUN = function(x) {
        list(x = get_individual_package_info(x))
      },
      FUN.VALUE = list(1L),
      USE.NAMES = TRUE
    )
  } else {
    out <- vapply(
      X = packagelist,
      FUN = function(x) {
        list(
          x = get_package_info(x)
        )
      },
      FUN.VALUE = list(1L),
      USE.NAMES = TRUE
    )
  }
  return(out)
}

#' Get package information for a package
#'
#' This function takes a single package name and returns a list of package
#' details, suitable for inclusion in manifest export.
#'
#' @param packagename Singular charater string of package name
#'
#' @return nested list of file details, length 11, with keys:
#' - `package`: The name of the package
#' - `version`: The version of the package
#' - `loaded_with_pkgload`: Is this package loaded with `pkgload`? (logical).
#' Useful for identifying local development versions
#' - `library`: The path of the library the package is installed in
#' - `library_index`: The index of the library in the `.libPaths()` vector
#' - `repository`: The repository the package was pulled from
#' - `platform`: The platform the package was built for
#' - `built`: Information about package build (relevant for binary packages)
#' - `remotetype`: The type of remote repository the package was pulled from
#' - `remotepkgref`: The reference used by `pak` to install the package
#' - `remoteref`: The reference of the package when it was pulled from REPO
#' - `remotesha`: the SHA-1 hash of the reference (if applicable)
get_individual_package_info <- function(packagename) {
  if (length(packagename) != 1L || !is.character(packagename)) {
    log_error("packagename must be a single string.")
    # Early return
    stop("packagename must be a single string.")
  }
  dev_package <- pkgload::is_dev_package(packagename)
  if (dev_package) {
    log_warn("Package \"{packagename}\" is a development package.")
    log_warn("Package information may not be accurate.")
    warning("Identifying development packages may not be accurate.")
    package_dev_dir <- pkgload::pkg_path(
      path = dirname(system.file("DESCRIPTION", package = packagename))
    )
    pkg_details <- list(
      package = pkgload::pkg_name(package_dev_dir),
      version = paste("DEV", pkgload::pkg_version(package_dev_dir)),
      library = NA_character_,
      library_index = NA_integer_,
      repository = NA_character_,
      platform = NA_character_,
      built = NA_character_,
      remotetype = "pkgload",
      remotepkgref = normalizePath(package_dev_dir),
      remoteref = NA_character_,
      remotesha = NA_character_
    )
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
    log_trace("Getting package info for {packagename}.")
    pkg_details <- as.list(
      pkgdepends::lib_status(
        library = lib,
        packages = packagename
      )
    )
    pkg_details[["library_index"]] <- lib_index
  }
  details_list <- list(
    package = pkg_details[["package"]],
    version = pkg_details[["version"]],
    loaded_with_pkgload = dev_package,
    library = pkg_details[["library"]],
    library_index = pkg_details[["library_index"]],
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
  return(clean_details_list)
}
