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
#' - `built`: Information about package build (relevant for binary packages)
#' - `remotepkgref`: The reference used by `pak` to install the package. NULL
#' for CRAN packages.
#' - `remoteref`: The reference of the package when it was pulled from REPO.
#' NULL for CRAN packages.
#' - `remotesha`: the SHA-1 hash of the reference (if applicable). NULL for
#' CRAN packages.
#' - `git`: Git information about the package, if it is loaded with `pkgload`
#' or installed from local filesystem (`local::` in `pak` syntax). See
#' `get_git_info`.
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
      repotype = "pkgload",
      remotepkgref = normalizePath(package_dev_dir)
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
    pkg_details <- lapply(
      X = pkg_details,
      FUN = function(x) {
        if (is.na(x)) {
          return(NULL)
        } else {
          return(x)
        }
      }
    )
    pkg_details[["library_index"]] <- lib_index
  }

  pkg_details[["pkg_source"]] <- switch(
    EXPR = tolower(
      (pkg_details[["repotype"]] %||% pkg_details[["remotetype"]]) %||%
        pkg_details[["priority"]] %||% "r_cmd_check"
    ),
    base = "Base",
    bioc = "Bioconductor",
    cran = "CRAN",
    github = "GitHub",
    local = "Local",
    pkgload = "Local (DEV)",
    r_cmd_check = "R CMD Check",
    standard = "Standard",
    "Unknown"
  )

  is_local_pkg <- pkg_details[["pkg_source"]] %in% c("Local", "Local (DEV)")
  if (length(is_local_pkg) != 1L) {
    log_warn("Package: {packagename}")
    log_warn("is_local_pkg: {length(is_local_pkg)}")
    log_warn("Sources: {pkg_details[\"pkg_source\"]}")
    log_warn("repotype: {pkg_details[\"repotype\"]}")
    log_warn("remotetype: {pkg_details[\"remotetype\"]}")
    log_warn("priority: {pkg_details[\"priority\"]}")

  }
  if (is_local_pkg) {
    git_info <- get_git_info(
      repo = gsub(
        x = pkg_details[["remotepkgref"]],
        pattern = "local::",
        replacement = "",
        fixed = TRUE
      )
    )
    pkg_details[["git"]] <- git_info
  } else {
    pkg_details[["git"]] <- NULL
  }

  if (pkg_details[["pkg_source"]] %in% c("CRAN", "R CMD Check")) {
    pkg_details[["remotepkgref"]] <- NULL
    pkg_details[["remoteref"]] <- NULL
    pkg_details[["remotesha"]] <- NULL
  }

  details_list <- list(
    package = pkg_details[["package"]],
    version = pkg_details[["version"]],
    loaded_with_pkgload = dev_package,
    library = pkg_details[["library"]],
    library_index = pkg_details[["library_index"]],
    repository = pkg_details[["repository"]],
    built = pkg_details[["built"]],
    pkg_source = pkg_details[["pkg_source"]],
    remotepkgref = pkg_details[["remotepkgref"]],
    remoteref = pkg_details[["remoteref"]],
    remotesha = pkg_details[["remotesha"]],
    git = pkg_details[["git"]]
  )
  return(details_list)
}
