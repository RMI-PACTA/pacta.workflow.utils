## save current settings so that we can reset later
threshold <- logger::log_threshold()
appender  <- logger::log_appender()
layout    <- logger::log_layout()
on.exit({
  ## reset logger settings
  logger::log_threshold(threshold)
  logger::log_layout(layout)
  logger::log_appender(appender)
})

logger::log_appender(logger::appender_stdout)
logger::log_threshold(logger::FATAL)
logger::log_layout(logger::layout_simple)

expect_package_info <- function(
  package_info,
  package_identical,
  version_identical,
  library_identical,
  repository_match,
  remotetype_identical,
  remotepkgref_match,
  remoteref_identical,
  remotesha_identical
) {
  testthat::expect_type(package_info, "list")
  testthat::expect_named(
    package_info,
    package_identical
  )
  testthat::expect_named(
    package_info[[package_identical]],
    c(
      "package",
      "version",
      "library",
      "library_index",
      "repository",
      "platform",
      "built",
      "remotetype",
      "remotepkgref",
      "remoteref",
      "remotesha"
    )
  )
  testthat::expect_identical(
    package_info[[package_identical]][["package"]],
    package_identical
  )
  testthat::expect_identical(
    package_info[[package_identical]][["version"]],
    version_identical
  )
  testthat::expect_identical(
    package_info[[package_identical]][["library"]],
    # gsub is used to make windows path into something matching .libPaths()
    gsub(
      x = library_identical,
      pattern = "\\",
      replacement = "/",
      fixed = TRUE
    )
  )
  testthat::expect_type(
    package_info[[package_identical]][["library_index"]],
    "integer"
  )
  testthat::expect_gt(
    package_info[[package_identical]][["library_index"]],
    0L
  )
  testthat::expect_lte(
    package_info[[package_identical]][["library_index"]],
    length(.libPaths()) #nolint: undesirable_function_linter
  )
  if (is.na(repository_match)) {
    testthat::expect_identical(
      package_info[[package_identical]][["repository"]],
      repository_match
    )
  } else {
    testthat::expect_match(
      package_info[[package_identical]][["repository"]],
      repository_match
    )
  }
  testthat::expect_match(
    package_info[[package_identical]][["platform"]],
    R.version[["platform"]]
  )
  testthat::expect_false(
    is.null(package_info[[package_identical]][["built"]])
  )
  testthat::expect_identical(
    package_info[[package_identical]][["remotetype"]],
    remotetype_identical
  )
  testthat::expect_match(
    package_info[[package_identical]][["remotepkgref"]],
    # gsub is used to make windows path work
    gsub(
      x = remotepkgref_match,
      pattern = "\\",
      replacement = "\\\\",
      fixed = TRUE
    )
  )
  testthat::expect_identical(
    package_info[[package_identical]][["remoteref"]],
    remoteref_identical
  )
  testthat::expect_identical(
    package_info[[package_identical]][["remotesha"]],
    remotesha_identical
  )
}

test_that("get_individual_package_info collects information for CRAN packages correctly", { #nolint: line_length_linter
  expect_package_info(
    package_info = get_individual_package_info("digest"),
    package_identical = "digest",
    version_identical = as.character(utils::packageVersion("digest")),
    library_identical = .libPaths()[1], #nolint: undesirable_function_linter
    repository_match = "^(CRAN|RSPM)$", #GH Actions installs from RSPM, not CRAN
    remotetype_identical = "standard",
    remotepkgref_match = "^digest$",
    remoteref_identical = "digest",
    remotesha_identical = as.character(utils::packageVersion("digest"))
  )
})

with_local_install <- function(new_lib, package, code) {
  cache_dir <- withr::local_tempdir()
  withr::local_envvar(.new = c(R_USER_CACHE_DIR = cache_dir))
  withr::local_libpaths(new_lib, action = "prefix")
  testthat::capture_output( #make pak quiet
    pak::pak(package, lib = normalizePath(new_lib))
  )
  eval(code)
}

test_that("get_individual_package_info collects information for local packages correctly", { #nolint: line_length_linter
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  dest_dir <- normalizePath(withr::local_tempdir())
  dl <- gert::git_clone(
    url = "https://github.com/yihui/rmini.git", #nolint: nonportable_path_linter
    path = dest_dir,
    verbose = FALSE
  )
  new_lib <- normalizePath(withr::local_tempdir())
  package_info <- with_local_install(new_lib, paste0("local::", dest_dir), {
    get_individual_package_info("rmini")
  })
  expect_package_info(
    package_info,
    package_identical = "rmini",
    version_identical = "0.0.4",
    library_identical = new_lib,
    repository_match = NA_character_,
    remotetype_identical = "local",
    remotepkgref_match = paste0("^local::", dest_dir, "$"),
    remoteref_identical = NA_character_,
    remotesha_identical = NA_character_
  )
})

test_that("get_individual_package_info collects information for GitHub packages correctly", { #nolint: line_length_linter
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  new_lib <- normalizePath(withr::local_tempdir())
  package_info <- with_local_install(new_lib, "yihui/rmini", { #nolint: nonportable_path_linter
    get_individual_package_info("rmini")
  })
  expect_package_info(
    package_info,
    package_identical = "rmini",
    version_identical = "0.0.4",
    library_identical = new_lib,
    repository_match = NA_character_,
    remotetype_identical = "github",
    remotepkgref_match = "^yihui/rmini$", #nolint: nonportable_path_linter
    remoteref_identical = "HEAD",
    remotesha_identical = "f839b7327c4cb422705b9f3b7c5ffc87555d98e2"
  )
})

test_that("get_individual_package_info errors for multiple packages", {
  expect_error(
    get_individual_package_info(c("digest", "jsonlite"))
  )
})

test_that("get_individual_package_info errors for package that doesn't exist", {
  expect_error(
    get_individual_package_info("this_package_does_not_exist")
  )
})

test_that("get_individual_package_info errors for empty string", {
  expect_error(
    get_individual_package_info("")
  )
})

test_that("get_individual_package_info errors for no arguments", {
  expect_error(
    get_individual_package_info(),
    "^argument \"packagename\" is missing, with no default$"
  )
})

test_that("get_individual_package_info gets correct libpath fdn version or multiple installs", { #nolint: line_length_linter
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  new_lib <- normalizePath(withr::local_tempdir())
  newer_lib <- normalizePath(withr::local_tempdir())
  expect_warning(
    package_info <- with_local_install(new_lib, "yihui/rmini", { #nolint: nonportable_path_linter
      with_local_install(newer_lib, "yihui/rmini@308d27d", { #nolint: nonportable_path_linter
        get_individual_package_info("rmini")
      })
    })
  )
  expect_package_info(
    package_info,
    package_identical = "rmini",
    version_identical = "0.0.3", # Note: not latest version
    library_identical = newer_lib,
    repository_match = NA_character_,
    remotetype_identical = "github",
    remotepkgref_match = "^yihui/rmini@308d27d$", #nolint: nonportable_path_linter
    remoteref_identical = "308d27d",
    remotesha_identical = "308d27ddb0b45fda34fc259492145834d72849a9"
  )
  expect_identical(
    package_info[["rmini"]][["library_index"]],
    1L
  )
})

test_that("get_individual_package_info gets correct libpath for lower search priority", { #nolint: line_length_linter
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  new_lib <- normalizePath(withr::local_tempdir())
  newer_lib <- normalizePath(withr::local_tempdir())
  package_info <- with_local_install(new_lib, "yihui/rmini", { #nolint: nonportable_path_linter
    with_local_install(newer_lib, "digest", {
      get_individual_package_info("rmini")
    })
  })
  expect_package_info(
    package_info,
    package_identical = "rmini",
    version_identical = "0.0.4",
    library_identical = new_lib,
    repository_match = NA_character_,
    remotetype_identical = "github",
    remotepkgref_match = "^yihui/rmini$", #nolint: nonportable_path_linter
    remoteref_identical = "HEAD",
    remotesha_identical = "f839b7327c4cb422705b9f3b7c5ffc87555d98e2"
  )
  expect_identical(
    package_info[["rmini"]][["library_index"]],
    2L
  )
})
