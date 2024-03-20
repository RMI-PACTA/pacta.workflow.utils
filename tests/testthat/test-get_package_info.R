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

test_that("get_individual_package_info collects information for CRAN packages correctly", { #nolint: line_length_linter
  expect_identical(
    get_individual_package_info("digest"),
    list(
      digest = list(
        package = "digest",
        version = as.character(utils::packageVersion("digest")),
        library = .libPaths()[1], #nolint: undesirable_function_linter
        repository = "CRAN",
        platform = strsplit(
          x = utils::packageDescription("digest")[["Built"]],
          split = "; ",
          fixed = TRUE
        )[[1L]][[2L]],
        built = utils::packageDescription("digest")[["Built"]],
        remotetype = "standard",
        remotepkgref = "digest",
        remoteref = "digest",
        remotesha  = as.character(utils::packageVersion("digest"))
      )
    )
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
  dl <- git2r::clone(
    url = "https://github.com/yihui/rmini.git", #nolint: nonportable_path_linter
    local_path = dest_dir,
    progress = FALSE
  )
  new_lib <- normalizePath(withr::local_tempdir())
  package_info <- with_local_install(new_lib, dest_dir, {
    get_individual_package_info("rmini")
  })
  expect_type(package_info, "list")
  expect_named(
    package_info,
    "rmini"
  )
  expect_named(
    package_info[["rmini"]],
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
  )
  expect_identical(
    package_info[["rmini"]][["package"]],
    "rmini"
  )
  expect_identical(
    package_info[["rmini"]][["version"]],
    "0.0.4"
  )
  expect_identical(
    package_info[["rmini"]][["library"]],
    normalizePath(new_lib)
  )
  expect_identical(
    package_info[["rmini"]][["repository"]],
    NA_character_
  )
  expect_identical(
    package_info[["rmini"]][["platform"]],
    R.version[["platform"]]
  )
  expect_false(
    is.null(package_info[["rmini"]][["built"]])
  )
  expect_identical(
    package_info[["rmini"]][["remotetype"]],
    "local"
  )
  expect_match(
    package_info[["rmini"]][["remotepkgref"]],
    paste0("^local::/.*", basename(dest_dir))
  )
  expect_identical(
    package_info[["rmini"]][["remoteref"]],
    NA_character_
  )
  expect_identical(
    package_info[["rmini"]][["remotesha"]],
    NA_character_
  )
})

test_that("get_individual_package_info collects information for GitHub packages correctly", { #nolint: line_length_linter
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  new_lib <- normalizePath(withr::local_tempdir())
  package_info <- with_local_install(new_lib, "yihui/rmini", { #nolint: nonportable_path_linter
    get_individual_package_info("rmini")
  })
  expect_type(package_info, "list")
  expect_named(
    package_info,
    "rmini"
  )
  expect_named(
    package_info[["rmini"]],
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
  )
  expect_identical(
    package_info[["rmini"]][["package"]],
    "rmini"
  )
  expect_identical(
    package_info[["rmini"]][["version"]],
    "0.0.4"
  )
  expect_identical(
    package_info[["rmini"]][["library"]],
    normalizePath(new_lib)
  )
  expect_identical(
    package_info[["rmini"]][["repository"]],
    NA_character_
  )
  expect_identical(
    package_info[["rmini"]][["platform"]],
    R.version[["platform"]]
  )
  expect_false(
    is.null(package_info[["rmini"]][["built"]])
  )
  expect_identical(
    package_info[["rmini"]][["remotetype"]],
    "github"
  )
  expect_match(
    package_info[["rmini"]][["remotepkgref"]],
    "yihui/rmini" #nolint: nonportable_path_linter
  )
  expect_identical(
    package_info[["rmini"]][["remoteref"]],
    "HEAD"
  )
  expect_identical(
    package_info[["rmini"]][["remotesha"]],
    "f839b7327c4cb422705b9f3b7c5ffc87555d98e2"
  )
})
