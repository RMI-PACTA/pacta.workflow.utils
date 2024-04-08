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
  repository_match,
  remotetype_identical,
  remotepkgref_match,
  remoteref_identical,
  remotesha_identical,
  loaded_with_pkgload_identical = FALSE,
  git = NULL
) {
  testthat::expect_type(package_info, "list")
  testthat::expect_named(
    package_info,
    expected = c(
      "package",
      "version",
      "loaded_with_pkgload",
      "library",
      "library_index",
      "repository",
      "platform",
      "built",
      "remotetype",
      "remotepkgref",
      "remoteref",
      "remotesha",
      "git"
    )
  )
  testthat::expect_identical(
    object = package_info[["package"]],
    expected = package_identical
  )
  testthat::expect_identical(
    object = package_info[["version"]],
    expected = version_identical
  )
  testthat::expect_identical(
    object = package_info[["loaded_with_pkgload"]],
    expected = loaded_with_pkgload_identical
  )

  if (loaded_with_pkgload_identical) {
    testthat::expect_identical(
      object = package_info[["library"]],
      expected = NA_character_
    )
    testthat::expect_identical(
      object = package_info[["library_index"]],
      expected = NA_integer_
    )
    testthat::expect_identical(
      object = package_info[["platform"]],
      expected = NA_character_
    )
  } else {
    testthat::expect_in(
      object = package_info[["library"]],
      .libPaths() #nolint: undesirable_function_linter
    )
    testthat::expect_gt(
      object = package_info[["library_index"]],
      expected = 0L
    )
    testthat::expect_lte(
      object = package_info[["library_index"]],
      expected = length(.libPaths()) #nolint: undesirable_function_linter
    )
    testthat::expect_match(
      object = package_info[["platform"]],
      regexp = R.version[["platform"]]
    )
  }

  testthat::expect_identical(
    object = package_info[["library"]],
    expected = .libPaths()[package_info[["library_index"]]] #nolint: undesirable_function_linter
  )
  testthat::expect_type(
    object = package_info[["library_index"]],
    type = "integer"
  )
  if (is.na(repository_match)) {
    testthat::expect_identical(
      package_info[["repository"]],
      expected = repository_match
    )
  } else {
    testthat::expect_match(
      object = package_info[["repository"]],
      regexp = repository_match
    )
  }
  testthat::expect_false(
    is.null(x = package_info[["built"]])
  )
  testthat::expect_identical(
    object = package_info[["remotetype"]],
    expected = remotetype_identical
  )
  if (is.na(remotepkgref_match)) {
    testthat::expect_identical(
      package_info[["remotepkgref"]],
      expected = remotepkgref_match
    )
  } else {
    testthat::expect_match(
      object = package_info[["remotepkgref"]],
      # gsub is used to make windows path work
      regexp = gsub(
        x = remotepkgref_match,
        pattern = "\\",
        replacement = "\\\\",
        fixed = TRUE
      )
    )
  }
  testthat::expect_identical(
    object = package_info[["remoteref"]],
    remoteref_identical
  )
  testthat::expect_identical(
    object = package_info[["remotesha"]],
    remotesha_identical
  )
  if (is.null(git)) {
    testthat::expect_identical(
      object = package_info[["git"]],
      expected = NA_character_
    )
  } else {
    testthat::expect_identical(
      object = package_info[["git"]],
      expected = git
    )
  }
}

test_that("get_individual_package_info collects information for CRAN packages correctly", { #nolint: line_length_linter
  expect_package_info(
    package_info = get_individual_package_info("digest"),
    package_identical = "digest",
    version_identical = as.character(utils::packageVersion("digest")),
    repository_match = "^(CRAN|RSPM)$", #GH Actions installs from RSPM, not CRAN
    remotetype_identical = "standard",
    remotepkgref_match = "^digest$",
    remoteref_identical = "digest",
    remotesha_identical = as.character(utils::packageVersion("digest"))
  )
})

test_that("get_individual_package_info collects information for base packages correctly", { #nolint: line_length_linter
  expect_package_info(
    package_info = get_individual_package_info("utils"),
    package_identical = "utils",
    version_identical = paste(
      R.version[["major"]],
      R.version[["minor"]],
      sep = "."
    ),
    repository_match = NA_character_, #GH Actions installs from RSPM, not CRAN
    remotetype_identical = NA_character_,
    remotepkgref_match = NA_character_,
    remoteref_identical = NA_character_,
    remotesha_identical = NA_character_
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
  with_local_install(new_lib, paste0("local::", dest_dir), {
    package_info <- get_individual_package_info("rmini")
    expect_package_info(
      package_info,
      package_identical = "rmini",
      version_identical = "0.0.4",
      repository_match = NA_character_,
      remotetype_identical = "local",
      remotepkgref_match = paste0("^local::", dest_dir, "$"),
      remoteref_identical = NA_character_,
      remotesha_identical = NA_character_,
      git = list(
        repo = normalizePath(dest_dir),
        is_git = TRUE,
        commit = "f839b7327c4cb422705b9f3b7c5ffc87555d98e2",
        clean = TRUE,
        branch = list(
          name = "master",
          commit = "f839b7327c4cb422705b9f3b7c5ffc87555d98e2",
          upstream = "refs/remotes/origin/master", #nolint: nonportable_path_linter
          remote_url = "https://github.com/yihui/rmini.git",
          up_to_date = TRUE,
          upstream_commit = "f839b7327c4cb422705b9f3b7c5ffc87555d98e2"
        ),
        changed_files = list(),
        tags = list()
      )
    )
    expect_identical(
      package_info[["library"]],
      normalizePath(new_lib, winslash = "/")
    )
  })
})

test_that("get_individual_package_info collects information for GitHub packages correctly", { #nolint: line_length_linter
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  new_lib <- normalizePath(withr::local_tempdir())
  package_info <- with_local_install(new_lib, "yihui/rmini", { #nolint: nonportable_path_linter
    package_info <- get_individual_package_info("rmini")
    expect_package_info(
      package_info,
      package_identical = "rmini",
      version_identical = "0.0.4",
      repository_match = NA_character_,
      remotetype_identical = "github",
      remotepkgref_match = "^yihui/rmini$", #nolint: nonportable_path_linter
      remoteref_identical = "HEAD",
      remotesha_identical = "f839b7327c4cb422705b9f3b7c5ffc87555d98e2"
    )
    expect_identical(
      package_info[["library"]],
      normalizePath(new_lib, winslash = "/")
    )
  })
})

test_that("get_individual_package_info collects information for packages loaded with pkgload correctly", { #nolint: line_length_linter
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  testthat::skip_if_not_installed("pkgload")
  dest_dir <- normalizePath(withr::local_tempdir())
  dl <- gert::git_clone(
    url = "https://github.com/yihui/rmini.git", #nolint: nonportable_path_linter
    path = dest_dir,
    verbose = FALSE
  )
  withr::with_envvar(
    c("_R_SHLIB_BUILD_OBJECTS_SYMBOL_TABLES_" = FALSE),
    {
      loaded <- pkgload::load_all(dest_dir, quiet = TRUE)
    }
  )
  withr::defer({
    pkgload::unload(package = "rmini")
  })
  testthat::expect_warning(
    object = {
      package_info <- get_individual_package_info("rmini")
      expect_package_info(
        package_info,
        package_identical = "rmini",
        version_identical = "DEV 0.0.4",
        loaded_with_pkgload_identical = TRUE,
        repository_match = NA_character_,
        remotetype_identical = "pkgload",
        remotepkgref_match = paste0("^", dest_dir, "$"),
        remoteref_identical = NA_character_,
        remotesha_identical = NA_character_,
        git = list(
          repo = normalizePath(dest_dir),
          is_git = TRUE,
          commit = "f839b7327c4cb422705b9f3b7c5ffc87555d98e2",
          clean = TRUE,
          branch = list(
            name = "master",
            commit = "f839b7327c4cb422705b9f3b7c5ffc87555d98e2",
            upstream = "refs/remotes/origin/master", #nolint: nonportable_path_linter
            remote_url = "https://github.com/yihui/rmini.git",
            up_to_date = TRUE,
            upstream_commit = "f839b7327c4cb422705b9f3b7c5ffc87555d98e2"
          ),
          changed_files = list(),
          tags = list()
        )
      )
      expect_identical(
        package_info[["remotepkgref"]],
        normalizePath(dest_dir)
      )
    },
    "^Identifying development packages may not be accurate.$"
  )
})

test_that("get_individual_package_info collects information for packages loaded with devtools correctly", { #nolint: line_length_linter
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  testthat::skip_if_not_installed("devtools")
  dest_dir <- normalizePath(withr::local_tempdir())
  dl <- gert::git_clone(
    url = "https://github.com/yihui/rmini.git", #nolint: nonportable_path_linter
    path = dest_dir,
    verbose = FALSE
  )
  withr::with_envvar(
    c("_R_SHLIB_BUILD_OBJECTS_SYMBOL_TABLES_" = FALSE),
    {
      loaded <- devtools::load_all(dest_dir, quiet = TRUE)
    }
  )
  withr::defer({
    devtools::unload(package = "rmini")
  })
  testthat::expect_warning(
    object = {
      package_info <- get_individual_package_info("rmini")
      expect_package_info(
        package_info,
        package_identical = "rmini",
        version_identical = "DEV 0.0.4",
        loaded_with_pkgload_identical = TRUE,
        repository_match = NA_character_,
        remotetype_identical = "pkgload",
        remotepkgref_match = paste0("^", dest_dir, "$"),
        remoteref_identical = NA_character_,
        remotesha_identical = NA_character_,
        git = list(
          repo = normalizePath(dest_dir),
          is_git = TRUE,
          commit = "f839b7327c4cb422705b9f3b7c5ffc87555d98e2",
          clean = TRUE,
          branch = list(
            name = "master",
            commit = "f839b7327c4cb422705b9f3b7c5ffc87555d98e2",
            upstream = "refs/remotes/origin/master", #nolint: nonportable_path_linter
            remote_url = "https://github.com/yihui/rmini.git",
            up_to_date = TRUE,
            upstream_commit = "f839b7327c4cb422705b9f3b7c5ffc87555d98e2"
          ),
          changed_files = list(),
          tags = list()
        )
      )
      expect_identical(
        package_info[["remotepkgref"]],
        normalizePath(dest_dir)
      )
    },
    "^Identifying development packages may not be accurate.$"
  )
})

test_that("get_individual_package_info collects information for altered packages loaded with devtools correctly", { #nolint: line_length_linter
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  testthat::skip_if_not_installed("devtools")
  dest_dir <- normalizePath(withr::local_tempdir())
  dl <- gert::git_clone(
    url = "https://github.com/yihui/rmini.git", #nolint: nonportable_path_linter
    path = dest_dir,
    verbose = FALSE
  )
  testing_git_config(repo = dest_dir)
  test_file <- file.path(dest_dir, "foo.txt")
  writeLines("Hello, world!", con = test_file)
  gert::git_add(files = basename(test_file), repo = normalizePath(dest_dir))
  commit_sha <- gert::git_commit(repo = dest_dir, message = "Initial commit")
  writeLines("Hello, testing!", con = test_file)
  withr::with_envvar(
    c("_R_SHLIB_BUILD_OBJECTS_SYMBOL_TABLES_" = FALSE),
    {
      loaded <- devtools::load_all(dest_dir, quiet = TRUE)
    }
  )
  withr::defer({
    devtools::unload(package = "rmini")
  })
  testthat::expect_warning(
    object = {
      package_info <- get_individual_package_info("rmini")
      expect_package_info(
        package_info,
        package_identical = "rmini",
        version_identical = "DEV 0.0.4",
        loaded_with_pkgload_identical = TRUE,
        repository_match = NA_character_,
        remotetype_identical = "pkgload",
        remotepkgref_match = paste0("^", dest_dir, "$"),
        remoteref_identical = NA_character_,
        remotesha_identical = NA_character_,
        git = list(
          repo = normalizePath(dest_dir),
          is_git = TRUE,
          commit = commit_sha,
          clean = FALSE,
          branch = list(
            name = "master",
            commit = commit_sha,
            upstream = "refs/remotes/origin/master", #nolint: nonportable_path_linter
            remote_url = "https://github.com/yihui/rmini.git",
            up_to_date = FALSE,
            upstream_commit = "f839b7327c4cb422705b9f3b7c5ffc87555d98e2"
          ),
          changed_files = list(
            foo.txt = "modified"
          ),
          tags = list()
        )
      )
      expect_identical(
        package_info[["remotepkgref"]],
        normalizePath(dest_dir)
      )
    },
    "^Identifying development packages may not be accurate.$"
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

test_that("get_individual_package_info gets correct libpath and version of multiple installs", { #nolint: line_length_linter
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  new_lib <- normalizePath(withr::local_tempdir())
  newer_lib <- normalizePath(withr::local_tempdir())
  expect_warning(
    with_local_install(new_lib, "yihui/rmini", { #nolint: nonportable_path_linter
      with_local_install(newer_lib, "yihui/rmini@308d27d", { #nolint: nonportable_path_linter
        package_info <- get_individual_package_info("rmini")
        expect_package_info(
          package_info,
          package_identical = "rmini",
          version_identical = "0.0.3", # Note: not latest version
          repository_match = NA_character_,
          remotetype_identical = "github",
          remotepkgref_match = "^yihui/rmini@308d27d$", #nolint: nonportable_path_linter
          remoteref_identical = "308d27d",
          remotesha_identical = "308d27ddb0b45fda34fc259492145834d72849a9"
        )
        expect_identical(
          package_info[["library"]],
          normalizePath(newer_lib, winslash = "/")
        )
        expect_identical(
          package_info[["library_index"]],
          1L
        )
      })
    })
  )
})

test_that("get_individual_package_info gets correct libpath for lower search priority", { #nolint: line_length_linter
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  new_lib <- normalizePath(withr::local_tempdir())
  newer_lib <- normalizePath(withr::local_tempdir())
  with_local_install(new_lib, "yihui/rmini", { #nolint: nonportable_path_linter
    with_local_install(newer_lib, "digest", {
      package_info <- get_individual_package_info("rmini")
      expect_package_info(
        package_info,
        package_identical = "rmini",
        version_identical = "0.0.4",
        repository_match = NA_character_,
        remotetype_identical = "github",
        remotepkgref_match = "^yihui/rmini$", #nolint: nonportable_path_linter
        remoteref_identical = "HEAD",
        remotesha_identical = "f839b7327c4cb422705b9f3b7c5ffc87555d98e2"
      )
      expect_identical(
        package_info[["library"]],
        normalizePath(new_lib, winslash = "/")
      )
      expect_identical(
        package_info[["library_index"]],
        2L
      )
    })
  })
})
