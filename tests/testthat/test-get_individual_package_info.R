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
  pkg_source_identical,
  remotepkgref_match,
  remoteref_identical,
  remotesha_identical,
  built_null = FALSE,
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
      "built",
      "pkg_source",
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
    testthat::expect_null(
      object = package_info[["library"]]
    )
    testthat::expect_null(
      object = package_info[["library_index"]]
    )
    testthat::expect_null(
      object = package_info[["library_index"]]
    )
  } else {
    testthat::expect_in(
      object = package_info[["library"]],
      .libPaths() # nolint: undesirable_function_linter
    )
    testthat::expect_gt(
      object = package_info[["library_index"]],
      expected = 0L
    )
    testthat::expect_lte(
      object = package_info[["library_index"]],
      expected = length(.libPaths()) # nolint: undesirable_function_linter
    )
    testthat::expect_type(
      object = package_info[["library_index"]],
      type = "integer"
    )
    testthat::expect_identical(
      object = package_info[["library"]],
      expected = .libPaths()[package_info[["library_index"]]] # nolint: undesirable_function_linter
    )
  }

  if (is.null(repository_match)) {
    testthat::expect_null(
      object = package_info[["repository"]]
    )
  } else {
    testthat::expect_match(
      object = package_info[["repository"]],
      regexp = repository_match
    )
  }
  if (built_null) {
    testthat::expect_null(
      object = package_info[["built"]]
    )
  } else {
    testthat::expect_false(
      is.null(x = package_info[["built"]])
    )
  }
  testthat::expect_identical(
    object = package_info[["pkg_source"]],
    expected = pkg_source_identical
  )
  if (is.null(remotepkgref_match)) {
    testthat::expect_null(
      package_info[["remotepkgref"]]
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
    testthat::expect_null(
      object = package_info[["git"]]
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
    pkg_source_identical = "CRAN",
    remotepkgref_match = NULL,
    remoteref_identical = NULL,
    remotesha_identical = NULL
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
    repository_match = NULL, #GH Actions installs from RSPM, not CRAN
    pkg_source_identical = "Base",
    remotepkgref_match = NULL,
    remoteref_identical = NULL,
    remotesha_identical = NULL
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
    url = remote_package[["url"]],
    path = dest_dir,
    verbose = FALSE
  )
  new_lib <- normalizePath(withr::local_tempdir())
  with_local_install(new_lib, paste0("local::", dest_dir), {
    package_info <- get_individual_package_info(remote_package[["name"]])
    expect_package_info(
      package_info,
      package_identical = remote_package[["name"]],
      version_identical = remote_package[["version"]],
      repository_match = NULL,
      pkg_source_identical = "Local",
      remotepkgref_match = paste0("^local::", dest_dir, "$"),
      remoteref_identical = NULL,
      remotesha_identical = NULL,
      git = list(
        repo = normalizePath(dest_dir),
        is_git = TRUE,
        commit = remote_package[["sha"]],
        clean = TRUE,
        branch = list(
          name = remote_package[["branch"]],
          commit = remote_package[["sha"]],
          upstream = remote_package[["upstream"]],
          remote_url = remote_package[["url"]],
          up_to_date = TRUE,
          upstream_commit = remote_package[["sha"]]
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
  package_info <- with_local_install(new_lib, remote_package[["gh_repo"]], {
    package_info <- get_individual_package_info(remote_package[["name"]])
    expect_package_info(
      package_info,
      package_identical = remote_package[["name"]],
      version_identical = remote_package[["version"]],
      repository_match = NULL,
      pkg_source_identical = "GitHub",
      remotepkgref_match = paste0("^", remote_package[["gh_repo"]], "$"),
      remoteref_identical = "HEAD",
      remotesha_identical = remote_package[["sha"]]
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
    url = remote_package[["url"]],
    path = dest_dir,
    verbose = FALSE
  )
  loaded <- pkgload::load_all(dest_dir, quiet = TRUE)
  withr::defer({
    pkgload::unload(package = remote_package[["name"]])
  })
  testthat::expect_warning(
    object = {
      package_info <- get_individual_package_info(remote_package[["name"]])
      expect_package_info(
        package_info,
        package_identical = remote_package[["name"]],
        version_identical = paste("DEV", remote_package[["version"]]),
        loaded_with_pkgload_identical = TRUE,
        repository_match = NULL,
        pkg_source_identical = "Local (DEV)",
        remotepkgref_match = paste0("^", dest_dir, "$"),
        remoteref_identical = NULL,
        remotesha_identical = NULL,
        built_null = TRUE,
        git = list(
          repo = normalizePath(dest_dir),
          is_git = TRUE,
          commit = remote_package[["sha"]],
          clean = TRUE,
          branch = list(
            name = remote_package[["branch"]],
            commit = remote_package[["sha"]],
            upstream = remote_package[["upstream"]],
            remote_url = remote_package[["url"]],
            up_to_date = TRUE,
            upstream_commit = remote_package[["sha"]]
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
    url = remote_package[["url"]],
    path = dest_dir,
    verbose = FALSE
  )
  loaded <- devtools::load_all(dest_dir, quiet = TRUE)
  withr::defer({
    devtools::unload(package = remote_package[["name"]])
  })
  testthat::expect_warning(
    object = {
      package_info <- get_individual_package_info(remote_package[["name"]])
      expect_package_info(
        package_info,
        package_identical = remote_package[["name"]],
        version_identical = paste("DEV", remote_package[["version"]]),
        loaded_with_pkgload_identical = TRUE,
        repository_match = NULL,
        pkg_source_identical = "Local (DEV)",
        remotepkgref_match = paste0("^", dest_dir, "$"),
        remoteref_identical = NULL,
        remotesha_identical = NULL,
        built_null = TRUE,
        git = list(
          repo = normalizePath(dest_dir),
          is_git = TRUE,
          commit = remote_package[["sha"]],
          clean = TRUE,
          branch = list(
            name = remote_package[["branch"]],
            commit = remote_package[["sha"]],
            upstream = remote_package[["upstream"]],
            remote_url = remote_package[["url"]],
            up_to_date = TRUE,
            upstream_commit = remote_package[["sha"]]
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
    url = remote_package[["url"]],
    path = dest_dir,
    verbose = FALSE
  )
  testing_git_config(repo = dest_dir)
  test_file <- file.path(dest_dir, "foo.txt")
  writeLines("Hello, world!", con = test_file)
  gert::git_add(files = basename(test_file), repo = normalizePath(dest_dir))
  commit_sha <- gert::git_commit(repo = dest_dir, message = "Initial commit")
  writeLines("Hello, testing!", con = test_file)
  loaded <- devtools::load_all(dest_dir, quiet = TRUE)
  withr::defer({
    devtools::unload(package = remote_package[["name"]])
  })
  testthat::expect_warning(
    object = {
      package_info <- get_individual_package_info(remote_package[["name"]])
      expect_package_info(
        package_info,
        package_identical = remote_package[["name"]],
        version_identical = paste("DEV", remote_package[["version"]]),
        loaded_with_pkgload_identical = TRUE,
        repository_match = NULL,
        pkg_source_identical = "Local (DEV)",
        remotepkgref_match = paste0("^", dest_dir, "$"),
        remoteref_identical = NULL,
        remotesha_identical = NULL,
        built_null = TRUE,
        git = list(
          repo = normalizePath(dest_dir),
          is_git = TRUE,
          commit = commit_sha,
          clean = FALSE,
          branch = list(
            name = remote_package[["branch"]],
            commit = commit_sha,
            upstream = remote_package[["upstream"]],
            remote_url = remote_package[["url"]],
            up_to_date = FALSE,
            upstream_commit = remote_package[["sha"]]
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
    with_local_install(new_lib, remote_package[["gh_repo"]], {
      with_local_install(newer_lib, remote_package[["gh_repo_old"]], {
        package_info <- get_individual_package_info(remote_package[["name"]])
        expect_package_info(
          package_info,
          package_identical = remote_package[["name"]],
          version_identical = remote_package[["old_version"]],
          repository_match = NULL,
          pkg_source_identical = "GitHub",
          remotepkgref_match = paste0(
            "^", remote_package[["gh_repo_old"]], "$"
          ),
          remoteref_identical = "28c716f",
          remotesha_identical = remote_package[["old_sha"]]
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
  with_local_install(new_lib, remote_package[["gh_repo"]], {
    with_local_install(newer_lib, "yihui/rmini", { # nolint: nonportable_path_linter
      package_info <- get_individual_package_info(remote_package[["name"]])
      expect_package_info(
        package_info,
        package_identical = remote_package[["name"]],
        version_identical = remote_package[["version"]],
        repository_match = NULL,
        pkg_source_identical = "GitHub",
        remotepkgref_match = paste0("^", remote_package[["gh_repo"]], "$"),
        remoteref_identical = "HEAD",
        remotesha_identical = remote_package[["sha"]]
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
