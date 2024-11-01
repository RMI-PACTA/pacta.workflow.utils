expect_warning_if_any_pkgload <- function(object, regexp) {
  attached_pkgs <- names(utils::sessionInfo()[["otherPkgs"]])
  has_pkgload <- any(
    vapply(
      X = attached_pkgs,
      FUN = pkgload::is_dev_package,
      FUN.VALUE = logical(1L)
    )
  )
  if (has_pkgload) {
    testthat::expect_warning(object = object, regexp = regexp)
  } else {
    # note not using testthat::expect_no_warning(object = object), since there
    # are often warnings on CI systems about multiple installations of the
    # tested package
    object
  }
}

test_that("get_package_info outputs correct structure for defaults", {
  testthat::skip_if(covr::in_covr())
  expect_warning_if_any_pkgload(
    object = {
      package_info <- get_package_info()
      expect_named(
        object = package_info,
        expected = c("base", "attached", "loaded")
      )
      expect_named(
        object = package_info[["base"]],
        expected = utils::sessionInfo()[["basePkgs"]]
      )
      expect_named(
        object = package_info[["attached"]],
        expected = names(utils::sessionInfo()[["otherPkgs"]])
      )
      expect_named(
        object = package_info[["loaded"]],
        expected = names(utils::sessionInfo()[["loadedOnly"]])
      )
      # Check that the structure of the package_info is correct
      expect_true(
        object = all(
          vapply(
            X = package_info,
            FUN = function(x) {
              all(
                vapply(
                  X = x,
                  FUN = function(x) {
                    all(
                      names(x) == c(
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
                  },
                  FUN.VALUE = logical(1L)
                )
              )
            },
            FUN.VALUE = logical(1L)
          )
        )
      )
    },
    "^Identifying development packages may not be accurate.$"
  )
})

test_that("get_package_info outputs expected value for single package", {
  package_info <- get_package_info("digest")
  expect_identical(
    package_info,
    list(
      digest = get_individual_package_info("digest")
    )
  )
})

test_that("get_package_info outputs expected value for simple vector", {
  package_info <- get_package_info(c("digest", "utils"))
  expect_identical(
    package_info,
    list(
      digest = get_individual_package_info("digest"),
      utils = get_individual_package_info("utils")
    )
  )
})

test_that("get_package_info outputs expected value for named vector", {
  package_info <- get_package_info(c(foo = "digest", bar = "utils"))
  expect_identical(
    package_info,
    list(
      foo = get_individual_package_info("digest"),
      bar = get_individual_package_info("utils")
    )
  )
})

test_that("get_package_info outputs unamed list for unnamed list input", {
  package_info <- get_package_info(list("digest", "utils"))
  expect_identical(
    package_info,
    list(
      list(digest = get_individual_package_info("digest")),
      list(utils = get_individual_package_info("utils"))
    )
  )
})

test_that("get_package_info outputs expected value for simple named list", {
  package_info <- get_package_info(list(foo = "digest", bar = "utils"))
  expect_identical(
    package_info,
    list(
      foo = list(digest = get_individual_package_info("digest")),
      bar = list(utils = get_individual_package_info("utils"))
    )
  )
})

test_that("get_package_info outputs expected value for list with mixed nesting", { #nolint: line_length_linter
  package_info <- get_package_info(
    list(
      foo = list(list("digest")),
      bar = list("digest"),
      baz = "utils"
    )
  )
  expect_identical(
    package_info,
    list(
      foo = list(list(list(digest = get_individual_package_info("digest")))),
      bar = list(list(digest = get_individual_package_info("digest"))),
      baz = list(utils = get_individual_package_info("utils"))
    )
  )
})

empty_named_list <- list()
names(empty_named_list) <- character(0L)

test_that("get_package_info with empty character args returns empty named lists", { #nolint: line_length_linter
  expect_identical(
    object = get_package_info(list(
      base = character(),
      attached = character(),
      loaded = character()
    )),
    expected = list(
      base = empty_named_list,
      attached = empty_named_list,
      loaded = empty_named_list
    )
  )
})

test_that("get_package_info with empty args returns empty lists", {
  expect_identical(
    object = get_package_info(list(
      base = c(), # nolint: unnecessary_concatenation_linter
      attached = c(), # nolint: unnecessary_concatenation_linter
      loaded = c() # nolint: unnecessary_concatenation_linter
    )),
    expected = list(
      base = list(),
      attached = list(),
      loaded = list()
    )
  )
})

test_that("get_package_info with NULL args returns empty lists", {
  expect_identical(
    object = get_package_info(list(
      base = NULL,
      attached = NULL,
      loaded = NULL
    )),
    expected = list(
      base = list(),
      attached = list(),
      loaded = list()
    )
  )
})

test_that("get_package_info respects order", {
  digest_info <- get_individual_package_info("digest")
  utils_info <- get_individual_package_info("utils")
  expect_identical(
    object = get_package_info(
      list(
        base = c("digest", "utils"),
        attached = c("utils", "digest")
      )
    ),
    expected = list(
      base = list(
        digest = digest_info,
        utils = utils_info
      ),
      attached = list(
        utils = utils_info,
        digest = digest_info
      )
    )
  )
})
