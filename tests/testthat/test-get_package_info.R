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

test_that("get_package_info outputs correct structure", {
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
              },
              FUN.VALUE = logical(1L)
            )
          )
        },
        FUN.VALUE = logical(1L)
      )
    )

  )
})

empty_named_list <- list()
names(empty_named_list) <- character(0L)

test_that("get_package_info with empty character args returns empty named lists", { #nolint: line_length_linter
  expect_identical(
    object = get_package_info(
      base = character(),
      attached = character(),
      loaded = character()
    ),
    expected = list(
      base = empty_named_list,
      attached = empty_named_list,
      loaded = empty_named_list
    )
  )
})

test_that("get_package_info with empty args returns empty lists", {
  expect_identical(
    object = get_package_info(
      base = c(), # nolint: unnecessary_concatenation_linter
      attached = c(), # nolint: unnecessary_concatenation_linter
      loaded = c() # nolint: unnecessary_concatenation_linter
    ),
    expected = list(
      base = list(),
      attached = list(),
      loaded = list()
    )
  )
})

test_that("get_package_info with NULL args returns empty lists", {
  expect_identical(
    object = get_package_info(
      base = NULL,
      attached = NULL,
      loaded = NULL
    ),
    expected = list(
      base = list(),
      attached = list(),
      loaded = list()
    )
  )
})

test_that("get_package_info accepts length 1 arguments", {
  digest_info <- get_individual_package_info("digest")
  expect_identical(
    object = get_package_info(
      base = "digest",
      attached = "digest",
      loaded = "digest"
    ),
    expected = list(
      base = digest_info,
      attached = digest_info,
      loaded = digest_info
    )
  )
})

test_that("get_package_info respects order", {
  digest_info <- get_individual_package_info("digest")
  utils_info <- get_individual_package_info("utils")
  expect_identical(
    object = get_package_info(
      base = c("digest", "utils"),
      attached = c("utils", "digest"),
      loaded = character()
    ),
    expected = list(
      base = c( # using c(), to concatenate already named lists)
        digest_info,
        utils_info
      ),
      attached = c(
        utils_info,
        digest_info
      ),
      loaded = empty_named_list
    )
  )
})
