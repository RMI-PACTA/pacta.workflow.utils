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

# test_that("get_package_info with empty args returns empty lists", {
#   expect_identical(
#     object = get_package_info(),
#     expected = list(
#       base = list(),
#       attached = list(),
#       loaded = list()
#     )
#   )
# })
