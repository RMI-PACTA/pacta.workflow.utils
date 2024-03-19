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

test_that("get_individual_package_info collects information for CRAN packages correctly", {
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

test_that("get_individual_package_info collects information for local packages correctly", {
  package_info <- get_individual_package_info("pacta.workflow.utils")
  expect_type(package_info, "list")
  expect_named(
    package_info,
    "pacta.workflow.utils"
  )
  expect_named(
    package_info[["pacta.workflow.utils"]],
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
    package_info[["pacta.workflow.utils"]][["package"]],
    "pacta.workflow.utils"
  )
  expect_identical(
    package_info[["pacta.workflow.utils"]][["version"]],
    as.character(utils::packageVersion("pacta.workflow.utils"))
  )
  expect_identical(
    package_info[["pacta.workflow.utils"]][["library"]],
    .libPaths()[1] #nolint: undesirable_function_linter
  )
  expect_identical(
    package_info[["pacta.workflow.utils"]][["repository"]],
    NA_character_
  )
  expect_identical(
    package_info[["pacta.workflow.utils"]][["platform"]],
    "*"
  )
  expect_false(
    is.null(package_info[["pacta.workflow.utils"]][["built"]])
  )
  expect_identical(
    package_info[["pacta.workflow.utils"]][["remotetype"]],
    "local"
  )
  expect_match(
    package_info[["pacta.workflow.utils"]][["remotepkgref"]],
    "^local::.*pacta\\.workflow\\.utils$"
  )
  expect_identical(
    package_info[["pacta.workflow.utils"]][["remoteref"]],
    NA_character_
  )
  expect_identical(
    package_info[["pacta.workflow.utils"]][["remotesha"]],
    NA_character_
  )
})
