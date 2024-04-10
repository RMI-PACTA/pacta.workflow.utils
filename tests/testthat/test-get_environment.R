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

test_that("get_r_session_info returns expected values", {
  expect_identical(
    get_r_session_info(),
    list(
      R.version = utils::sessionInfo()[["R.version"]],
      platform = utils::sessionInfo()[["platform"]],
      running = utils::sessionInfo()[["running"]],
      locale = utils::sessionInfo()[["locale"]],
      tzone = utils::sessionInfo()[["tzone"]],
      libPaths = .libPaths() # nolint: undesirable_function_linter
    )
  )
})

test_that("get_environment_info returns expected structure", {
  suppressWarnings({ # warnings from `load_all` are expected
    env_info <- get_manifest_envirionment_info()
  })
  expect_type(env_info, "list")
  expect_named(
    object = env_info,
    expected = c("session", "packages")
  )
  expect_named(
    object = env_info[["session"]],
    expected = c(
      "R.version",
      "platform",
      "running",
      "locale",
      "tzone",
      "libPaths"
    )
  )
  expect_named(
    object = env_info[["packages"]],
    expected = c(
      "base",
      "attached",
      "loaded"
    )
  )
})
