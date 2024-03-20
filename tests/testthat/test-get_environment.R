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

test_that("get_single_file_metadata processes CSV tables correctly", {
  expect_identical(
    get_r_session_info(),
    list(
      R.version = utils::sessionInfo()[["R.version"]],
      platform = utils::sessionInfo()[["platform"]],
      running = utils::sessionInfo()[["running"]],
      locale = utils::sessionInfo()[["locale"]],
      tzone = utils::sessionInfo()[["tzone"]],
      libPaths = .libPaths()
    )
  )
})
