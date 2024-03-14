.onLoad <- function(lib, pkg) {
  # define log formatter for package
  logger::log_formatter(logger::formatter_glue, namespace = pkg)
}
