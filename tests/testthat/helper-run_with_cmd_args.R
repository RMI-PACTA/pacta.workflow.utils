run_with_cmd_args <- function(
  code,
  cmdargs = character()
) {
  script <- withr::local_tempfile(fileext  = ".R")
  results_file <- withr::local_tempfile(fileext = ".RDS")
  writeLines(
    text = c(
      "args <- commandArgs(trailingOnly = TRUE)",
      "output <- pacta.workflow.utils:::parse_params(args[1])",
      paste0("saveRDS(output, '", results_file, "')")
    ),
    con = script
  )
  readLines(script)
  callr::rscript(
    script = script,
    cmdargs = cmdargs
  )
  output <- readRDS(results_file)
  return(output)
}
