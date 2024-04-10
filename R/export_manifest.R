#' Export manifest file with metadata
#'
#' @param manifest_path Path to the manifest file.
#' @param input_files List of files that are inputs to the workflow.
#' @param output_files List of files that are outputs from the workflow.
#'
#' @return (invisible) JSON string with metadata manifest.
#'
#' @export
export_manifest <- function(
  manifest_path,
  input_files,
  output_files,
  params,
  ...
) {

  manifest_list <- create_manifest(
    input_files = input_files,
    output_files = output_files,
    params = params,
    ...
  )

  manifest_json <- jsonlite::toJSON(
    manifest_list,
    pretty = TRUE,
    auto_unbox = TRUE,
    null = "null",
    na = "string"
  )

  logger::log_debug("Writing metadata to file: ", manifest_path)
  writeLines(
    text = manifest_json,
    con = manifest_path
  )
  return(invisible(manifest_json))
}

create_manifest <- function(
  input_files,
  output_files,
  ...
) {
  log_debug("Creating metadata manifest")
  log_trace("Checking ... arguments")
  args_list <- list(...)
  if (length(args_list) > 0L) {
    clean_args <- check_arg_type(args_list)
  }
  manifest_list <- list(
    input_files = get_file_metadata(input_files),
    output_files = get_file_metadata(output_files),
    envirionment = get_manifest_envirionment_info(),
    manifest_creation_datetime = format.POSIXct(
      x = Sys.time(),
      format = "%F %R",
      tz = "UTC",
      usetz = TRUE
    )
  )
  if (exists("clean_args")) {
    manifest_list <- c(manifest_list, clean_args)
  }
  return(manifest_list)
}

# Check that arguments are nicely coercible to JSON. called for side effect of
# `stop` if not.
check_arg_type <- function(arg) {
  log_trace("Checking argument type")
  if (inherits(arg, "list")) {
    if (
      length(arg) != length(names(arg)) ||
        any(names(arg) == "")
    ) {
      log_error("elements of lists in ... must be named")
      stop("unnamed arguments in ... of create_manifest (or in nested list)")
    }
    lapply(
      X = arg,
      FUN = check_arg_type
    )
  } else {
    if (
      inherits(arg, "character") ||
        inherits(arg, "numeric") ||
        inherits(arg, "integer") ||
        inherits(arg, "logical")
    ) {
      log_trace("arg is a simple type")
    } else {
      log_error("arg is not a simple type")
      stop("Arguments in ... must be simple types")
    }
  }
  return(arg)
}
