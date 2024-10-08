#' Export manifest file with metadata
#'
#' @param manifest_path Path to the manifest file.
#' @param input_files List or vector (named or unnamed) of files that are
#' inputs to the workflow. Passed to `[get_file_metadata()]`.
#' @param output_files List or vector (named or unnamed) of files that are
#' outputs from the workflow. Passed to `[get_file_metadata()]`.
#' @param params List parameters used to define the workflow.
#' @param ... Nested (named) lists to be included in manifest. Passed on to
#' @param file_summary_info Logical. If `TRUE`, include file summary
#' information.
#' `create_manifest`.
#'
#' @return (invisible) JSON string with metadata manifest.
#'
#' @export
export_manifest <- function(
  manifest_path,
  input_files,
  output_files,
  params,
  ...,
  file_summary_info = FALSE
) {

  manifest_list <- create_manifest(
    input_files = input_files,
    output_files = output_files,
    params = params,
    ...,
    file_summary_info = file_summary_info
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
  ...,
  file_summary_info = FALSE
) {
  log_debug("Creating metadata manifest")
  log_trace("Checking ... arguments")
  args_list <- list(...)
  if (length(args_list) > 0L) {
    clean_args <- check_arg_type(args_list)
  }
  manifest_list <- list(
    input_files = get_file_metadata(
      input_files,
      summary_info = file_summary_info
    ),
    output_files = get_file_metadata(
      output_files,
      summary_info = file_summary_info
    ),
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

#' check_arg_type
#'
#' Check that arguments are nicely coercible to JSON. Primarily a check that
#' lists are composed of simple types (other lists, characters,
#' numeric/integers, or logicals). Called for side effect of `stop` if not.
#'
#' @param arg object to check. Lists will be checked recursively, and must be
#' named.
#' @return the same object, unchanged. Function will throw an error if objects
#' are not simple
check_arg_type <- function(arg) {
  log_trace("Checking argument type")
  # remove AsIs class if necessary
  arg <- un_asis(arg)
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

#' un_asis
#'
#' Remove AsIs class from object
#'
#' @param x an object (with the `AsIs` class)
#' @return the same object, without AsIs class
un_asis <- function(x) {
  if (inherits(x, "AsIs")) {
    log_trace("Removing AsIs class from object")
    class(x) <- class(x)[-match("AsIs", class(x))]
  }
  return(x)
}
