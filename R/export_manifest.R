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
  output_files
) {

  manifest_list <- create_manifest(
    input_files = input_files,
    output_files = output_files
  )

  manifest_json <- jsonlite::toJSON(
    manifest_list,
    pretty = TRUE,
    auto_unbox = TRUE
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
  output_files
) {
  logger::log_debug("Creating metadata manifest")
  manifest_list <- list(
    input_files = get_file_metadata(input_files),
    output_files = get_file_metadata(output_files)
  )
  return(manifest_list)
}
