#' Get Metadata for a vector of filepaths
#'
#' This function takes a vector of filepaths and returns a list of file
#' details, suitable for inclusion in manifest export.
#'
#' @param filepaths vector of filepaths
#'
#' @return nested list of file details, length the same as the input vector.
get_file_metadata <- function(filepaths) {
  logger::log_trace("Getting metadata for files.")

  file_metadata <- lapply(filepaths, get_single_file_metadata)

  return(file_metadata)
}

#' Get Metadata for a file
#'
#' This function takes a single filepaths and returns a list of file
#' details, suitable for inclusion in manifest export.
#'
#' @param filepath vector of filepaths
#'
#' @return list of file details
get_single_file_metadata <- function(filepath) {
  if (length(filepath) > 1L) {
    logger::log_error("get_single_file_metadata only accepts single files.")
    stop("Only one file path can be passed to get_single_file_metadata.")
  }

  logger::log_trace("Getting metadata for file: \"{filepath}\".")

  file_name <- basename(filepath)
  file_extension <- tools::file_ext(filepath)
  file_path <- filepath
  file_size <- file.info(filepath)[["size"]]
  file_last_modified <- format(
    as.POSIXlt(file.info(filepath)[["mtime"]], tz = "UTC"),
    "%Y-%m-%dT%H:%M:%S+00:00"
  )
  file_md5 <- digest::digest(filepath, algo = "md5", file = TRUE)

  file_metadata <- list(
    file_name = file_name,
    file_extension = file_extension,
    file_path = file_path,
    file_size = file_size,
    file_last_modified = file_last_modified,
    file_md5 = file_md5
  )

  logger::log_trace("Getting summary information for file: \"{filepath}\".")
  if (tolower(tools::file_ext(filepath)) == "rds") {
    contents <- readRDS(filepath)
  } else if (tolower(tools::file_ext(filepath)) == "csv") {
    contents <- utils::read.csv(filepath)
  } else {
    logger::log_trace(
      "File not supported for summary information: \"{filepath}\"."
    )
    contents <- NULL
  }
  # expecting a data.frame for output files
  if (inherits(contents, "data.frame")) {
    summary_info <- list(
      nrow = nrow(contents),
      colnames = colnames(contents),
      class = class(contents)
    )
  } else {
    logger::log_trace(
      "Only data.frame objects supported for summary information."
    )
    summary_info <- list(
      class = class(contents)
    )
  }

  if (exists("summary_info")) {
    file_metadata[["summary_info"]] <- summary_info
  }

  return(file_metadata)
}
