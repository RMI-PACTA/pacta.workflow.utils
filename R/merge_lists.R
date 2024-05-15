#' Merge two lists
#'
#' This function takes two lists (`base` and `overlay`), and merges them (by
#' default recursively) into a single list. If a key is present in `overlay`,
#' it is inherited from `overlay`, but keys missing in `overlay` will be
#' inherited from `base`
#'
#' Code is heavily taken from the `merge` function in the `config` package.
#' Notable differences between the two functions are:
#' - This does not reorder keys in the lists
#'
#' @param base_list a named list
#' @param overlay_list a named list
#' @param recursive Should list be merged recurisvely, or only with top level
#' keys?
#'
#' @return merged list
merge_lists <- function(
  base_list,
  overlay_list,
  recursive = TRUE
) {
  if (length(base_list) == 0L) {
    log_trace("Base list is empty, returning overlay list")
    overlay_list
  } else if (length(overlay_list) == 0L) {
    log_trace("Overlay list is empty, returning base list")
    base_list
  } else {

    # Check for potential issues
    if (!is.list(base_list) || !is.list(overlay_list)) {
      log_error("Both base and overlay must be lists.")
      log_error("Type of base_list: ", typeof(base_list))
      log_error("Type of overlay_list: ", typeof(overlay_list))
      stop("Both base and overlay must be lists.")
    }
    combined_lists <- c(base_list, overlay_list)
    if (is.null(names(combined_lists)) || any(names(combined_lists) == "")) {
      log_error("Lists must be named.")
      stop("Lists must be named.")
    }

    # begin merging logic
    merged_list <- base_list
    for (name in names(overlay_list)) {
      base <- base_list[[name]]
      overlay <- overlay_list[[name]]
      if (is.list(base) && is.list(overlay) && recursive) {
        merged_list[[name]] <- merge_lists(base, overlay)
      } else {
        overlay_object <- overlay_list[[which(names(overlay_list) %in% name)]]
        merged_list[[name]] <- overlay_object
      }
    }
    return(merged_list)
  }
}
