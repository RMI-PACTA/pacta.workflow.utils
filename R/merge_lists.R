#' Merge two lists
#'
#' This function takes two lists (`base` and `overlay`), and merges them (by
#' default recursively) into a single list. If a key is present in `overlay`,
#' it is inherited from `overlay`, but keys missing in `overlay` will be
#' inherited from `base`
#' Code is heavily taken from the `merge` function in the `config` package.
#'
#' @param base_list a (named) list
#' @param overlay_list a (named) list
#' @param recursive Should list be merged recurisvely, or only with top level
#' keys?
#'
#' @return nested list of file details, length the same as the input vector.
merge_lists <- function(
  base_list,
  overlay_list,
  recursive = TRUE
) {
  if (length(base_list) == 0L) {
    overlay_list
  } else if (length(overlay_list) == 0L) {
    base_list
  } else {
    merged_list <- base_list
    for (name in names(overlay_list)) {
      base <- base_list[[name]]
      overlay <- overlay_list[[name]]
      if (is.list(base) && is.list(overlay) && recursive) {
        merged_list[[name]] <- merge_lists(base, overlay)
      } else {
        merged_list[[name]] <- NULL
        merged_list <- append(
          merged_list,
          overlay_list[which(names(overlay_list) %in% name)]
        )
      }
    }
    merged_list
  }
}
