modify_list_element <- function(
  x,
  positions,
  function_to_apply
) {
  # cast as list, if not already (if a simple vector was passed in)
  if (!is.list(positions)) {
    positions <- list(positions)
  }
  for (position in positions) {
    x <- modify_single_list_element(
      x = x,
      position = position,
      function_to_apply = function_to_apply
    )
  }
  return(x)
}

modify_single_list_element <- function(
  x,
  position,
  function_to_apply
) {
  if (is.null(x)) {
    browser()
  }
  if (length(position) == 1L) {
    x[[position]] <- function_to_apply(x[[position]])
  } else {
    x[[position[[1L]]]] <- modify_single_list_element(
      x = x[[position[[1L]]]],
      position = position[-1L],
      function_to_apply = function_to_apply
    )
  }
  invisible(x)
}
