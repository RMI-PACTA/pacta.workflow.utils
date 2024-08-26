#' @title modify_list_element
#'
#' Apply a function to a one or more elements of a list, given the positions of
#' the elements.
#'
#' @param x List to modify
#' @param positions (List or vector) Position of elements to modify, as a
#' vector of indices
#' @param function_to_apply Function to apply to elements
#' @return modified list
#' @examples
# nolint start
# test_list <- list(a = 1L, b = list(ba = 2L, bb = 2.2), c = "a")
# results <- modify_list_element(
#   x = test_list,
#   position = list(
#     c("b", "bb"),
#     "a"
#   ),
#   function_to_apply = I
# )
# nolint end
#' @export
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

#' @title modify_single_list_element
#'
#' Apply a function to a single element of a list, givin a vector of list
#' indices.
#'
#' @param x List to modify
#' @param position Position of element to modify, as a vector of indices
#' @param function_to_apply Function to apply to element
#' @return modified list
#' @examples
# nolint start
# test_list <- list(a = 1L, b = list(ba = 2L, bb = 2.2), c = "a")
# results <- modify_single_list_element(
#   x = test_list,
#   position = c("b", "bb"),
#   function_to_apply = I
# )
# nolint end
#' @export
modify_single_list_element <- function(
  x,
  position,
  function_to_apply
) {
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
