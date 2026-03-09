#' Transpose a Nested Named List
#'
#' Reverses the nesting order of a two-level named list, swapping the first
#' and second level keys. For example, a list structured as
#' `list[[stock]][[fleet]]` becomes `list[[fleet]][[stock]]`.
#'
#' Returns `ls` unchanged if all elements are `NULL`.
#'
#' @param ls A named list of named lists, where all inner lists share the same
#'   names.
#'
#' @return A named list with nesting levels transposed.
#'
#' @references
#' Adapted from <https://stackoverflow.com/questions/15263146>
#' @keywords internal
ReverseList <- function(ls) {
  if (all(vapply(ls, is.null, logical(1))))
    return(ls)
  
  inner_names <- names(ls[[1]])
  x <- lapply(ls, `[`, inner_names)
  apply(do.call(rbind, x), 2, as.list)
}