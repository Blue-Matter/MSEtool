#' Create a Named List
#'
#' Creates a named list of a given length, optionally filling every element
#' with the same value. A thin convenience wrapper around [stats::setNames()]
#' and [base::vector()].
#'
#' @param nms `character` vector of names for the list elements.
#' @param value Optional. A single value assigned to every element. When
#'   `NULL` (default), all elements are initialised to `NULL`.
#'
#' @return A named list of length `length(nms)`. If `value` is supplied,
#'   every element is set to `value`; otherwise every element is `NULL`.
#'
#' @examples
#' # Empty named list
#' MakeNamedList(c("a", "b", "c"))
#'
#' # Named list with a common value
#' MakeNamedList(c("x", "y"), value = 0)
#' 
#' @importFrom stats setNames
#' @export
MakeNamedList <- function(nms, value = NULL) {
  out <- setNames(vector("list", length(nms)), nms)
  if (!is.null(value))
    out[] <- list(value)
  out
}