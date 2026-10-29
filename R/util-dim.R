#' Named Dimensions of an Object
#'
#' A wrapper for `base::dim` that names the returned dimensions by the
#' dimension names (if applicable).
#'
#' @param x an R object, for example a matrix, array or data frame.
#'
#' @return An integer vector of dimensions, named by the dimension names when
#'   `x` has them, or `NULL` if `x` has no dimensions.
#'
#' @export
#' @examples
#' MyArray <- array(1:6,
#'   dim = c(6, 2),
#'   dimnames = list(
#'     Age = 1:6,
#'     Year = c(2025, 2026)
#'   )
#' )
#' Dim(MyArray)
Dim <- function(x) {
  dnames <- dimnames(x) |> names()
  d <- base::dim(x)
  if (is.null(dnames)) {
    return(d)
  }
  names(d) <- dnames
  d
}

# same extents, ignoring names on the dim attribute
.SameDim <- function(a, b) identical(unname(dim(a)), unname(dim(b)))


.SetDnames <- function(dnames, BySim=TRUE) {
  if (BySim & !"Sim" %in% dnames) 
    dnames <- c('Sim', dnames)
  dnames
}
