#' Dimensions of an Object
#'
#' Retrieve or set the dimension of an object.
#'
#' @param x an R object, for example a matrix, array or data frame.
#'
#' This is a wrapper for `base::dim`. The only difference is that this function
#' prints the dimension names (if applicable)
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
#' dim(MyArray)
dim <- function(x) {
  dnames <- dimnames(x) |> names()
  d <- base::dim(x)
  if (is.null(dnames)) {
    return(d)
  }
  names(d) <- dnames
  d
}