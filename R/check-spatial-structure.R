#' Check and simplify spatial structure of a named array
#'
#' If `array` lacks an `Area` dimension, has only one area, or is identical
#' across all areas, the `Area` dimension is dropped via [DropDimension()].
#' Aborts if the array varies across areas, as spatial structure is not
#' currently supported.
#'
#' @param array A named array, optionally with an `Area` dimension.
#' @param name `character()` or `NULL`. Name of the array used in the error
#'   message. Defaults to `NULL`.
#'
#' @return `array` with the `Area` dimension dropped, or aborts if the array
#'   varies across areas.
#'
#' @keywords internal
CheckSpatial <- function(array, name = NULL) {
  dn <- dimnames(array)
  areas <- dn[["Area"]]
  
  if (is.null(areas))
    return(array)
  if (length(areas) == 1)
    return(DropDimension(array, "Area"))
  
  area_dim <- which(names(dn) == "Area")
  
  if (identical_across_areas(array, area_dim))
    return(DropDimension(array, "Area", warn=FALSE))
  
  cli::cli_abort(c(
    "i" = "{.val {name}} varies over spatial areas",
    "x" = "This function currently does not support spatial structure"
  ))
}

#' Check if an array is identical across a given dimension
#'
#' @param array A named array.
#' @param area_dim `integer(1)`. Index of the dimension to check across.
#'
#' @return `logical`. `TRUE` if all slices along `area_dim` are identical.
#'
#' @keywords internal
identical_across_areas <- function(array, area_dim) {
  perm <- c(area_dim, seq_along(dim(array))[-area_dim])
  mat <- matrix(aperm(array, perm), nrow = dim(array)[area_dim])
  all(apply(mat[-1, , drop = FALSE], 1, identical, mat[1, ]))
}

