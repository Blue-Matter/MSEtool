#' Perform Operations on Two Arrays
#'
#' Multiply, divide, subtract, or add two arrays together.
#'
#' The arrays must have the same named dimensions, but do not need to have
#' the same length for each dimension.
#'
#'  * `ArraySum`: Sum `array1` and `array2`
#'  * `ArrayDivide`: Divide `array1` by `array2`
#'  * `ArrayMultiply`: Multiply `array1` and `array2`
#'  * `ArraySubtract`: Subtract `array2` from `array1` 
#'  * `ArrayExtend`: extends the arrays to have matching dimensions (see [Extend()])
#'  * `ArrayFill`: Update array `object` with the values in `value`
#'
#' @param array1 An array with named dimensions.
#' @param array2 An array with the same named dimensions as `array1`.
#' @param object An array with named dimensions
#' @param value An array with the same dimension names as `object`
#'
#' @return The resulting array
#' @example man-examples/ArrayMultiply.R

#' @name ArrayOperations
NULL

#' @rdname ArrayOperations
#' @export
ArraySum <- function(array1, array2) {
  .ArrayOperation(array1, array2, `+`)
}

#' @rdname ArrayOperations
#' @export
ArrayDivide <- function(array1, array2) {
  out <- .ArrayOperation(array1, array2, `/`)
  out[is.na(out)] <- 0
  out[is.infinite(out)] <- 0
  out
}

#' @rdname ArrayOperations
#' @export
ArrayMultiply <- function(array1, array2) {
  .ArrayOperation(array1, array2)
}

#' @rdname ArrayOperations
#' @export
ArraySubtract <- function(array1, array2) {
  .ArrayOperation(array1, array2, `-`)
}

.ResolveDim <- function(dimname, dname1, dname2) {
  ind1 <- which(names(dname1) == dimname)
  ind2 <- which(names(dname2) == dimname)
  if (!length(ind1) || !length(ind2)) return(NULL)
  list(ind1 = ind1, ind2 = ind2,
       vals1 = dname1[[ind1]], vals2 = dname2[[ind2]])
}

#' @rdname ArrayOperations
#' @export
ArrayExtend <- function(array1, array2) {
  dnames <- .CheckArrays(array1, array2)
  dname1 <- dnames$dname1
  dname2 <- dnames$dname2
  
  # Sim: extend to max nSim; if both length-1, keep as 1
  nSim <- local({
    r <- .ResolveDim("Sim", dname1, dname2)
    if (is.null(r)) return(NULL)
    n1 <- length(r$vals1); n2 <- length(r$vals2)
    if (n1 == 1L && n2 == 1L) return(NULL)
    max(n1, n2)
  })
  
  # Age: extend to union of age classes if lengths differ
  AgeClasses <- local({
    r <- .ResolveDim("Age", dname1, dname2)
    if (is.null(r)) return(NULL)
    if (length(r$vals1) == length(r$vals2)) return(NULL)
    c(r$vals1, r$vals2) |> as.numeric() |> unique() |> sort()
  })
  
  # Classes: extend to union of size classes if lengths differ
  Classes <- local({
    r <- .ResolveDim("Class", dname1, dname2)
    if (is.null(r)) return(NULL)
    if (length(r$vals1) == length(r$vals2)) return(NULL)
    c(r$vals1, r$vals2) |> as.numeric() |> unique() |> sort()
  })
  
  # Year: always union of both sets
  Years <- local({
    r <- .ResolveDim("Year", dname1, dname2)
    if (is.null(r)) return(NULL)
    c(r$vals1, r$vals2) |> as.numeric() |> unique() |> sort()
  })
  
  # Area: extend to union of areas if lengths differ
  Areas <- local({
    r <- .ResolveDim("Area", dname1, dname2)
    if (is.null(r)) return(NULL)
    if (length(r$vals1) == length(r$vals2)) return(NULL)
    c(r$vals1, r$vals2) |> as.numeric() |> unique() |> sort()
  })
  
  list(
    array1 = Extend(array1,
                    nSim       = nSim,
                    AgeClasses = AgeClasses,
                    Classes    = Classes,
                    Years      = Years,
                    Areas      = Areas,
                    backfill   = TRUE),
    array2 = Extend(array2,
                    nSim       = nSim,
                    AgeClasses = AgeClasses,
                    Classes    = Classes,
                    Years      = Years,
                    Areas      = Areas,
                    backfill   = TRUE)
  )
}

.ArrayOperation <- function(array1, array2, operation = `*`) {
  if (is.null(array2)) {
    return(array1)
  }
  if (identical(dim(array1), dim(array2)) &&
      identical(dimnames(array1), dimnames(array2))) {
    return(operation(array1, array2))
  }
  ArrayList <- ArrayExtend(array1, array2)
  operation(ArrayList$array1, ArrayList$array2)
}

.CheckArrays <- function(array1, array2) {
  d1 <- dim(array1)
  d2 <- dim(array2)

  if (length(d1) != length(d2)) {
    cli::cli_abort(c(" `array1` and `array2` must have same number of dimensions",
      "x" = "`array1` has {.val {length(d1)}} dimensions  while `array2` has {.val {length(d2)}} dimensions "
    ))
  }

  dm1 <- array1 |> dimnames()
  dm2 <- array2 |> dimnames()

  nm1 <- names(dm1)
  nm2 <- names(dm2)

  if (is.null(nm1) | is.null(nm2)) {
    cli::cli_abort("`array1` and `array2` must have named dimensions")
  }

  if (!(all(nm1 == nm2))) {
    cli::cli_abort(c("`array1` and `array2` must have same dimension names",
      "x" = "Dimension names are {.val {nm1}} for `array1` and {.val {nm2}} for `array2`"
    ))
  }
  list(
    dname1 = dm1,
    dname2 = dm2
  )
}


#' @rdname ArrayOperations
#' @export
`ArrayFill<-` <- function(object, value) {

  if (is.null(value)) 
    return(object)
  
  if (is.null(object)) {
    object <- value
    return(object)
  }
  

  
  if (is.null(dimnames(object)) || is.null(dimnames(value))) {
    cli::cli_abort('`object` and `value` must have named dimension')
  }
  
  if (!identical(names(dimnames(object)), names(dimnames(value)))) {
    cli::cli_abort('`object` and `value` must have same dimension names')
  }
  
  dn_object<- dimnames(object)
  dn_value <- dimnames(value)
  dim_names <- names(object)
  
  inArray <- purrr::map2(dn_value, dn_object, \(a,b) {
    all(a %in% b)
  }) |> unlist() |> all()
  
  if (inArray) {
    abind::afill(object) <- value
    return(object)
  }
  
  # Need to extend the output array
  full_dimnames <- Map(function(x, y) {
    sort(unique(c(x, y)))
  }, dn_object, dn_value)
  
  full_dims <- vapply(full_dimnames, length, integer(1))
  OutArray <- array(NA, dim=full_dims, dimnames=full_dimnames)
  abind::afill(OutArray) <- object
  abind::afill(OutArray) <- value
  
  OutArray
  
}
