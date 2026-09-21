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
  if (anyNA(out)) out[is.na(out)] <- 0
  inf_mask <- is.infinite(out)
  if (any(inf_mask)) out[inf_mask] <- 0
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

.DimExtendOptions <- function(dimname) {
  list(
    coerce_char       = !(dimname %in% c("Fleet", "Stock")),
    allow_missing_one = dimname == "Age"
  )
}

#' @rdname ArrayOperations
#' @export
ArrayExtend <- function(array1, array2) {
  dnames <- .CheckArrays(array1, array2)
  dname1 <- dnames$dname1
  dname2 <- dnames$dname2

  a1 <- array1
  a2 <- array2

  # Every dimension except Year: extend to the union of both sides if
  # lengths differ (a no-op if one side is already the union, e.g. length 1
  # vs length n). Year is handled separately below via forward/back-fill.
  dim_names <- setdiff(names(dname1), "Year")
  for (nm in dim_names) {
    r <- .ResolveDim(nm, dname1, dname2)
    if (is.null(r)) next
    if (length(r$vals1) == length(r$vals2)) next

    opts <- .DimExtendOptions(nm)
    target <- c(r$vals1, r$vals2)
    target <- if (opts$coerce_char) {
      target |> as.numeric() |> unique() |> sort()
    } else {
      unique(target)
    }

    a1 <- .ExtendDim(a1, nm, target, coerce_char = opts$coerce_char,
                     allow_missing_one = opts$allow_missing_one)
    a2 <- .ExtendDim(a2, nm, target, coerce_char = opts$coerce_char,
                     allow_missing_one = opts$allow_missing_one)
  }

  # Year: always union of both sets
  Years <- local({
    r <- .ResolveDim("Year", dname1, dname2)
    if (is.null(r)) return(NULL)
    c(r$vals1, r$vals2) |> as.numeric() |> unique() |> sort()
  })

  list(
    array1 = Extend(a1, Years = Years, backfill = TRUE),
    array2 = Extend(a2, Years = Years, backfill = TRUE)
  )
}

.OpCode <- function(operation) {
  if (identical(operation, `+`)) return(0L)
  if (identical(operation, `-`)) return(1L)
  if (identical(operation, `*`)) return(2L)
  if (identical(operation, `/`)) return(3L)
  cli::cli_abort("Unsupported operation for array broadcasting")
}

.ValidateBroadcastDims <- function(dim1, dim2, dim_names) {
  bad <- dim1 != dim2 & dim1 != 1L & dim2 != 1L
  if (any(bad)) {
    cli::cli_abort(c(
      "`array1` and `array2` are not conformable.",
      "x" = "Dimension {.val {dim_names[bad]}} has length {.val {dim1[bad]}} and {.val {dim2[bad]}}; each must be {.val 1} or match the other."
    ))
  }
}

.ArrayOperation <- function(array1, array2, operation = `*`) {
  if (is.null(array2)) {
    return(array1)
  }

  dn1 <- dimnames(array1)
  dn2 <- dimnames(array2)

  if (identical(dim(array1), dim(array2)) && identical(names(dn1), names(dn2))) {
    same_year <- if ("Year" %in% names(dn1)) identical(dn1$Year, dn2$Year) else TRUE
    if (same_year)
      return(operation(array1, array2))
  }

  .CheckArrays(array1, array2)

  a1 <- array1
  a2 <- array2

  if ("Year" %in% names(dn1)) {
    r <- .ResolveDim("Year", dn1, dn2)
    if (!identical(as.numeric(r$vals1), as.numeric(r$vals2))) {
      Years <- c(r$vals1, r$vals2) |> as.numeric() |> unique() |> sort()
      a1 <- ExtendYears(a1, Years = Years, backfill = TRUE)
      a2 <- ExtendYears(a2, Years = Years, backfill = TRUE)
    }
  }

  dim1 <- dim(a1)
  dim2 <- dim(a2)
  .ValidateBroadcastDims(dim1, dim2, names(dimnames(a1)))

  out <- ArrayBroadcastOp_(as.double(a1), as.integer(dim1),
                           as.double(a2), as.integer(dim2),
                           .OpCode(operation))

  dimnames(out) <- Map(function(v1, v2) if (length(v1) >= length(v2)) v1 else v2,
                       dimnames(a1), dimnames(a2))
  out
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
