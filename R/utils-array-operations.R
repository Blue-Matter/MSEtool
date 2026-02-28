#' Perform Operations on Two Arrays
#'
#' Multiply, divide, subtract, or add two arrays together.
#'
#' The arrays must have the same named dimensions, but do not need to have
#' the same length for each dimension.
#'
#'  * `ArrayAdd`: Sum `array1` and `array2`
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
ArrayAdd <- function(array1, array2) {
  ArrayOperation(array1, array2, `+`)
}

#' @rdname ArrayOperations
#' @export
ArrayDivide <- function(array1, array2) {
  out <- ArrayOperation(array1, array2, `/`)
  out[is.na(out)] <- 0
  out[is.infinite(out)] <- 0
  out
}

#' @rdname ArrayOperations
#' @export
ArrayMultiply <- function(array1, array2) {
  ArrayOperation(array1, array2)
}

#' @rdname ArrayOperations
#' @export
ArraySubtract <- function(array1, array2) {
  ArrayOperation(array1, array2, `-`)
}

#' @rdname ArrayOperations
#' @export
ArrayExtend <- function(array1, array2) {
  dnames <- CheckArrays(array1, array2)
  dname1 <- names(dnames$dname1)
  dname2 <- names(dnames$dname2)

  # Extend Sims
  sim_ind <- which(dname1 == "Sim")
  if (length(sim_ind)) {
    sim_ind_2 <- which(dname1 == "Sim")
    if (length(dnames$dname1[[sim_ind]])==1 &&
        length(dnames$dname2[[sim_ind_2]]==1)) {
      nSim <- 1
    } else {
      nSim <- c(dnames$dname1[[sim_ind]], dnames$dname2[[sim_ind_2]]) |>
        unique() |>
        length()   
    }
  
  } else {
    nSim <- NULL
  }

  # Extend Ages
  age_ind <- which(dname1 == "Age")
  if (length(age_ind)) {
    age_ind_2 <- which(dname1 == "Age")
    AgeClasses <- c(dnames$dname1[[age_ind]], dnames$dname2[[age_ind_2]]) |>
      unique() |>
      as.numeric()
  } else {
    AgeClasses <- NULL
  }

  # Extends Years
  year_ind <- which(dname1 == "Year")
  if (length(year_ind)) {
    year_ind_2 <- which(dname1 == "Year")
    Years <- c(dnames$dname1[[year_ind]], dnames$dname2[[year_ind_2]]) |>
      unique() |>
      as.numeric()
  } else {
    Years <- NULL
  }
  
  # Extend Areas 
  area_ind <- which(dname1 == "Area")
  if (length(area_ind)) {
    area_ind_2 <- which(dname1 == "Area")
    Areas <- c(dnames$dname1[[area_ind]], dnames$dname2[[area_ind]]) |>
      unique() |>
      as.numeric()
  } else {
    Areas <- NULL
  }
  
  array1 <- Extend(array1, nSim, AgeClasses, Years, Areas)
  array2 <- Extend(array2, nSim, AgeClasses, Years, Areas)

  list(
    array1 = array1,
    array2 = array2
  )
}

ArrayOperation <- function(array1, array2, operation = `*`) {
  if (is.null(array2)) {
    return(array1)
  }
  ArrayList <- ArrayExtend(array1, array2)
  operation(ArrayList$array1, ArrayList$array2)
}

CheckArrays <- function(array1, array2) {
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

  # if (is.null(object)) {
  #   object <- value
  #   return(object)
  # }
  
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
