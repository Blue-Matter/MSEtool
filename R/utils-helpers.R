


# Much quicker than apply
sumOverDim <- function(x, dimName) {
  
  if (!is.array(x)) 
    cli::cli_abort("`x` must be an array", .internal = TRUE)
  
  dn <- dimnames(x)
  dims <- dim(x)
  nd <- length(dims)
  
  # Determine which dimension to sum over
  sumDim <- if (!is.null(dn) && dimName %in% names(dn)) {
    match(dimName, names(dn))
  } else {
    nd  # last dimension if not named
  }
  
  if (dims[sumDim] < 2) {
    return(DropDimension(x, dimName))
    
  }
  
  # Move sumDim to last dimension
  perm <- c(setdiff(seq_len(nd), sumDim), sumDim)
  x_perm <- aperm(x, perm)
  
  new_dims <- dims[perm]
  x_mat <- matrix(x_perm, ncol = new_dims[length(new_dims)])
  
  summed <- rowSums(x_mat)
  
  # Rebuild array without the summed dimension
  out_dims <- new_dims[-length(new_dims)]
  if (length(out_dims) == 0) out_dims <- 1
  out <- array(summed, dim = out_dims)
  
  # Restore dimnames
  if (!is.null(dn)) {
    dn_new <- dn[-sumDim]
    if (length(dn_new) > 0) dimnames(out) <- dn_new
  }
  
  out
}


#' Sum over named array dimensions
#'
#' Convenience wrappers for summing an array over a specific named dimension.
#' These functions provide fast, explicit alternatives to `apply()` when
#' working with multi-dimensional arrays that use named dimensions.
#'
#'
#' If the requested dimension name is present in `names(dimnames(x))`, that
#' dimension is summed out. If the dimension is not named (or `x` has no
#' dimension names), the **last dimension** of the array is summed by default.
#' 
#' ## Exported functions
#' * `SumOverAge()` – sum over the `"Age"` dimension
#' * `SumOverArea()` – sum over the `"Area"` dimension
#' * `SumOverFleet()` – sum over the `"Fleet"` dimension
#' * `SumOverStock()` – sum over the `"Stock"` dimension
#' * `SumOverYear()` – sum over the `"Year"` dimension
#'
#' @param x An array. If the requested dimension is named, it will be summed
#'   out; otherwise the last dimension is used as a fallback.
#'
#' @return
#' An array with the specified dimension removed. Dimension names are preserved
#' where possible.
#'
#'
#' @rdname SumOverDim
#' @export
SumOverAge <- function(x) {
  sumOverDim(x, dimName = "Age")
}

#' @rdname SumOverDim
#' @export
SumOverArea <- function(x) {
  sumOverDim(x, dimName = "Area")
}

#' @rdname SumOverDim
#' @export
SumOverFleet <- function(x) {
  sumOverDim(x, dimName = "Fleet")
}

#' @rdname SumOverDim
#' @export
SumOverStock <- function(x) {
  sumOverDim(x, dimName = "Stock")
}

#' @rdname SumOverDim
#' @export
SumOverYear <- function(x) {
  sumOverDim(x, dimName = "Year")
}

#' Default Ages and Years
#'
#' Convenience functions that return a sensible default when their argument is
#' `NULL`. Used internally to allow optional `Ages` and `Years` arguments
#' throughout the package without requiring callers to always supply them.
#'
#' - `DefaultAges()`: returns `Ages` unchanged if supplied, otherwise creates
#'   an [ages-class] object with `MaxAge = 10` via [Ages()].
#' - `DefaultYears()`: returns `Years` unchanged if supplied, otherwise
#'   generates an annual sequence from 1950 to five years beyond the current
#'   calendar year.
#'
#' @param Ages An [ages-class] object, or `NULL` (default).
#' @param Years Numeric vector of years, or `NULL` (default).
#'
#' @return
#' - `DefaultAges()`: an [ages-class] object.
#' - `DefaultYears()`: a numeric vector of integer years.
#'
#' @examples
#' DefaultAges()
#' DefaultYears()
#'
#' @seealso [Ages()], [ages-class]
#' @name DefaultAges
#' @export
DefaultAges <- function(Ages=NULL) {
  if (!is.null(Ages))
    return(Ages)
  Ages(MaxAge=10)
}

#' @rdname DefaultAges
#' @export
DefaultYears <- function(Years=NULL) {
  if (!is.null(Years))
    return(Years)
  CurrentYear <- as.numeric(format(Sys.Date(), "%Y"))
  seq(1950, CurrentYear + 5)
}



#' Convert Between Season Count and Time Unit Labels
#'
#' Two complementary functions for converting between the integer number of
#' seasons per year and the corresponding time unit label used throughout the
#' package:
#'
#' - `CalcTSUnits()`: converts a season count to a time unit label string.
#' - `CalcSeasons()`: converts a time unit label string to a season count.
#'
#' Supported conversions:
#'
#' | Seasons | Units       |
#' |---------|-------------|
#' | 1       | `"year"`    |
#' | 2       | `"half-year"` |
#' | 4       | `"quarter"` |
#' | 12      | `"month"`   |
#' | 52      | `"week"`    |
#' | 365     | `"day"`     |
#'
#' @param Seasons Integer. Number of seasons per year. Must be one of
#'   `1`, `2`, `4`, `12`, `52`, or `365`. `NULL` is treated as `1`
#'   (annual). (`CalcTSUnits()` only.)
#' @param Units Character. Time unit label. Case-insensitive. Must be one of
#'   `"year"`, `"half-year"`, `"quarter"`, `"month"`, or `"week"`.
#'   (`CalcSeasons()` only.)
#'
#' @return
#' - `CalcTSUnits()`: a character string time unit label.
#' - `CalcSeasons()`: an integer season count.
#'
#' @examples
#' CalcTSUnits(1)    # "year"
#' CalcTSUnits(4)    # "quarter"
#' CalcTSUnits(NULL) # "year"
#'
#' CalcSeasons("year")    # 1
#' CalcSeasons("Quarter") # 4
#'
#' @seealso [CalcYears()]
#' @name CalcTSUnits
#' @export
CalcTSUnits <- function(Seasons) {
  if (is.null(Seasons))
    return("year")
  
  out <- switch(as.character(Seasons),
                "1"   = "year",
                "2"   = "half-year",
                "4"   = "quarter",
                "12"  = "month",
                "52"  = "week",
                "365" = "day"
  )
  
  if (is.null(out))
    cli::cli_abort(c(
      "x" = "{.val {Seasons}} is not a valid value for `Seasons`.",
      "i" = "Must be one of {.val {c(1, 2, 4, 12, 52, 365)}}."
    ))
  out
}

#' @rdname CalcTSUnits
#' @export
CalcSeasons <- function(Units) {
  if (!length(Units)) return(NULL)

  out <- switch(tolower(Units),
                "year"      = 1L,
                "half-year" = 2L,
                "quarter"   = 4L,
                "month"     = 12L,
                "week"      = 52L
  )
  
  if (is.null(out))
    cli::cli_abort(c(
      "x" = "{.val {Units}} is not a valid value for `Units`.",
      "i" = 'Must be one of {.val {c("year", "half-year", "quarter", "month", "week")}}.'
    ))
  out
}




# ----- Non-exported ----


#' Copy Slots Between S4 Objects
#'
#' Internal utility to copy specified slots from one S4 object to another.
#' The input and output objects must share the named slots.
#'
#' @param S4in An S4 object providing source slot values.
#' @param S4out An S4 object receiving copied slot values.
#' @param slots A character vector of slot names to copy.
#'
#' @return The modified `S4out` object.
#'
#' @keywords internal
CopySlots <- function(S4in, S4out, slots, ignore='Misc') {
  
  if (!isS4(S4in)) {
    cli::cli_abort("`S4in` must be an S4 object.")
  }
  if (!isS4(S4out)) {
    cli::cli_abort("`S4out` must be an S4 object.")
  }
  if (!is.character(slots)) {
    cli::cli_abort("`slots` must be a character vector.")
  }
  if (length(slots) == 0L) {
    return(S4out)
  }
  
  in_slots  <- slotNames(S4in)
  out_slots <- slotNames(S4out)
  
  missing_in  <- setdiff(slots, in_slots)
  missing_out <- setdiff(slots, out_slots)
  
  if (length(missing_in)) {
    cli::cli_abort(
      c("x" = "Some slots are missing from `S4in`:",
        "*" = paste(missing_in, collapse = ", ")
      )
    )
  }
  
  if (length(missing_out)) {
    cli::cli_abort(
      c("x"= "Some slots are missing from `S4out`:",
        "*" = paste(missing_out, collapse = ", ")
      )
    )
  }
  
  for (sl in slots) {
    slot(S4out, sl) <- slot(S4in, sl)
  }
  
  S4out
}


#' Linear interpolation of y at specified x values
#'
#' Computes linear interpolation of `y` at levels `xlev` based on input 
#' vectors `x` and `y`. Optionally prepends a zero-zero intercept and/or 
#' restricts interpolation to ascending x values.
#'
#' @param x Numeric vector of x values.
#' @param y Numeric vector of y values, same length as `x`.
#' @param xlev Numeric value or vector at which to interpolate `y`.
#' @param ascending Logical; if `TRUE`, only considers x values up to the 
#'   maximum in `x` for interpolation.
#' @param zeroint Logical; if `TRUE`, prepends a zero-zero intercept to `x` 
#'   and `y`.
#'
#' @details
#' Uses `stats::approx()` with `rule = 2` (constant extrapolation beyond range) 
#' and `ties = "ordered"`. If `xlev` lies outside the interpolated range, a 
#' warning is issued.
#'
#' @return
#' Numeric vector of interpolated y values corresponding to `xlev`.
#'
#' @author T. Carruthers
#' @keywords internal
LinInterp <- function(x, y, xlev, ascending = FALSE, zeroint = FALSE) {
  
  if (!is.numeric(x) || !is.numeric(y)) {
    cli::cli_abort("`x` and `y` must be numeric vectors.")
  }
  
  if (length(x) != length(y)) {
    cli::cli_abort("`x` and `y` must have the same length.")
  }
  
  # Prepend zero-zero if requested
  if (zeroint) {
    x <- c(0, x)
    y <- c(0, y)
  }
  
  # Restrict to ascending sequence if requested
  if (ascending) {
    idx <- 1:which.max(x)
    x_out <- x[idx]
    y_out <- y[idx]
  } else {
    x_out <- x
    y_out <- y
  }
  
  # Warn if xlev is outside the range
  if (any(xlev < min(x_out))) {
    cli::cli_warn(c("Some `xlev` values are less than min(x).",
                    '`xlev: {.val {xlev}}',
                    '`min(x): {.val {min(x)}}')
                  )
  }
  if (any(xlev > max(x_out))) {
    cli::cli_warn(c("Some `xlev` values are greater than max(x).",
                    '`xlev: {.val {xlev}}',
                    '`max(x): {.val {max(x)}}')
    )
  }
  
  stats::approx(x = x_out, y = y_out, xout = xlev, rule = 2, 
                ties = "ordered")$y
}




#' Copy first element along `Sim` dimension to all other elements
#'
#' @param x An n-dimensional array with the first dimension named `Sim`.
#'  Or a list of such arrays 
#'
#' @return An array of the same dimensions as `x` with the first element
#'   in the first dimension repeated along that dimension.
#' @keywords internal
CopyFirstSim <- function(x) {
  
  if (is.list(x)) {
    for (i in seq_along(x)) {
      x[[i]] <- Recall(x[[i]])
    }
    return(x)  
  }
  
  if (!is.array(x)) cli::cli_abort("x must be an array", .internal=TRUE)
  
  dims <- dim(x)
  dn <- dimnames(x)
  
  if (dims[1] < 2) return(x)
  
  if (!"Sim" %in% names(dn)) cli::cli_abort("x must have named `Sim` dimension", .internal=TRUE)
  
  first_slice <- abind::asub(x, idx = 1, dims = 1, drop = FALSE)
  new_dims <- dims
  new_dims[1] <- dims[1]
  x <- array(rep(first_slice, each = dims[1]), dim = dims)
  
  dimnames(x) <- dn
  x
}





#' Get last non-NA residual per simulation
#'
#' @param LogResiduals Numeric array, dimensions nSim x nYear
#' @return Numeric vector of length nSim, each element is the most recent non-NA residual
#' @keywords internal
LastResidual <- function(LogResiduals) {
  apply(LogResiduals, 1, function(x) {
    if (all(is.na(x))) return(NA_real_)
    x[max(which(!is.na(x)))]
  })
}