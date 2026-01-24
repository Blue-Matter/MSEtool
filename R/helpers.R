#' General Helper Functions
#' 
#' @name helpers
#' @export
nSim <- function(x) {
  if (isS4(x)) {
    slots <- slotNames(x)
    if ('nSim' %in% slots)
      return(x@nSim)
    return(x@OM@nSim)
  }

  if (is.list(x)) {
    return(purrr::map(x, nSim) |> unlist())
  }

  dnames <- dimnames(x)
  if (!is.null(dnames))
    return(length(dnames[['Sim']]))
}


#' @rdname helpers
#' @export
nArea <- function(x, st=1) {
  if (inherits(x, 'om')) {
    stock <- x@Stock
    if (is.list(stock)) {
      stock <- stock[[st]]
    }
    dd <- dim(stock@Spatial@UnfishedDist)
    d1 <- length(stock@Spatial@UnfishedDist)
  } else {
    stock <- x
    dd <- dim(stock@Spatial@UnfishedDist)
    d1 <- length(stock@Spatial@UnfishedDist)
  }

  if (length(dd)<1) {
    if (length(d1)>0)
      return(d1)
    return(1)
  }

  nms <- names(dimnames(stock@Spatial@UnfishedDist))
  dd[which(nms=='Area')]

}

#' @rdname helpers
#' @export
nStock <- function(object) {
  
  CheckClass(object, c('om', 'hist', 'mse'), 'object')
  
  if (inherits(object,'om')) {
    return(length(object@Stock))
  }
  
  if (inherits(object,'hist')) {
    return(length(object@OM@Stock))
  }
  
  if (inherits(object,'mse')) {
    return(length(object@OM@Stock))
  }
  
}

#' @rdname helpers
#' @export
nFleet <- function(object) {
  CheckClass(object, c('om', 'hist', 'mse'), 'object')
  
  if (inherits(object,'om')) {
    fleet <- object@Fleet
    if (is.null(fleet))
      return(0)
    if (inherits(fleet, 'fleet'))
      return(1)
    if (is.list(fleet[[1]]))
      return(length(fleet[[1]]))
    if(isS4(fleet[[1]])) {
      dd <- dim(object@Fleet[[1]]@Selectivity@MeanAtAge)
      return(dd[3])
    }
  }
  
  return(dim(object@Landings)[4])
  
}





# ----- Non-exported ----

DefaultAges <- function(Ages = NULL) {
  if (!is.null(Ages)) {
    return(Ages)
  }
  Ages(MaxAge = 10)
}

DefaultYears <- function(Years = NULL) {
  if (!is.null(Years)) {
    return(Years)
  }
  
  CurrentYear <- format(Sys.Date(), "%Y") |>
    as.numeric()
  
  seq(1950, CurrentYear + 5)
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

