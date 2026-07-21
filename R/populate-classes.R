#' Populate Default Size Classes for a Length or Weight Object
#'
#' If `object@Classes` is empty, calculates a default set of size class
#' lower bounds spanning from 0 to the maximum expected size. The upper bound is
#' determined by `.CalcMaxBin()` using the object's mean-at-age, CV-at-age,
#' truncation SD, and distribution. Returns `object` unchanged if `Classes` is
#' already populated.
#'
#' The default class structure uses 39 equal-width bins. `Classes` stores the
#' lower bound of each bin; the upper bound of each bin is the lower bound of
#' the next, and the final bin is open-ended (captures all values above its
#' lower bound).
#'
#' @param object A [Length()] or [Weight()] object with slots `Classes`,
#'   `MeanAtAge`, `CVatAge`, `TruncSD`, and `Dist`.
#'
#' @return `object` with `object@Classes` populated if it was previously empty.
#' @keywords internal
.PopulateClasses <- function(object) {
  if (!EmptyObject(object@Classes))
    return(object)
  
  MaxBin <- .CalcMaxBin(
    MeanAtAge = object@MeanAtAge,
    CVatAge   = object@CVatAge,
    TruncSD   = object@TruncSD,
    dist      = object@Dist
  )
  
  bins           <- round(seq(0, to=MaxBin, length.out=40), 2)
  object@Classes <- bins[-length(bins)]
  object
}

#' Compute Bin Midpoints from Lower Bounds
#'
#' Given a vector of bin lower bounds (as stored in `object@Classes`), returns
#' the midpoint of each bin. For bins 1 to n-1 the midpoint is the average of
#' consecutive lower bounds. For the final open-ended bin, the midpoint is
#' extrapolated using the same width as the preceding bin.
#'
#' @param Classes Numeric vector of strictly increasing bin lower bounds.
#'
#' @return Numeric vector of bin midpoints, same length as `Classes`.
#' @keywords internal
.ClassMidpoints <- function(Classes) {
  n <- length(Classes)
  if (n == 1L) return(Classes)
  widths     <- diff(Classes)
  mids       <- numeric(n)
  mids[-n]   <- Classes[-n] + 0.5 * widths
  mids[n]    <- Classes[n]  + 0.5 * widths[n - 1L]
  mids
}


#' Calculate the Maximum Size Bin for a Length or Weight Distribution
#'
#' Computes the upper bound of the size class range by finding the maximum
#' expected size across all simulations and ages, extended to `TruncSD`
#' standard deviations above the mean. Used by `.PopulateClasses()` to set
#' default size class lower bounds.
#'
#' For the normal distribution the upper bound is:
#' `max(mean + TruncSD * SD)`
#'
#' For the log-normal distribution the upper bound is:
#' `max(exp(log_mean + TruncSD * CV))`
#'
#' where `log_mean = log(MeanAtAge) - 0.5 * CV^2`.
#'
#' @param MeanAtAge Numeric array of mean size-at-age (`Sim × Age × Year`).
#' @param CVatAge Numeric array of CV-at-age, matching the dimensions of
#'   `MeanAtAge`.
#' @param TruncSD Numeric. Number of standard deviations above the mean used
#'   to define the upper size bound. Default `2`.
#' @param dist Character. Distribution assumption. One of `"normal"` or
#'   `"lognormal"`.
#'
#' @return A single integer: the ceiling of the maximum expected size across
#'   all simulations, ages, and years.
#' @keywords internal
.CalcMaxBin <- function(MeanAtAge, CVatAge, TruncSD=2, dist='normal') {
  
  flat_index <- function(arr) {
    d <- dim(arr)
    expand.grid(seq_len(d[1]), seq_len(d[2]), seq_len(d[3])) |> as.matrix()
  }
  
  if (dist == 'normal') {
    SDatAge <- ArrayMultiply(MeanAtAge, CVatAge)
    MaxBin  <- max(MeanAtAge[flat_index(MeanAtAge)] +
                     TruncSD * SDatAge[flat_index(SDatAge)]) |> ceiling()
    
  } else if (dist == 'lognormal') {
    CVatAge[!is.finite(CVatAge)]  <- 0.01
    MeanAtAge[MeanAtAge <= 0]     <- 1e-6
    logMeanAtAge <- ArraySubtract(log(MeanAtAge), 0.5 * CVatAge^2)
    MaxBin <- max(exp(logMeanAtAge[flat_index(logMeanAtAge)] +
                        TruncSD * CVatAge[flat_index(CVatAge)])) |> ceiling()
    
  } else {
    cli::cli_abort(
      '{.val {dist}} is not a valid `Dist`. Options are {.val normal} or {.val lognormal}.'
    )
  }
  MaxBin
}
