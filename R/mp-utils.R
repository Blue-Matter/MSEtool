#' Filter a TAC Recommendation for `data`/`mp`-Style Management Procedures
#'
#' Replaces negative or non-finite TAC values with `NA`. Intended as a final
#' robustness check on TAC values proposed inside a 
#' management procedure (see [Advice()]) before they are returned. Each
#' element of `TAC` (e.g. one per fleet) is checked independently.
#'
#' @param TAC A numeric vector of TAC recommendations (e.g. one value, or
#'   one per fleet).
#'
#' @return A numeric vector the same length as `TAC`, with negative or
#'   non-finite values replaced by `NA`.
#'
#' @examples
#' FilterTAC(c(120, -5, NA, Inf, 340))
#'
#' @seealso [IndexRate()], [Advice()]
#' @export
FilterTAC <- function(TAC) {
  TAC[TAC < 0 | !is.finite(TAC)] <- NA
  as.numeric(TAC)
}


#' Hockey-Stick Harvest Control Rule
#'
#' Modifies a trial rate (e.g. an exploitation or fishing mortality rate)
#' based on the current level of some status indicator (e.g. an index, or
#' biomass) relative to a reference (target) level.
#'
#' A two-inflection-point ("hockey stick") control rule:
#'  - below `ControlPointsIndex[1]`, `Rate = TrialRate * ControlPointsRate[1]`
#'  - above `ControlPointsIndex[2]`, `Rate = TrialRate * ControlPointsRate[2]`
#'  - in between, the multiplier is linearly interpolated.
#'
#' The returned rate is a multiplier to be applied to current biomass (or an
#' index used as a biomass proxy) by the caller to obtain a TAC - it is not
#' itself a TAC.
#'
#' @param TrialRate Positive number. Trial rate before HCR adjustment.
#' @param Est,Ref Positive numbers, same units. Current status level and its
#'   target/reference level.
#' @param ControlPointsIndex Numeric vector, length 2 (`c(Lx, Ux)`). The
#'   lower and upper control points, in units of `Est / Ref`.
#' @param ControlPointsRate Numeric vector, length 2 (`c(Ly, Uy)`). The rate
#'   multipliers corresponding to `ControlPointsIndex`.
#'
#' @return A numeric rate.
#'
#' @examples
#' # At or above target (Est/Ref >= 1): full trial rate applies
#' HockeyStickHCR(TrialRate = 0.2, Est = 1.2, Ref = 1,
#'                ControlPointsIndex = c(0.5, 1), ControlPointsRate = c(0, 1))
#'
#' # Partway up the 0.5-1 ramp: rate scaled down accordingly
#' HockeyStickHCR(TrialRate = 0.2, Est = 0.75, Ref = 1,
#'                ControlPointsIndex = c(0.5, 1), ControlPointsRate = c(0, 1))
#'
#' # At or below the lower control point: rate set to zero
#' HockeyStickHCR(TrialRate = 0.2, Est = 0.3, Ref = 1,
#'                ControlPointsIndex = c(0.5, 1), ControlPointsRate = c(0, 1))
#'
#' # Plot the control rule across a range of current status (Est / Ref)
#' Level <- seq(0, 1.5, length.out = 200)
#' Rate  <- sapply(Level, HockeyStickHCR,
#'                  TrialRate = 0.2, Ref = 1,
#'                  ControlPointsIndex = c(0.5, 1), ControlPointsRate = c(0, 1))
#' plot(Level, Rate, type = "l", lwd = 2,
#'      xlab = "Current status (Est / Ref)", ylab = "Rate")
#' abline(v = c(0.5, 1), lty = 3, col = "grey50")
#'
#' @seealso [IndexRate()], [Advice()]
#' @export
HockeyStickHCR <- function(TrialRate, Est, Ref,
                           ControlPointsIndex = c(0, 1),
                           ControlPointsRate = c(0, 1)) {
  Level <- Est / Ref
  if (Level <= ControlPointsIndex[1]) return(TrialRate * ControlPointsRate[1])
  if (Level >= ControlPointsIndex[2]) return(TrialRate * ControlPointsRate[2])
  TrialRate * ControlPointsRate[1] +
    TrialRate * (ControlPointsRate[2] - ControlPointsRate[1]) *
    (Level - ControlPointsIndex[1]) / (ControlPointsIndex[2] - ControlPointsIndex[1])
}


#' Constrain a TAC Change Between Management Cycles
#'
#' Applies minimum/maximum fractional change limits to a proposed TAC
#' modifier, then clips the resulting TAC to an absolute range.
#'
#' @param PrevTAC Positive number. Previous TAC (see [LastTAC()]).
#' @param Mod Positive number. Proposed modifier (e.g. `1.2` = 20% increase).
#' @param DeltaDown,DeltaUp Numeric vector, length 2 (`c(min, max)`). Minimum
#'   and maximum allowed fractional change, downward and upward respectively.
#' @param TACRange Numeric vector, length 2 (`c(min, max)`). Absolute bounds
#'   on the TAC.
#'
#' @return A numeric TAC.
#'
#' @examples
#' # Proposed 50% increase, capped at the 20% max upward change
#' ConstrainTAC(PrevTAC = 100, Mod = 1.5,
#'              DeltaDown = c(0.01, 0.3), DeltaUp = c(0.01, 0.2),
#'              TACRange = c(0, 1000))
#'
#' # Proposed 5% decrease, within the minimum-change band: TAC held constant
#' ConstrainTAC(PrevTAC = 100, Mod = 0.95,
#'              DeltaDown = c(0.1, 0.3), DeltaUp = c(0.1, 0.2),
#'              TACRange = c(0, 1000))
#'
#' # Allowed change would exceed the absolute TAC range: clipped to the max
#' ConstrainTAC(PrevTAC = 100, Mod = 1.15,
#'              DeltaDown = c(0.01, 0.3), DeltaUp = c(0.01, 0.2),
#'              TACRange = c(0, 110))
#'
#' @seealso [IndexRate()], [Advice()]
#' @export
ConstrainTAC <- function(PrevTAC, Mod, DeltaDown, DeltaUp, TACRange) {
  if (Mod > (1 + DeltaUp[2]))   Mod <- 1 + DeltaUp[2]
  if (Mod < (1 - DeltaDown[2])) Mod <- 1 - DeltaDown[2]
  if (Mod < (1 + DeltaUp[1]) && Mod > (1 - DeltaDown[1])) Mod <- 1

  TrialTAC <- PrevTAC * Mod
  if (TrialTAC > TACRange[2]) return(TACRange[2])
  if (TrialTAC < TACRange[1]) return(TACRange[1])
  TrialTAC
}


#' Smooth a Time Series with a Loess Polynomial Smoother
#'
#' Fits a [stats::loess()] smoother to a numeric vector, for use inside a
#' management procedure to reduce the influence of observation noise in a
#' data series (an index, effort, catch, etc.) before it is used to set
#' advice. `NA`s in `x` are dropped before fitting and left as `NA` in the
#' returned vector; all other positions are replaced by their fitted value.
#'
#' The number of effective parameters used by the smoother is
#' `sum(!is.na(x)) * ENPMult`, so the degree of smoothing scales with the
#' length of the series: `ENPMult` close to `0` gives a very smooth fit
#' (few effective parameters), while `ENPMult` close to `1` tracks the
#' observations closely.
#'
#' @param x Numeric vector to smooth. May contain `NA`s. When
#'   `Log = TRUE` (the default), every non-`NA` value must be strictly
#'   positive.
#' @param Time Numeric vector the same length as `x` giving the position of
#'   each observation (e.g. a vector of years). `NULL` (default) uses
#'   `seq_along(x)`, i.e. assumes `x` is regularly spaced.
#' @param ENPMult Fraction. Effective-number-of-parameters multiplier
#'   controlling the degree of smoothing; see Details. Default `0.3`.
#' @param Log Logical. If `TRUE` (default), the smoother is fit in
#'   log-space (`log(x)`) and back-transformed, appropriate for series that
#'   are strictly positive and multiplicative in their variability (most
#'   abundance indices, catch, and effort series). If `FALSE`, the smoother
#'   is fit to `x` directly.
#'
#' @return A numeric vector the same length as `x`, with smoothed values in
#'   place of every non-`NA` input and `NA` retained where `x` was `NA`.
#'
#' @examples
#' set.seed(1)
#' index <- 1.5 * exp(-0.05 * (1:20)) * rlnorm(20, 0, 0.2)
#' index[15] <- NA # a missed survey year
#' SmoothSeries(index, ENPMult = 0.3)
#'
#' # An irregularly-spaced series (e.g. surveys in specific calendar years)
#' years <- c(2001, 2003, 2004, 2008, 2012, 2015, 2018, 2020)
#' SmoothSeries(index[1:8], Time = years, ENPMult = 0.5)
#'
#' # A series that isn't strictly positive/multiplicative (e.g. an anomaly)
#' SmoothSeries(rnorm(20), Log = FALSE)
#'
#' @seealso [stats::loess()], [IndexRate()], [IndexTarget()]
#' @export
SmoothSeries <- function(x, Time = NULL, ENPMult = 0.3, Log = TRUE) {
  if (is.null(Time))
    Time <- seq_along(x)

  if (length(Time) != length(x))
    cli::cli_abort("{.arg Time} must be the same length as {.arg x}.")

  ToFill <- !is.na(x)

  if (Log && any(x[ToFill] <= 0))
    cli::cli_abort(c(
      "{.arg x} must be strictly positive when {.arg Log = TRUE}.",
      "i" = "Set {.code Log = FALSE} to smooth {.arg x} directly instead."
    ))

  if (sum(ToFill) < 4)
    cli::cli_abort("At least 4 non-{.val NA} values of {.arg x} are required to fit a smoother.")

  y   <- if (Log) log(x) else x
  Dat <- data.frame(Time = Time, y = y)
  Fit <- stats::loess(y ~ Time, data = Dat, enp.target = sum(ToFill) * ENPMult)

  Fitted <- stats::predict(Fit)
  if (Log) Fitted <- exp(Fitted)

  Pred <- rep(NA_real_, length(x))
  Pred[ToFill] <- Fitted
  Pred
}
