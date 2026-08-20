#' Hockey-Stick Harvest Control Rule
#'
#' Modifies a trial rate (e.g. an exploitation or fishing mortality rate)
#' based on the current level of some status indicator (e.g. an index, or
#' biomass) relative to a reference (target) level.
#'
#' A piecewise control rule over 2 or more control points:
#'  - below `ControlPointsIndex[1]`, `Rate = TrialRate * ControlPointsRate[1]`
#'  - above `ControlPointsIndex[n]`, `Rate = TrialRate * ControlPointsRate[n]`
#'  - between consecutive control points, the multiplier is interpolated
#'    according to `RampType`.
#'
#' With 2 control points this is the classic two-inflection-point
#' ("hockey-stick") rule. With 3, a middle control point can be added, 
#' e.g. to cap the rate above a very healthy status as well as floor it
#' below a limit.
#'
#' @param TrialRate Positive number. Trial rate before HCR adjustment.
#' @param Est,Ref Positive numbers, same units. Current status level and its
#'   target/reference level.
#' @param ControlPointsIndex Numeric vector, length `>= 2`, non-decreasing
#'   (`c(Lx, Ux)`, or `c(Lx, Mx, Ux)`, ...). Control points in units of
#'   `Est / Ref`. Equal adjacent points collapse that segment to a step
#'   (e.g. `c(0, 0)` with `ControlPointsRate = c(0, 1)` disables the HCR - a
#'   positive `Est / Ref` always resolves to the upper branch).
#' @param ControlPointsRate Numeric vector, the same length as
#'   `ControlPointsIndex`. The rate multipliers corresponding to
#'   `ControlPointsIndex`.
#' @param RampType Character. Shape of the interpolation between consecutive
#'   control points: `'linear'` (default), or `'smooth'` (a cubic
#'   smoothstep, avoiding the slope discontinuity a linear ramp has at each
#'   control point).
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
#' # Three control points: ramp up to target, then taper above a high level
#' HockeyStickHCR(TrialRate = 0.2, Est = 1.8, Ref = 1,
#'                ControlPointsIndex = c(0.5, 1, 2),
#'                ControlPointsRate  = c(0, 1, 0.7))
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
#' # Linear vs smooth ramp: same control points, different interpolation
#' # 'smooth' avoids the kink at each control point
#' RateLinear <- sapply(Level, HockeyStickHCR,
#'                       TrialRate = 0.2, Ref = 1,
#'                       ControlPointsIndex = c(0.5, 1), ControlPointsRate = c(0, 1),
#'                       RampType = 'linear')
#' RateSmooth <- sapply(Level, HockeyStickHCR,
#'                       TrialRate = 0.2, Ref = 1,
#'                       ControlPointsIndex = c(0.5, 1), ControlPointsRate = c(0, 1),
#'                       RampType = 'smooth')
#' plot(Level, RateLinear, type = "l", lwd = 2,
#'      xlab = "Current status (Est / Ref)", ylab = "Rate")
#' lines(Level, RateSmooth, lwd = 2, col = "dodgerblue")
#' abline(v = c(0.5, 1), lty = 3, col = "grey50")
#' legend("topleft", c("linear", "smooth"), col = c("black", "dodgerblue"), lwd = 2)
#'
#' # Three control points, tapering above target 
#' Level3 <- seq(0, 2.5, length.out = 200)
#' Rate3  <- sapply(Level3, HockeyStickHCR,
#'                   TrialRate = 0.2, Ref = 1,
#'                   ControlPointsIndex = c(0.5, 1, 2),
#'                   ControlPointsRate  = c(0, 1, 0.7))
#' plot(Level3, Rate3, type = "l", lwd = 2,
#'      xlab = "Current status (Est / Ref)", ylab = "Rate")
#' abline(v = c(0.5, 1, 2), lty = 3, col = "grey50")
#'
#' # Same three control points, with a smooth ramp between each pair
#' Rate3Smooth <- sapply(Level3, HockeyStickHCR,
#'                        TrialRate = 0.2, Ref = 1,
#'                        ControlPointsIndex = c(0.5, 1, 2),
#'                        ControlPointsRate  = c(0, 1, 0.7),
#'                        RampType = 'smooth')
#' plot(Level3, Rate3, type = "l", lwd = 2,
#'      xlab = "Current status (Est / Ref)", ylab = "Rate")
#' lines(Level3, Rate3Smooth, lwd = 2, col = "dodgerblue")
#' abline(v = c(0.5, 1, 2), lty = 3, col = "grey50")
#' legend("topleft", c("linear", "smooth"), col = c("black", "dodgerblue"), lwd = 2)
#'
#' @seealso [IndexRate()], [Advice()]
#' @export
HockeyStickHCR <- function(TrialRate, Est, Ref,
                           ControlPointsIndex = c(0, 1),
                           ControlPointsRate = c(0, 1),
                           RampType = c('linear', 'smooth')) {
  RampType <- match.arg(RampType)
  n <- length(ControlPointsIndex)

  if (length(ControlPointsRate) != n)
    cli::cli_abort("{.arg ControlPointsIndex} and {.arg ControlPointsRate} must be the same length.")
  if (n < 2)
    cli::cli_abort("{.arg ControlPointsIndex} must have at least 2 control points.")
  if (is.unsorted(ControlPointsIndex))
    cli::cli_abort("{.arg ControlPointsIndex} must be non-decreasing.")

  Level <- Est / Ref
  if (Level <= ControlPointsIndex[1]) return(TrialRate * ControlPointsRate[1])
  if (Level >= ControlPointsIndex[n]) return(TrialRate * ControlPointsRate[n])

  Seg <- findInterval(Level, ControlPointsIndex, rightmost.closed = TRUE)
  x0  <- ControlPointsIndex[Seg]
  x1  <- ControlPointsIndex[Seg + 1]
  y0  <- ControlPointsRate[Seg]
  y1  <- ControlPointsRate[Seg + 1]

  Frac <- (Level - x0) / (x1 - x0)
  if (RampType == 'smooth') Frac <- Frac^2 * (3 - 2 * Frac)

  TrialRate * (y0 + (y1 - y0) * Frac)
}
