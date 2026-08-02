#' The `effort` S4 Class
#'
#' Defines historical fishing effort and associated spatial structure for use
#' in a [fleet-class] object. Objects are typically created via the [Effort()]
#' constructor, which documents all parameters in detail.
#'
#' @slot Effort `numeric` array, data frame, or `NULL`. Historical fishing
#'   effort. See [Effort()] for accepted formats.
#' @slot Units `character` or `NULL`. Units of fishing effort (e.g.,
#'   `"hours"`, `"trips"`). When `Units = "trips"` the effort array is already
#'   on the absolute trips scale and [TripsScalar()] is ignored by bag-limit
#'   management procedures. See [Effort()].
#' @slot Distribution `numeric` array or `NULL`. Fraction of total effort
#'   allocated to each spatial area (`Sim x Year x Area`). See [Effort()].
#' @slot Targeting `numeric` array or `NULL`. Fleet targeting concentration
#'   parameter (`Sim x Year`). See [Effort()].
#' @slot Maximum `numeric` array or `NULL`. Maximum allowable effort. See
#'   [Effort()].
#' @slot Mode `character` or `NULL`. Spatial utility calculation mode:
#'   `"Density"` (default) or `"Biomass"`. See [Effort()].
#' @slot TripsScalar `numeric` array or `NULL`. Time-varying scalar
#'   \eqn{\alpha_f(t)} converting the effort index to absolute angler trip
#'   counts via \eqn{T_f(t) = \alpha_f(t) \cdot E_f(t)} (`Sim x Year`).
#'   The `Year` dimension may be length 1 (replicated internally) or span the
#'   full model time series. Required by bag-limit management procedures when
#'   `Units != "trips"`. Ignored when `Units = "trips"`. See [Effort()].
#' @slot AnglerPerTrip `numeric` array or `NULL`. Mean number of anglers per
#'   trip (`Sim x Year`). Used by bag-limit management procedures to convert
#'   a per-angler bag limit to a fleet-level retention cap via
#'   \eqn{C^{\text{bag}}_f(t) = B_f \cdot A_f(t) \cdot T_f(t)}, where
#'   \eqn{B_f} is the bag limit (fish per angler per trip) and \eqn{T_f(t)}
#'   is the number of trips. See [Effort()].
#' @slot Theta `numeric` array or `NULL`. Overdispersion parameter
#'   \eqn{\theta} of the negative binomial within-trip catch distribution
#'   (`Sim x Year`). The `Year` dimension may be length 1 (replicated
#'   internally) or span the full model time series. Used by bag-limit
#'   management procedures to model the distribution of retained catch
#'   across trips at a given mean catch rate. See [Effort()].
#' @slot StockTargetingLambda `numeric` array or `NULL`. Multiplier on this
#'   fleet's resistance to changing its stock-targeting mix (`Sim x Year`).
#'   Multi-stock TAC solving only. `1` (default) leaves the internally
#'   derived value unchanged; `0` removes the resistance; larger values make
#'   the fleet hold its existing mix more strongly. See [Effort()].
#' @slot Misc `list`. Miscellaneous additional inputs.
#'
#' @seealso [Effort()] for the constructor and full parameter documentation.
#'   [fleet-class] for the enclosing fleet object.
#'
#' @family fleet
#'
#' @include class-unions.R
#' @name effort-class
setClass(
  "effort",
  slots = c(
    Effort         = "num.array.df",
    Units          = "char.null",
    Distribution   = "num.array.null",
    Targeting      = "num.array.null",
    Maximum        = "num.array.null",
    Mode           = "char.null",
    TripsScalar    = "num.array.null",
    AnglerPerTrip  = "num.array.null",
    Theta          = "num.array.null",
    StockTargetingLambda = "num.array.null",
    Misc           = "list"
  )
)

setValidity("effort", function(object) {
  # TODO
  TRUE
})

