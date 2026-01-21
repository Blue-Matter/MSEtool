#' Reference Points Object
#'
#' The `reference` class stores biological and management reference points
#' derived from per-recruit, equilibrium, or dynamic population analyses.
#' These reference points are typically used for management strategy
#' evaluation, harvest control rules, and performance metrics.
#'
#' @slot SPR0 List or array of unfished spawning potential ratio (SPR0),
#' by stock and simulation.
#'
#' @slot MSY An object containing MSY-based reference points
#' such as FMSY, BMSY, and MSY.
#'
#' @slot RefLandings List or array of reference landings values.
#'
#' @slot RefRemovals List or array of reference removals values.
#'
#' @slot F01 List or array of fishing mortality corresponding to the
#' 0.1 slope reference point.
#'
#' @slot FMax List or array of fishing mortality at maximum yield-per-recruit.
#'
#' @slot FCrash List or array of fishing mortality leading to population collapse.
#'
#' @slot SPRcrash List or array of spawning potential ratio at collapse.
#'
#' @slot MGT List or array of maximum generation time reference points.
#'
#' @slot BLow List or array defining lower biomass reference points.
#'
#' @slot Equilibrium An object describing equilibrium
#' population dynamics at reference conditions.
#'
#' @slot Dynamic A n object describing dynamic population
#' trajectories used to compute reference points.
#'
#' @slot Misc Miscellaneous list for additional reference quantities or
#' diagnostics.
#'
#'
#' @include class-unions.R
#' @include class-internal.R
#' @export
setClass("reference",
         slots = c(
           SPR0          = "array.list.null",
           MSY           = "refpointsMSY",
           RefLandings   = "array.list.null",
           RefRemovals   = "array.list.null",
           
           F01           = "array.list.null",
           FMax          = "array.list.null",
           FCrash        = "array.list.null",
           SPRcrash      = "array.list.null",
           MGT           = "array.list.null",
           BLow          = "array.list.null",
           
           Equilibrium   = "popdynamics",
           Dynamic       = "popdynamics",
           
           Misc          = "list"
         )
)
