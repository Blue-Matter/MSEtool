#' `mse` Class
#'
#' The `mse` class stores the complete results of a Management Strategy
#' Evaluation. It extends [timeseries-class] with the operating model,
#' management procedures, unfished reference states, biological reference
#' points, and the historical time-series used to initialize the projections.
#'
#' @slot OM The [om-class] object used as the operating model. 
#'
#' @slot MPs Named list of management procedures evaluated in the MSE. Each
#'   element corresponds to one MP applied across all simulations.
#'
#' @slot Unfished An [unfished-class] object containing unfished equilibrium
#'   and dynamic reference trajectories. See [unfished-class] for details.
#'
#' @slot Reference A [reference-class] object containing biological and
#'   management reference points derived from the historical period. See
#'   [reference-class] for details.
#'
#' @slot Hist A [timeseries-class] object containing the historical
#'   time-series of population and fishery dynamics used to initialize the MSE
#'   projections. Typed as [timeseries-class] rather than [hist-class] because
#'   slots such as `OM`, `Unfished`, and `Reference` are already present
#'   directly on the `mse` object, making a full [hist-class] object redundant.
#'   Use [Hist()] to access this slot. For the full standalone historical
#'   object, see [hist-class].
#'
#' @slot PPD List containing posterior predictive distribution outputs and
#'   diagnostics generated during the MSE.
#'
#' @slot Log Internal list storing MSE diagnostics and execution metadata.
#'   Not intended for direct user access.
#'
#' @slot Misc Named list for carrying arbitrary additional objects alongside
#'   the MSE results.
#'
#' The `mse` class inherits all time-series slots from [timeseries-class],
#' covering the projection period. See [timeseries-class] for full slot
#' descriptions including `Number`, `Biomass`, `Landings`, `Discards`,
#' `Effort`, and fishing mortality arrays.
#'
#' @seealso [hist-class], [timeseries-class], [om-class], [unfished-class],
#'   [reference-class], [Hist()], [Advice()]
#'
#' @include class-unions.R
#' @include class-om.R
#' @include class-hist.R
#' @include class-unfished.R
#' @include class-reference.R
#' @name mse-class
#' @export
setClass("mse",
         slots = c(
           OM        = "om",
           MPs       = "list",
           Unfished  = "unfished",
           Reference = "reference",
           Hist      = "timeseries",
           PPD       = "list",
           Log       = "list",
           Misc      = "list"
         ),
         contains = c("timeseries")
)
