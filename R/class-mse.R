#' `MSE` Object
#'
#' The `mse` class stores the results of a full Management Strategy
#' Evaluation (MSE). It combines the operating model, historical state,
#' management procedures, and simulated population and fishery outcomes.
#'
#' @slot OM The [OM()] object used as the operating model.
#'
#' @slot MPs List of management procedures evaluated in the MSE.
#'
#' @slot Unfished An bject containing unfished equilibrium
#'   and dynamic reference trajectories.
#'
#' @slot Reference A object containing biological and
#'   management reference points.
#'
#' @slot Hist A object describing the historical period
#'   used to initialize the MSE.
#'
#' @slot PPD List containing posterior predictive distribution outputs
#'   and diagnostics.
#'
#' @slot Log Internal log of MSE diagnostics and execution metadata.
#'
#' @slot Misc Miscellaneous additional objects carried with the MSE.
#'
#' @seealso [MSE], [OM()], [Hist()], [Advice()]
#'
#' @include class-unions.R
#' @include class-om.R
#' @include class-hist.R
#' @include class-internal.R
#' @include class-reference.R
#' 
#' @name MSE
#' @rdname MSE 
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
