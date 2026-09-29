
#' Internal OM Initilization 
#'
#' Internal helper that initializes an [OM()] object prior to simulation.
#'
#' @param OM An [OM()] object.
#' @param nSim Optional integer specifying the maximum number of simulations 
#' to set in the OM (`nSim` must be < `OM@nsim` to have any impact).
#' If `NULL`, the number of simulations is unchanged.
#' @param silent Logical. Currently unused. Included for future support
#'   of suppressed messaging.
#'
#' @return An initialized [OM()] object.
#'
#' @keywords internal
.StartUp <- function(OM, nSim=NULL, silent=FALSE) {
  .CheckClass(OM)

  .CheckOMControl(OM@Control)

  if (is.null(OM@Control$Clone))
    OM@Control$Clone <- 0

  if (EmptyObject(OM@StockTargeting))
    OM@StockTargeting <- new("stocktargeting")

  OM |>
    ReduceNSim(OM@nSim) |>
    PopulateOM(silent = silent) |>
    ReduceNSim(nSim)

}

.OMControlNames <- c(
  'MSYType', 'RefYears', 'CorrelatedRecDevs', 'HistRel', 'BackCalcEffort',
  'SeasonalAllocationYears', 'StockTargeting', 'EffortOptim', 'RefYield',
  'ProjectChunks', 'DataOM',
  'Clone', 'CalcCatchAtSizeNeeded', 'CalcCatchAtSizeCpp'
)

.CheckOMControl <- function(Control) {
  Unknown <- setdiff(names(Control), .OMControlNames)
  if (length(Unknown)) {
    Hint <- intersect(Unknown, methods::slotNames('hist'))
    cli::cli_alert_warning(
      "{.val {Unknown}} {?is not a recognised/are not recognised} {.code OM@Control} setting{?s} and will be ignored. See {.topic MSEtool::OMControl}."
    )
    if (length(Hint)) {
      Code <- paste("Control(OM)$DataOM <-", deparse(Hint))
      cli::cli_alert_info("To pass {.cls hist} slots to MPs, use {.code {Code}}.")
    }
  }
  .CheckDataOM(Control$DataOM)
}
