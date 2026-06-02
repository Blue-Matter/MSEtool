
#' Check catch data availability and units
#'
#' Warns if a catch slot (`Landings` or `Discards`) contains no data, or if
#' any catch values are not in units of biomass
#'
#' If no `Landings` or `Discards` data are available, and a finite TAC value 
#' cannot be produced, the MP will likely fall back to setting effort to last
#' historical effort - ie same as [CurrentEffort()] (the default for 
#' empty [Advice()] objects).
#' 
#' If the `Landings` or `Discards` data are not in units of `Biomass` the 
#' subsequent TAC values may be incorrect because the TAC is always set in units
#' of `Biomass`.
#' 
#' Future versions of `MSEtool` may allow an option to set TAC in units of `Number`.
#'
#' Warnings are suppressed after the last historical year (i.e. once
#' `max(Data@Years) > Data@YearLH`) so that they appear only on the first
#' call within a projection, not on every subsequent timestep.
#'
#' @param Data A [data-class] object.
#' @param incUnits Logical. If `TRUE` (default) an additional warning is
#'   issued when any values in `slot_name` are not in units of biomass.
#' @param slot_names Slots to check. Defaults are `Landings` and `Discards`
#'
#' @return `NULL` invisibly, called for its side-effect warnings.
#' @export
CheckCatch <- function(Data, incUnits = TRUE, slot_names = c('Landings', 'Discards')) {
  
  if (max(Data@Years) > Data@YearLH || Data@Misc$Sim > 1)
    return(invisible(NULL)) # warnings only the first projection year
  

  # Check errors for each slot
  value_errors <- sapply(slot_names, function(s) is.null(slot(Data, s)@Value))
  unit_errors  <- sapply(slot_names, function(s) {
    obj <- slot(Data, s)
    !is.null(obj@Units) && any(obj@Units != 'Biomass') && incUnits
  })
  
  if (!any(value_errors, na.rm = TRUE) && !any(unit_errors, na.rm = TRUE))
    return(invisible(NULL))
  
  if (!is.null(Data@Misc$StockName))
    cli::cli_alert_warning("Stock: {Data@Misc$StockName}")
  
  if (any(value_errors)) {
    affected <- slot_names[value_errors]
    cli::cli_alert_warning(
      'No {.val {affected}} data available for: {.val {Data@Name}}'
    )
    cli::cli_alert_warning(
      'If this data is used to set TAC, the MP will likely use {.val CurrentEffort} instead'
    )
  }
  
  if (any(unit_errors)) {
    affected <- slot_names[unit_errors]
    cli::cli_alert_warning(
      'Caution: TAC is always set in units of {.val Biomass} but some \\
      {.val {affected}} values are not in units of {.val Biomass}'
    )
    cli::cli_alert_warning(
      'Using these {.val {affected}} data may result in incorrect TAC values'
    )
  }
  
  invisible(NULL)
  
}