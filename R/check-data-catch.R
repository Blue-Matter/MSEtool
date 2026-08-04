
#' Check catch data availability and units
#'
#' Warns if a catch slot (`Landings` or `Discards`) contains no data, or if
#' `Landings` and `Discards` are not in the same units as each other.
#'
#' If no `Landings` or `Discards` data are available, and a finite TAC value
#' cannot be produced, the MP will likely fall back to setting effort to last
#' historical effort - ie same as [CurrentEffort()] (the default for
#' empty [Advice()] objects).
#'
#' If `Landings` and `Discards` are not in the same units (e.g. one in
#' `Biomass` and the other in `Number`), summing them to produce a removals
#' TAC is not meaningful. The unit-mismatch check is only performed when both
#' `'Landings'` and `'Discards'` are included in `slot_names`.
#'
#' Warnings are suppressed after the last historical year (i.e. once
#' `max(Data@Years) > Data@YearLH`) so that they appear only on the first
#' call within a projection, not on every subsequent timestep.
#'
#' @param Data A [data-class] object.
#' @param incUnits Logical. If `TRUE` (default), an additional warning is
#'   issued when `Landings` and `Discards` units do not match each other
#'   (only checked when both are included in `slot_names`).
#' @param slot_names Slots to check. Defaults are `Landings` and `Discards`
#'
#' @return `NULL` invisibly, called for its side-effect warnings.
#' @export
CheckCatch <- function(Data, incUnits = TRUE, slot_names = c('Landings', 'Discards')) {

  if (max(Data@Years) > Data@YearLH || Data@Misc$Sim > 1)
    return(invisible(NULL)) # warnings only the first projection year


  # Check errors for each slot
  value_errors <- sapply(slot_names, function(s) is.null(slot(Data, s)@Value))

  unit_mismatch <- FALSE
  if (incUnits && all(c('Landings', 'Discards') %in% slot_names)) {
    LandingsUnits <- Data@Landings@Units
    DiscardsUnits <- Data@Discards@Units
    if (!is.null(LandingsUnits) && !is.null(DiscardsUnits))
      unit_mismatch <- any(LandingsUnits != DiscardsUnits)
  }

  if (!any(value_errors, na.rm = TRUE) && !unit_mismatch)
    return(invisible(NULL))

  if (!is.null(Data@Misc$StockName))
    cli::cli_alert_warning("Stock: {Data@Misc$StockName}")

  if (any(value_errors)) {
    affected <- slot_names[value_errors]
    if (length(affected)>1) {
      cli::cli_alert_warning(
        'No {.val {affected}} data available for: {.val {Data@Name}}'
      )
      cli::cli_alert_warning(
        'If this data is used to set TAC, the MP will likely use {.val CurrentEffort} instead'
      )
    }

  }

  if (unit_mismatch) {
    cli::cli_alert_warning(
      'Caution: {.val Landings} and {.val Discards} are not in the same units for: {.val {Data@Name}}'
    )
    cli::cli_alert_warning(
      'Summing these values to set TAC may result in incorrect TAC values'
    )
  }

  invisible(NULL)

}
