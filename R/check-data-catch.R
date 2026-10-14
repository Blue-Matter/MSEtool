
#' Check catch data availability and units
#'
#' Warns if none of the catch slots in `slot_names` (by default `Landings` and
#' `Discards`) contain data, or if `Landings` and `Discards` are not in the
#' same units as each other. Missing `Discards` alone is not a problem: TACs
#' are then set from `Landings`.
#'
#' If no catch data are available, and a finite TAC value cannot be produced,
#' the MP will likely fall back to setting effort to last historical effort -
#' ie same as [CurrentEffort()] (the default for empty [Advice()] objects).
#'
#' If `Landings` and `Discards` are not in the same units (e.g. one in
#' `Biomass` and the other in `Number`, or one in `"t"` and the other in
#' `"kg"`), summing them to produce a removals
#' TAC is not meaningful. The unit-mismatch check is only performed when both
#' `'Landings'` and `'Discards'` are included in `slot_names`.
#'
#' Within a projection, warnings are issued only for the first simulation on
#' the MP's first call (`Data@Misc$FirstAdvice`), so they appear once per MP.
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

  if (isTRUE(Data@Misc$Sim > 1) || isFALSE(Data@Misc$FirstAdvice))
    return(invisible(NULL))

  no_data <- all(vapply(slot_names, function(s) is.null(slot(Data, s)@Value), logical(1)))

  unit_mismatch <- FALSE
  if (incUnits && all(c('Landings', 'Discards') %in% slot_names)) {
    LandingsUnits <- Data@Landings@Units
    DiscardsUnits <- Data@Discards@Units
    if (!is.null(LandingsUnits) && !is.null(DiscardsUnits)) {
      generic <- c('biomass', 'number')
      specific <- !tolower(LandingsUnits) %in% generic & !tolower(DiscardsUnits) %in% generic
      unit_mismatch <- any(.CatchUnitType(LandingsUnits) != .CatchUnitType(DiscardsUnits) |
                             (specific & LandingsUnits != DiscardsUnits), na.rm = TRUE)
    }
  }

  if (!no_data && !unit_mismatch)
    return(invisible(NULL))

  Stock    <- Data@Misc$StockName %||% Data@Name
  MPPrefix <- if (is.null(Data@Misc$MPName)) "" else cli::format_inline("MP {.val {Data@Misc$MPName}}: ")

  if (no_data)
    cli::cli_alert_warning(paste0(
      MPPrefix,
      "no {.val {slot_names}} data for {.val {Stock}}, so no TAC can be calculated from catch. ",
      "The MP will likely return no TAC and effort will stay at the last historical level (as {.fn CurrentEffort})."
    ))

  if (unit_mismatch)
    cli::cli_alert_warning(paste0(
      MPPrefix,
      "{.val Landings} and {.val Discards} for {.val {Stock}} are not in the same units, ",
      "so summing them to set a TAC may give incorrect TAC values."
    ))

  invisible(NULL)

}
