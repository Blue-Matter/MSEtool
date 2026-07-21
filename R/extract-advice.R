#' Extract TAC Recommendations from an MSE Object
#'
#' Extracts the total allowable catch (TAC) recommendations issued by each
#' management procedure (MP) in each simulation, year, stock, and fleet from
#' the `PPD` slot of an [mse-class] object. Returns a tidy data frame
#' suitable for plotting and performance indicator calculations.
#'
#' @param MSE An [mse-class] object.
#'
#' @details
#' An MP's `Advice(TAC = ...)` only gets recorded in the year it actually
#' changes (see [Advice()]: a TAC applies from the year it's issued "until a
#' new Advice object is returned"), so the raw recommendation history is
#' sparse. `TACs()` forward-fills each gap (via [ExtendYears()]) so the
#' returned series has one row per year the TAC was actually in effect, not
#' just the years it changed. Years before an MP's first recommendation
#' (i.e. the historical period, where no MP has run yet) are absent rather
#' than filled with a placeholder.
#'
#' @return A data frame with columns:
#'   - `Sim`: simulation index.
#'   - `Stock`: stock name.
#'   - `MP`: management procedure name.
#'   - `Year`: calendar year (or decimal date for sub-annual time steps).
#'   - `Period`: always `"Projection"` for the reasons above.
#'   - `Fleet`: fleet name.
#'   - `Value`: TAC recommendation in the units of the operating model.
#'   - `Variable`: always `"TAC"`.
#'
#' @seealso [mse-class], [Advice()], [PPD()]
#' @export
TACs <- function(MSE) {
  .CheckClass(MSE, 'mse', 'MSE')

  HistYears <- Years(MSE, 'Historical')
  ProjYears <- Years(MSE, 'Projection')
  AllYears  <- c(HistYears, ProjYears)

  ppd        <- PPD(MSE)
  mpNames    <- names(ppd)
  stockNames <- StockNames(MSE)

  purrr::map(mpNames, \(mp) {
    simNames <- names(ppd[[mp]])
    purrr::map(simNames, \(sim) {
      purrr::map(seq_along(stockNames), \(st) {
        tac <- ppd[[mp]][[sim]][[st]]@Advice@TAC
        if (is.null(tac) || !length(tac)) return(NULL)

        tac <- ExtendYears(tac, Years = AllYears)

        Array2DF(tac) |>
          dplyr::mutate(
            Sim      = as.numeric(sim),
            Stock    = stockNames[st],
            MP       = mp,
            Period   = ifelse(.data$Year %in% ProjYears, 'Projection', 'Historical'),
            Variable = 'TAC',
            Fleet    = as.character(.data$Fleet)
          )
      }) |> dplyr::bind_rows()
    }) |> dplyr::bind_rows()
  }) |> dplyr::bind_rows() |>
    dplyr::relocate('Sim', 'Stock', 'MP', 'Year', 'Period') |>
    dplyr::arrange(.data$Sim, .data$Stock, .data$MP, .data$Year)
}
