#' Extract TAC Recommendations from an MSE Object
#'
#' Extracts the total allowable catch (TAC) recommendations issued by each
#' management procedure (MP) in each simulation, year, and stock from the
#' `PPD` slot of an [mse-class] object. Returns a tidy data frame suitable
#' for plotting and performance indicator calculations.
#'
#' @param MSE An [mse-class] object.
#'
#' @return A data frame with columns:
#'   - `Sim`: simulation index.
#'   - `Stock`: stock name.
#'   - `MP`: management procedure name.
#'   - `Year`: calendar year (or decimal date for sub-annual time steps).
#'   - `Period`: `"Historical"` or `"Projection"`.
#'   - `Value`: TAC recommendation in the units of the operating model.
#'   - `Variable`: always `"TAC"`.
#'
#' @seealso [mse-class], [Advice()]
#' @export
TACs <- function(MSE) {
  MSE@PPD |>
    purrr::map(\(DataMP)
               purrr::map(DataMP, \(DataSim)
                          purrr::map(DataSim, \(DataStock) DataStock@TAC) |>
                            List2Array('Stock')
               ) |> List2Array('Sim')
    ) |>
    List2Array('MP') |>
    array2DF() |>
    ConvertDF() |>
    dplyr::mutate(Variable='TAC') |>
    dplyr::left_join(YearDF(MSE), by='Year') |>
    dplyr::arrange(Sim, Stock, Year, MP, Period, Value, Variable)
}