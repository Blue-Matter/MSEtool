#' Return the TAC Recommendations from an MSE object
#' 
#' @export
TACs <- function(MSE) {
  TimeStepsDF <- TimeStepsDF(MSE)
  
  purrr::map(MSE@PPD, \(DataMP) 
             purrr::map(DataMP, \(DataSim) {
               purrr::map(DataSim, \(DataStock) DataStock@TAC) |> List2Array('Stock')
             }) |> List2Array('Sim')
  ) |> List2Array('MP') |>
    array2DF() |>
    MSEtool:::ConvertDF() |>
    dplyr::mutate(Variable='TAC') |>
    dplyr::left_join(TimeStepsDF, by='TimeStep') |>
    dplyr::arrange("Sim", "Stock", 'TimeStep', 'Value', 'Variable', 'Period', 'MP')
}