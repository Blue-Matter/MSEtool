
#' Extract and Reconstruct Posterior Predicted Data (PPD)
#'
#' Extracts the projected data objects from an [MSEtool::mse-class] object.
#'
#' @param MSE An object of class [MSEtool::mse-class].
#'
#' @return A named list of PPD ([MSEtool::data-class] objects) for 
#' each simulation, MP, and stock/complex.
#'
#' @details
#' To avoid duplication, historical data are stored only for the first MP.
#' `PPD()` extracts the historical data from the first MP, and then prepends
#' those data to the other (projection only) data objects stored in `MSE@PPD`
#'
#' @seealso [JoinYear()], [Years()], [MSEtool::mse-class]
#'
#'
#' @export
PPD <- function(MSE) {
  CheckClass(MSE, 'mse', 'MSE')
  
  if (length(MSE@PPD)<2)
    return(MSE@PPD)
  
  # Get historical data (only stored in first MP)
  HistYears <- Years(MSE,'H')
  HistData <- SubsetYear(MSE@PPD[[1]], Years=HistYears)
  
  ProjDataList <- MSE@PPD[-1]
  purrr::map(ProjDataList, \(ProjData) JoinYear(HistData, ProjData))
  
}
