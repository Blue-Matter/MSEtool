
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
  .CheckClass(MSE, 'mse', 'MSE')
  
  if (length(MSE@PPD)<2)
    return(MSE@PPD)
  
  # Get historical data (only stored in first MP)
  HistYears <- Years(MSE,'H')
  HistData <- .SubsetYear(MSE@PPD[[1]], Years=HistYears) |> .SetPPDMisc()

  ProjDataList <- MSE@PPD[-1]
  ppd_2 <- purrr::map(ProjDataList, \(ProjData)
    JoinYear(HistData, ProjData) |> .SetPPDMisc(ProjData)
  )
  c(MSE@PPD[1], ppd_2)

}

# Data@Misc holds the MP's last Advice@Misc, so it is taken from `From` (empty if NULL)
.SetPPDMisc <- function(DataSimList, From = NULL) {
  purrr::imap(DataSimList, \(DataList, sim)
    purrr::imap(DataList, \(Data, st) {
      Data@Misc <- if (is.null(From)) list() else From[[sim]][[st]]@Misc
      Data
    })
  )
}
