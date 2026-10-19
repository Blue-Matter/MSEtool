
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

  MSE@PPD <- .DropPaddedPPDYears(MSE@PPD)

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


.PPDObsSlots <- c('Effort', 'Landings', 'Discards', 'CPUE', 'Survey',
                  'LandingsAtAge', 'DiscardsAtAge', 'LandingsAtSize', 'DiscardsAtSize')

# observation rows after max(Data@Years) are forward-filled copies, not observed data
.DropPaddedPPDYears <- function(PPDList) {
  purrr::map(PPDList, \(DataSimList)
    purrr::map(DataSimList, \(DataList)
      purrr::map(DataList, .DropPaddedDataYears)
    )
  )
}

.DropPaddedDataYears <- function(Data) {
  if (!methods::is(Data, 'data') || !length(Data@Years))
    return(Data)
  LastYear <- max(Data@Years)
  for (sl in .PPDObsSlots) {
    obj <- methods::slot(Data, sl)
    for (s in methods::slotNames(obj)) {
      val <- methods::slot(obj, s)
      if (!is.array(val)) next
      TSind <- match('Year', names(dimnames(val)))
      if (is.na(TSind)) next
      # tolerance for seasonal year dimnames rounded to 4 decimals
      idx <- as.numeric(dimnames(val)[[TSind]]) <= LastYear + 1e-3
      if (all(idx)) next
      methods::slot(obj, s) <- do.call(`[`, c(list(val), .MakeDimIndex(idx, val, TSind),
                                             list(drop = FALSE)))
    }
    methods::slot(Data, sl) <- obj
  }
  Data
}
