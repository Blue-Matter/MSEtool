#' `hist` Class and Accessor
#'
#' The [hist-class] object stores the complete historical time-series generated
#' during a MSE spool-up, including population dynamics, exploitation history,
#' reference points, and observational data. See [hist-class] for full
#' documentation of all slots.
#'
#' `hist` objects are created by [Simulate()]. `Hist()` recovers the `hist`
#' object from an [mse-class] object, combining the historical time-series in
#' `MSE@Hist` with the operating model, reference points, and historical
#' data stored in the `mse` object. 
#'
#' @param MSE An [mse-class] object. If `NULL` (default), an empty [hist-class]
#'   object is returned.
#'
#' @return A [hist-class] object.
#'
#' @examples
#' \dontrun{
#' Hist <- Simulate(SingleStockOM)
#' MSE  <- Project(Hist, MPs = c('CurrentCatch', 'CurrentEffort'))
#' MSE2 <- Project(Hist(MSE), MPs = 'NoFishing')
#' }
#'
#' @seealso [hist-class], [mse-class], [Simulate()], [Project()]
#' @include class-hist.R
#' @rdname Hist
#' @export
Hist <- function(MSE=NULL) {
  if (is.null(MSE)) {
    return(methods::new("hist"))
  }
  .CheckClass(MSE, 'mse', 'MSE')

  Hist <- methods::new('hist')
  for (sl in methods::slotNames('timeseries'))
    methods::slot(Hist, sl) <- methods::slot(MSE@Hist, sl)
  Hist@Misc      <- list()
  Hist@OM        <- MSE@OM
  Hist@Unfished  <- MSE@Unfished
  Hist@Reference <- MSE@Reference
  Hist@Log       <- .HistLog(MSE@Log)
  Hist@Data      <- .HistData(MSE)
  Hist
}

# drop entries recorded while projecting an MP
.HistLog <- function(Log) {
  purrr::map(Log, \(entries) {
    entries[!purrr::map_lgl(entries, \(e) .IsLogEntry(e) && !is.null(e$mp))] |>
      unique()
  })
}

# historical data are stored with the first MP's PPD
.HistData <- function(MSE) {
  if (!length(MSE@PPD))
    return(list())
  .SubsetYear(MSE@PPD[[1]], Years = Years(MSE, 'Historical')) |>
    purrr::imap(\(DataList, sim) purrr::map(DataList, \(Data) {
      if (is.null(Data)) return(Data)
      Data@Advice   <- AdviceData()
      Data@Misc$Sim <- as.numeric(sim)
      Data
    }))
}


.OM2Hist <- function(OM, silent) {

  # Create a Hist object from an OM and extend for all Sims and Years
  
  .MsgStep("Initializing hist object", "Initialized hist object", silent)
  id <- NULL
  if (.MsgShowProgress(silent))
    id <- cli::cli_progress_bar("Initializing hist object")

  # Populate if needed
  OM <- PopulateOM(OM, silent = TRUE)

  Hist <- new("hist")
  Hist@OM <- OM
  HistYears <- Years(OM, "Historical")
  
  # Create Time Series Arrays
  Hist <- .InitializeTimeSeries(Hist)
  
  # Add values included in Misc
  # These values won't be over-written by the model
  # TODO - new feature not used or testedd
  Hist <- .FillFromMisc(Hist)
  
  # Extend all arrays for all sims, ages, historical years, and area
  Hist <- .ExtendHist(Hist, HistYears, silent, id)
  
  Hist@OM <- .UpdateSPFrom(Hist@OM)
  histStockNames <- StockNames(Hist@OM)
  for (idx in seq_along(Hist@OM@Stock)) {
    if (is.null(Hist@OM@Stock[[idx]]@SRR@SPFrom))
      Hist@OM@Stock[[idx]]@SRR@SPFrom <- histStockNames[idx]
  }

  Hist
}
