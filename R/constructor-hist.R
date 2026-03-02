#' Create or Access a `Hist` Object
#'
#' The `Hist()` constructor initializes a new [hist-class] object using
#' information from a populated [OM()] object or,
#' when applied to an [MSE()] object, returns the historical results stored
#' within that object.
#'
#' @param OM A [OM()] or an [MSE()] object. If missing, an empty [hist-class] object is returned.
#' @param silent Should messages be printed out to the console?
#'
#' @return A [hist-class] object.
#'
#' @seealso [OM()], [Data()]
#' @include class-hist.R
#' @rdname Hist
#' @export
Hist <- function(OM=NULL, silent = FALSE) {
  
  if (is.null(OM)) {
    return(methods::new("hist"))
  }
  
  # Create a Hist object from an OM and extend for all Sims and Years
  if (!silent) {
    id <- cli::cli_progress_bar("Initializing `Hist` Object")
  }

  # Populate if needed
  OM <- PopulateOM(OM, silent = TRUE)

  Hist <- new("hist")
  Hist@OM <- OM
  HistYears <- Years(OM, "Historical")

  # Create Time Series Arrays
  Hist <- InitializeTimeSeries(Hist)
  
  # Add values included in Misc
  # These values won't be over-written by the model
  # TODO - new feature not used or testedd
  Hist <- FillFromMisc(Hist)
  
  # Extend all arrays for all sims, ages, historical years, and area
  Hist <- ExtendHist(Hist, HistYears, silent, id)
  
  Hist@OM@Stock <- purrr::imap(Hist@OM@Stock, \(Stock, idx) {
    SPFrom <- Stock@SRR@SPFrom
    if (!length(SPFrom)) {
      SPFrom <- idx
    }
    if (is.character(SPFrom)) {
      Stock@SRR@SPFrom <- match(SPFrom, StockNames(OM))
    } else if (is.numeric(SPFrom)) {
      Stock@SRR@SPFrom <- SPFrom
    }
    Stock
  })
  
  if (!silent) {
    cli::cli_alert_success("Initialized `Hist` Object")
  }
  Hist 
}

