#' `hist` Class and Accessor
#'
#' The [hist-class] object stores the complete historical time-series generated
#' during a MSE spool-up, including population dynamics, exploitation history,
#' reference points, and observational data. See [hist-class] for full
#' documentation of all slots.
#'
#' Users do not typically need to create or manipulate `hist` objects directly —
#' they are generated automatically as part of [Simulate()] and stored in the
#' `Hist` slot of the resulting [mse-class] object. `Hist()` is provided to
#' extract that slot when needed.
#'
#' @param MSE An [mse-class] object. If `NULL` (default), an empty [hist-class]
#'   object is returned.
#'
#' @return When `MSE` is supplied, a [timeseries-class] object extracted from
#'   the `Hist` slot of the [mse-class] object. Note this is a
#'   [timeseries-class] and not a [hist-class] — slots such as `OM`,
#'   `Unfished`, and `Reference` are stored directly on the parent [mse-class]
#'   object to avoid duplication. When `MSE = NULL`, an empty [hist-class]
#'   object is returned.
#'
#' @seealso [hist-class], [mse-class], [timeseries-class], [Simulate()]
#' @include class-hist.R
#' @rdname Hist
#' @export
Hist <- function(MSE=NULL) {
  if (is.null(MSE)) {
    return(methods::new("hist"))
  }
  .CheckClass(MSE, 'mse', 'MSE')
  MSE@Hist
}


.OM2Hist <- function(OM, silent) {

  # Create a Hist object from an OM and extend for all Sims and Years
  
  if (!silent) 
    id <- cli::cli_progress_bar("Initializing `Hist` Object")
  
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
  
  Hist@OM@Stock <- purrr::imap(Hist@OM@Stock, \(Stock, idx) {
    SPFrom <- Stock@SRR@SPFrom
    if (!length(SPFrom)) {
      SPFrom <- idx  
    } else if (is.numeric(SPFrom)) {
      SPFrom <- StockNames(OM)[SPFrom]
    }
    Stock@SRR@SPFrom <- SPFrom
    Stock
  })
  
  if (!silent) {
    cli::cli_alert_success("Initialized `Hist` Object")
  }
  Hist 
}
