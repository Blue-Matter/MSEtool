
#' Condition Observation Error for Fishery Data
#'
#' Internal wrapper to condition all observed fishery data associated with
#' an operating model `Hist` object.
#'
#' Iterates over stock complexes and observed data sets, conditioning
#' landings, discards, CPUE, and survey indices for historical years and
#' generating observation error for projection years.
#'
#' @param Hist Operating model history object
#' @param silent Logical; if `TRUE`, suppress progress bars and status messages
#' @param EstimateBeta Logical. Estimate the index hyperstability/
#'   hyperdepletion parameter `Beta` by regression? If `FALSE`, `Beta` is
#'   fixed at `1` for indices without a user-supplied `Beta`. Default `TRUE`
#'   for this internal function; callers normally pass through
#'   `SimControl()`'s own default of `FALSE`.
#'
#' @keywords internal
.ConditionObs <- function(Hist, silent=FALSE, EstimateBeta=TRUE) {
  
  HistYears <- Years(Hist,'H')
  ProjYears <- Years(Hist,'P')
  nSim <- Hist@OM@nSim
  
  FisheryDataList <- Hist@OM@Data
  nData <- length(FisheryDataList)
  
  if (nData < 1) return(Hist)
  
  Complexes <- Hist@OM@Complexes
  
  nms <- ""  
  if (!silent)
    id <- cli::cli_progress_bar(
      total  = nData,
      format = "Conditioning Observation Error [{cli::pb_current}/{cli::pb_total}]: {.val {nms}}"
    )

  for (i in seq_along(FisheryDataList)) {
    
    nms         <- names(FisheryDataList)[i]
    stocks      <- Complexes[[i]]
    FisheryData <- FisheryDataList[[i]]
    
    # TODO
    # - life history
    # - exploitation

    Hist <- .ConditionObsEffort(Hist, FisheryData, HistYears, ProjYears, stocks, i)

    Hist <- .ConditionObsCatch(Hist, FisheryData, HistYears, ProjYears, stocks,
                               i, type = 'Landings')
    
    Hist <- .ConditionObsCatch(Hist, FisheryData, HistYears, ProjYears, stocks, i,
                               type = 'Discards')
    
    Hist <- .ConditionObsIndex(Hist, FisheryData, HistYears, ProjYears, stocks,
                               i, type = 'CPUE', EstimateBeta = EstimateBeta)

    Hist <- .ConditionObsIndex(Hist, FisheryData, HistYears, ProjYears,  stocks,
                               i, type = 'Survey', EstimateBeta = EstimateBeta)
    
    Hist <- .ConditionObsComp(Hist, FisheryData, HistYears, ProjYears,  stocks, 
                              i, type = 'LandingsAtAge')
    
    Hist <- .ConditionObsComp(Hist, FisheryData, HistYears, ProjYears,  stocks, 
                              i, type = 'DiscardsAtAge')
    
    Hist <- .ConditionObsComp(Hist, FisheryData, HistYears, ProjYears,  stocks, 
                              i, type = 'LandingsAtSize')
    
    Hist <- .ConditionObsComp(Hist, FisheryData, HistYears, ProjYears,  stocks, 
                              i, type = 'DiscardsAtSize')
    
    if (!silent) 
      cli::cli_progress_update(id=id)
  }
  
  if (!silent) 
    cli::cli_alert_success("Conditioned Observation Error for Real Fishery Data: {.val {names(FisheryDataList)}}")
  
  Hist
  
}
