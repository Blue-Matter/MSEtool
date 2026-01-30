
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
#'
#' @keywords internal
ConditionObs <- function(Hist, silent=FALSE) {
  
  HistYears <- Years(Hist,'H')
  ProjYears <- Years(Hist,'P')
  nSim <- Hist@OM@nSim
  
  FisheryDataList <- Hist@OM@Data
  nData <- length(FisheryDataList)
  
  if (nData<1) return(Hist)
  Complexes <- Hist@OM@Complexes
  
  if (!silent) 
    cli::cli_progress_bar("Conditioning Observation Error for Real Fishery Data: {.val {names(FisheryDataList)}}")
  
  
  for (i in seq_along(FisheryDataList)) {
    
    stocks <- Complexes[[i]]
    FisheryData <- FisheryDataList[[i]]
    Hist <- ConditionObs_Catch(Hist, FisheryData, HistYears, ProjYears, stocks, i)
    
    if (!silent) cli::cli_progress_update()
    
    Hist <- ConditionObs_Catch(Hist, FisheryData, HistYears, ProjYears, stocks, i, 'Discards')
    
    if (!silent) cli::cli_progress_update()
    
    Hist <- ConditionObs_Index(Hist, FisheryData, HistYears, ProjYears, stocks, i)
    
    if (!silent) cli::cli_progress_update()
    
    Hist <- ConditionObs_Index(Hist, FisheryData, HistYears, ProjYears,  stocks, i, 'Survey')
    
    if (!silent) cli::cli_progress_update()

  }
  
  
  if (!silent) 
    cli::cli_alert_success("Conditioned Observation Error for Real Fishery Data: {.val {names(FisheryDataList)}}")
  
  Hist
  
}

