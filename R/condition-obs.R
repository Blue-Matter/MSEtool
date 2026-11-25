
#' Condition Observation Error for Fishery Data
#'
#' Internal wrapper to condition all observed fishery data associated with
#' an operating model `Hist` object.
#'
#' Iterates over stock complexes and observed data sets, conditioning
#' life history, exploitation, landings, discards, CPUE, and survey indices
#' for historical years and generating observation error for projection years.
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

  Saved <- .SaveRNG()
  on.exit(.RestoreRNG(Saved), add = TRUE)
  .SetSeed(paste(Hist@OM@Seed %||% 101, 'ConditionObs'))

  FisheryDataList <- Hist@OM@Data
  nData <- length(FisheryDataList)

  if (nData < 1) return(Hist)

  Complexes <- Hist@OM@Complexes

  .MsgStep("Conditioning observation error to fishery data",
           "Conditioned observation error to fishery data for {.complex {names(FisheryDataList)}}", silent)

  nms <- ""
  show <- .MsgShowProgress(silent)
  if (show)
    id <- cli::cli_progress_bar(
      total  = nData,
      format = "Conditioning observation error [{cli::pb_current}/{cli::pb_total}]: {.complex {nms}}"
    )

  for (i in seq_along(FisheryDataList)) {

    nms         <- names(FisheryDataList)[i]
    stocks      <- Complexes[[i]]
    FisheryData <- FisheryDataList[[i]]

    Hist <- .ConditionObsLifeHistory(Hist, FisheryData, HistYears, stocks, i)
    Hist <- .ConditionObsExploitation(Hist, FisheryData, HistYears, stocks, i)

    Hist <- .ConditionObsEffort(Hist, FisheryData, HistYears, ProjYears, stocks, i)

    for (type in c('Landings', 'Discards'))
      Hist <- .ConditionByElement(Hist, FisheryData, i, stocks, type, \(H, D, st)
        .ConditionObsCatch(H, D, HistYears, ProjYears, st, i, type = type))

    for (type in c('CPUE', 'Survey'))
      Hist <- .ConditionByElement(Hist, FisheryData, i, stocks, type, \(H, D, st)
        .ConditionObsIndex(H, D, HistYears, ProjYears, st, i, type = type,
                           EstimateBeta = EstimateBeta, silent = silent))

    for (type in c('LandingsAtAge', 'DiscardsAtAge', 'LandingsAtSize', 'DiscardsAtSize',
                   'SurveyAtAge', 'SurveyAtSize'))
      Hist <- .ConditionObsComp(Hist, FisheryData, HistYears, ProjYears, stocks, i, type)

    if (show)
      cli::cli_progress_update(id=id)
  }

  Hist

}
