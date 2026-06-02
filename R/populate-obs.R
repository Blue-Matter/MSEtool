#' Populate an Obs Object
#'
#' Populate an [obs-class] object by expanding observation error parameters
#' across simulation replicates and years, generating stochastic error arrays,
#' and applying default values where slots are unspecified.
#'
#' @param Obs An [obs-class] object to populate.
#' @param HistYears Integer vector. Calendar years of the historical period.
#' @param ProjYears Integer vector. Calendar years of the projection period.
#' @param nSim Integer. Number of simulation replicates.
#' @param AgeBins Numeric vector. Age class values used to name
#'   the `Bin` dimension of populated age-composition arrays. Required when
#'   `Obs@LandingsAtAge` or `Obs@DiscardsAtAge` is non-empty.
#' @param SizeBins Numeric vector. Length class midpoints, used to name the
#'   `Bin` dimension of populated size-composition arrays. Required when
#'   `Obs@LandingsAtSize` or `Obs@DiscardsAtSize` is non-empty.
#'
#' @details
#' `PopulateObs()` coordinates population of all observation error components
#' of an [obs-class] object, calling a dedicated sub-function for each slot:
#'
#' * Effort observation error via [PopulateEffortObs()]
#' * Landed catch observation error via [PopulateCatchObs()]
#' * Discarded catch observation error via [PopulateCatchObs()]
#' * CPUE index observation error via [PopulateIndexObs()]
#' * Survey index observation error via [PopulateIndexObs()]
#' * Landed catch-at-age composition error via [PopulateCompObs()]
#' * Discarded catch-at-age composition error via [PopulateCompObs()]
#' * Landed catch-at-size composition error via [PopulateCompObs()]
#' * Discarded catch-at-size composition error via [PopulateCompObs()]
#'
#' Slots for `LifeHistory` and `Exploitation` observation error are reserved
#' for future implementation and are not populated.
#'
#' If a slot contains an empty object (as determined by [EmptyObject()]), it
#' is returned unchanged. See the individual sub-function documentation for
#' details on expansion rules, default values, and accepted input forms.
#'
#' @return A populated [obs-class] object.
#'
#' @seealso
#' [Populate()], [PopulateEffortObs()], [PopulateCatchObs()],
#' [PopulateIndexObs()], [PopulateCompObs()]
#'
#' @export
PopulateObs <- function(Obs,
                        HistYears,
                        ProjYears,
                        nSim,
                        AgeBins  = NULL,
                        SizeBins = NULL) {
  
  CheckClass(Obs, "obs", "Obs")
  
  # TODO: LifeHistory observation error 
  # TODO: Exploitation observation error
  
  Obs@Effort <- PopulateEffortObs(
    Effort    = Obs@Effort,
    nSim      = nSim,
    HistYears = HistYears,
    ProjYears = ProjYears
  )
  
  Obs@Landings <- PopulateCatchObs(
    Catch     = Obs@Landings,
    nSim      = nSim,
    HistYears = HistYears,
    ProjYears = ProjYears
  )
  
  Obs@Discards <- PopulateCatchObs(
    Catch     = Obs@Discards,
    nSim      = nSim,
    HistYears = HistYears,
    ProjYears = ProjYears
  )
  
  Obs@CPUE <- PopulateIndexObs(
    Index     = Obs@CPUE,
    nSim      = nSim,
    HistYears = HistYears,
    ProjYears = ProjYears
  )
  
  Obs@Survey <- PopulateIndexObs(
    Index     = Obs@Survey,
    nSim      = nSim,
    HistYears = HistYears,
    ProjYears = ProjYears
  )
  
  Obs@LandingsAtAge <- PopulateCompObs(
    Comp      = Obs@LandingsAtAge,
    nSim      = nSim,
    HistYears = HistYears,
    ProjYears = ProjYears,
    Bins      = AgeBins,
    BinName   = "Age"
  )
  
  Obs@DiscardsAtAge <- PopulateCompObs(
    Comp      = Obs@DiscardsAtAge,
    nSim      = nSim,
    HistYears = HistYears,
    ProjYears = ProjYears,
    Bins      = AgeBins,
    BinName   = "Age"
  )
  
  Obs@LandingsAtSize <- PopulateCompObs(
    Comp      = Obs@LandingsAtSize,
    nSim      = nSim,
    HistYears = HistYears,
    ProjYears = ProjYears,
    Bins      = SizeBins,
    BinName   = "Size"
  )
  
  Obs@DiscardsAtSize <- PopulateCompObs(
    Comp      = Obs@DiscardsAtSize,
    nSim      = nSim,
    HistYears = HistYears,
    ProjYears = ProjYears,
    Bins      = SizeBins,
    BinName   = "Size"
  )
  
  ReduceDims(Obs, IncYear = TRUE)
}
