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
#' @param SizeBins Numeric vector. Lower bounds of the length bins, used to name the
#'   `Bin` dimension of populated size-composition arrays. Required when
#'   `Obs@LandingsAtSize`, `Obs@DiscardsAtSize`, or `Obs@SurveyAtSize` is
#'   non-empty.
#'
#' `AgeBins` and `SizeBins` may also be lists named by the complex and/or
#' stocks, giving the classes of each element of a composition slot that is a
#' list named by stock (see [obs-class]).
#'
#' @details
#' `PopulateObs()` coordinates population of all observation error components
#' of an [obs-class] object, calling a dedicated sub-function for each slot:
#'
#' * Life history observation error, checked by [LifeHistoryObs()] rules
#' * Exploitation observation error, checked by [ExploitationObs()] rules
#' * Effort observation error via [PopulateEffortObs()]
#' * Landed catch observation error via [PopulateCatchObs()]
#' * Discarded catch observation error via [PopulateCatchObs()]
#' * CPUE index observation error via [PopulateIndexObs()]
#' * Survey index observation error via [PopulateIndexObs()]
#' * Landed catch-at-age composition error via [PopulateCompObs()]
#' * Discarded catch-at-age composition error via [PopulateCompObs()]
#' * Landed catch-at-size composition error via [PopulateCompObs()]
#' * Discarded catch-at-size composition error via [PopulateCompObs()]
#' * Survey age and size composition error via [PopulateCompObs()]
#'
#' The `LifeHistory` and `Exploitation` specifications are validated and kept
#' as supplied; their random draws are made when the data are generated, from
#' the `Seed` of the operating model.
#'
#' If a slot contains an empty object (as determined by [EmptyObject()]), it
#' is returned unchanged. Otherwise the slots set by the user before
#' population are recorded in `Misc$UserSupplied`; when conditioning on real
#' data, [Simulate()] uses these values for projection years in place of the
#' conditioned estimates. See the individual sub-function documentation for
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
  
  .CheckClass(Obs, "obs", "Obs")
  
  Obs@LifeHistory  <- .PopulateLifeHistoryObs(Obs@LifeHistory)
  Obs@Exploitation <- .PopulateExploitationObs(Obs@Exploitation)
  
  Obs@Effort <- .SafePopulate(\() PopulateEffortObs(
    Effort    = Obs@Effort,
    nSim      = nSim,
    HistYears = HistYears,
    ProjYears = ProjYears
  ), "Effort")
  
  for (type in c("Landings", "Discards")) {
    PopulateEl <- function(Catch) .SafePopulate(function() PopulateCatchObs(
      Catch     = Catch,
      nSim      = nSim,
      HistYears = HistYears,
      ProjYears = ProjYears
    ), type)
    x <- slot(Obs, type)
    slot(Obs, type) <- if (is.list(x)) lapply(x, PopulateEl) else PopulateEl(x)
  }

  for (type in c("CPUE", "Survey")) {
    PopulateEl <- function(Index) .SafePopulate(function() PopulateIndexObs(
      Index     = Index,
      nSim      = nSim,
      HistYears = HistYears,
      ProjYears = ProjYears
    ), type)
    x <- slot(Obs, type)
    slot(Obs, type) <- if (is.list(x)) lapply(x, PopulateEl) else PopulateEl(x)
  }

  for (type in c("LandingsAtAge", "DiscardsAtAge", "LandingsAtSize", "DiscardsAtSize",
                 "SurveyAtAge", "SurveyAtSize")) {
    isAge   <- endsWith(type, "AtAge")
    Bins    <- if (isAge) AgeBins else SizeBins
    BinName <- if (isAge) "Age" else "Class"
    PopulateEl <- function(Comp, El) .SafePopulate(function() PopulateCompObs(
      Comp      = Comp,
      nSim      = nSim,
      HistYears = HistYears,
      ProjYears = ProjYears,
      Bins      = .ElementBins(Bins, El),
      BinName   = BinName
    ), type)
    Comp <- slot(Obs, type)
    slot(Obs, type) <- if (is.list(Comp)) purrr::imap(Comp, PopulateEl) else PopulateEl(Comp, NULL)
  }

  ReduceDims(Obs, IncYear = TRUE)
}


# classes of element `El`: `Bins` is a vector, or a list named by element
.ElementBins <- function(Bins, El) {
  if (!is.list(Bins)) return(Bins)
  if (!is.null(El) && !is.null(Bins[[El]])) return(Bins[[El]])
  Bins[[1]]
}
