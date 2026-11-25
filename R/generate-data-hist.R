
#' Generate Historical Fishery Data
#'
#' Internal wrapper function to generate historical fishery data
#' for all simulation replicates in an operating model history object.
#'
#' Iterates over simulation replicates, generating life history, exploitation,
#' effort, landings, discards, CPUE, and survey data for each stock or stock complex. If all
#' simulation replicates produce identical data, only the first
#' replicate is returned.
#'
#'
#' @param Hist `Hist` object populated with historical fishery dynamics
#' @param parallel Logical; if `TRUE`, generates data across simulations in
#'   parallel using a `future` plan established by [SetupParallel()].
#'   Default `FALSE`.
#' @param silent Logical; if `TRUE`, suppress progress bars and status messages
#'
#' @return Updated `Hist` object with `Hist@Data` containing simulated historical data
#'
#' @keywords internal
.GenerateHistoricalData <- function(Hist, parallel=FALSE, silent=FALSE) {

  .MsgStep("Generating historical data", "Generated historical data", silent)

  Hist <- .CheckObs(Hist, silent)

  HistYears <- Years(Hist,'H')
  nSim <- Hist@OM@nSim
  nArea <- nArea(Hist)

  FleetNames <- FleetNames(Hist@OM)
  StockNames <- StockNames(Hist@OM)

  parallel <- CheckParallel(parallel)

  Observed <- withCallingHandlers(
    .GenerateObservedBiology(Hist, HistYears),
    warning = function(w) {
      Hist <<- .CaptureLog(Hist, cli::ansi_strip(conditionMessage(w)),
                           name = "Observed biology", type = "warning")
      invokeRestart("muffleWarning")
    })

  SimDataList <- if (parallel) {
    CheckPackage('furrr')
    # No live progress bar under parallel - workers can't update the
    # parent's cli::cli_progress_bar (id=NULL below, per .GenerateHistoricalDataSim).
    .MsgStatus("Generating historical data ({nSim} simulation{?s} in parallel)", silent)
    furrr::future_map(
      seq_len(nSim), .GenerateHistoricalDataSim,
      Hist = Hist, HistYears = HistYears, nArea = nArea,
      FleetNames = FleetNames, StockNames = StockNames, Observed = Observed,
      silent = TRUE, id = NULL,
      .options = furrr::furrr_options(
        globals  = c('Hist', 'HistYears', 'nArea', 'FleetNames', 'StockNames', 'Observed'),
        packages = "MSEtool",
        seed     = 101
      )
    )
  } else {
    id <- NULL
    show <- .MsgShowProgress(silent)
    if (show)
      id <- cli::cli_progress_bar("Generating historical data")

    purrr::map(seq_len(nSim), \(sim)
              .GenerateHistoricalDataSim(sim,
                                         Hist,
                                         HistYears,
                                         nArea,
                                         FleetNames,
                                         StockNames,
                                         !show, id,
                                         Observed))
  }
  names(SimDataList) <- seq_len(nSim)

  # Check if all `nSim` data objects are identical and if so, only return sim 1
  identical <- rep(TRUE, nSim)
  if (nSim>1) {
    for (sim in seq(2, nSim)) {
      identical[sim] <- .IdenticalS4(SimDataList[[1]], SimDataList[[sim]])
    }
  }

  if (prod(identical)) {
    Hist@Data <- list("1"= SimDataList[[1]])
  } else {
    Hist@Data <- SimDataList
  }

  # update Index Obs: the indicesobs used for each index, with Efficiency of every simulation
  for (type in c('Survey', 'CPUE')) {
    for (st in seq_along(SimDataList[[1]])) {
      Complex  <- names(Hist@OM@Complexes)[st]
      isList   <- is.list(slot(SimDataList[[1]][[st]], type))
      for (el in names(.SlotElements(slot(SimDataList[[1]][[st]], type), Complex))) {
        BySim <- purrr::map(SimDataList, \(DataList)
          .SlotElements(slot(DataList[[st]], type), Complex)[[el]]@Misc$IndexObs)
        if (is.null(BySim[[1]])) next
        for (nm in names(BySim[[1]])) {
          IndexObs <- BySim[[1]][[nm]]
          IndexObs@Efficiency <- .NameSims(unlist(purrr::map(BySim, \(x) x[[nm]]@Efficiency),
                                                  use.names = FALSE), nSim)
          if (isList) {
            Current <- slot(Hist@OM@Obs[[st]][[nm]], type)
            if (!is.list(Current)) Current <- list()
            Current[[el]] <- IndexObs
            IndexObs <- Current
          }
          slot(Hist@OM@Obs[[st]][[nm]], type) <- IndexObs
        }
      }
      for (x in seq_along(Hist@Data)) {
        Slot <- slot(Hist@Data[[x]][[st]], type)
        if (isList) {
          for (el in names(Slot)) Slot[[el]]@Misc$IndexObs <- NULL
        } else {
          Slot@Misc$IndexObs <- NULL
        }
        slot(Hist@Data[[x]][[st]], type) <- Slot
      }
    }
  }

  Hist
}

#' Generate Historical Data for a Single Simulation
#'
#' Internal function to generate historical data for a single simulation
#'
#' Applies the appropriate observation bias, error, and selectivity for
#' each data type and updates metadata such as names, years, and seasons.
#'
#' @param sim Integer index of the simulation
#' @param Hist Operating model history object
#' @param HistYears Numeric vector of historical years
#' @param nArea Integer number of spatial areas
#' @param FleetNames Character vector of fleet names
#' @param StockNames Character vector of stock names
#' @param silent Logical; if `TRUE`, suppress progress bars and status messages
#' @param id Optional progress bar ID from `cli::cli_progress_bar`
#' @param Observed Observed life history and exploitation of all sims from
#'   `.GenerateObservedBiology()`
#'
#' @return A list of `data` objects for each stock complex, containing
#'   simulated historical values for effort, catch, and indices
#'
#' @keywords internal
.GenerateHistoricalDataSim <- function(sim,
                                       Hist,
                                       HistYears,
                                       nArea,
                                       FleetNames,
                                       StockNames,
                                       silent=FALSE,
                                       id=NULL,
                                       Observed=NULL) {

  Saved <- .SaveRNG()
  on.exit(.RestoreRNG(Saved), add = TRUE)
  .SetRNG(.SeedInt(Hist@OM@Seed %||% 101, 'HistData', .GlobalSim(Hist@OM, sim)))

  Complexes <- Hist@OM@Complexes

  if (!is.null(Hist@OM@Data)) {
    # real data exists
   DataList <- Hist@OM@Data
  } else {
    # make Data object for each stock complex
    DataList <- MakeNamedList(names(Complexes), new('data'))
  }

  nStockData <- length(DataList)

  for (i in seq_len(nStockData)) {

    if (!silent) cli::cli_progress_update(id=id)

    stocks <- Complexes[[i]]
    Data <- DataList[[i]]
    Data@nArea <- nArea

    nSim <- Hist@OM@nSim
    Data@LifeHistory  <- .GenHistDataLifeHistory(sim, Data, Observed$LH[[i]], StockNames[stocks], nSim)
    Data@Exploitation <- .GenHistDataExploitation(sim, Data, Observed$Ex[[i]], StockNames[stocks],
                                                  FleetNames, nSim)

    Data@Effort         <- .GenHistDataEffort(sim, Data, Hist, HistYears, i,
                                              stocks, FleetNames)

    for (type in c('Landings', 'Discards'))
      slot(Data, type) <- .GenHistByElement(Data, Hist, i, stocks, type, \(D, H, st)
        .GenHistDataCatch(sim, D, H, HistYears, i, st, FleetNames, type = type))

    for (type in c('CPUE', 'Survey'))
      slot(Data, type) <- .GenHistByElement(Data, Hist, i, stocks, type, \(D, H, st)
        .GenHistDataIndices(sim, D, H, HistYears, i, st, StockNames, nArea, type = type))

    for (type in c('LandingsAtAge', 'DiscardsAtAge', 'LandingsAtSize', 'DiscardsAtSize',
                   'SurveyAtAge', 'SurveyAtSize'))
      slot(Data, type) <- .GenHistDataComp(sim, Data, Hist, HistYears, i, stocks, type)

    # Metadata
    if (is.null(Data@Name))
      Data@Name <- purrr::map(Hist@OM@Stock[stocks], slot, 'Name') |> unlist() |> paste(collapse='-')

    if (is.null(Data@Years))
      Data@Years <- HistYears

    # projection years are appended by .GenerateProjectionData()
    Data@Years <- Data@Years[Data@Years <= max(HistYears)]
    Data <- .DropDataYearsAfter(Data, max(HistYears))

    if (is.null(Data@Seasons))
      Data@Seasons <- Hist@OM@Seasons

    if (is.null(Data@YearLH) || Data@YearLH > max(HistYears))
      Data@YearLH <- floor(Data@Years[length(Data@Years)])

    # Add Pop Dyn if specified
    Data <- .AddPopDyn(Data, Hist, sim, DataOM = .ResolveDataOM(Hist@OM@Control$DataOM))

    DataList[[i]] <- Data
  }

  DataList
}
