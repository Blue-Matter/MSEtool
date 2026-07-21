
#' Apply a Management Procedure if in a Management Year
#'
#' Checks whether `Year` falls in a scheduled management year. If not,
#' the previous advice is carried forward unchanged. If so, the MP is
#' applied via `.CalcAdvice()` and a new nested list of `Advice` objects
#' is returned.
#'
#' @param Year              Integer. Current projection year.
#' @param ManagementYears   Integer vector. Years in which the MP is applied,
#'                          as computed by `.CalcManagementYears`.
#' @param LastAdviceSimList Nested list of `Advice` objects from the most
#'                          recent management year. Returned unchanged in
#'                          non-management years.
#' @param LastAggBagLimitSimList Nested list of `aggbaglimit` objects (one
#'                          list per sim, possibly empty) from the most
#'                          recent management year. Returned unchanged in
#'                          non-management years.
#' @param MPName            Character. Name of the management procedure.
#' @param MPfunction        Function. The MP to apply; must accept a `Data`
#'                          object and return an `Advice` object (or, for
#'                          `mmp`-class MPs, a `DataList` and return an
#'                          `AdviceList`/`AggregateBagLimit` structure - see
#'                          `.CalcAdviceSimMMP()`).
#' @param DataSimList       Nested list of `Data` objects, one per sim and
#'                          stock, trimmed to the current data year.
#' @param Proj              `Hist` object containing the current operating
#'                          model state.
#' @param YearsProj         Integer vector. All projection years.
#' @param mp                Integer. Index of the MP within the `MSE` object.
#' @param FleetNames        Character vector. Fleet names.
#' @param Areas             Integer vector. Area indices.
#'
#' @return A list with elements `AdviceSimList` (nested list of `Advice`
#'   objects) and `AggBagLimitSimList` (nested list of `aggbaglimit`
#'   objects) - either newly computed by `.CalcAdvice` if `Year` is a
#'   management year, or both carried forward unchanged otherwise.
#'
#' @keywords internal
.ApplyMP <- function(Year,
                     ManagementYears,
                     LastAdviceSimList,
                     LastAggBagLimitSimList,
                     MPName,
                     MPfunction,
                     DataSimList,
                     Proj,
                     YearsProj,
                     mp,
                     FleetNames,
                     Areas) {

  MPStartYear <- Proj@OM@MPStartYear
  if (!is.null(MPStartYear) && floor(Year) < MPStartYear)
    return(.BuildInterimAdvice(Proj, Year, YearsProj, FleetNames, Areas))

  if (!Year %in% ManagementYears)
    return(list(AdviceSimList      = LastAdviceSimList,
                AggBagLimitSimList = LastAggBagLimitSimList))

  .CalcAdvice(
    MPName,
    MPfunction,
    DataSimList,
    Year,
    Proj,
    YearsProj,
    mp,
    FleetNames,
    Areas
  )
}

#' Calculate MP Advice for All Simulations
#'
#' Runs a management procedure (`MP`) across all simulations in `DataSimList`
#' and returns a nested list of `Advice` objects for each simulation and stock/complex.
#'
#' @param MPName Name of the management procedure (MP) to run.
#' @param MPfunction MP Function, taken from `MSE@MPS[[MPName]]`
#' @param DataSimList Nested list of `Data` objects length nSim, then length nStock/nComplex
#' @param Year Numeric year for which advice is calculated.
#' @param Proj `Hist` object containing population dynamics up to `Year`-1
#' @param YearsProj Numeric vector of projected years
#'
#' @return A list with elements `AdviceSimList` (a list of length
#'   `length(DataSimList)`, each element a named list of `Advice` objects
#'   for the stocks/complexes) and `AggBagLimitSimList` (a list of the same
#'   length, each element a list of `aggbaglimit` objects returned by an
#'   `mmp`-class MP, or `NULL`).
#'
#' @keywords internal
#'
.CalcAdvice <- function(MPName, MPfunction, DataSimList, Year, Proj, YearsProj, mp,
                       FleetNames, Areas) {
  nSim <- length(DataSimList)

  if (nSim != Proj@OM@nSim)
    cli::cli_abort("Mismatch in number of simulations", .internal=TRUE)

  is_mmp <- inherits(MPfunction, 'mmp')

  AdviceSimList      <- MakeNamedList(1:nSim)
  AggBagLimitSimList <- MakeNamedList(1:nSim)

  for (sim in seq_along(AdviceSimList)) {
    DataList <- DataSimList[[sim]]

    if (is_mmp) {
      result <- try(
        .CalcAdviceSimMMP(sim = sim,
                          MPName = MPName,
                          MPfunction = MPfunction,
                          DataList = DataList,
                          Year = Year,
                          Proj = Proj,
                          YearsProj = YearsProj,
                          mp = mp,
                          FleetNames = FleetNames,
                          Areas = Areas),
        silent = TRUE
      )
      if (inherits(result, 'try-error')) {
        AdviceSimList[[sim]] <- result
      } else {
        AdviceSimList[[sim]]      <- result$AdviceList
        AggBagLimitSimList[[sim]] <- result$AggBagLimit
      }
    } else {
      AdviceSimList[[sim]] <- try(
        .CalcAdviceSimMP(sim = sim,
                          MPName = MPName,
                          MPfunction = MPfunction,
                          DataList = DataList,
                          Year = Year,
                          Proj = Proj,
                          YearsProj = YearsProj,
                          mp = mp,
                          FleetNames = FleetNames,
                          Areas = Areas),
        silent=TRUE
      )
    }
  }

  list(AdviceSimList = AdviceSimList, AggBagLimitSimList = AggBagLimitSimList)
}

#' Calculate MP Advice for a Single Simulation
#'
#' Runs a management procedure (MP) for a single simulation across all stocks/complexes
#' and returns a list of `Advice` objects.
#'
#' @param sim Simulation index (integer) identifying which simulation to run.
#' @param MPName Name of the management procedure (MP).
#' @param MPfunction Function object corresponding to `MPName`.
#' @param DataList List of stock/complex data objects for this simulation.
#' @param Year Numeric year for which advice is calculated.
#' @param Proj Projection object containing OM and control settings.
#' @param YearsProj Numeric vector of projected years (default = `YearsProj` from parent scope).
#'
#' If `Proj@OM@Control$DataOM` is `TRUE` or a named list, population dynamics
#' information from the `Proj` will be included in `Data@Misc` 
#' for each simulation.
#' 
#' @return A named list of `Advice` objects for each stock/complex.
#'
#' @keywords internal
.CalcAdviceSimMP <- function(sim,
                              MPName, 
                              MPfunction, 
                              DataList, 
                              Year, 
                              Proj,
                              YearsProj,
                              mp, 
                              FleetNames, 
                              Areas) {
  
  AdviceList <- MakeNamedList(names(DataList))
  
  # loop over stocks/complexes
  nms <- names(DataList)
  for (i in seq_along(DataList)) {  
    Data <- DataList[[i]] |> .AddPopDyn(Proj, sim, Year, YearsProj, mp)
    
    Data@Misc$MPName <- MPName
    Data@Misc$StockName <- names(DataList)[i]
    Advice <- try(MPfunction(Data=Data), silent=TRUE)
    Advice <- .CheckAdvice(Advice, Proj, FleetNames, Areas, sim, name=nms[i])
    Advice <- .LogMPError(Advice, MPName, Data, Sim=sim, Year)
    AdviceList[[i]] <- Advice
  }
  AdviceList
}

#' Calculate MP Advice for a Single Simulation, `mmp`-Class MPs
#'
#' Runs an `mmp`-class management procedure for a single simulation. Unlike
#' `.CalcAdviceSimMP()`, the MP function is called once, with `Data`
#' objects for every stock/complex passed together as `DataList`, so the MP
#' can coordinate advice across stocks (e.g. an aggregate bag limit).
#'
#' The MP function must return either:
#' - a bare named list of `Advice` objects (one per stock/complex, same
#'   names as `DataList`) - i.e. everything an ordinary MP could express,
#'   just computed jointly; or
#' - `list(Advice = <the same named list>, AggregateBagLimit = <a list of
#'   `AggregateBagLimit()` objects, or NULL>)` when at least one aggregate
#'   bag limit is being declared.
#'
#' @inheritParams .CalcAdviceSimMP
#'
#' @return A list with elements `AdviceList` (a named list of `Advice`
#'   objects, one per stock/complex) and `AggBagLimit` (a list of
#'   `aggbaglimit` objects, or `NULL`).
#'
#' @keywords internal
.CalcAdviceSimMMP <- function(sim,
                               MPName,
                               MPfunction,
                               DataList,
                               Year,
                               Proj,
                               YearsProj,
                               mp,
                               FleetNames,
                               Areas) {

  nms <- names(DataList)

  DataList <- purrr::imap(DataList, \(Data, nm) {
    Data <- Data |> .AddPopDyn(Proj, sim, Year, YearsProj, mp)
    Data@Misc$MPName    <- MPName
    Data@Misc$StockName <- nm
    Data
  })

  result <- MPfunction(DataList = DataList)

  is_bare <- is.list(result) && length(result) &&
    all(vapply(result, inherits, logical(1), what = "advice"))

  if (is_bare) {
    AdviceList  <- result
    AggBagLimit <- NULL
  } else if (is.list(result) && "Advice" %in% names(result)) {
    AdviceList  <- result$Advice
    AggBagLimit <- result$AggregateBagLimit
  } else {
    cli::cli_abort(
      c("`mmp` function {.val {MPName}} returned an unrecognised structure.",
        "i" = "Must return either a named list of `Advice` objects, or ",
        "i" = "`list(Advice = <named list of Advice objects>, AggregateBagLimit = <list, optional>)`."),
      call = NULL
    )
  }

  if (!is.list(AdviceList) || !setequal(names(AdviceList), nms))
    cli::cli_abort(
      "`mmp` function {.val {MPName}} must return `Advice` for exactly the stocks/complexes it was given",
      call = NULL
    )

  AdviceList <- AdviceList[nms]

  AdviceList <- purrr::imap(AdviceList, \(Advice, nm) {
    Advice <- .CheckAdvice(Advice, Proj, FleetNames, Areas, sim, name = nm)
    .LogMPError(Advice, MPName, DataList[[nm]], Sim = sim, Year)
  })

  list(AdviceList = AdviceList, AggBagLimit = AggBagLimit)
}

#' Add Population Dynamics Data to a Data Object
#'
#' Optionally populates `Data@Misc$DataOM` with historical population dynamics
#' from a [Hist()] object, controlled by `OM@Control$DataOM`. Supports adding
#' all slots (`TRUE`), a named subset of slots (named list), or nothing (`NULL`
#' or unrecognised value). Warnings for invalid configuration are shown once
#' only, on the first simulation, year, and MP.
#'
#' @param Data A `Data` S4 object.
#' @param Hist A [Hist()] object containing population dynamics.
#' @param sim Integer. Current simulation index.
#' @param Year Integer or `NULL`. Current projection year. Used to gate
#'   one-time warnings.
#' @param Years Integer vector or `NULL`. All projection years. Used to gate
#'   one-time warnings.
#' @param mp Integer. Current MP index. Used to gate one-time warnings.
#'   Default is `1`.
#'
#' @return The `Data` object, with `Data@Misc$DataOM` populated if
#'   `OM@Control$DataOM` is set, otherwise unchanged.
#' @keywords internal
.AddPopDyn <- function(Data, Hist, sim, Year=NULL, Years=NULL, mp=1) {
  
  if (!length(Hist@OM@Control$DataOM))
    return(Data)
  
  Hist@Data <- list()
  
  warn_once <- !is.null(Year) && sim == 1 && Year == min(Years) && mp == 1
  
  if (isTRUE(Hist@OM@Control$DataOM)) {
    # Add all slots
    Data@Misc$DataOM <- .SubsetSim(Hist, sim)
    
  } else if (is.list(Hist@OM@Control$DataOM)) {
    # Add named subset of slots
    nms <- names(Hist@OM@Control$DataOM)
    Data@Misc$DataOM <- new('hist')
    
    for (nm in nms) {
      if (!nm %in% slotNames('hist')) {
        if (warn_once)
          cli::cli_alert_warning("{.val {nm}} is not a valid slot name for `Hist`. Ignoring.")
      } else {
        slot(Data@Misc$DataOM, nm) <- slot(Hist, nm) |> .SubsetSim(Sims=sim)
      }
    }
    
  } 
  
  Data
}
