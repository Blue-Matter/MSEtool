
#' Apply a Management Procedure if in a Management Year
#'
#' Checks whether `Year` falls in a scheduled management year. If not,
#' the previous advice is carried forward unchanged. If so, the MP is
#' applied via [CalcAdvice()] and a new nested list of `Advice` objects
#' is returned.
#'
#' @param Year              Integer. Current projection year.
#' @param ManagementYears   Integer vector. Years in which the MP is applied,
#'                          as computed by `CalcManagementYears`.
#' @param LastAdviceSimList Nested list of `Advice` objects from the most
#'                          recent management year. Returned unchanged in
#'                          non-management years.
#' @param MPName            Character. Name of the management procedure.
#' @param MPfunction        Function. The MP to apply; must accept a `Data`
#'                          object and return an `Advice` object.
#' @param DataSimList       Nested list of `Data` objects, one per sim and
#'                          stock, trimmed to the current data year.
#' @param Proj              `Hist` object containing the current operating
#'                          model state.
#' @param YearsProj         Integer vector. All projection years.
#' @param mp                Integer. Index of the MP within the `MSE` object.
#' @param FleetNames        Character vector. Fleet names.
#' @param Areas             Integer vector. Area indices.
#'
#' @return A nested list of `Advice` objects — either newly computed by
#'   `CalcAdvice` if `Year` is a management year, or `LastAdviceSimList`
#'   carried forward otherwise.
#'
#' @keywords internal
Apply_MP <- function(Year,
                     ManagementYears,
                     LastAdviceSimList,
                     MPName,
                     MPfunction,
                     DataSimList,
                     Proj,
                     YearsProj,
                     mp,
                     FleetNames,
                     Areas) {
  
  if (!Year %in% ManagementYears) return(LastAdviceSimList)
  
  CalcAdvice(
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
#' @return A list of length `length(DataSimList)`, each element containing
#'   a named list of `Advice` objects for the stocks/complexes.
#'
#' @keywords internal
#' 
CalcAdvice <- function(MPName, MPfunction, DataSimList, Year, Proj, YearsProj, mp,
                       FleetNames, Areas) {
  nSim <- length(DataSimList)
  
  if (nSim != Proj@OM@nSim) 
    cli::cli_abort("Mismatch in number of simulations", .internal=TRUE)
  
  if (inherits(MPfunction,'mmp')) 
    cli::cli_abort("MP class `mmp` currently not supported", call=NULL)
  
  AdviceSimList <- MakeNamedList(1:nSim)
  
  for (sim in seq_along(AdviceSimList)) {
    DataList  <- DataSimList[[sim]]
    AdviceSimList[[sim]] <- try(
      CalcAdvice_Sim_MP(sim = sim, 
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
  AdviceSimList
  
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
CalcAdvice_Sim_MP <- function(sim,
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
    Data <- DataList[[i]] |> AddPopDyn(Proj, sim, Year, YearsProj, mp)
    
    Data@Misc$MPName <- MPName
    Data@Misc$StockName <- names(DataList)[i]
    Advice <- try(MPfunction(Data=Data), silent=TRUE)
    Advice <- CheckAdvice(Advice, Proj, FleetNames, Areas, sim, name=nms[i]) 
    Advice <- Log_MPError(Advice, MPName, Data, Sim=sim, Year)
    AdviceList[[i]] <- Advice
  }
  AdviceList
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
AddPopDyn <- function(Data, Hist, sim, Year=NULL, Years=NULL, mp=1) {
  
  if (!length(Hist@OM@Control$DataOM))
    return(Data)
  
  Hist@Data <- list()
  
  warn_once <- !is.null(Year) && sim == 1 && Year == min(Years) && mp == 1
  
  if (isTRUE(Hist@OM@Control$DataOM)) {
    # Add all slots
    Data@Misc$DataOM <- SubsetSim(Hist, sim)
    
  } else if (is.list(Hist@OM@Control$DataOM)) {
    # Add named subset of slots
    nms <- names(Hist@OM@Control$DataOM)
    Data@Misc$DataOM <- new('hist')
    
    for (nm in nms) {
      if (!nm %in% slotNames('hist')) {
        if (warn_once)
          cli::cli_alert_warning("{.val {nm}} is not a valid slot name for `Hist`. Ignoring.")
      } else {
        slot(Data@Misc$DataOM, nm) <- slot(Hist, nm) |> SubsetSim(Sims=sim)
      }
    }
    
  } 
  
  Data
}

