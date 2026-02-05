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
#' @param MSE Management Strategy Evaluation object containing MPs and OM.
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
  if (nSim != MSE@OM@nSim) 
    cli::cli_abort("Mismatch in number of simulations", .internal=TRUE)
  
  if (inherits(MPfunction,'mmp')) 
    cli::cli_abort("MP class `mmp` currently not supported", call=NULL)
  

  AdviceSimList <- MakeNamedList(1:nSim)
  
  for (x in seq_along(AdviceSimList)) {
    AdviceSimList[[x]] <- try(
      CalcAdvice_Sim_MP(x = x, 
                        MPName = MPName, 
                        MPfunction = MPfunction, 
                        DataList = DataSimList[[x]],
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
#' @param x Simulation index (integer) identifying which simulation to run.
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
CalcAdvice_Sim_MP <- function(x, MPName, 
                              MPfunction, DataList, 
                              Year, Proj,
                              YearsProj,
                              mp, FleetNames, 
                              Areas) {
  
  MPAdviceList <- MakeNamedList(names(DataList))
  
  # loop over stocks/complexes
  for (i in seq_along(DataList)) {  
    Data <- DataList[[i]] |> AddPopDyn(Proj, Year, x, mp)
    
    MPAdvice <- try(MPfunction(Data=Data), silent=TRUE)
    
    Log_MPError(MPAdvice, MPName, Data, Sim=x, Year)
    
    MPAdvice <- CheckAdvice(MPAdvice, Proj, FleetNames, Areas, x) 
    MPAdviceList[[i]] <- MPAdvice
  }
  MPAdviceList
}


AddPopDyn <- function(Data, Proj, Year, x, mp, YearsProj) {
  if (!length(Proj@OM@Control$DataOM)) 
    return(Data)
  
  Proj@Data <- list()
  
  if (is.logical(Proj@OM@Control$DataOM) && Proj@OM@Control$DataOM) {
    # add everything
    Data@Misc$DataOM <- SubsetSim(Proj, x)
    
  } else if (is.list(Proj@OM@Control$DataOM)) {
    nms <- names(Proj@OM@Control$DataOM)
    Data@Misc$DataOM <- new('hist')
    for (nm in nms) {
      if (!nm %in% slotNames('hist') && x==1 && Year == min(YearsProj) && mp==1) {
        cli::cli_alert_warning("{.val {nm}} not a valid slot name for `Hist` object. Ignoring")
      } else {
        slot(Data@Misc$DataOM, nm) <- slot(Proj,nm) |> SubsetSim(Sims=x)
      }
    }
  } else {
    if (x==1 && Year == min(YearsProj) && mp==1){
      cli::cli_alert_warning('`Proj@OM@Control$DataOM` must be either TRUE or a named list')
    }
      
  }
  Data
}
