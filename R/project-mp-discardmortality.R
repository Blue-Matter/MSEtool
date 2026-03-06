#' Update discard mortality across all simulations
#'
#' Expands discard mortality arrays to cover future projection years, then
#' delegates per-simulation updates to [Update_DiscardMortality_Sim()].
#'
#' @param Proj A `Proj` object.
#' @param Year Integer. Current projection year.
#' @param AdviceSimList Nested list of `advice` objects, indexed by sim then complex.
#' @param LastAdviceSimList Same structure as `AdviceSimList` for the previous year.
#' @param YearsHist Integer vector of historical years (unused here, kept for
#'   consistent `update_funs` signature).
#' @param YearsProj Integer vector of projection years.
#' @param Areas Integer vector of area indices.
#' @param FleetNames Character vector of fleet names.
#' @param StockNames Character vector of stock names (unused here, kept for
#'   consistent `update_funs` signature).
#' @return Updated `Proj` object.
#' @keywords internal
Update_DiscardMortality <- function(Proj, 
                                    Year, 
                                    AdviceSimList, 
                                    LastAdviceSimList, 
                                    YearsHist,
                                    YearsProj,
                                    Areas, 
                                    FleetNames,
                                    StockNames) {
  
  nSim        <- Proj@OM@nSim
  nStock      <- nStock(Proj)
  nFleet      <- length(FleetNames)
  FutureYears <- YearsProj[YearsProj >= Year]
  
  if (AllAdviceNull(AdviceSimList, 'DiscardMortality'))
    return(Proj)
  
  # Expand DiscardMortality arrays to cover future projection years
  for (st in seq_len(nStock)) {
    for (fl in seq_len(nFleet)) {
      target <- Proj@OM@Fleet[[st]][[fl]]@DiscardMortality
      target@MeanAtAge    <- Extend(target@MeanAtAge,    nSim = nSim, NULL, FutureYears, Areas)
      target@MeanAtLength <- Extend(target@MeanAtLength, nSim = nSim, NULL, FutureYears, Areas)
      Proj@OM@Fleet[[st]][[fl]]@DiscardMortality <- target
    }
  }
  
  for (sim in seq_len(nSim)) {
    
    AdviceList <- AdviceSimList[[sim]]
    LastAdviceList <- LastAdviceSimList[[sim]]
    
    Proj <- Update_DiscardMortality_Sim(
      Proj           = Proj,
      sim            = sim,
      FutureYears    = FutureYears,
      AdviceList     = AdviceList,
      LastAdviceList = LastAdviceList,
      nFleet         = nFleet,
      Complexes      = Proj@OM@Complexes,
      nArea          = length(Areas),
      FleetNames     = FleetNames
    )
  }
  
  Proj
}

#' Update discard mortality for a single simulation
#'
#' Populates discard mortality arrays from advice for all future projection
#' years, updating `Proj@OM@Fleet`. Skips a complex when management is
#' unchanged.
#'
#' @param Proj A `Proj` object.
#' @param sim Integer. Simulation index.
#' @param FutureYears Integer vector of years from current year to end of projection.
#' @param AdviceList List of `advice` objects for this simulation, one per complex.
#' @param LastAdviceList Same structure as `AdviceList` for the previous year.
#' @param nFleet Integer. Number of fleets.
#' @param Complexes List mapping complex indices to stock indices.
#' @param nArea Integer. Number of areas.
#' @param FleetNames Character vector of fleet names.
#' @return Updated `Proj` object.
#' @keywords internal
Update_DiscardMortality_Sim <- function(Proj,
                                        sim, 
                                        FutureYears,
                                        AdviceList,
                                        LastAdviceList,
                                        nFleet,
                                        Complexes,
                                        nArea,
                                        FleetNames) {
  
  for (i in seq_along(AdviceList)) {
    stocks         <- Complexes[[i]]
    Advice         <- AdviceList[[i]]
    AdvicePrevious <- LastAdviceList[[i]]
    
    if (!inherits(Advice, 'advice')) next    
    if (is.null(Advice@DiscardMortality)) next
    if (UnchangedManagement(Advice, AdvicePrevious, 'DiscardMortality')) next
    
    DiscardMortalityList <- Advice@DiscardMortality
    
    if (length(DiscardMortalityList) > 1 && length(DiscardMortalityList) != nFleet)
      stop("Advice@DiscardMortality must be a `DiscardMortality()` object or a list of ",
           "`DiscardMortality()` objects of length nFleet (", nFleet, ")")
    
    for (st in stocks) {
      Ages   <- Proj@OM@Stock[[st]]@Ages
      Length <- Proj@OM@Stock[[st]]@Length |> SubsetSim(sim) |> SubsetYear(FutureYears)
      
      for (fl in seq_len(nFleet)) {
        dm <- if (is.list(DiscardMortalityList)) DiscardMortalityList[[fl]] else DiscardMortalityList
        
        dm <- PopulateDiscardMortality(dm,
                                       Ages  = Ages,
                                       Length = Length,
                                       nSim  = 1,
                                       Years = FutureYears,
                                       nArea = nArea,
                                       silent = TRUE)
        
        dm@MeanAtAge    <- set_sim_dimname(dm@MeanAtAge,    sim) |> ExtendAreas(1:nArea) |> ExtendYears(FutureYears)
        dm@MeanAtLength <- set_sim_dimname(dm@MeanAtLength, sim) |> ExtendAreas(1:nArea) |> ExtendYears(FutureYears)
        
        target <- Proj@OM@Fleet[[st]][[fl]]@DiscardMortality
        ArrayFill(target@MeanAtAge)    <- dm@MeanAtAge
        ArrayFill(target@MeanAtLength) <- dm@MeanAtLength
        Proj@OM@Fleet[[st]][[fl]]@DiscardMortality <- target
        
      } # end fleet loop
    }   # end stock loop
  }     # end complex loop
  
  Proj
}