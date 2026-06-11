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
  nArea       <- length(Areas)
  FutureYears <- YearsProj[YearsProj >= Year]
  
  if (AllAdviceNull(AdviceSimList, 'DiscardMortality'))
    return(Proj)
  
  # Expand DiscardMortality arrays to cover future projection years
  for (st in seq_len(nStock)) {
    for (fl in seq_len(nFleet)) {
      target <- Proj@OM@Fleet[[st]][[fl]]@DiscardMortality
      target@MeanAtAge    <- Extend(target@MeanAtAge,    nSim = nSim, 
                                    Years = FutureYears, Areas = Areas)
      target@MeanAtLength <- Extend(target@MeanAtLength, nSim = nSim, 
                                    Years = FutureYears, Areas = Areas)
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
      nArea          = nArea,
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
                                        nArea,
                                        FleetNames) {
  
  Complexes <- Proj@OM@Complexes
  
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
      Stock    <- Proj@OM@Stock[[st]]
      Ages     <- Stock@Ages
      Length   <- Subset(Stock@Length,   Sims = sim, Years = FutureYears)
      Weight   <- Subset(Stock@Weight,   Sims = sim, Years = FutureYears)
      Maturity <- Subset(Stock@Maturity, Sims = sim, Years = FutureYears)
  
      ReComputeALKList <- vector('list', length(DiscardMortalityList))
      
      if (is.list(DiscardMortalityList)) {
        ClassesList <- purrr::map(DiscardMortalityList, slot, 'Classes') 
      } else {
        ClassesList <- list(DiscardMortalityList@Classes)
      }
      
      for (fl in seq_along(ClassesList)) {
        if (is.null(ClassesList[[fl]])) 
          ClassesList[[fl]]  <- Length@Classes
        
        ReComputeALKList[[fl]] <- !setequal(Length@Classes, ClassesList[[fl]])
      }
      
      all_same <- all(sapply(ReComputeALKList[-1], identical, ReComputeALKList[[1]]))
      if (all_same) ReComputeALKList <- list(ReComputeALKList[[1]])
      
      all_same <- all(sapply(ClassesList[-1], identical, ClassesList[[1]])) &&
        all(sapply(ClassesList, setequal, Length@Classes))
      
      if (all_same) ClassesList <- list(ClassesList[[1]])
      
      LinIntAge <- length(Ages@Classes) < 50

      for (fl in seq_len(nFleet)) {
        dm <- if (is.list(DiscardMortalityList)) DiscardMortalityList[[fl]] else DiscardMortalityList
        
        FleetLength  <- Length 
        Classes      <- ClassesList[[min(length(ClassesList), fl)]]
        ReComputeALK <- ReComputeALKList[[min(length(ReComputeALKList), fl)]]
        if (fl == 1) {
          # Get ALK using Length@Classes
          if (LinIntAge) {
            # Increases the temporal resolution of `ObjectMeanAtAge` and `ASK`
            # by linear interpolate Mean length-at-age and CV length-at-age
            ALK_1 <- CalcAgeSizeKey(MeanAtAge=LinearInterpolate_Age(Length@MeanAtAge),
                                    CVatAge=LinearInterpolate_Age(Length@CVatAge),
                                    Classes=Length@Classes,
                                    TruncSD=Length@TruncSD,
                                    Dist=Length@Dist,
                                    silent=TRUE)
          } else {
            ALK_1 <- Length@ALK
          }
        }
        
        if (ReComputeALK) {
          if (LinIntAge) {
            ALK <- CalcAgeSizeKey(MeanAtAge=LinearInterpolate_Age(Length@MeanAtAge),
                                  CVatAge=LinearInterpolate_Age(Length@CVatAge),
                                  Classes=Classes,
                                  TruncSD=Length@TruncSD,
                                  Dist=Length@Dist,
                                  silent=TRUE)
          } else {
            ALK <- CalcAgeSizeKey(MeanAtAge=Length@MeanAtAge,
                                  CVatAge=Length@CVatAge,
                                  Classes=Classes,
                                  TruncSD=Length@TruncSD,
                                  Dist=Length@Dist,
                                  silent=TRUE)
          }
          FleetLength@ALK <- CalcAgeSizeKey(MeanAtAge = Length@MeanAtAge,
                                            CVatAge   = Length@CVatAge,
                                            Classes   = Classes,
                                            TruncSD   = Length@TruncSD,
                                            Dist      = Length@Dist,
                                            silent    = TRUE)
        } else {
          ALK <- ALK_1
        }
        
        FleetLength@Classes <- Classes
        
        dm <- ProcessSelectMeanAtAge(dm, Ages, nArea,
                                     type = 'DiscardMortality', 
                                     Year = FutureYears[1])
        
        dm <- ProcessSelectMeanAtLength(dm, FleetLength, nArea, 
                                        type = 'DiscardMortality', 
                                        Year = FutureYears[1])
        
        dm <- PopulateDiscardMortality(dm,
                                       Ages  = Ages,
                                       Length = FleetLength,
                                       nSim  = 1,
                                       Years = FutureYears,
                                       nArea = nArea,
                                       silent = TRUE,
                                       replace = TRUE,
                                       ASKOverride = ALK)
        
        dm@MeanAtAge    <- set_sim_dimname(dm@MeanAtAge,    sim) |> ExtendAreas(1:nArea) |> ExtendYears(FutureYears)
        dm@MeanAtLength <- set_sim_dimname(dm@MeanAtLength, sim) |> ExtendAreas(1:nArea) |> ExtendYears(FutureYears)
        
        target <- Proj@OM@Fleet[[st]][[fl]]@DiscardMortality
        ArrayFill(target@MeanAtAge)    <- dm@MeanAtAge
        ArrayFill(target@MeanAtLength) <- dm@MeanAtLength
        Proj@OM@Fleet[[st]][[fl]]@DiscardMortality <- target
        
        ArrayFill(Proj@Misc$DiscMortList[[st]]) <- AddDimension(dm@MeanAtAge,
                                                                'Fleet', 
                                                                val=FleetNames[fl],
                                                                pos=4)
        
        ArrayFill(Proj@Misc$DiscMortSizeList[[st]][[fl]]) <- dm@MeanAtLength
        
        
      } # end fleet loop
    }   # end stock loop
  }     # end complex loop
  
  Proj
}