Update_DiscardMortality <- function(Proj, Year, 
                                    AdviceSimList, 
                                    LastAdviceSimList, 
                                    YearsProj, Areas, FleetNames) {
  
  nSim <- Proj@OM@nSim
  nStock <- nStock(Proj)
  nFleet <- length(FleetNames)
  nArea <- length(Areas)
  FutureYears <- YearsProj[YearsProj>=Year]
  
  # Expand Arrays
  for (st in 1:nStock) {
    for (fl in 1:nFleet) {
      Proj@OM@Fleet[[st]][[fl]]@DiscardMortality@MeanAtAge <-  Proj@OM@Fleet[[st]][[fl]]@DiscardMortality@MeanAtAge |> 
        Extend(nSim=nSim, NULL, FutureYears, Areas)
      
      Proj@OM@Fleet[[st]][[fl]]@DiscardMortality@MeanAtLength <-Proj@OM@Fleet[[st]][[fl]]@DiscardMortality@MeanAtLength |> 
        Extend(nSim=nSim, NULL, FutureYears, Areas)
    }
  }
  
  for (sim in seq_len(nSim)) {
    Proj <- Update_DiscardMortality_Sim(
      Proj = Proj,
      sim = sim,
      FutureYears = FutureYears,
      AdviceList = AdviceSimList[[sim]],
      LastAdviceList = LastAdviceSimList[[sim]],
      nFleet = nFleet,
      Complexes = Proj@OM@Complexes,
      nArea = nArea,
      nSim = nSim
    )
  }
  
  Proj
}


Update_DiscardMortality_Sim <- function(Proj,
                                        sim, 
                                        FutureYears = FutureYears,
                                        AdviceList = AdviceSimList[[sim]],
                                        LastAdviceList = LastAdviceSimList[[sim]],
                                        nFleet = nFleet,
                                        Complexes = Proj@OM@Complexes,
                                        nArea = nArea,
                                        nSim = nSim) {
  
  nComplex <- length(AdviceList)
  
  for (i in seq_len(nComplex)) {
  
    stocks <- Complexes[[i]]
    Advice <- AdviceList[[i]]
    AdvicePrevious <- LastAdviceList[[i]]
    if (UnchangedManagement(Current=Advice, Previous=AdvicePrevious, slotName='DiscardMortality'))
      next()
    
    DiscardMortalityList <- Advice@DiscardMortality
    
    if (length(DiscardMortalityList)>1 && length(DiscardMortalityList)!=nFleet) {
      stop(
        "Advice@DiscardMortality must be an `Advice()` object or a list of `Advice()` objects length `nFleet`"
      )
    }
    
    
    for (st in stocks) {
      Ages <- Proj@OM@Stock[[st]]@Ages
      Length <- Proj@OM@Stock[[st]]@Length |> SubsetSim(sim) |>
        SubsetYear(FutureYears)
      
      for (fl in seq_along(FleetNames)) {
        if (is.list(DiscardMortalityList)) {
          DiscardMortality <- DiscardMortalityList[[fl]]
        } else {
          DiscardMortality <- DiscardMortalityList
        }
        
        DiscardMortality <- PopulateDiscardMortality(DiscardMortality,
                                                     Ages = Ages,
                                                     Length = Length,
                                                     nSim = 1,
                                                     Years = FutureYears,
                                                     nArea = nArea,
                                                     silent = TRUE)
        
        DiscardMortality@MeanAtAge <- set_sim_dimname(DiscardMortality@MeanAtAge, sim) |> 
          ExtendAreas(1:nArea)  |>
          ExtendYears(FutureYears)
        
        DiscardMortality@MeanAtLength <- set_sim_dimname(DiscardMortality@MeanAtLength, sim) |> 
          ExtendAreas(1:nArea) |>
          ExtendYears(FutureYears)
        
        ArrayFill(DiscardMortality@MeanAtAge) <- DiscardMortality@MeanAtAge
        ArrayFill(DiscardMortality@MeanAtLength) <- DiscardMortality@MeanAtLength
  
        # Misc for C++ 
        ArrayFill(Proj@OM@Fleet[[st]][[fl]]@DiscardMortality@MeanAtAge) <- DiscardMortality@MeanAtAge
     
      } # end fleet loop
    }  # end stock loop
  } # end complex loop
  
  Proj
}