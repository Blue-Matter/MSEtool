Update_TAC <- function(Proj, Year, 
                       AdviceSimList, 
                       LastAdviceSimList, 
                       YearsHist, 
                       YearsProj, 
                       Areas, 
                       FleetNames) {
  
  nSim <- Proj@OM@nSim
  nStock <- nStock(Proj)
  nFleet <- length(FleetNames)
  nArea <- length(Areas)
  TSIndex <- match(Year, c(YearsHist, YearsProj))
  
  AdviceList <- AdviceSimList[[sim]]
  LastAdviceList <- LastAdviceSimList[[sim]]
  Complexes <- Proj@OM@Complexes
  ComplexNames <- names(Proj@OM@Complexes)
  
  Update_TAC_Sim (Proj,
                  sim,
                  Year, 
                  TSIndex,
                  AdviceList,
                  LastAdviceList,
                  nFleet,
                  FleetNames,
                  Complexes,
                  ComplexNames,
                  nArea) 
  
  Proj
}



Update_TAC_Sim <- function(Proj,
                           sim,
                           Year, 
                           TSIndex,
                           AdviceList,
                           LastAdviceList,
                           nFleet,
                           FleetNames,
                           Complexes,
                           ComplexNames,
                           nArea) {
  
  nComplex <- length(Complexes)
  
  # NOTE: TAC applies to Removals
  # TODO: if there are illegal catches in implementation error model, these
  #       should not contribute to the TAC
  #       should add overages vs IUU to imp model
  
  # Calculate the effort required to catch the specified TAC
  # constrain for each fleet by the minimum effort before copmlex-specific TAC
  # is met
  
  ReqEffortMatrix <- matrix(NA, nrow=nFleet, ncol=nComplex, 
                            dimnames = list(
                              Fleet=FleetNames,
                              Complex=ComplexNames
                            ))
  
  for (i in seq_len(nComplex)) {
    
    stocks <- Complexes[[i]]
    Advice <- AdviceList[[i]]
    AdvicePrevious <- LastAdviceList[[i]]
    
    if (EmptyObject(Advice@TAC)) {
      if (!is.null(AdvicePrevious) && !EmptyObject(AdvicePrevious@TAC)) {
        Advice@TAC <- AdvicePrevious@TAC
      } else {
        next()
      }
    }
    
    TAC <- Advice@TAC
    
    # TAC options:
    # - numeric length 1 - global TAC to be allocated across fleets
    # - numeric length nFleet - TAC by Fleet
    # - numeric matrix nFleet x nArea - fleet/area-specific TAC
    
    # TODO currently the most restrictive TAC limits fleet-specific Effort
    #  additions to include later:
    #  - add maximum discard rate to include discards of stocks with limiting TACS
    #    ie constrain effort to most restrictive TAC x (1+ max discard rate)
    #  - account for implementation error to track 'illegal' catch overages (ie kept fish > TAC)
    
    dd <- dim(TAC)
    
    if (length(dd)==1) {
      # not by area
      
      if (length(TAC)==1) {
        # global TAC - distribute over Fleets according to Allocation 
        allocation <- OM@Allocation[[i]]
        all_sim <- min(nrow(allocation), sim)
        TAC <- as.numeric(TAC) * OM@Allocation[[i]][all_sim, ]
      } 
      if (length(TAC) == nFleet) {
        
        TAC_by_Fleet <- TAC
        # TAC by Fleet 
        
        # Calculate the effort required to catch the TAC for this complex
        ReqEffortMatrix[,i] <- OptEffort(Proj, Year, TSIndex, sim, stocks, TAC_by_Fleet)
  
    
        ## C++ version - not currently working 
        # tictoc::tic()
        # ReqEffort2 <- OptimizeEffort(Proj, sim, TSIndex,
        #                              TAC_by_Fleet,
        #                              Years=Year,
        #                              AllYears,
        #                              stocks, 
        #                              nSim, nStock, nArea)
        # tictoc::toc()
        
        
        
      } else {
        stop("Advice@TAC must be length 1 or length `nFleet`")
      }
       
      
      
    } else {
      # Area-specific TAC 
      dd <- dim(TAC)
      if (dd != c(nFleet, nArea))
        stop("Advice@TAC must be numeric length 1 or length `nFleet` or a nFleet x nArea matrix")
      
      stop("TAC by Area not done")
      return()
    }
     
  } # end complex loop
    
  
  if (nComplex > 1) {
    # Calculate minimum effort 
    MinEffortInd <- apply(ReqEffortMatrix, 1, which.min) |> as.numeric() # complex with lowest effort
    
    # apply minumum effort constraint for first 
  }
  
  
  stop() 

    
  
  
  
  Proj
}

