Update_TAC <- function(Proj,
                       Year, 
                       AdviceSimList, 
                       LastAdviceSimList, 
                       YearsHist, 
                       YearsProj, 
                       Areas, 
                       FleetNames,
                       StockNames) {
  
  nSim <- Proj@OM@nSim
  nStock <- nStock(Proj)
  nFleet <- length(FleetNames)
  nArea <- length(Areas)
  TSIndex <- match(Year, c(YearsHist, YearsProj))
  
  for (sim in seq_len(nSim)) {
   
    AdviceList <- AdviceSimList[[sim]]
    LastAdviceList <- LastAdviceSimList[[sim]]
    Complexes <- Proj@OM@Complexes
    ComplexNames <- names(Proj@OM@Complexes)
    
    Proj <- Update_TAC_Sim(Proj,
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
    
    
  }
  
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
  
  # Initialize matrix to hold effort required per fleet per complex
  RequiredEffort <- matrix(NA, nrow=nFleet, ncol=nComplex, 
                           dimnames = list(
                             Fleet=FleetNames,
                             Complex=ComplexNames
                           ))
  
  # Calculate fleet-specific effort needed to catch TAC
  for (i in seq_len(nComplex)) {
    
    stocks <- Complexes[[i]]
    Advice <- AdviceList[[i]]
    AdvicePrevious <- LastAdviceList[[i]]
    
    # If no TAC for this complex, try to take previous advice
    if (EmptyObject(Advice@TAC)) {
      if (!is.null(AdvicePrevious) && !EmptyObject(AdvicePrevious@TAC)) {
        Advice@TAC <- AdvicePrevious@TAC
      } else {
        next()
      }
    }
    
    TAC <- Advice@TAC
    TACType <- Advice@TACType
    AreaSpecific <- FALSE
    
    # TAC options:
    # - numeric length 1 - global TAC to be allocated across fleets
    # - numeric length nFleet - TAC by Fleet
    # - numeric matrix nFleet x nArea - fleet/area-specific TAC
    
    
    dd <- dim(TAC)
    
    if (length(dd)==1) {
      # TAC is either a single value (global) or vector by fleet
      
      if (length(TAC)==1) {
        # Global TAC - distribute over Fleets according to Allocation 
        if (nFleet>1) {
          allocation <- Proj@OM@Allocation[[i]]
          if (is.null(allocation))
            stop("Proj@OM@Allocation is NULL")
          all_sim <- min(nrow(allocation), sim)
          TAC <- as.numeric(TAC) * OM@Allocation[[i]][all_sim, ]  
        }
      } 
      if (length(TAC) == nFleet) {
        # Fleet-specific TAC
        TAC_by_Fleet <- TAC
        RequiredEffort[,i] <- OptEffort(Proj, Year, TSIndex, sim, stocks, TAC_by_Fleet, TACType)
      } else {
        stop("Advice@TAC must be length 1 or length `nFleet`")
      }
      
    } else {
      # TAC is Fleet × Area
      AreaSpecific <- TRUE
      dd <- dim(TAC)
      if (all(dd != c(nFleet, nArea)))
        stop("Advice@TAC must be numeric length 1 or length `nFleet` or a nFleet x nArea matrix")
      
      
      
      
      # TODO area-specific effort optimization
      stop("TAC by Area not done")
      
    }
    
  } # end complex loop
  
  # Apply minimum effort constraint per fleet 
  FleetEffort <- apply(RequiredEffort, 1, function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_real_)
    min(x)
  })
  
  for (fl in seq_len(nFleet)) {
    if (!is.na(Proj@Effort[sim,TSIndex,fl])) {
      FleetEffort[fl] <- min(FleetEffort[fl], Proj@Effort[sim,TSIndex,fl])
    }
  }
  Proj@Effort[sim,TSIndex,] <- FleetEffort
  
  
  if (nComplex == 1) 
    return(Proj)
  
  # Multi-stock complex dynamics
  ProjCopy <- Proj
  Temp <- CalcFisheryDynamics(Hist = ProjCopy, Years = Year, Sims = sim)
  
  ChokeConstraintMatrix <- matrix(NA, nrow=nFleet, ncol=nComplex, 
                                  dimnames = list(
                                    Fleet=FleetNames,
                                    Complex=ComplexNames
                                  ))
  
  for (i in seq_len(nComplex)) {
    Advice <- AdviceList[[i]]
    stocks <- Complexes[[i]]
    
    Landings <- Temp@Landings[sim,stocks,TSIndex,, drop=FALSE] |>
      abind::adrop(c(1,3)) |> SumOverStock()
    Discards <- Temp@Discards[sim,stocks,TSIndex,, drop=FALSE] |>
      abind::adrop(c(1,3)) |> SumOverStock()
    
    Removals <- Landings +  Discards
    nonzero <- which(Removals>0)
    ChokeConstraintMatrix[,i] <- Advice@TAC[nonzero]/Removals[nonzero]
  }
  
  ChokeConstraintInd <- apply(ChokeConstraintMatrix, 1, which.min) |> as.numeric()
  row_idx <- seq_len(nrow(ChokeConstraintMatrix))
  ChokeConstraint <- ChokeConstraintMatrix[cbind(row_idx, ChokeConstraintInd)] 
  
  # Scale Effort based on avoidance
  for (fl in 1:nFleet) {
    avoidance <- 1 # Proj@OM@Fleet[[1]][[fl]]@Avoidance
    EffortScaling <- (1 - avoidance) * ChokeConstraint[fl] + avoidance
    ProjCopy@Effort[sim, TSIndex, fl] <- ProjCopy@Effort[sim, TSIndex, fl] * EffortScaling
  }
  
  
  # Recalculate fishing mortality with scaled effort
  Temp_scaled <- CalcFisheryDynamics(Hist = ProjCopy, Years = Year, Sims = sim)
  
  # Apply TAC fractions to FRetainArea and FDeadArea
  for (i in seq_len(nComplex)) {
    Advice <- AdviceList[[i]]
    stocks <- Complexes[[i]]
    
    fleet_only_TAC <- length(dim(Advice@TAC)) == 1 || is.null(dim(Advice@TAC))
    
    for (st in stocks) {
      nAge <- nAge(Proj@OM@Stock[[st]])
      for (a in 1:nAge) {
        for (fl in 1:nFleet) {
          dexterity <- 1 # Proj@OM@Fleet[[1]][[fl]]@Dexterity
          for (ar in 1:nArea) {
            if (fleet_only_TAC) {
              # Fleet-only TAC: same TAC fraction for all areas
              TAC_total <- Advice@TAC[fl]
              total_removal <- Temp@Landings[sim,stocks,TSIndex,fl] + Temp@Discards[sim,stocks,TSIndex,fl]
            } else {
              # Fleet × Area TAC
              TAC_total <- Advice@TAC[fl, ar]
              stop('TAC by Fleet x Area not done')
              
              # calc landings + discards by area in biomass
              total_removal <- Temp_scaled@Landings[[st]][sim,a,TSIndex,fl,ar] +
                Temp_scaled@Discards[[st]][sim,a,TSIndex,fl,ar]
            }
            
            tac_fraction <- if (total_removal > 0) min(TAC_total / total_removal, 1) else 1
            frac_retain <- tac_fraction
            
            frac_discard <- (1 - tac_fraction) * dexterity
            
            # Scale retained F
            ProjCopy@FRetainArea[[st]][sim,a,TSIndex,fl,ar] <- 
              Temp_scaled@FRetainArea[[st]][sim,a,TSIndex,fl,ar] * frac_retain
            
            # Add excess discard mortality to existing FDeadArea
            excess_dead <- frac_discard * Proj@Misc$DiscMortList[[st]][sim,a,TSIndex,fl,ar]
            ProjCopy@FDeadArea[[st]][sim,a,TSIndex,fl,ar] <- 
              Temp_scaled@FDeadArea[[st]][sim,a,TSIndex,fl,ar] + excess_dead
            
          } # end area
        } # end fleet
      } # end age
    } # end stocks
  } # end complex
  

  # TODO - calculate apical F directly rather than run full CalcFisheryDynamics
  Proj <- CalcFisheryDynamics(Hist = ProjCopy, Years = Year, Sims = sim)
  
  # TODO - overall effort calcs aren't correct - review above code 
  # back calculate actual effort
  Proj@Effort[sim,TSIndex,] <- apply(Proj@FInteract[sim, , TSIndex,]/Proj@Misc$Catchability[sim,,TSIndex,], 2, max)
  
  Proj
}




