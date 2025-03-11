CalcInitialTimeStep <- function(OMList, Unfished) {
  
  nStock <- length(OMList$NumberAtAgeArea)
  
  for (st in 1:nStock) {
  
    RecDevInit <- OMList$SRR$RecDevInit[[st]]
    RecDevHist <- OMList$SRR$RecDevs[[st]] 
  
    InitAgeClassRecDevs <- abind::abind(RecDevHist[,1,drop=FALSE],
                                        RecDevInit, along=2,
                                        use.dnns = TRUE,
                                        use.first.dimnames = FALSE)
    ages <- dimnames(InitAgeClassRecDevs)$Age |> as.numeric()
    ages[1] <- ages[2]-1
    dimnames(InitAgeClassRecDevs)$Age <- ages
    
    N0atAge <- Unfished@Equilibrium@Number[[st]][,,1,, drop=FALSE]
    
    InitAgeClassRecDevs <- InitAgeClassRecDevs |> 
      AddDimension('Time Step') |> 
      AddDimension('Area')
    
    NatAgeInitial <- ArrayMultiply(N0atAge, InitAgeClassRecDevs)
    
    if (length(OMList$Depletion$Initial[[st]]) > 0){
      cli::cli_abort('Initial depletion not done', .internal=TRUE)
      # NatAgeInitial update for initial depletion
    }
    
    OMList$NumberAtAgeArea[[st]][,,1,] <- NatAgeInitial
    
  }
  OMList
}





CalcPopDynamics <- function(OMListSim, TimeSteps=NULL, MP=NULL) {
  
  nTS <- length(TimeSteps)
  
  for (ts in seq_along(TimeSteps)) {
    TSindex <- ts - 1
    
    # Biomass by Area beginning of this time step
    OMListSim$BiomassArea = CalcBiomass_(
      OMListSim$BiomassArea,
      OMListSim$NumberAtAgeArea,
      OMListSim$Weight$MeanAtAge,
      TSindex
    )
    
    # VB by Area
    OMListSim$VBiomassArea = CalcVBiomass_(
      OMListSim$VBiomassArea,
      OMListSim$NumberAtAgeArea,
      OMListSim$FleetWeightAtAge,
      OMListSim$Selectivity$MeanAtAge,
      OMListSim$Distribution$Closure,
      TSindex
    )
    
    # Relative VB Density by Area & Fleet
    OMListSim$DensityArea = CalcDensity_(
      OMListSim$DensityArea,
      OMListSim$VBiomassArea,
      OMListSim$Spatial$RelativeSize,
      TSindex
    )
    
    # Distribute Effort over Areas (currently proportional to VB Density)
    OMListSim$EffortArea = DistEffort_(
      OMListSim$EffortArea,
      OMListSim$DensityArea,
      OMListSim$Effort$Effort,
      TSindex
    )
    
    # Calculate F within each Area
    List <- CalcFArea_(
      OMListSim$FDeadAtAgeArea,
      OMListSim$FRetainAtAgeArea,
      OMListSim$EffortArea,
      OMListSim$DensityArea,
      OMListSim$Effort$Catchability,
      OMListSim$Selectivity$MeanAtAge,
      OMListSim$Retention$MeanAtAge,
      OMListSim$DiscardMortality$MeanAtAge,
      TSindex)
    
    OMListSim$FDeadAtAgeArea <- List$FDeadAtAgeArea
    OMListSim$FRetainAtAgeArea <- List$FRetainAtAgeArea
    
    # Removals and Retained Number and Biomass by Area
    List <- CalcCatch_(
      OMListSim$RemovalAtAgeArea,
      OMListSim$RetainAtAgeArea,
      OMListSim$RemovalNumberAtAge,
      OMListSim$RetainNumberAtAge,
      OMListSim$RemovalBiomassAtAge,
      OMListSim$RetainBiomassAtAge,
      OMListSim$NaturalMortality$MeanAtAge,
      OMListSim$FleetWeightAtAge,
      OMListSim$NumberAtAgeArea,
      OMListSim$FDeadAtAgeArea,
      OMListSim$FRetainAtAgeArea,
      TSindex)
  
    OMListSim$RemovalAtAgeArea <- List$RemovalAtAgeArea
    OMListSim$RetainAtAgeArea <- List$RetainAtAgeArea
    OMListSim$RemovalNumberAtAge <- List$RemovalNumberAtAge
    OMListSim$RetainNumberAtAge <- List$RetainNumberAtAge
    OMListSim$RemovalBiomassAtAge <- List$RemovalBiomassAtAge
    OMListSim$RetainBiomassAtAge <- List$RetainBiomassAtAge
    
    List <- CalcFfromCatch_(
      OMListSim$FDeadAtAge,
      OMListSim$FRetainAtAge,
      OMListSim$NumberAtAgeArea,
      OMListSim$RemovalNumberAtAge,
      OMListSim$NaturalMortality$MeanAtAge,
      OMListSim$Selectivity$MeanAtAge,
      OMListSim$Retention$MeanAtAge,
      OMListSim$DiscardMortality$MeanAtAge,
      OMListSim$FDeadAtAgeArea,
      OMListSim$FRetainAtAgeArea,
      TSindex
    )
    
    OMListSim$FDeadAtAge <- List$FDeadAtAge
    OMListSim$FRetainAtAge <- List$FRetainAtAge
    
    # Calc Spawning Production
    OMListSim$SProduction = CalcSpawnProduction_(
      OMListSim$SProduction,
      OMListSim$NumberAtAgeArea,
      OMListSim$NaturalMortality$MeanAtAge,
      OMListSim$Fecundity$MeanAtAge,
      OMListSim$SRR$SpawnTimeFrac,
      OMListSim$SRR$SPFrom,
      OMListSim$FDeadAtAge,
      TSindex
    )
    
    # Calc Recruitment 
    Recruits <- CalcRecruitment_(
      OMListSim$SProduction,
      OMListSim$SP0,
      OMListSim$SRR$R0,
      OMListSim$SRR$RecDevs,
      OMListSim$SRR$SRRModel,
      OMListSim$SRR$SRRPars,
      TSindex
    )
    
    # Add Recruits
    OMListSim$NumberAtAgeArea <- AddRecruits_(
      OMListSim$NumberAtAgeArea,
      Recruits,
      OMListSim$Spatial$UnfishedDist,
      TSindex
    )
    
    if (TSindex<nTS) {
      # Update Number beginning of next Time Step
      OMListSim$NumberAtAgeArea <- CalcNumberNext_(
        OMListSim$NumberAtAgeArea,
        OMListSim$NaturalMortality$MeanAtAge,
        OMListSim$FDeadAtAgeArea,
        OMListSim$Maturity$Semelparous,
        OMListSim$Ages,
        TSindex
      )
      
      OMListSim$NumberAtAgeArea <- MoveStock_(
        OMListSim$NumberAtAgeArea,
        OMListSim$Spatial$Movement,
        TSindex+1
      )
      
    
    }
  }

  OMListSim
  
  
}
