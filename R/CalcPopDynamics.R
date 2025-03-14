CalcInitialTimeStep <- function(OMList) {
  
  nSim <- length(OMList)
  
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
    
  
    InitAgeClassRecDevs <- InitAgeClassRecDevs |> 
      AddDimension('Area')
    
    N0atAge <- OMList$N0atAge[[st]][,,1,]
    
    NatAgeInitial <- ArrayMultiply(N0atAge, InitAgeClassRecDevs)
    
    if (length(OMList$Depletion$Initial[[st]]) > 0) {
      cli::cli_abort('Initial depletion not done', .internal=TRUE)
      # NatAgeInitial update for initial depletion
    }
    
    OMList$NumberAtAgeArea[[st]][,,1,] <- NatAgeInitial
    
  }
  OMList
}

# OMList <- MakeOMList(OM, Unfished)
# OMListSim <- OMList[[8]]
# Period <- "Historical"
# MP=NULL
# DataListSim=NULL

CalcPopDynamics <- function(OMListSim, 
                            Period=c("Historical", "Projection", "All"), 
                            MP=NULL, 
                            DataListSim=NULL,
                            AddDimNames=TRUE) {

  Period <- match.arg(Period)

  if (!inherits(OMListSim, "OMListSim"))
    stop('Object must be class `OMListSim`')
  
  TimeStepsAll <- OMListSim$TimeSteps[[1]]
  if (Period=="Historical") {
    TimeSteps <- OMListSim$TimeStepsHist[[1]]
  } else if (Period=="Projection") {
    TimeSteps <- OMListSim$TimeStepsProj[[1]]
  } else {
    TimeSteps <- TimeStepsAll
  }
    
  StockNames <- names(OMListSim$NumberAtAgeArea)
  nStock <- length(StockNames)
  FleetNames <- dimnames(OMListSim$Selectivity$MeanAtAge[[1]])[["Fleet"]]
  nFleet <- length(FleetNames)
  nTStotal <- dim(OMListSim$NumberAtAgeArea[[1]])[2]
  
  # timestep <- TimeSteps[1]
  for (timestep in TimeSteps) {

    ts <- match(timestep, TimeStepsAll)
    TSindex <- ts - 1
    
    # Biomass by Area beginning of this time step
    OMListSim$BiomassArea = CalcBiomass_(
      OMListSim$BiomassArea,
      OMListSim$NumberAtAgeArea,
      OMListSim$Weight$MeanAtAge,
      TSindex
    )

    # Spawning Biomass by Area beginning of this time step
    # OMListSim$SBiomassArea = CalcBiomass_(
    #   OMListSim$BiomassArea,
    #   OMListSim$NumberAtAgeArea,
    #   OMListSim$Weight$MeanAtAge,
    #   TSindex
    # )
    
    # Apply MICE 
    
    # Apply MP 
    if (!is.null(MP)) {
      OMListSim <- ApplyMP(OMListSim, 
                           Data=NULL, 
                           MP=MP,
                           TimeStep=timestep)
    }
    
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

    # dimnames(OMListSim$EffortArea[[1]])

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

    
    # dimnames(List$FDeadAtAgeArea[[1]][[1]])
    
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
  
    # Calc overall F from catch and pop by Area
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
 
  
    # Generate Data 
    
    if (ts<nTStotal) {
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
  
  if (AddDimNames) {
    for (st in 1:nStock) {
      ages <- OMListSim$Ages[[st]]@Classes
      areas <- 1:dim(OMListSim$NumberAtAgeArea[[st]])[3]
    
      dimnames(OMListSim$VBiomassArea[[st]]) <- list(
        "Time Step"= TimeStepsAll,
        "Fleet" = FleetNames,
        "Area" = areas
      )
      
      dimnames(OMListSim$DensityArea[[st]]) <- list(
        "Time Step"= TimeStepsAll,
        "Fleet" = FleetNames,
        "Area" = areas
      )
      
      
      dimnames(OMListSim$EffortArea[[st]]) <- list(
        "Time Step"= TimeStepsAll,
        "Fleet" = FleetNames,
        "Area" = areas
      )
      
      OMListSim$FDeadAtAgeArea[[st]] <- purrr::map(OMListSim$FDeadAtAgeArea[[st]], \(x) {
        dimnames(x) <- list(
          "Age" = ages,
          "Fleet" = FleetNames,
          "Area" = areas
        )
        x
      })
      
      OMListSim$FRetainAtAgeArea[[st]] <- purrr::map(OMListSim$FRetainAtAgeArea[[st]], \(x) {
        dimnames(x) <- list(
          "Age" = ages,
          "Fleet" = FleetNames,
          "Area" = areas
        )
        x
      })
      
      OMListSim$RemovalAtAgeArea[[st]] <- purrr::map(OMListSim$RemovalAtAgeArea[[st]], \(x) {
        dimnames(x) <- list(
          "Age" = ages,
          "Fleet" = FleetNames,
          "Area" = areas
        )
        x
      })
      
      OMListSim$RetainAtAgeArea[[st]] <- purrr::map(OMListSim$RetainAtAgeArea[[st]], \(x) {
        dimnames(x) <- list(
          "Age" = ages,
          "Fleet" = FleetNames,
          "Area" = areas
        )
        x
      })
      
      dimnames(OMListSim$RemovalNumberAtAge[[st]]) <- list(
        "Age" = ages,
        "Time Step"= TimeStepsAll,
        "Fleet" = FleetNames
      )
      
      dimnames(OMListSim$RetainNumberAtAge[[st]]) <- list(
        "Age" = ages,
        "Time Step"= TimeStepsAll,
        "Fleet" = FleetNames
      )
   
      dimnames(OMListSim$RemovalBiomassAtAge[[st]]) <- list(
        "Age" = ages,
        "Time Step"= TimeStepsAll,
        "Fleet" = FleetNames
      )
      dimnames(OMListSim$RetainBiomassAtAge[[st]]) <- list(
        "Age" = ages,
        "Time Step"= TimeStepsAll,
        "Fleet" = FleetNames
      )
      
      dimnames(OMListSim$FDeadAtAge[[st]]) <- list(
        "Age" = ages,
        "Time Step"= TimeStepsAll,
        "Fleet" = FleetNames
      )
      
      dimnames(OMListSim$FRetainAtAge[[st]]) <- list(
        "Age" = ages,
        "Time Step"= TimeStepsAll,
        "Fleet" = FleetNames
      )
      
      OMListSim$SProduction[[st]] <- array(OMListSim$SProduction[[st]][,1])
      dimnames(OMListSim$SProduction[[st]]) <- list(
        "Time Step"= TimeStepsAll)
      
      dimnames(OMListSim$NumberAtAgeArea[[st]]) <- list(
        "Age" = ages,
        "Time Step"= TimeStepsAll,
        "Area" = areas
      )
    }
  
  }

  
  OMListSim
}
