
UpdateTAC <- function(ProjSim, MPAdviceList, MPAdviceList_Previous, Year, YearsAll) {
  
  FleetNames <- FleetNames(ProjSim@OM)
  Complexes <- ProjSim@OM@Complexes
  TSIndex <- match(Year, YearsAll)
  nArea <- nArea(ProjSim@OM)
  nStock <- nStock(ProjSim@OM)
  StockNames <- StockNames(ProjSim@OM)
  
  for (complex in seq_along(MPAdviceList)) {
    stocks <- Complexes[[complex]]
    MPAdvice <- MPAdviceList[[complex]]
    MPAdvicePrevious <- MPAdviceList_Previous[[complex]]

    if (EmptyObject(MPAdvice@TAC)) {
      if (!is.null(MPAdvicePrevious) && !EmptyObject(MPAdvicePrevious@TAC)) {
        MPAdvice@TAC <- MPAdvicePrevious@TAC
      } else {
        next()
      }
    }
    
    if (!is.null(MPAdvice@Effort))
      cli::cli_alert_warning("MP has set both TAC and Effort. Only TAC being used - Effort is being ignored. TAC & Effort in development! ")

      
    TAC_StockFleetArea <- DistributeTAC(ProjSim, MPAdvice, nArea, FleetNames, StockNames, stocks, TSIndex)
    
    # Solve for F for each Stock & Fleet within each Area
    # TODO - Fleet effort-area distribution is independent between stocks ...  
    # Can address by distributing TAC differently
    for (st in stocks) {
      for (area in 1:nArea) {
        # calc F within each area
        NumberAtAge <- ProjSim@Number[[st]][, TSIndex,area, drop=FALSE] 
        SelectivityAtAge <- ProjSim@OM@Fleet[[st]]@Selectivity@MeanAtAge[,TSIndex,, drop=FALSE] |> abind::adrop(2)
        RetentionAtAge <- ProjSim@OM@Fleet[[st]]@Retention@MeanAtAge[,TSIndex,, drop=FALSE] |> abind::adrop(2)
        DiscardMortalityAtAge <- ProjSim@OM@Fleet[[st]]@DiscardMortality@MeanAtAge[,TSIndex,, drop=FALSE] |> abind::adrop(2)
        FleetWeightAtAge <- ProjSim@OM@Fleet[[st]]@WeightFleet[,TSIndex,, drop=FALSE] |> abind::adrop(2)
        NaturalMortalityAtAge <- ProjSim@OM@Stock[[st]]@NaturalMortality@MeanAtAge[,TSIndex]
        TotalRemovalsFleet <- TAC_StockFleetArea[st,,area]
        
        SolvedF <- SolveForFishingMortality(NumberAtAge,
                                            TotalRemovalsFleet,
                                            SelectivityAtAge,
                                            RetentionAtAge,
                                            DiscardMortalityAtAge,
                                            FleetWeightAtAge,
                                            NaturalMortalityAtAge)
        
        
        FInteract <- t(SolvedF$ApicalFInteract)
        apicalFDead <- apply(SolvedF$FDeadAtAge, 1, sum) |> max()
        
        # maxF here applies within each area
        if (apicalFDead > ProjSim@OM@maxF) {
          FInteract <- FInteract * ProjSim@OM@maxF/apicalFDead
        }
        
        RequiredEffort <- FInteract/ProjSim@OM@Fleet[[st]]@qArea[TSIndex,,area]
        RequiredEffort[RequiredEffort<1E-5] <- 1E-5
        ProjSim@Distribution[[st]][TSIndex,,area] <- RequiredEffort
      }
      
      ProjSim@Effort[st,TSIndex,] <- apply(ProjSim@Distribution[[st]][TSIndex,,, drop=FALSE], 2, sum)
    }
  }
  ProjSim
}




