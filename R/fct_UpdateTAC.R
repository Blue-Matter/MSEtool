# TACUnchanged <- function(MPAdvice, MPAdvicePrevious) {
#   !is.null(MPAdvicePrevious) && 
#     (IdenticalS4(MPAdvice@TAC, MPAdvicePrevious@TAC)) ||
#     EmptyObject(MPAdvice@TAC)
#   
# }


ProcessAdvice_TAC <- function(MPAdvice, FleetNames, nArea) {
  nFleet <- length(FleetNames)

  if (is.array(MPAdvice@TAC)) {
    dd <- dim(MPAdvice@TAC)
    if (dd[1] != nFleet)
      cli::cli_abort(c(
        "x"="If `Advice@TAC` is an array, it must have `nFleet` ({.val {nFleet}}) rows and `nArea` ({.val {nArea}}) columns "
      ))
    
    if (dd[2] != nArea)
      cli::cli_abort(c(
        "x"="If `Advice@TAC` is an array, it must have `nFleet` ({.val {nFleet}}) rows and `nArea` ({.val {nArea}}) columns "
      ))
    
    dimnames(MPAdvice@TAC) <- list(
      Fleet=FleetNames,
      Area=1:nArea
    )
    return(MPAdvice)
  }
  if (length(MPAdvice@TAC)>1 && length(MPAdvice@TAC)!=nFleet)
    cli::cli_abort(c(
      "x"="If `Advice@TAC` is a numeric vector, it must be length 1 or length `nFleet` ({.val {nFleet}})"
    ))
    
  if(length(MPAdvice@TAC)>1) 
    MPAdvice@TAC <- array(MPAdvice@TAC, dim=length(MPAdvice@TAC), dimnames=list(
      Fleet=FleetNames
    ))
  MPAdvice@TAC
}



CalcTAC_StockAllocation <- function(ProjSim, stocks, StockNames, TSIndex) {
  # Distribute TAC across stocks in complex 
  # according to vulnerable biomass
  
  # TODO - alternatives such as by price/profit or user function
  nStocks <- length(stocks)
  StockAllocation <- rep(1, nStocks)
  if (nStocks==1)
    return(StockAllocation)
  
  VulnBiomass <- rep(0, nStocks)
  for (st in stocks) {
    NumberAtAge <- ProjSim@Number[[st]][, TSIndex,, drop=FALSE] |> apply(1, sum)
    SelectivityAtAge <- ProjSim@OM@Fleet[[st]]@Selectivity@MeanAtAge[,TSIndex,, drop=FALSE] |> abind::adrop(2)
    RetentionAtAge <- ProjSim@OM@Fleet[[st]]@Retention@MeanAtAge[,TSIndex,, drop=FALSE] |> abind::adrop(2)
    FleetWeightAtAge <- ProjSim@OM@Fleet[[st]]@WeightFleet[,TSIndex,, drop=FALSE] |> abind::adrop(2)
    VulnBiomass[st] <- sum(NumberAtAge * SelectivityAtAge * RetentionAtAge * FleetWeightAtAge)
  }
  StockAllocation <- VulnBiomass/sum(VulnBiomass)
  array(StockAllocation, dim=length(StockAllocation),
        dimnames = list(Stock=StockNames[stocks]))
}

# TOO - OM@Allocation should already have this structure
CalcTAC_FleetAllocation <- function(ProjSim, stocks, StockNames, FleetNames) {
  
  FleetAllocation <- array(0, dim=c(length(stocks),
                                    length(FleetNames)),
                           dimnames = list(
                             Stock=StockNames[stocks],
                             Fleet=FleetNames
                           ))
  for (st in stocks) {
    FleetAllocation[st,] <- ProjSim@OM@Allocation[[st]]
  }
  FleetAllocation
}

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
      if (!EmptyObject(MPAdvicePrevious@TAC)) {
        MPAdvice@TAC <- MPAdvicePrevious@TAC
      } else {
        next()
      }
    }
    
    TAC <- ProcessAdvice_TAC(MPAdvice, FleetNames, nArea)
    
    DimNames <- dimnames(TAC)
    
    StockAllocation <- CalcTAC_StockAllocation(ProjSim, stocks, StockNames, TSIndex)
    
    if (is.null(DimNames)) {
      # single TAC needs to be distributed across stocks and fleets
   
      FleetAllocation <- CalcTAC_FleetAllocation(ProjSim, stocks, StockNames, FleetNames)

      ProcessAdvice_GlobalTAC <- function(ProjSim, stocks, StockAllocation, FleetAllocation) {
        
        TEMP <- matrix(NA, 2, length(FleetNames))
        for (st in stocks) {
          NumberAtAge <- ProjSim@Number[[st]][, TSIndex,, drop=FALSE] |> apply(1, sum)
          SelectivityAtAge <- ProjSim@OM@Fleet[[st]]@Selectivity@MeanAtAge[,TSIndex,, drop=FALSE] |> abind::adrop(2)
          RetentionAtAge <- ProjSim@OM@Fleet[[st]]@Retention@MeanAtAge[,TSIndex,, drop=FALSE] |> abind::adrop(2)
          DiscardMortalityAtAge <- ProjSim@OM@Fleet[[st]]@DiscardMortality@MeanAtAge[,TSIndex,, drop=FALSE] |> abind::adrop(2)
          FleetWeightAtAge <- ProjSim@OM@Fleet[[st]]@WeightFleet[,TSIndex,, drop=FALSE] |> abind::adrop(2)
          NaturalMortalityAtAge <- ProjSim@OM@Stock[[st]]@NaturalMortality@MeanAtAge[,TSIndex]
        
          TotalRemovalsFleet <- MPAdvice@TAC * StockAllocation[st] * FleetAllocation[st,] 
          
          SolvedF <- SolveForFishingMortality(NumberAtAge,
                                              TotalRemovalsFleet,
                                              SelectivityAtAge,  
                                              RetentionAtAge,  
                                              DiscardMortalityAtAge,  
                                              FleetWeightAtAge,
                                              NaturalMortalityAtAge) 
          
          FInteract <- t(SolvedF$ApicalFInteract)
          apicalFDead <- apply(SolvedF$FDeadAtAge, 1, sum) |> max()
          
          if (apicalFDead > ProjSim@OM@maxF) {
            FInteract <- FInteract * ProjSim@OM@maxF/apicalFDead
          }
          
          RequiredEffort <- FInteract / ProjSim@OM@Fleet[[st]]@Catchability[TSIndex,] 
          RequiredEffort[RequiredEffort<1E-5] <- 1E-5 
          TEMP[st,] <- RequiredEffort
        }
        TEMP[1,]/TEMP[2,]
        
          ProjSim@Effort[st,TSIndex,] <- as.vector(RequiredEffort)
          ProjSim
        }
        
        
      }
      
      plot(SAVE, RequiredEffort)
      
      SAVE <- RequiredEffort
      
      
      
    } else if (all(c('Fleet', 'Area') %in% names(DimNames))) {
      # TAC defined for each fleet and area
      
      # distribute over stocks in complex
      # solve for F
      
    } else {
      # TAC defined by fleet
    }
    
    
    # Distribute TAC across stocks according to vulnerable biomass

    
      
    
    
    
    
  }
  

    
    
  
  # calculate effort for a given TAC ... 
  # TODO doesn't account for spatial closures
  # TODO calculate effortArea here instead - calculate distribution of F/Effort by area
  
  # TODO applies for all stocks - TODO multistock with stock specific TAC
  
  nStock <- length(ProjSim@Number)
  
  NumberAtAge <- purrr::map(ProjSim@Number, \(stock) stock[,TSIndex,, drop=FALSE] |> abind::adrop(2) |> apply(1, sum)) # summed over areas
  SelectivityAtAge <- purrr::map(ProjSim@OM@Fleet, \(stock) stock@Selectivity@MeanAtAge[,TSIndex,, drop=FALSE] |> abind::adrop(2))
  RetentionAtAge <- purrr::map(ProjSim@OM@Fleet, \(stock) stock@Retention@MeanAtAge[,TSIndex,, drop=FALSE] |> abind::adrop(2))
  DiscardMortalityAtAge <- purrr::map(ProjSim@OM@Fleet, \(stock) stock@DiscardMortality@MeanAtAge[,TSIndex,, drop=FALSE] |> abind::adrop(2))
  FleetWeightAtAge <- purrr::map(ProjSim@OM@Fleet, \(stock) stock@WeightFleet[,TSIndex,, drop=FALSE] |> abind::adrop(2))
  NaturalMortalityAtAge <- purrr::map(ProjSim@OM@Stock, \(stock) stock@NaturalMortality@MeanAtAge[,TSIndex])
  
  RelVuln <- purrr::map2(NumberAtAge, SelectivityAtAge, \(number, select) {
    sum(number * select)
    # sum(ArrayMultiply(number, select))
  }) |> unlist()
  
  
  StockAllocation <- RelVuln/sum(RelVuln)
  FleetAllocation <- ProjSim@OM@Allocation
  
  # TODO - this is setting effort individually for stocks - ie different effort by stock for each fleet
  
  for (st in 1:nStock) {
    
    TotalRemovalsFleet <- MPAdvice@TAC * StockAllocation[st] * FleetAllocation[[st]] |> as.numeric()
    
    SolvedF <- SolveForFishingMortality(NumberAtAge[[st]],
                                        TotalRemovalsFleet,
                                        SelectivityAtAge[[st]],  
                                        RetentionAtAge[[st]],  
                                        DiscardMortalityAtAge[[st]],  
                                        FleetWeightAtAge[[st]],
                                        NaturalMortalityAtAge[[st]]) 
    
    FInteract <- t(SolvedF$ApicalFInteract)
    apicalFDead <- apply(SolvedF$FDeadAtAge, 1, sum) |> max()
    
    if (apicalFDead > ProjSim@OM@maxF) {
      FInteract <- FInteract * ProjSim@OM@maxF/apicalFDead
    }
    
    RequiredEffort <- FInteract / ProjSim@OM@Fleet[[st]]@Catchability[TSIndex,] 
    RequiredEffort[RequiredEffort<1E-5] <- 1E-5 
    
    ProjSim@Effort[st,TSIndex,] <- as.vector(RequiredEffort)
  }
  
  ProjSim
}