
DistributeTAC <- function(ProjSim, MPAdvice, nArea, FleetNames, StockNames, stocks, TSIndex) {
  # Distributes TAC over stocks (for complexes), fleets, and areas
  # According to vulnerable biomass
  # TODO - distribute according to density across areas?
  # TODO - short-cuts if there is only 1 stock, fleet, and/or area
  
  # single value - distribute across stocks, fleets, areas
  if (!is.array(MPAdvice@TAC)) {
    if (length(MPAdvice@TAC)==1) {
      TAC <- DistributeTAC_StockFleetArea(ProjSim, MPAdvice, nArea, FleetNames, StockNames, stocks, TSIndex)
    } else if (length(MPAdvice@TAC)==length(FleetNames)) {
      TAC <- DistributeTAC_StockArea(ProjSim, MPAdvice, nArea, FleetNames, StockNames, stocks, TSIndex)
    } else {
      cli::cli_abort(c(
        "x"="If `Advice@TAC` is a numeric vector, it must be length 1 or length `nFleet` ({.val {nFleet}})"
      ))
    }
  } else {
    nFleet <- length(FleetNames)
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
    TAC <- DistributeTAC_Stock(ProjSim, MPAdvice, nArea, FleetNames, StockNames, stocks, TSIndex)
  }
  TAC
}


DistributeTAC_StockFleetArea <- function(ProjSim, MPAdvice, nArea, FleetNames, StockNames, stocks, TSIndex)  {
  nStock <- length(StockNames[stocks])
  nFleet <- length(FleetNames)
  TAC <- array(NA, dim=c(nStock, nFleet, nArea),
               dimnames = list(
                 Stock=StockNames[stocks],
                 Fleet=FleetNames,
                 Area=1:nArea))
  
  # TODO - clean up loop
  VBiomassStockFleetArea <- array(NA, dim=c(nStock, nFleet, nArea),
                                  dimnames = list(
                                    Stock=StockNames[stocks],
                                    Fleet=FleetNames,
                                    Area=1:nArea)
  )
  
  for (st in stocks) {
    NumberAtAge <- ProjSim@Number[[st]][, TSIndex,, drop=FALSE] |> abind::adrop(2) # nAge, nArea
    SelectivityAtAge <- ProjSim@OM@Fleet[[st]]@Selectivity@MeanAtAge[,TSIndex,, drop=FALSE] |> abind::adrop(2) # nAge, nFleet
    RetentionAtAge <- ProjSim@OM@Fleet[[st]]@Retention@MeanAtAge[,TSIndex,, drop=FALSE] |> abind::adrop(2) # nAge, nFleet
    FleetWeightAtAge <- ProjSim@OM@Fleet[[st]]@WeightFleet[,TSIndex,, drop=FALSE] |> abind::adrop(2)  # nAge, nFleet
    
    VulnWeight <- SelectivityAtAge * RetentionAtAge * FleetWeightAtAge
    for (fl in seq_along(FleetNames)) {
      for (area in 1:nArea) {
        VBiomassStockFleetArea[st, fl, area] <- sum(NumberAtAge[,area] * VulnWeight[,fl]) * 
          ProjSim@OM@Fleet[[st]]@Closure[TSIndex,fl,area]
      }
    }
  }
  
  for (st in stocks) {
    for (fl in seq_along(FleetNames)) {
      FleetArea <- VBiomassStockFleetArea[,fl,,drop=FALSE] |> abind::adrop(2) # nStock, nArea
      StockDist <- apply(FleetArea, 1, sum)/sum(apply(FleetArea, 1, sum))
      AreaDist <- apply(FleetArea, 2, sum)/sum(apply(FleetArea, 2, sum))
      for (area in 1:nArea) {
        TAC[st, fl, area] <- MPAdvice@TAC * ProjSim@OM@Allocation[[st]][fl] * AreaDist[area] 
      }
    }
    
    for (area in 1:nArea) {
      TAC[st,,area] <- TAC[st,,area] * StockDist[st]
    }
  }
  TAC
}

DistributeTAC_StockArea <- function(ProjSim, MPAdvice, nArea, FleetNames, StockNames, stocks, TSIndex)  {
  nStock <- length(StockNames[stocks])
  nFleet <- length(FleetNames)
  TAC <- array(NA, dim=c(nStock, nFleet, nArea),
               dimnames = list(
                 Stock=StockNames[stocks],
                 Fleet=FleetNames,
                 Area=1:nArea))
  
  # TODO - clean up loop
  VBiomassStockFleetArea <- array(NA, dim=c(nStock, nFleet, nArea),
                                  dimnames = list(
                                    Stock=StockNames[stocks],
                                    Fleet=FleetNames,
                                    Area=1:nArea)
  )
  
  for (st in stocks) {
    NumberAtAge <- ProjSim@Number[[st]][, TSIndex,, drop=FALSE] |> abind::adrop(2) # nAge, nArea
    SelectivityAtAge <- ProjSim@OM@Fleet[[st]]@Selectivity@MeanAtAge[,TSIndex,, drop=FALSE] |> abind::adrop(2) # nAge, nFleet
    RetentionAtAge <- ProjSim@OM@Fleet[[st]]@Retention@MeanAtAge[,TSIndex,, drop=FALSE] |> abind::adrop(2) # nAge, nFleet
    FleetWeightAtAge <- ProjSim@OM@Fleet[[st]]@WeightFleet[,TSIndex,, drop=FALSE] |> abind::adrop(2)  # nAge, nFleet
    
    VulnWeight <- SelectivityAtAge * RetentionAtAge * FleetWeightAtAge
    for (fl in seq_along(FleetNames)) {
      for (area in 1:nArea) {
        VBiomassStockFleetArea[st, fl, area] <- sum(NumberAtAge[,area] * VulnWeight[,fl]) *
          ProjSim@OM@Fleet[[st]]@Closure[TSIndex,fl,area]
      }
    }
  }
  
  for (st in stocks) {
    for (fl in seq_along(FleetNames)) {
      FleetArea <- VBiomassStockFleetArea[,fl,,drop=FALSE] |> abind::adrop(2) # nStock, nArea
      StockDist <- apply(FleetArea, 1, sum)/sum(apply(FleetArea, 1, sum))
      AreaDist <- apply(FleetArea, 2, sum)/sum(apply(FleetArea, 2, sum))
      for (area in 1:nArea) {
        TAC[st, fl, area] <- MPAdvice@TAC[fl] * AreaDist[area] 
      }
    }
    
    for (area in 1:nArea) {
      TAC[st,,area] <- TAC[st,,area] * StockDist[st]
    }
  }
  TAC
}

DistributeTAC_Stock <- function(ProjSim, MPAdvice, nArea, FleetNames, StockNames, stocks, TSIndex)  {
  nStock <- length(StockNames[stocks])
  nFleet <- length(FleetNames)
  TAC <- array(NA, dim=c(nStock, nFleet, nArea),
               dimnames = list(
                 Stock=StockNames[stocks],
                 Fleet=FleetNames,
                 Area=1:nArea))
  
  # TODO - clean up loop
  VBiomassStockFleetArea <- array(NA, dim=c(nStock, nFleet, nArea),
                                  dimnames = list(
                                    Stock=StockNames[stocks],
                                    Fleet=FleetNames,
                                    Area=1:nArea)
  )
  
  for (st in stocks) {
    NumberAtAge <- ProjSim@Number[[st]][, TSIndex,, drop=FALSE] |> abind::adrop(2) # nAge, nArea
    SelectivityAtAge <- ProjSim@OM@Fleet[[st]]@Selectivity@MeanAtAge[,TSIndex,, drop=FALSE] |> abind::adrop(2) # nAge, nFleet
    RetentionAtAge <- ProjSim@OM@Fleet[[st]]@Retention@MeanAtAge[,TSIndex,, drop=FALSE] |> abind::adrop(2) # nAge, nFleet
    FleetWeightAtAge <- ProjSim@OM@Fleet[[st]]@WeightFleet[,TSIndex,, drop=FALSE] |> abind::adrop(2)  # nAge, nFleet
    
    VulnWeight <- SelectivityAtAge * RetentionAtAge * FleetWeightAtAge
    for (fl in seq_along(FleetNames)) {
      for (area in 1:nArea) {
        VBiomassStockFleetArea[st, fl, area] <- sum(NumberAtAge[,area] * VulnWeight[,fl]) * 
          ProjSim@OM@Fleet[[st]]@Closure[TSIndex,fl,area]
      }
    }
  }
  
  for (st in stocks) {
    for (fl in seq_along(FleetNames)) {
      FleetArea <- VBiomassStockFleetArea[,fl,,drop=FALSE] |> abind::adrop(2) # nStock, nArea
      StockDist <- apply(FleetArea, 1, sum)/sum(apply(FleetArea, 1, sum))
      for (area in 1:nArea) {
        TAC[st, fl, area] <- MPAdvice@TAC[fl, area] 
      }
    }
    
    for (area in 1:nArea) {
      TAC[st,,area] <- TAC[st,,area] * StockDist[st]
    }
  }
  TAC
}