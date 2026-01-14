
UpdateMSEObject <- function(MSE, SimList_MP, MP, mp, YearsHist, YearsProj) {
  
  YearsAll <- c(YearsHist, YearsProj)
  
  nStock <- nStock(SimList_MP[[1]]@OM)
  
  FleetNames <- as.vector(SimList_MP[[1]]@OM@Fleet[[1]]@Name)
  StockNames <- StockNames(SimList_MP[[1]]@OM)
  for (st in 1:nStock) {
    MSE@Number[[st]][,,,,mp] <- purrr::map(SimList_MP, \(x) x@Number[[st]]) |> 
      List2Array("Sim") |>
      AddDimNames(names=c("Age", "Year", "Area", "Sim"), Years=YearsAll) |> 
      ArraySubsetYear(Years=YearsProj) |>
      aperm(c("Sim", "Age", "Year", "Area"))
  }
  
  MSE@Biomass[,,,mp] <- purrr::map(SimList_MP, \(x) x@Biomass) |>
    List2Array("Sim") |>
    AddDimNames(names=c("Stock", "Year", "Sim"), Years=YearsAll) |> 
    ArraySubsetYear(Years=YearsProj) |>
    aperm(c("Sim", "Stock", "Year"))
  
  MSE@SBiomass[,,,mp] <- purrr::map(SimList_MP, \(x) x@SBiomass) |>
    List2Array("Sim") |>
    AddDimNames(names=c("Stock", "Year", "Sim"), Years=YearsAll) |> 
    ArraySubsetYear(Years=YearsProj) |>
    aperm(c("Sim", "Stock", "Year"))
  
  MSE@SProduction[,,,mp] <- purrr::map(SimList_MP, \(x) x@SProduction) |>
    List2Array("Sim") |>
    AddDimNames(names=c("Stock", "Year", "Sim"), Years=YearsAll) |> 
    ArraySubsetYear(Years=YearsProj) |>
    aperm(c("Sim", "Stock", "Year"))
  
  MSE@Landings[,,,,mp] <- purrr::map(SimList_MP, \(ProjSim) 
                                     ProjSim@Landings) |>
    List2Array('Year') |>
    AddDimNames(c("Stock", "Year", "Fleet", "Sim"),
                Years = YearsAll) |>
    aperm(c('Sim', 'Stock', 'Year', 'Fleet')) |>
    ArraySubsetYear(Years=YearsProj) 
  
  MSE@Discards[,,,,mp] <- purrr::map(SimList_MP, \(ProjSim) 
                                     ProjSim@Discards) |>
    List2Array('Year') |>
    AddDimNames(c("Stock", "Year", "Fleet", "Sim"),
                Years = YearsAll) |>
    aperm(c('Sim', 'Stock', 'Year', 'Fleet')) |>
    ArraySubsetYear(Years=YearsProj) 
  
  
  LandingsAtAgeList <- purrr::map(SimList_MP, \(ProjSim) {
    purrr::map(ProjSim@LandingsAtAge, \(Landings) {
      List2Array(Landings, "Year") |>
        AddDimNames(c("Age", "Fleet", "Area", "Year"),
                    values=c(list(NA), list(FleetNames), list(NA), list(NA)),
                    Years=YearsAll) |>
        ArraySubsetYear(Years=YearsProj) |>
        ReduceDims(IncYear=FALSE)
    })
  }) |> 
    ReverseList() |>
    purrr::map(List2Array,"Sim") |>
    purrr::map(aperm, c('Sim', 'Age', 'Year',  'Fleet', 'Area'))
  
  MSE@LandingsAtAge <- purrr::map2(MSE@LandingsAtAge, LandingsAtAgeList, \(MSELanding, Landings) {
    MSELanding[,,,,,mp] <- Landings
    MSELanding
  })
  
  DiscardsAtAgeList <- purrr::map(SimList_MP, \(ProjSim) {
    purrr::map(ProjSim@DiscardsAtAge, \(Discards) {
      List2Array(Discards, "Year") |>
        AddDimNames(c("Age", "Fleet", "Area", "Year"),
                    values=c(list(NA), list(FleetNames), list(NA), list(NA)),
                    Years=YearsAll) |>
        ArraySubsetYear(Years=YearsProj) |>
        ReduceDims(IncYear=FALSE)
    })
  }) |> 
    ReverseList() |>
    purrr::map(List2Array,"Sim") |>
    purrr::map(aperm, c('Sim', 'Age', 'Year',  'Fleet', 'Area'))
  
  MSE@DiscardsAtAge <- purrr::map2(MSE@DiscardsAtAge, DiscardsAtAgeList, \(MSEDiscards, Discards) {
    MSEDiscards[,,,,,mp] <- Discards
    MSEDiscards
  })
  
  
  MSE@Effort[,,,,mp] <- purrr::map(SimList_MP, \(x) 
                                   x@Effort |> 
                                     AddDimNames(c("Stock", "Year", "Fleet"),
                                                 values=c(list(StockNames),
                                                          list(NA),
                                                          list(FleetNames)),
                                                 Years=YearsAll) |>
                                     ArraySubsetYear(Years=YearsProj)
  ) |>
    List2Array("Sim") |>
    aperm(c("Sim", "Stock", "Year", "Fleet"))
  
  for (st in 1:nStock) {
    MSE@FDead[[st]][,,,,mp] <- purrr::map(SimList_MP, \(x) x@FDead[[st]]) |> 
      List2Array("Sim") |>
      AddDimNames(names=c("Age", "Year", "Fleet", "Sim"), Years=YearsAll) |> 
      ArraySubsetYear(Years=YearsProj) |>
      aperm(c("Sim", "Age", "Year", "Fleet"))
    
    MSE@FRetain[[st]][,,,,mp] <- purrr::map(SimList_MP, \(x) x@FRetain[[st]]) |> 
      List2Array("Sim") |>
      AddDimNames(names=c("Age", "Year", "Fleet", "Sim"), Years=YearsAll) |> 
      ArraySubsetYear(Years=YearsProj) |>
      aperm(c("Sim", "Age", "Year", "Fleet"))
    
    
    MSE@Distribution[[st]][,,,,mp] <- purrr::map(SimList_MP, \(x) x@Distribution[[st]]) |> 
      List2Array("Sim") |>
      AddDimNames(names=c("Year", "Fleet", "Area", "Sim"), Years=YearsAll) |> 
      ArraySubsetYear(Years=YearsProj) |>
      aperm(c("Sim", "Year", "Fleet", "Area"))
    
    MSE@FDeadArea[[st]][,,,,,mp] <- purrr::map(SimList_MP, \(x) {
      x@FDeadArea[[st]] |> 
        List2Array("Year") |>
        AddDimNames(c("Age", "Fleet", "Area", "Year"),
                    values=c(list(NA), list(FleetNames), list(NA), list(NA)),
                    Years=YearsAll) |>
        ArraySubsetYear(Years=YearsProj) 
    }) |>
      List2Array("Sim") |>
      aperm(c("Sim", "Age", "Year", "Fleet", "Area"))
    
    MSE@FRetainArea[[st]][,,,,,mp] <- purrr::map(SimList_MP, \(x) {
      x@FRetainArea[[st]] |> 
        List2Array("Year") |>
        AddDimNames(c("Age", "Fleet", "Area", "Year"),
                    values=c(list(NA), list(FleetNames), list(NA), list(NA)),
                    Years=YearsAll) |>
        ArraySubsetYear(Years=YearsProj) 
    }) |>
      List2Array("Sim") |>
      aperm(c("Sim", "Age", "Year", "Fleet", "Area"))
  }
  
  # Misc 
  # keep MPAdvice 
  if (is.null(MSE@Misc$Advice)) 
    MSE@Misc$Advice <- list()
  
  MPName <- names(MSE@MPs)[mp]
  MSE@Misc$Advice[[MPName]] <- lapply(SimList_MP, slot, 'Misc') |> lapply("[[", "MPAdvice")
  MSE@Misc$Failed[[MPName]] <- lapply(SimList_MP, slot, 'Misc') |> 
    lapply("[[", "Failed") |> 
    unlist() |> 
    as.numeric()
  
  MSE <- MSE |> 
    KeepRetention(SimList_MP, mp) |>
    KeepSelectivity(SimList_MP, mp) |> 
    KeepDiscardMortality(SimList_MP, mp) |>
    AddPPD(SimList_MP, mp) |>
    ProcessLogMSE(SimList_MP, mp, MP)
  
  MSE
}



ProcessLogMSE <- function(MSE, SimList_MP, mp, MP) {
  
  LogList <- purrr::map(SimList_MP, \(ProjSim) {
    Log <- ProjSim@Log
    Log$OptDepletionRatio <- NULL
    Log
  })
  
  if (is.null(MSE@Log[["MP"]]))
    MSE@Log[["MP"]] <- list()
  
  MSE@Log[["MP"]][[MP]] <- LogList
  MSE
}


KeepSelectRetenDisc <- function(MSE, SimList_MP, mp, Slot='Retention') {
  
  ProjYear <- Years(MSE@OM, 'Projection')
  AtAge <- purrr::map(SimList_MP, \(ProjSim) {
    purrr::map(ProjSim@OM@Fleet, \(fleet) {
      slot(fleet, Slot)@MeanAtAge |> ArraySubsetYear(ProjYear) 
    }) 
  }) |> 
    ReverseList() |>
    purrr::map(List2Array, 'Sim') |>
    purrr::map(aperm, c('Sim', 'Age', 'Year', 'Fleet')) 
  
  # AtLength <- purrr::map(SimList_MP, \(ProjSim) {
  #   purrr::map(ProjSim@OM@Fleet, \(fleet) {
  #     slot(fleet, Slot)@MeanAtLength
  #   }) 
  # }) |> 
  #   ReverseList() |>
  #   purrr::map(List2Array, 'Sim') |>
  #   purrr::map(aperm, c('Sim', 'Class', 'Year', 'Fleet'))
  
  stocks <- StockNames(MSE@OM)
  MPName <- names(MSE@MPs)[mp]
  for (st in seq_along(stocks)) {
    AgeClasses <- MSE@OM@Stock[[st]]@Ages@Classes
    
    dd <- dimnames(AtAge[[st]])
    
    omvals <- slot(MSE@OM@Fleet[[st]],Slot)@MeanAtAge |> ArraySubsetYear(ProjYear) |>
      Extend(nSim=length(dd$Sim), AgeClasses, Years = dd$Year)
    
    if (!prod(AtAge[[st]] == omvals)) {
      if (is.null(MSE@Misc[[Slot]])) {
        MSE@Misc[[Slot]] <- list()
      }
      if (is.null(MSE@Misc[[Slot]][[MPName]])) {
        MSE@Misc[[Slot]][[MPName]] <- list()
      }
      MSE@Misc[[Slot]][[MPName]][[stocks[st]]] <- AtAge[[st]]
    }
  }
  # MSE@Misc[[Slot]][[MPName]] <- MSE@Misc[[Slot]][[MPName]][stocks]
  MSE
  
}

KeepRetention <- function(MSE, SimList_MP, mp) {
  KeepSelectRetenDisc(MSE, SimList_MP, mp)
}

KeepSelectivity <- function(MSE, SimList_MP, mp) {
  KeepSelectRetenDisc(MSE, SimList_MP, mp, 'Selectivity')
}

KeepDiscardMortality <- function(MSE, SimList_MP, mp) {
  KeepSelectRetenDisc(MSE, SimList_MP, mp, 'DiscardMortality')
}

AddPPD <- function(MSE, SimList_MP, mp) {
  PPD <- purrr::map(SimList_MP, slot, 'Data')
  MPName <- names(MSE@MPs)[mp]
  MSE@PPD[[MPName]] <- PPD
  MSE
}

