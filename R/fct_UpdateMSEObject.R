


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



