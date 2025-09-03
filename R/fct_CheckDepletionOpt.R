CheckDepletionOpt <- function(HistSimList, HistTimeSteps) {
  OptRatio <- purrr::map(HistSimList, \(HistSim){
    Reference <- purrr::map(HistSim@OM@Stock, \(stock) stock@Depletion@Reference)
    Final <- purrr::map(HistSim@OM@Stock, \(stock) stock@Depletion@Final)
    
    OptRatio <- rep(0, length(Reference))
    
    for (st in seq_along(Reference)) {
      if (Reference[[st]] == 'B0') {
        RefVal <- HistSim@Unfished@Equilibrium@Biomass |> 
          ArraySubsetTimeStep(tail(HistTimeSteps,1)) |>
          tail(1) |> Array2List(1)
        RefVal <- RefVal[[st]]
        
        CurrVal <- HistSim@Biomass[st,] |> tail(1)
        
      } else if (Reference[[st]] == 'SB0') {
        RefVal <- HistSim@Unfished@Equilibrium@SBiomass |> 
          ArraySubsetTimeStep(tail(HistTimeSteps,1)) |>
          tail(1) |> Array2List(1)
        RefVal <- RefVal[[st]]
        CurrVal <- HistSim@SBiomass[st,] |> tail(1)
      }
      OptRatio[st] <- abs((CurrVal/RefVal)/Final[[st]] - 1)
    }
    OptRatio
  }) |> unlist() |> matrix(nrow=nStock(HistSimList$`1`@OM), ncol=length(HistSimList))
  
  dimnames(OptRatio) <- list(Stock=StockNames(HistSimList$`1`@OM),
                             Sim=1:length(HistSimList)
  )
  OptRatio <- t(OptRatio)
  
}