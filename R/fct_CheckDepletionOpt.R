CheckDepletionOpt <- function(SimList, HistYears) {
  OptRatio <- purrr::map(SimList, \(HistSim){
    Reference <- purrr::map(HistSim@OM@Stock, \(stock) stock@Depletion@Reference)
    Final <- purrr::map(HistSim@OM@Stock, \(stock) stock@Depletion@Final)
    
    OptRatio <- rep(NA, length(Reference))
    
    for (st in seq_along(Reference)) {
      if (!length(Final[[st]]))
          next()
      if (Reference[[st]] == 'B0') {
        RefVal <- HistSim@Unfished@Equilibrium@Biomass |> 
          ArraySubsetYear(tail(HistYears,1)) |>
          tail(1) |> Array2List(1)
        RefVal <- RefVal[[st]]
        
        CurrVal <- HistSim@Biomass[st,] |> tail(1)
        
      } else if (Reference[[st]] == 'SB0') {
        RefVal <- HistSim@Unfished@Equilibrium@SBiomass |> 
          ArraySubsetYear(tail(HistYears,1)) |>
          tail(1) |> Array2List(1)
        RefVal <- RefVal[[st]]
        CurrVal <- HistSim@SBiomass[st,] |> tail(1)
      }
      OptRatio[st] <- (CurrVal/RefVal)/Final[[st]] 
    }
    OptRatio
  }) |> unlist() |> matrix(nrow=nStock(SimList$`1`@OM), ncol=length(SimList))
  
  dimnames(OptRatio) <- list(Stock=StockNames(SimList$`1`@OM),
                             Sim=1:length(SimList)
  )
  OptRatio <- t(OptRatio)
  
}