
#' Calculate Equilibrium Unfished
#' 
#' Calculates the equilbrium unfished number, biomass, spawning biomass, and 
#' spawning production for all [Stock()] objects in an [OM()]
#' 
#' @param OM An [OM()] or [Hist] object
#' 
#' @return A [PopDynamics] object with equilibrium unfished Number, Biomass, SBiomass, and SProduction
#' @export
CalcUnfished_Equilibrium <- function(OM) {
  
  if (inherits(OM,'hist')) {
    OM <- OM@OM
  }
  
  # Populate OM if neccessary
  OM <- PopulateOM(OM) 
  
  # New `popdynamics` object that will be return
  EquilibriumUnfished <- new('popdynamics')
  
  # Calculate unfished Number-at-Age
  UnfishedNumberAtAge <- CalcUnfishedNumber(OM)
  EquilibriumUnfished@Number <- UnfishedNumberAtAge
  
  # Calculate unfished Spawning Number-at-Age (only different if SpawnFrac > 0)
  UnfishedSpawnNumberAtAge <- CalcUnfishedNumber(OM, SP=TRUE)
  
  # Biomass 
  WeightAtAge <- purrr::map(OM@Stock, \(x) {
    x@Weight@MeanAtAge 
  })
  
  EquilibriumUnfished@Biomass <- purrr::map2(UnfishedNumberAtAge, WeightAtAge, ArrayMultiply) |>
    purrr::map(\(x) apply(x, c('Sim', 'Year'), sum)) |>  # sum over ages within each Stock
    List2Array('Stock') |> # Convert to array
    aperm(c('Sim', 'Stock', 'Year'))
  
  # SBiomass 
  MaturityAtAge <- purrr::map(OM@Stock, \(x) {
    x@Maturity@MeanAtAge 
  })
  EquilibriumUnfished@SBiomass <- purrr::map2(UnfishedSpawnNumberAtAge, WeightAtAge, ArrayMultiply) |> 
    purrr::map2(MaturityAtAge, ArrayMultiply) |>
    purrr::map(\(x) apply(x, c('Sim', 'Year'), sum)) |>
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'Year'))
  
  
  # SProduction 
  FecundityAtAge <- purrr::map(OM@Stock, \(x) {
    x@Fecundity@MeanAtAge 
  })
  EquilibriumUnfished@SProduction <- purrr::map2(UnfishedSpawnNumberAtAge, FecundityAtAge, ArrayMultiply) |>
    purrr::map(\(x) apply(x, c('Sim', 'Year'), sum)) |>
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'Year'))
  
  # apply SPFrom for SProduction
  stockNames <- StockNames(OM)
  for (st in seq_along(stockNames)) {
    SPFrom <- OM@Stock[[st]]@SRR@SPFrom
    if (!is.null(SPFrom)) {
      ind <- match(SPFrom, stockNames)
      EquilibriumUnfished@SProduction[,st,] <- EquilibriumUnfished@SProduction[,ind,]
    }
  }
  EquilibriumUnfished
}
