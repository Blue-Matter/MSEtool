

#' Calculate Unfished Survival
#' 
#' @param OM An [OM()] object, a [Stock()] object, or a list of [Stock()] objects.
#' @param SP Logical. Account for `SpawnTimeFrac`? Accounts for spawning timing within a time step to 
#' calculate the number-at-age at the time of spawning
#' @param Years Numeric vector of years to calculate
#' @param silent Logical. Print messages?
#' @param Extend Logical. Extend the array to include all Sims and Years?
#' 
#' @return A list of length [nStock()] with an array with the unfished survival for each Stocks
#' @examples
#' \dontrun{
#' CalcUnfishedSurvival(OM)
#' }
#' 
#' @export
CalcUnfishedSurvival <- function(OM, SP = FALSE, Years = NULL, silent = FALSE, Extend = TRUE) {
 
  if (inherits(OM, "stock")) {
    return(CalcUnfishedSurvivalStock(OM, SP, Years, Extend))
  }
  
  if (inherits(OM, "list") || inherits(OM, "StockList")) {
    return(CalcUnfishedSurvivalStockList(OM, SP, Years, Extend))
  }
  
  if (inherits(OM, "om")) {
    OM <- PopulateOM(OM, silent)
    if (is.null(Years)) {
      Years <- Years(OM,'Hist')
    }
    return(CalcUnfishedSurvivalStockList(StockList=OM@Stock, SP, Years, Extend))
  }
  
  cli::cli_abort("`OM` must be either an `OM()` object, a `Stock()` object, or a list of `Stock()` objects")
  
}

CalcUnfishedSurvivalStockList <- function(StockList, SP = FALSE, Years = NULL, Extend = TRUE) {
  purrr::map(StockList, \(Stock) 
             CalcUnfishedSurvivalStock(Stock, SP, Years, Extend))
}

CalcUnfishedSurvivalStock <- function(Stock, SP = FALSE, Years = NULL, Extend = TRUE) {
  
  NaturalMortality <- Stock@NaturalMortality@MeanAtAge |>
    Extend(nSim=Stock@nSim, AgeClasses=Stock@Ages@Classes, Years) |>
    ArraySubsetYear(Years)
  
  PlusGroup <- Stock@Ages@PlusGroup
  SpawnTimeFrac <- ifelse(SP, Stock@SRR@SpawnTimeFrac, 0)
  
  Survival <- CalcSurvival(NaturalMortality, 
                           FishingMortality=NULL, 
                           PlusGroup=Stock@Ages@PlusGroup,
                           SpawnTimeFrac=ifelse(SP, Stock@SRR@SpawnTimeFrac, 0),
                           Semelparous=Stock@Maturity@Semelparous)
  

  if (Extend) {
    Survival <- Survival |> Extend(Stock@nSim, Stock@Ages@Classes, Years)
  }
  Survival
}
