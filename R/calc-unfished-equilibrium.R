
#' Calculate Equilibrium Unfished Population Dynamics
#'
#' Calculate equilibrium unfished population quantities for all [Stock()]
#' objects in an operating model. Quantities include unfished number-at-age,
#' total biomass, spawning biomass, and spawning production, evaluated under
#' equilibrium unfished conditions.
#'
#' If a [Hist()] object is supplied, its embedded operating model is used.
#' The operating model is populated if required prior to calculation.
#'
#' * Biomass is calculated as the sum over ages of unfished number-at-age
#'   multiplied by mean weight-at-age.
#' * Spawning biomass additionally applies maturity-at-age.
#' * Spawning production is calculated using fecundity-at-age and may be
#'   reassigned across stocks using the `SPFrom` slot of the stocks [SRR()] 
#'   objects.
#'
#' @param OM An [OM()] or [Hist()] object.
#' @param silent Logical; if `TRUE`, suppress messages during calculation.
#'
#' @return A [popdynamics-class] object containing equilibrium unfished
#'   `Number`, `Biomass`, `SBiomass`, and `SProduction` arrays with
#'   dimensions `Sim × Stock × Year`.
#'
#' @seealso [CalcUnfished_Dynamic()]
#'
#' @export
CalcUnfished_Equilibrium <- function(OM, silent=FALSE) {
  
  if (inherits(OM,'hist')) 
    OM <- OM@OM
  
  # Populate OM if neccessary
  OM <- PopulateOM(OM, silent = TRUE)
  
  # New `popdynamics` object that will be return
  EquilibriumUnfished <- new('popdynamics')
  
  # Calculate unfished Number-at-Age
  UnfishedNumberAtAge <- CalcUnfishedNumber(OM)
  EquilibriumUnfished@Number <- UnfishedNumberAtAge |> ReduceDims()
  
  # Calculate unfished Spawning Number-at-Age (only different if SpawnFrac > 0)
  UnfishedSpawnNumberAtAge <- CalcUnfishedNumber(OM, SP=TRUE) |> ReduceDims()
  
  # Biomass 
  WeightAtAge <- purrr::map(OM@Stock, \(x) {
    x@Weight@MeanAtAge 
  })
  
  EquilibriumUnfished@Biomass <- purrr::map2(UnfishedNumberAtAge, WeightAtAge, ArrayMultiply) |>
    purrr::map(\(x) apply(x, c('Sim', 'Year'), sum)) |>  # sum over ages within each Stock
    List2Array('Stock') |> 
    aperm(c('Sim', 'Stock', 'Year')) |>
    ReduceDims()
  
  # SBiomass 
  MaturityAtAge <- purrr::map(OM@Stock, \(x) {
    x@Maturity@MeanAtAge 
  })
  EquilibriumUnfished@SBiomass <- purrr::map2(UnfishedSpawnNumberAtAge, WeightAtAge, ArrayMultiply) |> 
    purrr::map2(MaturityAtAge, ArrayMultiply) |>
    purrr::map(\(x) apply(x, c('Sim', 'Year'), sum)) |>
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'Year')) |>
    ReduceDims()
  
  
  # SProduction 
  FecundityAtAge <- purrr::map(OM@Stock, \(x) {
    x@Fecundity@MeanAtAge 
  }) 
  
  EquilibriumUnfished@SProduction <- purrr::map2(UnfishedSpawnNumberAtAge, FecundityAtAge, ArrayMultiply) |>
    purrr::map(\(x) apply(x, c('Sim', 'Year'), sum)) |>
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'Year')) |>
    ReduceDims()
  
  # apply SPFrom for SProduction
  stockNames <- StockNames(OM)
  
  if (length(stockNames) > 1) {
    for (st in seq_along(stockNames)) {
      SPFrom <- OM@Stock[[st]]@SRR@SPFrom
      if (!is.null(SPFrom)) {
        ind <- match(SPFrom, stockNames)
        EquilibriumUnfished@SProduction[,st,] <- EquilibriumUnfished@SProduction[,ind,]
      }
    }
  }

  
  if (!silent) {
    cli::cli_alert_success("Calculated Equilibrium Unfished Conditions ")
  }
    
  EquilibriumUnfished
}
