
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
#' The values are the equilibrium for the parameters of each year. They are
#' calculated and stored only for the years (historical and projection) in
#' which the parameters affecting them change (see [RefChangeYears()], with
#' `Type = 'Unfished'`); they are constant in between. Accessors such as
#' [SB0()] fill the remaining years from the most recent stored year.
#'
#' @param OM An [OM()] or [Hist()] object.
#' @param silent Logical; if `TRUE`, suppress messages during calculation.
#'
#' @return A [popdynamics-class] object. `Number` is a list of length `nStock`,
#'   each element an array with dimensions `Sim × Age × Year × Area`. `Biomass`,
#'   `SBiomass`, and `SProduction` are arrays with dimensions `Sim × Stock × Year`
#'   (summed over ages and areas). `Year` holds the change-point years only
#'   (for seasonal models, all time steps of each change-point year).
#'
#' @seealso [CalcUnfished_Dynamic()]
#'
#' @export
CalcUnfished_Equilibrium <- function(OM, silent=FALSE) {

  .MsgStep("Calculating equilibrium unfished conditions",
           "Calculated equilibrium unfished conditions", silent)

  if (inherits(OM,'hist')) 
    OM <- OM@OM
  
  # Populate OM if neccessary
  OM <- .PopulateForUnfished(OM)

  # change-point years of the inputs, historical and projection: the values
  # are constant between them (see RefChangeYears())
  Years <- .UnfishedYears(OM)
  AtYears <- \(x) .SubsetYearIfAny(x, Years)

  # New `popdynamics` object that will be return
  EquilibriumUnfished <- new('popdynamics')

  # Calculate unfished Number-at-Age and distribute across areas
  UnfishedNumberAtAge <- CalcUnfishedNumber(OM, Years = Years)
  EquilibriumUnfished@Number <- purrr::map2(
    UnfishedNumberAtAge,
    OM@Stock,
    \(N, Stock) {
      UD <- .UnfishedDistPlusGroup(Stock, N, Years) |>
        .Aperm(c('Sim', 'Age', 'Year', 'Area')) |>
        AtYears()
      ArrayMultiply(N |> AddDimension('Area'), UD) |> ReduceDims()
    }
  )

  # Calculate unfished Spawning Number-at-Age (only different if SpawnFrac > 0)
  UnfishedSpawnNumberAtAge <- CalcUnfishedNumber(OM, SP=TRUE, Years = Years) |> ReduceDims()

  # Biomass
  WeightAtAge <- purrr::map(OM@Stock, \(x) {
    AtYears(x@Weight@MeanAtAge)
  })
  
  EquilibriumUnfished@Biomass <- purrr::map2(UnfishedNumberAtAge, 
                                             WeightAtAge, ArrayMultiply) |>
    purrr::map(\(x) apply(x, c('Sim', 'Year'), sum)) |>  # sum over ages within each Stock
    List2Array('Stock') |> 
    .Aperm(c('Sim', 'Stock', 'Year')) |>
    ReduceDims()
  
  # SBiomass 
  MaturityAtAge <- purrr::map(OM@Stock, \(x) {
    AtYears(x@Maturity@MeanAtAge)
  })
  EquilibriumUnfished@SBiomass <- purrr::map2(UnfishedSpawnNumberAtAge, 
                                              WeightAtAge, ArrayMultiply) |> 
    purrr::map2(MaturityAtAge, ArrayMultiply) |>
    purrr::map(\(x) apply(x, c('Sim', 'Year'), sum)) |>
    List2Array('Stock') |>
    .Aperm(c('Sim', 'Stock', 'Year')) |>
    ReduceDims()
  
  # SProduction 
  FecundityAtAge <- purrr::map(OM@Stock, \(x) {
    AtYears(x@Fecundity@MeanAtAge)
  })
  
  EquilibriumUnfished@SProduction <- purrr::map2(UnfishedSpawnNumberAtAge,
                                                 FecundityAtAge, ArrayMultiply) |>
    purrr::map(\(x) apply(x, c('Sim', 'Year'), sum)) |>
    List2Array('Stock') |>
    .Aperm(c('Sim', 'Stock', 'Year')) |>
    ReduceDims()
  
  # apply SPFrom for SProduction
  stockNames <- StockNames(OM)

  if (length(stockNames) > 1) {
    RawSProduction <- EquilibriumUnfished@SProduction
    for (st in seq_along(stockNames)) {
      w <- .ResolveSPFromWeights(OM@Stock[[st]]@SRR@SPFrom, stockNames, st)
      if (length(w$from) == 1L && identical(w$from, st)) next
      EquilibriumUnfished@SProduction[,st,] <- Reduce(
        `+`,
        purrr::map2(w$from, w$weight, \(idx, wt) wt * RawSProduction[,idx,])
      )
    }
  }


  EquilibriumUnfished
}

# Spatial@UnfishedDist with the plus group weighted by unfished numbers entering from the previous age
.UnfishedDistPlusGroup <- function(Stock, Number, Years) {
  Spatial <- Stock@Spatial
  Mov <- Spatial@Movement
  if (is.null(dim(Mov)) || dim(Mov)[2] < 2L || dim(Mov)[4] < 2L || !isTRUE(Stock@Ages@PlusGroup))
    return(Spatial@UnfishedDist)

  nSim <- Stock@nSim
  AgeClasses <- Stock@Ages@Classes
  Ext <- \(x) Extend(x, nSim = nSim, AgeClasses = AgeClasses, Years = Years)
  Survival <- exp(-Ext(Stock@NaturalMortality@MeanAtAge))
  Semel <- Stock@Maturity@Semelparous
  if (length(Semel))
    Survival <- ArrayMultiply(Survival, 1 - Ext(Semel))

  CalcUnfishedDist(Spatial, Stock@Ages, Years, Number = Number, Survival = Survival)@UnfishedDist
}
