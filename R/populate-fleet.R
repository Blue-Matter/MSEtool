#' Populate a Fleet Object
#'
#' Populate a `Fleet` object by generating effort, catchability, selectivity,
#' retention, discard mortality, and spatial closures for a fleet.
#'
#' @param Fleet A [Fleet()] object to populate.
#' @param Stock A populated [Stock()] object.
#' @param seed Integer. Random seed used for stochastic generation.
#' @param silent Logical. If `TRUE`, suppress informational messages.
#' @param force Logical. If `TRUE`, force re-population even if digest indicates
#' the object is current.
#'
#' @details
#' `PopulateFleet()` performs the following steps:
#'
#' * Populates fleet effort for historical years.
#' * Populates fleet catchability for historical and projected years.
#' * Populates selectivity and retention objects.
#' * Populates discard mortality and spatial closures.
#' * Initializes fleet-specific weight-at-age if not already defined.
#'
#' @return
#' A populated [Fleet()] object.
#' 
#' @seealso [Populate()], [PopulateStock()]
#'
#' @examples
#' \dontrun{
#' F <- Fleet()
#' F_pop <- PopulateFleet(Fleet = F, Stock = Stock, seed = 123, nSim = 50)
#' }
#'
#' @export
PopulateFleet <- function(Fleet,
                          Stock,
                          seed = 103,
                          silent = FALSE,
                          force = FALSE) {
  
  Ages <- Stock@Ages
  Length <- Stock@Length
  Weight <- Stock@Weight
  Maturity <- Stock@Maturity
  RelativeSize <- Stock@Spatial@RelativeSize
  
  Fleet@CurrentYear <- Stock@CurrentYear
  Fleet@nSim <- Stock@nSim
  Fleet@Years <- Stock@Years
  Fleet@nYear <- Stock@nYear
  Fleet@pYear <- Stock@pYear
  Fleet@Seasons <- Stock@Seasons
  
  Fleet@Years <- CalcYears(
    nYear = Stock@nYear,
    pYear = Stock@pYear,
    CurrentYear = Stock@CurrentYear,
    Seasons = Stock@Seasons
  )
  
  nSim <- Fleet@nSim
  Years <- Fleet@Years
  HistYears <- Years(Fleet, "Historical")
  ProjYears <- Years[!Years %in% HistYears]
  nArea <- ncol(RelativeSize)
  
  argList <- list(Ages, Length, Weight, RelativeSize, nSim, Years, seed)
  
  if (EmptyObject(Fleet)) {
    return(Fleet)
  }
  
  if (CheckDigest(Fleet, argList) & !force) {
    return(Fleet)
  }
  
  SetSeed(seed)
  
  Fleet@Effort <- PopulateEffort(
    Effort = Fleet@Effort,
    HistYears = HistYears,
    nArea = nArea,
    nSim = nSim,
    seed = seed
  )
  
  Fleet@Catchability <- PopulateCatchability(
    Catchability = Fleet@Catchability,
    nSim = nSim,
    HistYears = HistYears,
    ProjYears = ProjYears,
    seed = seed,
    silent = silent
  )
  
  Fleet@Selectivity <- PopulateSelectivity(
    Selectivity = Fleet@Selectivity,
    Ages = Ages,
    Length = Length,
    Weight = Weight,
    Maturity = Maturity,
    nSim = nSim,
    Years = Years,
    nArea = nArea,
    CalcAtLength = TRUE,
    seed = seed,
    silent = silent
  )
  
  Fleet@Retention <- PopulateRetention(
    Retention = Fleet@Retention,
    Ages = Ages,
    Length = Length,
    Weight = Weight,
    Maturity = Maturity,
    nSim = nSim,
    Years = Years,
    nArea = nArea,
    CalcAtLength = TRUE,
    seed = seed,
    silent = silent,
    force = force
  )
  
  Fleet@DiscardMortality <- PopulateDiscardMortality(
    DiscardMortality = Fleet@DiscardMortality,
    Ages = Ages,
    Length = Length,
    nSim = nSim,
    Years = Years,
    nArea = nArea,
    CalcAtLength = TRUE,
    seed = seed,
    silent = silent
  )
  
  Fleet@Closure <- PopulateClosure(
    Closure = Fleet@Closure,
    nArea = nArea,
    nSim = nSim,
    Years = Years,
    silent = silent
  )
  
  if (all(is.na(Fleet@WeightFleet))) {
    Fleet@WeightFleet <- Weight@MeanAtAge
  }
  
  SetDigest(Fleet, argList)
}
