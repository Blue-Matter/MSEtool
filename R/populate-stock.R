#' Populate a Stock Object
#'
#' Populate a `Stock` object by generating stochastic biological quantities,
#' expanding values across simulation replicates and years, and filling
#' derived age- and length-based components.
#'
#' @param Stock A [Stock()] object to populate.
#' @param nYear Integer. Number of historical years.
#' @param pYear Integer. Number of projection years.
#' @param CurrentYear Numeric or character. Calendar year of the final
#'   historical year. Defaults to the current system year if `NULL`.
#' @param nSim Integer. Number of simulation replicates.
#' @param Seasons Integer. Number of seasons per year.
#' @param ALK Logical. If `TRUE`, populate age–length keys.
#' @param AWK Logical. If `TRUE`, populate age–weight keys.
#' @param seed Integer. Random seed used for stochastic generation.
#' @param silent Logical. If `TRUE`, suppress informational messages.
#' @param force Logical. If `TRUE`, force re-population even if the object
#'   digest is unchanged.
#' @param CalcAtLength Logical. If `TRUE`, calculate at-length quantities
#'   from at-age arrays where applicable
#'
#' @details
#' `PopulateStock()` coordinates population of all biological components
#' associated with a stock, including:
#'
#' * Length-at-age via [PopulateLength()]
#' * Weight-at-age via [PopulateWeight()]
#' * Natural mortality via [PopulateNaturalMortality()]
#' * Maturity via [PopulateMaturity()]
#' * Fecundity via [PopulateFecundity()]
#' * Stock–recruitment via [PopulateSRR()]
#' * Spatial structure via [PopulateSpatial()]
#' * Initial depletion via [PopulateDepletion()]
#'
#'
#' To avoid unnecessary recomputation, a digest of the stock object and key
#' arguments is checked before population. If the digest is unchanged and
#' `force = FALSE`, the input object is returned unchanged.
#'
#' @return
#' A populated [Stock()] object.
#'
#' @seealso
#' [Populate()], [PopulateOM()]
#'
#' @examples
#' \dontrun{
#' Stock <- PopulateStock(
#'   Stock,
#'   nYear = 40,
#'   pYear = 20,
#'   nSim = 100,
#'   seed = 123
#' )
#'
#' }
#'
#' @export
PopulateStock <- function(Stock,
                          nYear,
                          pYear,
                          CurrentYear = NULL,
                          nSim = NULL,
                          Seasons = 1,
                          ALK = TRUE,
                          AWK = TRUE,
                          seed = 102,
                          silent = FALSE,
                          force = FALSE,
                          CalcAtLength = TRUE) {
  
  if (is.null(seed)) {
    seed <- 102
  }
  
  argList <- list(seed, ALK, AWK, nYear, pYear, CurrentYear, nSim)
  if (EmptyObject(Stock)) {
    return(Stock)
  }
  
  if (CheckDigest(Stock, argList) & !force) {
    return(Stock)
  }
  
  if (is.null(CurrentYear)) {
    CurrentYear <- as.numeric(format(Sys.Date(), "%Y"))
  }
  Stock@nYear <- nYear
  Stock@pYear <- pYear
  Stock@CurrentYear <- CurrentYear
  Stock@nSim <- nSim
  
  SetSeed(seed)
  
  Stock@Years <- CalcYears(
    nYear = Stock@nYear,
    pYear = Stock@pYear,
    CurrentYear = Stock@CurrentYear,
    Seasons = Stock@Seasons
  )
  
  Years <- Stock@Years
  Stock@Ages@Classes <- CalcAgeClasses(Stock@Ages)
  
  Stock@Length <- PopulateLength(
    Length = Stock@Length,
    Ages = Stock@Ages,
    Years = Years,
    nSim = nSim,
    ALK = ALK,
    seed = seed + 1,
    silent = silent,
    force = force
  )
  
  Stock@Weight <- PopulateWeight(
    Weight = Stock@Weight,
    Ages = Stock@Ages,
    Length = Stock@Length,
    Years = Years,
    nSim = nSim,
    AWK = AWK,
    seed = seed + 2,
    silent = silent,
    force = force,
    CalcAtLength = CalcAtLength
  )
  
  Stock@NaturalMortality <- PopulateNaturalMortality(
    NaturalMortality = Stock@NaturalMortality,
    Ages = Stock@Ages,
    Length = Stock@Length,
    Years = Years,
    nSim = nSim,
    seed = seed + 3,
    silent = silent,
    force = force,
    CalcAtLength = CalcAtLength
  )
  
  Stock@Maturity <- PopulateMaturity(
    Maturity = Stock@Maturity,
    Ages = Stock@Ages,
    Length = Stock@Length,
    Weight = Stock@Weight,
    Years = Years,
    nSim = nSim,
    seed = seed + 4,
    silent = silent,
    force = force,
    CalcAtLength = CalcAtLength
  )
  
  Stock@Fecundity <- PopulateFecundity(
    Fecundity = Stock@Fecundity,
    Ages = Stock@Ages,
    Length = Stock@Length,
    Weight = Stock@Weight,
    Maturity = Stock@Maturity,
    Years = Years,
    nSim = nSim,
    seed = seed + 5,
    silent = silent,
    force = force,
    CalcAtLength = CalcAtLength
  )
  
  Stock@SRR <- PopulateSRR(
    SRR = Stock@SRR,
    Ages = Stock@Ages,
    CurrentYear = Stock@CurrentYear,
    Years = Years,
    nSim = nSim,
    seed = seed + 6,
    silent = silent
  )
  
  Stock@Spatial <- PopulateSpatial(
    Spatial = Stock@Spatial,
    Ages = Stock@Ages,
    Years = Years,
    nSim = nSim,
    seed = seed + 7,
    silent = silent
  )
  
  Stock@Depletion <- PopulateDepletion(
    Depletion = Stock@Depletion,
    nSim = nSim,
    seed = seed + 8,
    silent = silent
  )
  
  SetDigest(Stock, argList)
}