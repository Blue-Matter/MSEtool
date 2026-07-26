#' Populate a DiscardMortality Object
#'
#' Populates a `DiscardMortality` object with mean-at-age and mean-at-length 
#' values.
#'
#' @param DiscardMortality A [DiscardMortality()] object to populate.
#' @param Ages An [Ages()] object defining age classes.
#' @param Length A [Length()] object. Required if the model depends on
#'   length-at-age.
#' @param Selectivity A populated [Selectivity()] object. Optional, but
#'   recommended whenever discard mortality is defined at length.
#'   Ignored if either `Selectivity`
#'   or `Retention` is unavailable, or either's `isAtLength` is `FALSE`.
#' @param Retention A populated [Retention()] object. See `Selectivity`.
#' @param nSim Integer. Number of simulation replicates.
#' @param Years Numeric vector of model years.
#' @param nArea Integer. Number of spatial areas.
#' @param CalcAtLength Logical; if `TRUE`, calculates mean-at-length from
#'   age-based arrays.
#' @param seed Integer. Random seed used for stochastic generation.
#' @param silent Logical; if `TRUE`, suppresses informational messages.
#' @param force Logical; if `TRUE`, forces re-population even if digest
#'   indicates object is current.
#' @param replace Used internally.
#' @param ASKOverride Used internally.
#' @details
#' `PopulateDiscardMortality()` handles population of discard mortality at age
#' and optionally at length. Steps include:
#'
#' * Converting mean-at-length to mean-at-age, weighted by
#'   `Selectivity x (1 - Retention)` when both are available and length-native
#'   (see `Selectivity`), otherwise via the plain (unweighted) age-length key
#' * Calculating mean-at-length from age-based arrays if `CalcAtLength = TRUE`
#' * Adding an 'Area' dimension and setting dimension names
#' * Handling empty objects by setting discard mortality to zero
#'
#' @return
#' A populated [DiscardMortality()] object.
#'
#' @examples
#' \dontrun{
#' DM <- DiscardMortality()
#' DM_pop <- PopulateDiscardMortality(
#'   DM, Ages = Ages, Length = Length, Years = 2000:2025, nSim = 5
#' )
#' }
#'
#' @export
PopulateDiscardMortality <- function(DiscardMortality,
                                     Ages = NULL,
                                     Length = NULL,
                                     Selectivity = NULL,
                                     Retention = NULL,
                                     nSim = 5,
                                     Years = NULL,
                                     nArea = 1,
                                     CalcAtLength = TRUE,
                                     seed = NULL,
                                     silent = FALSE,
                                     force = FALSE,
                                     replace = FALSE,
                                     ASKOverride = NULL) {

  argList <- list(Ages, Length, nSim, Years, CalcAtLength, seed, Selectivity, Retention)
  
  Ages  <- DefaultAges(Ages)
  Years <- DefaultYears(Years)
  nSim  <- .GetNSim(DiscardMortality, nSim)
  
  if (.CheckDigest(DiscardMortality, argList) & !force) 
    return(DiscardMortality)
  
  # Default: no discard mortality if object is empty
  if (EmptyObject(DiscardMortality)) {
    DiscardMortality@MeanAtAge <- array(0, dim = c(1, length(Ages@Classes), 1, 1)) 
    dimnames(DiscardMortality@MeanAtAge) <- list(
      Sim = 1,
      Age = Ages@Classes,
      Year = Years[1],
      Area = 1
    )
    
    DiscardMortality@Classes <- Length@Classes
    DiscardMortality@MeanAtLength <- array(0, dim = c(1, length(DiscardMortality@Classes), 1, 1))
    dimnames(DiscardMortality@MeanAtLength) <- list(
      Sim = 1,
      Class = DiscardMortality@Classes,
      Year = Years[1],
      Area = 1
    )
    return(.SetDigest(DiscardMortality, argList))
  }
  
  .SetSeed(seed)

  # Discard mortality applies only to fish caught and released, so the
  # length-to-age conversion should be weighted by the length distribution of
  # the discarded portion of the catch specifically (Selectivity x (1 -
  # Retention))
  hasWeighting <- !is.null(Selectivity) && !is.null(Retention) &&
    !is.null(Selectivity@MeanAtLength) && !is.null(Retention@MeanAtLength) &&
    !identical(Selectivity@isAtLength, FALSE) && !identical(Retention@isAtLength, FALSE)

  if (is.null(DiscardMortality@MeanAtAge) && !is.null(DiscardMortality@MeanAtLength) && hasWeighting) {
    sel <- Selectivity@MeanAtLength
    ret <- Retention@MeanAtLength
    if ('Area' %in% names(dimnames(sel))) sel <- DropDimension(sel, 'Area', warn = FALSE)
    if ('Area' %in% names(dimnames(ret))) ret <- DropDimension(ret, 'Area', warn = FALSE)

    Weighting <- Selectivity
    Weighting@MeanAtLength <- ArrayMultiply(sel, 1 - ret)

    DiscardMortality <- .WeightedAtSize2AtAge(DiscardMortality, Weighting, Length)
  } else {
    DiscardMortality <- .MeanAtLength2MeanAtAge(DiscardMortality, Length)
  }

  DiscardMortality <- .AddAtAgeDimnames(DiscardMortality, Ages, Years)
  
  if (CalcAtLength) 
    DiscardMortality <- .MeanAtAge2MeanAtLength(DiscardMortality, Length,
                                               replace = replace, 
                                               Years=Years,
                                               ASK = ASKOverride)
  
  # Add Area dimension
  DiscardMortality@MeanAtLength <- AddDimension(DiscardMortality@MeanAtLength, "Area")
  DiscardMortality@MeanAtAge <- AddDimension(DiscardMortality@MeanAtAge, "Area")
  
  # Add dimension names if missing
  DiscardMortality <- .AddAtAgeDimnames(DiscardMortality, Ages, Years)
  DiscardMortality <- .AddAtLengthDimnames(DiscardMortality, Years)
  
  .SetDigest(.SetAgeDimnames(DiscardMortality, Ages), argList)
}
