#' Populate a Retention Object
#'
#' Populates a `Retention` object with mean-at-age, mean-at-length, or 
#' mean-at-weight values.
#'
#' @param Retention A [Retention()] object to populate.
#' @param Ages An [Ages()] object defining age classes.
#' @param Length A [Length()] object. Required if the retention model depends 
#'   on length-at-age.
#' @param Weight A [Weight()] object. Required if the retention model depends 
#'   on weight-at-age.
#' @param Maturity A [Maturity()] object. Required if the retention model uses
#'   relative selectivity.
#' @param Selectivity A populated [Selectivity()] object. Optional, but
#'   recommended whenever the retention model is defined at length or weight:
#'   used to weight the age-length key when collapsing retention-at-length (or
#'   -weight) to retention-at-age, so that the result reflects the length
#'   distribution of fish the fleet actually catches at each age rather than
#'   the unconditional population length distribution at age. Ignored for
#'   retention models defined directly at age.
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
#'
#' @details
#' `PopulateRetention()` handles population of retention-at-age, retention-
#' at-length, or retention-at-weight. Steps include:
#' 
#' * Resolving the retention model class
#' * Generating mean retention at age, length, or weight as appropriate
#' * Converting between mean-at-length/weight and mean-at-age
#' * Adding stochastic variation via `.PopulateRandom()`
#' * Adding an 'Area' dimension and setting dimension names
#'
#' @return
#' A populated [Retention()] object.
#'
#' @examples
#' \dontrun{
#' Ret <- Retention()
#' Ret_pop <- PopulateRetention(
#'   Ret, Ages = Ages, Length = Length, Weight = Weight, 
#'   Maturity = Maturity, Years = 2000:2025, nSim = 5
#' )
#' }
#'
#' @export
PopulateRetention <- function(Retention,
                              Ages = NULL,
                              Length = NULL,
                              Weight = NULL,
                              Maturity = NULL,
                              Selectivity = NULL,
                              nSim = 5,
                              Years = NULL,
                              nArea = 1,
                              CalcAtLength = TRUE,
                              seed = NULL,
                              silent = FALSE,
                              force = FALSE,
                              replace = FALSE,
                              ASKOverride = NULL) {

  argList <- list(Ages, Length, Years, nSim, CalcAtLength, seed, Selectivity)
  
  Ages  <- DefaultAges(Ages)
  Years <- DefaultYears(Years)
  nSim  <- .GetNSim(Retention, nSim)
  
  if (.CheckDigest(Retention, argList) & !force) 
    return(Retention)
  
  # Default: fully retained if object is empty
  if (EmptyObject(Retention)) {
    Retention@MeanAtAge <- array(1, dim = c(1, length(Ages@Classes), 1, 1))
    dimnames(Retention@MeanAtAge) <- list(
      Sim = 1,
      Age = Ages@Classes,
      Year = Years[1],
      Area = 1
    )
    
    Retention@Classes <- Length@Classes
    Retention@MeanAtLength <- array(1, dim = c(1, length(Retention@Classes), 1, 1))
    dimnames(Retention@MeanAtLength) <- list(
      Sim = 1,
      Class = Retention@Classes,
      Year = Years[1],
      Area = 1
    )
    
    return(.SetDigest(Retention, argList))
  }
  
  .SetSeed(seed)
  
  Retention@Pars <- .StructurePars(Pars = Retention@Pars, nSim, Years)
  Retention@Model <- .FindModel(Retention)
  
  ModelClass <- .GetModelClass(Retention@Model)
  
  if (!is.null(ModelClass)) {
    if (Retention@isRel) {
      .CheckRequiredObject(Maturity, "maturity", "Maturity")
      .CheckDependency(Maturity, "MeanAtAge", "Maturity", "Retention")
      L50 <- .FindL50(Maturity)
      Retention@Pars$LR5 <- ArrayMultiply(Retention@Pars$LR5, L50)
      Retention@Pars$LFR <- ArrayMultiply(Retention@Pars$LFR, L50)
    }
    
    if (grepl("at-Length", ModelClass)) {
      Retention <- .PopulateMeanAtLength(
        object = Retention, 
        Length = Length, 
        Years = Years, 
        Ages = Ages, 
        seed = seed,
        silent = silent
      )
  
    } else if (grepl("at-Weight", ModelClass)) {
      Retention <- .PopulateMeanAtWeight(
        object = Retention, 
        Weight = Weight, 
        Years = Years, 
        Ages = Ages, 
        seed = seed,
        silent = silent
      )
      

    } else if (grepl("at-Age", ModelClass)) {
      Retention <- .PopulateMeanAtAge(
        object = Retention, 
        Ages= Ages, 
        Years = Years)
    }
  }
  
  hasSelectivity <- !is.null(Selectivity) &&
    (!is.null(Selectivity@MeanAtLength) || !is.null(Selectivity@MeanAtWeight))

  if (is.null(Retention@MeanAtAge) && !is.null(Retention@MeanAtLength) && hasSelectivity) {
    Retention <- .WeightedAtSize2AtAge(Retention, Selectivity, Length)
  } else if (is.null(Retention@MeanAtAge) && !is.null(Retention@MeanAtWeight) && hasSelectivity) {
    Retention <- .WeightedAtSize2AtAge(Retention, Selectivity, Weight)
  } else {
    Retention <- .MeanAtLength2MeanAtAge(Retention, Length, max1 = FALSE)
    Retention <- .MeanAtWeight2MeanAtAge(Retention, Weight, max1 = FALSE)
  }
  Retention <- .AddAtAgeDimnames(Retention, Ages, Years)
  
  if (CalcAtLength && !is.null(Length@ALK)) {
    Retention <- .MeanAtAge2MeanAtLength(
      object = Retention, 
      Length = Length, 
      replace = replace, 
      Years = Years,
      ASK = ASKOverride)
  }
  
  # Add Area dimension
  Retention@MeanAtLength <- AddDimension(Retention@MeanAtLength, "Area")
  Retention@MeanAtWeight <- AddDimension(Retention@MeanAtWeight, "Area")
  Retention@MeanAtAge    <- AddDimension(Retention@MeanAtAge, "Area")
  
  # Add dimension names if missing
  Retention <- .AddAtAgeDimnames(Retention, Ages, Years)
  Retention <- .AddAtLengthDimnames(Retention, Years)
  Retention <- .AddAtWeightDimnames(Retention, Years)
  
  .SetDigest(.SetAgeDimnames(Retention, Ages), argList)
}
