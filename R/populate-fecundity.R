#' Populate a Fecundity Object
#'
#' Populate a `Fecundity` object by generating mean-at-age values, adding 
#' stochastic deviations, and optionally calculating fecundity-at-length. Handles
#' models that depend on age, length, or weight.
#'
#' @param Fecundity A [Fecundity()] object to populate.
#' @param Ages An [Ages()] object defining age classes.
#' @param Length A [Length()] object. Required if the fecundity model depends on
#'   length-at-age.
#' @param Weight A [Weight()] object. Required if the fecundity model depends on
#'   weight-at-age.
#' @param Maturity A [Maturity()] object. Required to calculate fecundity as egg
#'   production of mature individuals.
#' @param Years Numeric vector of model years.
#' @param nSim Integer. Number of simulation replicates.
#' @param seed Integer. Random seed used for stochastic generation.
#' @param silent Logical. If `TRUE`, suppress informational messages.
#' @param CalcAtLength Logical. If `TRUE`, calculate fecundity-at-length values
#'   from age-based arrays.
#' @param force Logical. If `TRUE`, force re-population even if the object
#'   digest is unchanged.
#'
#' @details
#' `PopulateFecundity()` handles population of mean fecundity-at-age for a stock.
#' Steps include:
#'
#' * Structuring model parameters across `nSim` and `Years`
#' * Resolving the fecundity model class
#' * Generating mean fecundity at age, or at length if applicable
#' * Converting between mean-at-length and mean-at-age
#' * Multiplying by maturity to represent egg production of mature individuals
#' * Adding stochastic variation via `.PopulateRandom()`
#'
#' @return
#' A populated [Fecundity()] object.
#'
#' @seealso
#' [Populate()], [PopulateStock()], [PopulateLength()], [PopulateWeight()],
#' [PopulateMaturity()]
#'
#' @examples
#' \dontrun{
#' # Assuming `Ages`, `Length`, `Weight`, and `Maturity` objects exist
#' F <- Fecundity()
#' F_pop <- PopulateFecundity(
#'   F,
#'   Ages = Ages,
#'   Length = Length,
#'   Weight = Weight,
#'   Maturity = Maturity,
#'   Years = 2000:2025,
#'   nSim = 5,
#'   seed = 123,
#'   CalcAtLength = TRUE
#' )
#' }
#'
#' @export
PopulateFecundity <- function(Fecundity,
                              Ages = NULL,
                              Length = NULL,
                              Weight = NULL,
                              Maturity = NULL,
                              Years = NULL,
                              nSim = 5,
                              seed = NULL,
                              silent = FALSE,
                              CalcAtLength = FALSE,
                              force = FALSE) {
  Ages  <- DefaultAges(Ages)
  Years <- DefaultYears(Years)
  nSim  <- .GetNSim(Fecundity, nSim)
  
  argList <- list(Ages, Length, Weight, Maturity, nSim, Years, CalcAtLength, seed)
  
  if (EmptyObject(Fecundity)) {
    .CheckRequiredObject(Ages, "ages", "Ages")
    .CheckRequiredObject(Weight, "weight", "Weight")
    .CheckRequiredObject(Length, "length", "Length")
    .CheckRequiredObject(Maturity, "maturity", "Maturity")
    
    Weight <- PopulateWeight(Weight,
                             Ages = Ages,
                             Length = Length,
                             Years = Years,
                             nSim = nSim,
                             seed = seed,
                             AWK = FALSE
    )
    
    Maturity <- PopulateMaturity(
      Maturity,
      Ages = Ages,
      Length = Length,
      Weight = Weight,
      Years = Years,
      nSim = nSim,
      seed = seed
    )
    
    Fecundity@MeanAtAge <- ArrayMultiply(
      array1 = Weight@MeanAtAge,
      array2 = Maturity@MeanAtAge
    )
    
    return(.SetDigest(Fecundity, argList))
  }
  
  if (.CheckDigest(Fecundity, argList) & !force) 
    return(Fecundity)
  
  .SetSeed(seed)
  
  Fecundity@Pars <- .StructurePars(Pars = Fecundity@Pars, nSim, Years)
  Fecundity@Model <- .FindModel(Fecundity)
  
  if (is.null(Fecundity@Model) | all(is.na(Fecundity@Pars))) {
    if (is.null(Fecundity@MeanAtAge)) {
      .CheckRequiredObject(Ages, "ages", "Ages")
      .CheckRequiredObject(Weight, "weight", "Weight")
      .CheckRequiredObject(Length, "length", "Length")
      .CheckRequiredObject(Maturity, "maturity", "Maturity")
      
      Weight <- PopulateWeight(Weight,
                               Ages,
                               Length,
                               Years,
                               nSim,
                               seed,
                               AWK = FALSE
      )
      
      Maturity <- PopulateMaturity(
        Maturity,
        Ages,
        Length,
        Weight,
        Years,
        nSim,
        seed
      )
      
      Fecundity@MeanAtAge <- ArrayMultiply(
        array1 = Weight@MeanAtAge,
        array2 = Maturity@MeanAtAge
      )
      
      return(.SetDigest(Fecundity, argList))
    }
  }
  
  ModelClass <- .GetModelClass(Fecundity@Model)
  if (!is.null(ModelClass)) {
    if (grepl("at-Length", .GetModelClass(Fecundity@Model))) {
      .CheckRequiredObject(Length, "length", "Length")
      Length <- PopulateLength(Length, 
                               Ages = Ages, 
                               Years = Years, 
                               nSim = nSim)
      
      Fecundity <- .PopulateMeanAtLength(
        object = Fecundity, 
        Length = Length, 
        Years = Years, 
        Ages = Ages, 
        seed = seed,
        silent = silent
      )
    } else {
      Fecundity <- .PopulateMeanAtAge(Fecundity, Ages, Years)
    }
  }
  
  Fecundity <- .MeanAtLength2MeanAtAge(Fecundity, Length)

  Fecundity <- .AddAtAgeDimnames(Fecundity, Ages, Years)
  
  if (CalcAtLength) 
    Fecundity <- .MeanAtAge2MeanAtLength(Fecundity, Length)
  
  Fecundity <- .AddAtLengthDimnames(Fecundity, Years, 'Fecundity')
  
  .SetDigest(.SetAgeDimnames(Fecundity, Ages), argList)
}
