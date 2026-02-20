#' Populate a Natural Mortality Object
#'
#' Populate a `NaturalMortality` object by generating stochastic natural
#' mortality-at-age values, expanding parameters across simulation replicates
#' and years, and optionally calculating values at length.
#'
#' @param NaturalMortality A [NaturalMortality()] object to populate.
#' @param Ages An [Ages()] object defining age classes. 
#' @param Length A [Length()] object. Required if the mortality model depends on
#'   length-at-age.
#' @param Years Numeric vector of model years. 
#' @param nSim Integer. Number of simulation replicates.
#' @param CalcAtLength Logical. If `TRUE`, calculate mortality-at-length values
#'   from age-based arrays.
#' @param seed Integer. Random seed used for stochastic generation.
#' @param silent Logical. If `TRUE`, suppress informational messages.
#' @param force Logical. If `TRUE`, force re-population even if the object
#'   digest is unchanged.
#'
#' @details
#' `PopulateNaturalMortality()` handles natural mortality-at-age population for
#' a stock, including stochastic generation and propagation across simulations
#' and years. Steps include:
#'
#' * Structuring model parameters across `nSim` and `Years`
#' * Resolving the mortality model class
#' * Generating mean natural mortality at age or at length
#' * Converting between mean-at-length and mean-at-age if necessary
#' * Adding stochastic variation via `PopulateRandom()`
#'
#'
#' @return
#' A populated [NaturalMortality()] object.
#'
#' @seealso
#' [Populate()], [PopulateStock()], [PopulateLength()]
#'
#' @examples
#' \dontrun{
#' NM <- PopulateNaturalMortality(
#'   NaturalMortality,
#'   Ages = Ages,
#'   Length = Length,
#'   Years = Years,
#'   nSim = 100,
#'   CalcAtLength = TRUE,
#'   seed = 123
#' )
#' }
#'
#' @export
PopulateNaturalMortality <- function(NaturalMortality,
                                     Ages = NULL,
                                     Length = NULL,
                                     Years = NULL,
                                     nSim = NULL,
                                     CalcAtLength = FALSE,
                                     seed = NULL,
                                     silent = FALSE,
                                     force = FALSE) {
  
  Ages <- DefaultAges(Ages)
  Years <- DefaultYears(Years)
  
  argList <- list(Ages, Length, nSim, Years, CalcAtLength, seed)
  
  if (EmptyObject(NaturalMortality)) {
    cli::cli_abort('{.val NaturalMortality} is required but is currently empty')
    return(NaturalMortality)
  }
  
  if (CheckDigest(NaturalMortality, argList) & !force) {
    return(NaturalMortality)
  }
  
  SetSeed(seed)
  
  NaturalMortality@Pars <- StructurePars(Pars = NaturalMortality@Pars, nSim, Years)
  NaturalMortality@Model <- FindModel(NaturalMortality)
  NaturalMortality <- PopulateMeanAtAge(object = NaturalMortality, 
                                        Ages= Ages, 
                                        Years = Years)
  
  ModelClass <- getModelClass(NaturalMortality@Model)
  if (!is.null(ModelClass)) {
    if (grepl("at-Length", getModelClass(NaturalMortality@Model))) {
      NaturalMortality <- PopulateMeanAtLength(
        object = NaturalMortality, 
        Length = Length, 
        Years = Years, 
        Ages = Ages, 
        seed = seed,
        silent = silent
      )
    } else {
      NaturalMortality <- PopulateMeanAtAge(
        object = NaturalMortality, 
        Ages= Ages, 
        Years = Years)
    }
  }
  
  NaturalMortality <- MeanAtLength2MeanAtAge(NaturalMortality, Length)
  if (CalcAtLength) {
    NaturalMortality <- MeanAtAge2MeanAtLength(NaturalMortality, Length)
  }
  
  
  NaturalMortality <- PopulateRandom(NaturalMortality)
  
  SetDigest(NaturalMortality, argList)
}