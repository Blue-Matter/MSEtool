#' Populate a Maturity Object
#'
#' Populate a `Maturity` object by generating maturity-at-age or maturity-at-weight
#' values for a stock, expanding parameters across simulation replicates and years,
#' and optionally calculating values at length.
#'
#' @param Maturity A [Maturity()] object to populate.
#' @param Ages An [Ages()] object defining age classes. 
#' @param Length A [Length()] object. Required if the maturity model depends on
#'   length-at-age.
#' @param Weight A [Weight()] object. Required if the maturity model depends on
#'   weight-at-age.
#' @param Years Numeric vector of model years. 
#' @param nSim Integer. Number of simulation replicates.
#' @param seed Integer. Random seed used for stochastic generation.
#' @param silent Logical. If `TRUE`, suppress informational messages.
#' @param CalcAtLength Logical. If `TRUE`, calculate maturity-at-length values
#'   from age-based arrays.
#' @param force Logical. If `TRUE`, force re-population even if the object
#'   digest is unchanged.
#'
#' @details
#' `PopulateMaturity()` handles population of maturity-at-age or maturity-at-weight
#' for a stock. Steps include:
#'
#' * Structuring model parameters across `nSim` and `Years`
#' * Resolving the maturity model class
#' * Generating mean maturity at age, at length, or at weight as appropriate
#' * Converting between mean-at-length/weight and mean-at-age
#' * Adding stochastic variation via `PopulateRandom()`
#' * Setting semelparous array if applicable
#'
#'
#' @return
#' A populated [Maturity()] object.
#'
#' @seealso
#' [Populate()], [PopulateStock()], [PopulateLength()], [PopulateWeight()]
#'
#' @examples
#' \dontrun{
#' M <- PopulateMaturity(
#'   Maturity,
#'   Ages = Ages,
#'   Length = Length,
#'   Weight = Weight,
#'   Years = Years,
#'   nSim = 100,
#'   CalcAtLength = TRUE,
#'   seed = 123
#' )
#' }
#'
#' @export
PopulateMaturity <- function(Maturity,
                             Ages = NULL,
                             Length = NULL,
                             Weight = NULL,
                             Years = NULL,
                             nSim = NULL,
                             seed = NULL,
                             silent = FALSE,
                             CalcAtLength = FALSE,
                             force = FALSE) {
  
  Ages <- DefaultAges(Ages)
  Years <- DefaultYears(Years)
  
  argList <- list(Ages, Length, nSim, Years, CalcAtLength, seed)
  
  if (EmptyObject(Maturity)) {
    # cli::cli_alert_danger('Warning: {.val Maturity} is required but is currently empty')
    cli::cli_abort('{.val Maturity} is required but is currently empty')
    # return(Maturity)
  }
  
  if (CheckDigest(Maturity, argList) & !force) {
    return(Maturity)
  }
  
  SetSeed(seed)
  
  Maturity@Pars <- StructurePars(Pars = Maturity@Pars, nSim, Years)
  Maturity@Model <- FindModel(Maturity)
  Maturity <- PopulateMeanAtAge(object = Maturity, 
                              Ages, 
                              Years,
                              Length)
  
  ModelClass <- getModelClass(Maturity@Model)
  
  if (!is.null(ModelClass)) {
    if (grepl("at-Length", getModelClass(Maturity@Model))) {
      CheckRequiredObject(Length, "length", "Length")
      CheckRequiredObject(Ages, "ages", "Ages")
      Length <- PopulateLength(Length, 
                               Ages = Ages, 
                               Years = Years, 
                               nSim = nSim,
                               ALK = TRUE,
                               seed = seed,
                               silent = silent)
      
      Maturity <- PopulateMeanAtLength(
        object = Maturity, 
        Length = Length, 
        Years = Years, 
        Ages = Ages, 
        seed = seed, 
        silent = silent
      )
      
    } else if (grepl("at-Weight", getModelClass(Maturity@Model))) {
      Maturity <- PopulateMeanAtWeight(
        object = Maturity, 
        Weight = Weight, 
        Years  = Years, 
        Ages   = Ages,
        seed   = seed,
        silent = silent
      )
    } else {
      Maturity <- PopulateMeanAtAge(Maturity, Ages, Years)
    }
  }
  
  Maturity <- MeanAtLength2MeanAtAge(Maturity, Length)
  Maturity <- MeanAtWeight2MeanAtAge(Maturity, Weight)
  
  if (CalcAtLength) {
    Maturity <- MeanAtAge2MeanAtLength(Maturity, Length)
  }
  
  
  # Semelparous
  if (inherits(Maturity@Semelparous, "array")) {} else {
    if (Maturity@Semelparous) {
      Maturity@Semelparous <- Maturity@MeanAtAge
    } else {
      Maturity@Semelparous <- Maturity@MeanAtAge
      Maturity@Semelparous[] <- 0
    }
  }
  
  if (is.null(dimnames(Maturity@MeanAtAge))) {
    dd <- dim(Maturity@MeanAtAge)
    dimnames(Maturity@MeanAtAge) <- list(
      Sim=1:dd[1],
      Age=Ages@Classes[1:dd[2]],
      Year=Years[1:dd[3]]
    )
  }
  
  if (is.null(dimnames(Maturity@Semelparous))) {
    dd <- dim(Maturity@Semelparous)
    dimnames(Maturity@Semelparous) <- list(
      Sim=1:dd[1],
      Age=Ages@Classes[1:dd[2]],
      Year=Years[1:dd[3]]
    )
  }
  
  SetDigest(Maturity, argList)
  
}