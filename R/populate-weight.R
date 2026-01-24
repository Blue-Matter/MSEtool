#' Populate a Weight-at-Age Object
#'
#' Populate a `Weight` object by generating stochastic weight-at-age values,
#' expanding parameters across simulation replicates and years, and optionally
#' constructing age–weight keys.
#'
#' @param Weight A [Weight()] object to populate.
#' @param Ages An [Ages()] object defining age classes. If `NULL`, defaults
#'   are inferred from `Weight`.
#' @param Length A [Length()] object. Required if the weight model depends on
#'   length-at-age.
#' @param Years Numeric vector of model years. If `NULL`, defaults are inferred
#'   from `Weight`.
#' @param nSim Integer. Number of simulation replicates.
#' @param AWK Logical. If `TRUE`, populate age–weight keys.
#' @param CalcAtLength Logical. If `TRUE`, calculate weight-at-length values
#'   from weight-at-age arrays.
#' @param seed Integer. Random seed used for stochastic generation.
#' @param silent Logical. If `TRUE`, suppress informational messages.
#' @param force Logical. If `TRUE`, force re-population even if the object
#'   digest is unchanged.
#'
#' @details
#' `PopulateWeight()` handles weight-at-age population for a stock,
#' including stochastic generation and propagation across simulations and years.
#' Steps include:
#'
#' * Structuring model parameters across `nSim` and `Years`
#' * Resolving the weight model class
#' * Generating mean weight-at-age or weight-at-length values
#' * Converting between mean-at-length and mean-at-age as needed
#' * Optionally constructing age–weight keys if `AWK = TRUE`
#' * Generating random variation and coefficients of variation
#'
#'
#' @return
#' A populated [Weight()] object.
#'
#' @seealso
#' [Populate()], [PopulateStock()], [PopulateLength()]
#'
#' @examples
#' \dontrun{
#' Weight <- PopulateWeight(
#'   Weight,
#'   Ages = Ages,
#'   Length = Length,
#'   Years = Years,
#'   nSim = 100,
#'   AWK = TRUE,
#'   CalcAtLength = TRUE,
#'   seed = 123
#' )
#' }
#'
#' @export
PopulateWeight <- function(Weight,
                           Ages = NULL,
                           Length = NULL,
                           Years = NULL,
                           nSim = NULL,
                           AWK = FALSE,
                           CalcAtLength = FALSE,
                           seed = NULL,
                           silent = FALSE,
                           force = FALSE) {
  
  Ages <- DefaultAges(Ages)
  Years <- DefaultYears(Years)
  
  argList <- list(
    Ages, Length, nSim, Years, AWK,
    CalcAtLength, seed
  )
  
  if (EmptyObject(Weight)) {
    return(Weight)
  }
  
  if (CheckDigest(Weight, argList) & !force) {
    return(Weight)
  }
  
  SetSeed(seed)
  
  Weight@Pars <- StructurePars(Pars = Weight@Pars, nSim, Years)
  Weight@Model <- FindModel(Weight)
  
  ModelClass <- getModelClass(Weight@Model)
  if (!is.null(ModelClass)) {
    if (grepl("at-Length", getModelClass(Weight@Model))) {
      CheckRequiredObject(Length, "length", "Length")
      # chk <- Check(Length, silent=TRUE)
      # if (!chk@populated) {
      CheckRequiredObject(Ages, "ages", "Ages")
      Length <- PopulateLength(Length, Ages, Years, nSim, seed, AWK = TRUE, silent)
      # }
      Weight <- PopulateMeanAtLength(
        Weight, Length, Years, Ages,
        nSim, seed, silent
      )
    } else {
      Weight <- PopulateMeanAtAge(Weight, Ages, Years, Length)
    }
  }
  
  Weight <- MeanAtLength2MeanAtAge(Weight, Length)
  
  if (CalcAtLength) {
    Weight <- MeanAtAge2MeanAtLength(Weight, Length)
  }
  
  Weight <- PopulateRandom(Weight)
  Weight@CVatAge <- StructureCV(Weight@CVatAge, nSim)
  dd <- dim(Weight@CVatAge)
  if (!is.null(dd)) {
    dimnames(Weight@CVatAge) <- list(
      Sim = (1:nSim)[1:dd[1]],
      Age = Ages@Classes[1:dd[2]],
      Year = Years[1:dd[3]]
    )
  }
  if (is.null(Weight@CVatAge)) {
    AWK <- FALSE
  }
  
  if (is.null(dimnames(Weight@MeanAtAge))) {
    dd <- dim(Weight@MeanAtAge)
    dimnames(Weight@MeanAtAge) <- list(
      Sim=1:dd[1],
      Age=Ages@Classes[1:dd[2]],
      Year=Years[1:dd[3]]
    )
  }
  
  if (AWK) {
    Weight <- PopulateClasses(Weight)
    Weight <- PopulateASK(Weight, Ages, Years, silent, type = "Weight")
  }
  
  SetDigest(Weight, argList)
}