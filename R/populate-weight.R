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
#' Ages   <- AlbacoreExStock@@Ages
#' Length <- PopulateLength(AlbacoreExStock@@Length, Ages = Ages, Years = 2000:2025, nSim = 10)
#' Weight <- PopulateWeight(
#'   AlbacoreExStock@@Weight,
#'   Ages = Ages,
#'   Length = Length,
#'   Years = 2000:2025,
#'   nSim = 10,
#'   AWK = TRUE,
#'   CalcAtLength = TRUE,
#'   seed = 123
#' )
#'
#' @export
PopulateWeight <- function(Weight,
                           Ages = NULL,
                           Length = NULL,
                           Years = NULL,
                           nSim = 5,
                           AWK = FALSE,
                           CalcAtLength = TRUE,
                           seed = NULL,
                           silent = FALSE,
                           force = FALSE) {
  
  Ages  <- DefaultAges(Ages)
  Years <- DefaultYears(Years)
  nSim  <- .GetNSim(Weight, nSim)
  
  argList <- list(
    Ages, Length, nSim, Years, AWK,
    CalcAtLength, seed
  )
  
  if (EmptyObject(Weight)) {
    cli::cli_abort('{.val Weight} is required but is currently empty')
    return(Weight)
  }
  
  if (.CheckDigest(Weight, argList) & !force) {
    return(Weight)
  }
  
  .SetSeed(seed)
  Spec <- list(Pars = Weight@Pars, CVatAge = Weight@CVatAge)
  
  Weight@Pars <- .StructurePars(Pars = .AddDefaultPars(Weight), nSim, Years)
  Weight@Model <- .FindModel(Weight)
  Weight <- .PopulateMeanAtAge(object = Weight, 
                              Ages= Ages, 
                              Years = Years,
                              Length = Length)
  
  ModelClass <- .GetModelClass(Weight@Model)
  if (!is.null(ModelClass)) {
    if (grepl("at-Length", .GetModelClass(Weight@Model))) {
      .CheckRequiredObject(Length, "length", "Length")
      # chk <- Check(Length, silent=TRUE)
      # if (!chk@populated) {
      .CheckRequiredObject(Ages, "ages", "Ages")
      Length <- PopulateLength(Length = Length, 
                               Ages = Ages, 
                               Years = Years, 
                               nSim = nSim, 
                               ALK = TRUE, 
                               seed = seed, 
                               silent = silent)
      # }
      Weight <- .PopulateMeanAtLength(
        object = Weight, 
        Length = Length, 
        Years = Years, 
        Ages = Ages, 
        seed = seed,
        silent = silent
      )
    } else {
      Weight <- .PopulateMeanAtAge(
        object = Weight, 
        Ages= Ages, 
        Years = Years,
        Length = Length)
  
    }
  }
  
  Weight <- .MeanAtLength2MeanAtAge(Weight, Length)
  
  if (CalcAtLength) {
    Weight <- .MeanAtAge2MeanAtLength(Weight, Length)
  }
  
  Weight@CVatAge <- .StructureCV(Weight@CVatAge, nSim)
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
  
  Weight <- .AddAtAgeDimnames(Weight, Ages, Years)
  
  if (AWK) {
    Weight <- .PopulateClasses(Weight, .SpecMaxBinWeight(Weight, Spec, Length, Ages, Years))
    Weight <- .PopulateASK(Weight, Ages, silent, type = "Weight")
  }
  
  .SetDigest(.SetAgeDimnames(Weight, Ages), argList)
}

# weight at the top of the length classes bounds weight-at-age for models of weight at length
.SpecMaxBinWeight <- function(Weight, Spec, Length, Ages, Years) {
  Model <- if (is.null(Weight@Model)) NULL else .FindModel(Weight)
  if (is.null(Model) || !"Length" %in% names(formals(Model)))
    return(.SpecMaxBin(Weight, Spec$Pars, Spec$CVatAge, Ages, Years))
  if (is.null(Length@Classes))
    return(NULL)
  BoundLength <- Length
  BoundLength@MeanAtAge <- array(.MaxClassBound(Length@Classes), c(1, 1, 1),
                                 dimnames = list(Sim = 1, Age = Ages@Classes[1], Year = Years[1]))
  .SpecMaxBin(Weight, Spec$Pars, Spec$CVatAge, Ages, Years, Length = BoundLength)
}
