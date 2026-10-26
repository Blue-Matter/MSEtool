#' Populate a Selectivity Object
#'
#' Populate a `Selectivity` object by generating selectivity-at-age, 
#' selectivity-at-length, or selectivity-at-weight values for a fleet. 
#'
#' @param Selectivity A [Selectivity()] object to populate.
#' @param Ages An [Ages()] object defining age classes.
#' @param Length A [Length()] object. Required if the selectivity model 
#'   depends on length-at-age.
#' @param Weight A [Weight()] object. Required if the selectivity model 
#'   depends on weight-at-age.
#' @param Maturity A [Maturity()] object. Required if `isRel = TRUE` to 
#'   scale selectivity relative to maturity `L50`.
#' @param nSim Integer. Number of simulation replicates.
#' @param Years Numeric vector of model years.
#' @param nArea Integer. Number of spatial areas.
#' @param CalcAtLength Logical. If `TRUE`, calculate selectivity-at-length 
#'   values from age-based arrays.
#' @param seed Integer. Random seed used for stochastic generation.
#' @param silent Logical. If `TRUE`, suppress informational messages.
#' @param CheckMaxValue Logical. If `TRUE`, ensure selectivity has a maximum
#' value of 1 across age classes.
#' @param replace Used interally.
#' @param ASKOverride Used internally.
#'
#' @details
#' `PopulateSelectivity()` performs the following steps:
#'
#' * Structures selectivity parameters across simulation replicates, years, 
#'   and areas.
#' * Resolves the selectivity model class and applies relative scaling to 
#'   maturity if `isRel = TRUE`.
#' * Generates mean selectivity-at-age, selectivity-at-length, or 
#'   selectivity-at-weight depending on the model.
#' * Converts between mean-at-length/weight and mean-at-age.
#' * Ensures maximum selectivity does not exceed 1 if `CheckMaxValue = TRUE`.
#' * Adds an area dimension to all arrays and sets dimension names.
#'
#' @return
#' A populated [Selectivity()] object.
#'
#' @examples
#' Ages     <- AlbacoreExStock@@Ages
#' Length   <- PopulateLength(AlbacoreExStock@@Length, Ages = Ages, Years = 2000:2025, nSim = 10)
#' Weight   <- PopulateWeight(AlbacoreExStock@@Weight, Ages = Ages, Length = Length,
#'   Years = 2000:2025, nSim = 10)
#' Maturity <- PopulateMaturity(AlbacoreExStock@@Maturity, Ages = Ages,
#'   Length = Length, Weight = Weight, Years = 2000:2025, nSim = 10)
#' S_pop <- PopulateSelectivity(
#'   Selectivity = AsympExFleet@@Selectivity,
#'   Ages = Ages,
#'   Length = Length,
#'   Weight = Weight,
#'   Maturity = Maturity,
#'   Years = 2000:2025,
#'   nSim = 10
#' )
#'
#' @export
PopulateSelectivity <- function(Selectivity,
                                Ages = NULL,
                                Length = NULL,
                                Weight = NULL,
                                Maturity = NULL,
                                nSim = 5,
                                Years = NULL,
                                nArea = 1,
                                CalcAtLength = TRUE,
                                seed = NULL,
                                silent = FALSE,
                                CheckMaxValue = TRUE,
                                replace = FALSE,
                                ASKOverride = NULL) {
  
  argList <- list(
    Ages, Length, Weight, Years, nArea, nSim, CalcAtLength, seed
  )
  
  nSim  <- .GetNSim(Selectivity, nSim)
  Years <- DefaultYears(Years)
  
  if (EmptyObject(Selectivity)) 
    cli::cli_abort('{.val Selectivity} is required but is currently empty')

  if (.CheckDigest(Selectivity, argList))  return(Selectivity)
  
  .SetSeed(seed)
  
  Selectivity@Pars <- .StructurePars(
    Pars = Selectivity@Pars,
    nSim = nSim,
    Years = Years,
    nArea = nArea
  )
  Selectivity@Model <- .FindModel(Selectivity)
  
  ModelClass <- .GetModelClass(Selectivity@Model)
  
  if (!is.null(ModelClass)) {
    Selectivity <- .ScaleRelPars(Selectivity, Maturity, ModelClass, Ages, Length,
                                 Weight, Years, nSim, "Selectivity")

    if (grepl("at-Length", ModelClass)) {
      Length <- PopulateLength(Length, Ages, Years, nSim)
      Selectivity <- .PopulateMeanAtLength(
        object = Selectivity,
        Length = Length,
        Years = Years,
        Ages = Ages,
        seed = seed,
        silent = silent
      )
    } else if (grepl("at-Weight", ModelClass)) {
      Weight <- PopulateWeight(Weight, Ages, Length,Years, nSim)
      
      Selectivity <- .PopulateMeanAtWeight(
        object = Selectivity,
        Weight = Weight,
        Years = Years,
        Ages = Ages,
        seed = seed,
        silent = silent
      )
    } else if (grepl("at-Age", ModelClass)) {
      Selectivity <- .PopulateMeanAtAge(
        object = Selectivity,
        Ages = Ages,
        Years = Years,
        Length = Length
      )
    }
  }
  
  Selectivity <- .MeanAtLength2MeanAtAge(
    object = Selectivity,
    Length = Length,
    max1 = TRUE
  )
  Selectivity <- .MeanAtWeight2MeanAtAge(
    object = Selectivity,
    Weight = Weight,
    max1 = TRUE
  )
  
  Selectivity <- .AddAtAgeDimnames(Selectivity, Ages, Years)
  
  if (CalcAtLength && is.null(Selectivity@MeanAtWeight) && !is.null(Length@ALK)) {
    Selectivity <- .MeanAtAge2MeanAtLength(
      object = Selectivity,
      Length = Length,
      replace = replace,
      Years = Years,
      ASK = ASKOverride
    )
  }
  
  if (is.null(Selectivity@MeanAtAge)) 
    cli::cli_abort("{.var Selectivity} requires values for either `Model` & `Pars` or `MeanAtAge`")
  
  if (CheckMaxValue) 
    Selectivity <- .CheckSelectivityMaximum(Selectivity)
  
  Selectivity@MeanAtLength <- AddDimension(Selectivity@MeanAtLength, "Area")
  Selectivity@MeanAtWeight <- AddDimension(Selectivity@MeanAtWeight, "Area")
  Selectivity@MeanAtAge    <- AddDimension(Selectivity@MeanAtAge, "Area")
  
  # Add dimension names if missing
  Selectivity <- .AddAtAgeDimnames(Selectivity, Ages, Years)
  Selectivity <- .AddAtLengthDimnames(Selectivity, Years)
  Selectivity <- .AddAtWeightDimnames(Selectivity, Years)

  
  .SetDigest(.SetAgeDimnames(Selectivity, Ages), argList)
}

.RelLengthPars <- c('L5', 'LFS', 'SL50', 'SL50_95', 'SL',
                    'LR5', 'LFR', 'RL50', 'RL50_95', 'RL')

# multiply the length parameters of a relative (`isRel`) selectivity or retention model by maturity L50
.ScaleRelPars <- function(object, Maturity, ModelClass, Ages, Length, Weight, Years, nSim,
                          what = c('Selectivity', 'Retention')) {
  what <- match.arg(what)
  if (!isTRUE(as.logical(object@isRel))) return(object)

  RelPars <- intersect(names(object@Pars), .RelLengthPars)
  isLength <- any(ModelClass == "function") || any(grepl("at-Length", ModelClass))
  if (!isLength || !length(RelPars))
    cli::cli_abort(c(
      "x" = "{.code isRel = TRUE} requires an at-length {what} model.",
      "i" = "Relative parameters are multiples of maturity {.val L50}: {.val {(.RelLengthPars)}}.",
      "i" = "Set {.code isRel = FALSE} and supply absolute values for other models."
    ), call = NULL)

  .CheckRequiredObject(Maturity, "maturity", "Maturity")
  Maturity <- PopulateMaturity(Maturity, Ages, Length, Weight, Years, nSim)
  .CheckDependency(Maturity, "MeanAtAge", "Maturity", what)
  L50 <- .FindL50(Maturity)

  for (p in RelPars) {
    L50p <- if (!is.null(dimnames(object@Pars[[p]])$Area)) AddDimension(L50, 'Area') else L50
    object@Pars[[p]] <- ArrayMultiply(object@Pars[[p]], L50p)
  }
  object@isRel <- FALSE
  object
}

.FindL50Vec <- function(prob_vec) {
  classes <- names(prob_vec) |> as.numeric()
  .LinInterp(prob_vec, y=classes, 0.5)
}

.FindL50 <- function(Maturity) {
  if (!is.null(Maturity@Pars$L50)) {
    return(Maturity@Pars$L50)
  }
  
  MaturityAtLength <- Maturity@MeanAtLength
  if (is.null(MaturityAtLength)) {
    cli::cli_abort("Values required for `Maturity@MeanAtLength` if `isRel == TRUE`")
  }
  
  apply(Maturity@MeanAtLength, c('Sim', 'Year'), .FindL50Vec)
}
