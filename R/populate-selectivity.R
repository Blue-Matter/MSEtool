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
#'   scale selectivity relative to maturity `L50.`
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
#' \dontrun{
#' S <- Selectivity()
#' S_pop <- PopulateSelectivity(
#'   Selectivity = S,
#'   Ages = Ages,
#'   Length = Length,
#'   Weight = Weight,
#'   Maturity = Maturity,
#'   Years = 2000:2025,
#'   nSim = 50,
#'   nArea = 5
#' )
#' }
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
  
  if (EmptyObject(Selectivity)) 
    cli::cli_abort('{.val Selectivity} is required but is currently empty')

  if (CheckDigest(Selectivity, argList))  return(Selectivity)
  
  SetSeed(seed)
  
  Selectivity@Pars <- StructurePars(
    Pars = Selectivity@Pars,
    nSim = nSim,
    Years = Years,
    nArea = nArea
  )
  Selectivity@Model <- FindModel(Selectivity)
  
  ModelClass <- getModelClass(Selectivity@Model)
  
  if (!is.null(ModelClass)) {
    if (Selectivity@isRel) {
      CheckRequiredObject(Maturity, "maturity", "Maturity")
      L50 <- FindL50(Maturity)
      Selectivity@Pars$L5 <- ArrayMultiply(Selectivity@Pars$L5, L50)
      Selectivity@Pars$LFS <- ArrayMultiply(Selectivity@Pars$LFS, L50)
      Selectivity@isRel <- FALSE
    }
    
    if (grepl("at-Length", ModelClass)) {
      Selectivity <- PopulateMeanAtLength(
        object = Selectivity,
        Length = Length,
        Years = Years,
        Ages = Ages,
        seed = seed,
        silent = silent
      )
    } else if (grepl("at-Weight", ModelClass)) {
      Selectivity <- PopulateMeanAtWeight(
        object = Selectivity,
        Weight = Weight,
        Years = Years,
        Ages = Ages,
        seed = seed,
        silent = silent
      )
    } else if (grepl("at-Age", ModelClass)) {
      Selectivity <- PopulateMeanAtAge(
        object = Selectivity,
        Ages = Ages,
        Years = Years,
        Length = Length
      )
    }
  }
  
  Selectivity <- MeanAtLength2MeanAtAge(
    object = Selectivity,
    Length = Length,
    max1 = TRUE
  )
  Selectivity <- MeanAtWeight2MeanAtAge(
    object = Selectivity,
    Weight = Weight,
    max1 = TRUE
  )
  
  Selectivity@MeanAtAge <- AddAtAgeDimnames(Selectivity@MeanAtAge, 
                                            Ages, Years,
                                            name='Selectivity')
  
  if (CalcAtLength && is.null(Selectivity@MeanAtWeight)) {
    Selectivity <- MeanAtAge2MeanAtLength(
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
    Selectivity <- CheckSelectivityMaximum(Selectivity)
  
  Selectivity@MeanAtLength <- AddDimension(Selectivity@MeanAtLength, "Area")
  Selectivity@MeanAtWeight <- AddDimension(Selectivity@MeanAtWeight, "Area")
  Selectivity@MeanAtAge <- AddDimension(Selectivity@MeanAtAge, "Area")
  
  # Add dimension names if missing
  if (is.null(dimnames(Selectivity@MeanAtLength))) {
    dd <- dim(Selectivity@MeanAtLength)
    if (!is.null(dd)) {
      dimnames(Selectivity@MeanAtLength) <- list(
        Sim = 1:dd[1],
        Class = Selectivity@Classes,
        Year = Years[1:dd[3]],
        Area = 1:dd[4]
      )
    }
  }
  
  if (is.null(dimnames(Selectivity@MeanAtWeight))) {
    dd <- dim(Selectivity@MeanAtWeight)
    if (!is.null(dd)) {
      dimnames(Selectivity@MeanAtWeight) <- list(
        Sim = 1:dd[1],
        Class = Selectivity@Classes,
        Year = Years[1:dd[3]],
        Area = 1:dd[4]
      )
    }
  }
  
  if (is.null(dimnames(Selectivity@MeanAtAge))) {
    dd <- dim(Selectivity@MeanAtAge)
    if (!is.null(dd)) {
      dimnames(Selectivity@MeanAtAge) <- list(
        Sim = 1:dd[1],
        Age = Ages@Classes[1:dd[2]],
        Year = Years[1:dd[3]],
        Area = 1:dd[4]
      )
    }
  }
  
  SetDigest(SetAgeDimnames(Selectivity, Ages), argList)
}

FindL50_vec <- function(prob_vec) {
  classes <- names(prob_vec) |> as.numeric()
  LinInterp(prob_vec, y=classes, 0.5)
}

FindL50 <- function(Maturity) {
  if (!is.null(Maturity@Pars$L50)) {
    return(Maturity@Pars$L50)
  }
  
  MaturityAtLength <- Maturity@MeanAtLength
  if (is.null(MaturityAtLength)) {
    cli::cli_abort("Values required for `Maturity@MeanAtLength` if `Selectivity@isRel == TRUE`")
  }
  
  apply(Maturity@MeanAtLength, c('Sim', 'Year'), FindL50_vec)
}


AddAtAgeDimnames <- function(MeanAtAge, Ages, Years, name='Selectivity') {
  dd <- dim(MeanAtAge)
  dnames <- dimnames(MeanAtAge)
  if (!is.null(dnames)) 
    return(MeanAtAge)
  
  AgeClasses <- Ages@Classes
  nAge <- length(AgeClasses)
  
  if (is.null(dd)) {
    len <- length(MeanAtAge)
    if (len != nAge) 
      cli::cli_abort(c('x'='If `{name}@MeanAtAge` is numeric vector it must be length `nAge`',
                       'i'='`length({name}@MeanAtAge)` = {.val {len}}',
                       'i'='`nAge` = {.val {nAge}}'
      )
      )
    
    MeanAtAge <- array(MeanAtAge, dim=c(1,nAge,1),
                       dimnames = list(
                         Sim=1,
                         Age=AgeClasses,
                         Year=min(Years)
                       ))
    return(MeanAtAge)
  }
  
  if (dd[2]!=nAge)
    cli::cli_abort(c('x'='If `{name}@MeanAtAge` is an array, the second dimension must be length `nAge`',
                     'i'='`dim({name}@MeanAtAge)` = {.val { dd}}',
                     'i'='`nAge` = {.val { nAge}}')
    )
  
  if (length(dd)==2) {
    dimnames(MeanAtAge) <- list(
      Sim=seq_len(dd[1]),
      Age=AgeClasses
    )
    MeanAtAge <- AddDimension(MeanAtAge, 'Year', val=min(Years))
    
  } else if (length(dd)==3) {
    dimnames(MeanAtAge) <- list(
      Sim=seq_len(dd[1]),
      Age=AgeClasses,
      Year=Years[seq_len(dd[3])]
    )
    
  } else if (length(dd)==4) {
    dimnames(MeanAtAge) <- list(
      Sim=seq_len(dd[1]),
      Age=AgeClasses,
      Year=Years[seq_len(dd[3])],
      Area=seq_len(dd[4])
    )
  }
  MeanAtAge
}