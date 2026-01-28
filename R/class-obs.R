
#' Catch Observation Component
#'
#' Internal class describing observation error associated with catch data.
#'
#' @slot CV Coefficient of variation.
#' @slot Error Realized observation error.
#' @slot Bias Observation bias.
#' @slot Years Observation years.
#' @slot Ref Reference values.
#' @slot Misc Miscellaneous additional information.
#'
#' @include class-unions.R
setClass(
  "catchobs",
  slots = c(
    CV    = "num.array.null",          # sim
    Error = "num.array.null",
    Bias  = "num.array.null",
    Years = "num.null",
    Ref   = "num.array.null",
    Misc  = "list"
  )
)


#' Effort Observation Component
#'
#' Internal class describing observation error associated with effort data.
#'
#' @slot CV Coefficient of variation.
#' @slot Error Realized observation error.
#' @slot Bias Observation bias.
#' @slot Years Observation years.
#' @slot Ref Reference values.
#' @slot Misc Miscellaneous additional information.
#'
#' @include class-unions.R
setClass(
  "effortobs",
  slots = c(
    CV    = "num.array.null",
    Error = "num.array.null",
    Bias  = "num.array.null",
    Years = "num.null",
    Ref   = "num.array.null",
    Misc  = "list"
  )
)


#' Index Observation Component
#'
#' Internal class describing observation error associated with abundance indices.
#'
#' @slot CV Coefficient of variation.
#' @slot Error Realized observation error.
#' @slot Beta Observation bias parameters.
#' @slot AC Autocorrelation parameters.
#' @slot Years Observation years.
#' @slot Selectivity Observation selectivity definition.
#' @slot Type Index type.
#' @slot Ref Reference values.
#' @slot Efficiency Catchability.
#' @slot Misc Miscellaneous additional information.
#'
#' @include class-unions.R
setClass(
  "indicesobs",
  slots = c(
    CV          = "num.array.null",
    Error       = "num.array.null",
    Beta        = "num.array.null",
    AC          = "num.array.null",
    Years       = "num.array.null",
    Areas       = "num.null",
    Selectivity = "array.char.num.list",
    Type        = "character",
    Ref         = "num.array.null",
    Efficiency  = "num.array.null",
    TruncSD     = "num.null",   # default 2 
    Stats       = 'df.null',
    Misc        = "list"
  )
)


#' Composition Observation Component
#'
#' Internal class describing observation error associated with composition data.
#'
#' @slot SampleSize Sample sizes.
#' @slot ESS Effective sample sizes.
#' @slot Years Observation years.
#' @slot Bias Observation bias.
#' @slot Misc Miscellaneous additional information.
#'
#' @include class-unions.R
setClass(
  "CompObs",
  slots = c(
    SampleSize = "num.array.null",
    ESS        = "num.array.null",
    Years      = "num.null",
    Bias       = "num.array.null",
    Misc       = "list"
  )
)


#' Life-History Observation Component
#'
#' Internal class describing observation error on life-history parameters.
#'
#' @slot Ages Age observations.
#' @slot Length Length observations.
#' @slot Weight Weight observations.
#' @slot NaturalMortality Natural mortality observations.
#' @slot Maturity Maturity observations.
#' @slot Fecundity Fecundity observations.
#' @slot SRR Stock–recruit observations.
#' @slot Spatial Spatial observations.
#' @slot Depletion Depletion observations.
#' @slot Misc Miscellaneous additional information.
#'
#' @include class-unions.R
setClass(
  "lifehistoryobs",
  slots = c(
    Ages             = "list",
    Length           = "list",
    Weight           = "list",
    NaturalMortality = "list",
    Maturity         = "list",
    Fecundity        = "list",
    SRR              = "list",
    Spatial          = "list",
    Depletion        = "list",
    Misc             = "list"
  )
)


#' Exploitation Observation Component
#'
#' Internal class describing observation error on exploitation processes.
#'
#' @slot Selectivity Selectivity observations.
#' @slot Retention Retention observations.
#' @slot DiscardMortality Discard mortality observations.
#' @slot Misc Miscellaneous additional information.
#'
#' @include class-unions.R
setClass(
  "exploitationobs",
  slots = c(
    Selectivity      = "list",
    Retention        = "list",
    DiscardMortality = "list",
    Misc             = "list"
  )
)

#' `Obs` Object
#'
#' The `obs` class defines the observation model used to generate observed data
#' from the operating model, including observation error, bias, and sampling
#' structure.
#'
#' @slot Name Name of the observation model.
#' @slot LifeHistory Observation error on life-history parameters.
#' @slot Exploitation Observation error on exploitation processes.
#' @slot Effort Observation error on effort data.
#' @slot Landings Observation error on landings.
#' @slot Discards Observation error on discards.
#' @slot CPUE Observation error on CPUE indices.
#' @slot Survey Observation error on survey indices.
#' @slot CAA Observation error on catch-at-age.
#' @slot CAL Observation error on catch-at-length.
#' @slot Misc Miscellaneous additional objects.
#'
#' @seealso [Obs()], [OM()]
#'
#' @include class-unions.R
#' @name ObsClass
#' @export
setClass(
  "obs",
  slots = c(
    Name          = "character",
    LifeHistory   = "lifehistoryobs",
    Exploitation  = "exploitationobs",
    Effort        = "effortobs",
    Landings      = "catchobs",
    Discards      = "catchobs",
    CPUE          = "indicesobs",
    Survey        = "indicesobs",
    CAA           = "CompObs",
    CAL           = "CompObs",
    Misc          = "list"
  )
)

#' @rdname ObsClass
#' @export
Obs <- function(object = NULL) {
  if (inherits(object, "om"))
    return(object@Obs)
  
  .Object <- methods::new("obs")
  validObject(.Object)
  .Object
}


setValidity("obs", function(object) {
  # TODO: structural checks
  TRUE
})

setMethod(
  "initialize",
  "obs",
  function(.Object) {
    .Object
  }
)


#' @rdname ObsClass

#' @param x An [OM()] object.
#' @param value An [Obs()] object to assign.
#'
#' @export
`Obs<-` <- function(x, value) {
  x@Obs <- value
  x
}
