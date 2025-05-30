



## Fecundity ----

#' Fecundity Object
#' 
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#' 
#' The `Fecundity` function is used to create S4 class `fecundity` objects or to
#' access or assign `fecundity` objects to [Stock()] class objects
#'
#' @details
#' ## About the `fecundity` Class
#' Objects of class `Fecundity` contain information relating to the fecundity-at-age
#' schedule of a stock. Fecundity is used to calculate the spawning output.
#'
#' If `Fecundity` is not populated, it is assumed to be equal to the product of
#' mean [Weight()] and [Maturity()] at age.
#'
#' ## Creating New Objects
#' `r Creating_New_Objects('fecundity')`
#'
#' ## Accessing and Assigning Slots
#' `r Accessing_Assigning_Slots('fecundity')`
#'
#' ## Parameters (`Pars`)
#' The `Pars` slot is used to store the parameters for the `Model` that generates
#' the mean fecundity-at-age curve. The `MeanAtAge` and `MeanAtLength` slots are
#'  populated internally from `Pars` and `Model`.
#'
#' `Pars` should be a named list, with the names corresponding to the parameters
#' for a valid length-at-age model (see [FecundityModels()]).
#'
#' The elements in `Pars` can be structured several different ways:
#'
#' - **Constant value over all simulations and time steps**: Numeric length 1
#' - **Uniformly distributed over simulations, constant all time steps**: Numeric length 2,
#' representing the lower and upper bounds of a uniform distribution.
#' - **Log-normally distributed over time steps**: `SD` appended to a previously
#' specified parameter representing the lower and upper bound
#' of a uniform distribution for the log-normally distributed inter-annual variation.
#' - **Non-uniform distribution over simulations**: Numeric vector of length `nSim` (must be >2).
#' - **Time-varying**: Numeric matrix with either 1 or `nSim` rows and `nTS` columns,
#' where `nTS` is the total number of time steps. See note below.
#'
#' **Note:** For time-varying parameters, if the parameter only varies in particular
#' time steps (rather than every time step), the number of columns in the matrix
#' can be equal to the number of change points instead of `nTS`. The time step
#' corresponding with each change point can be specified using `attributes`, see `Examples`.
#'
#' @slot Pars `r Pars_param()`
#' @slot Model `r Pars_param()`
#' @slot Units Character. Length 1. The units corresponding to `MeanAtAge` and
#' `MeanAtLength`. Defaults to `eggs` if not specified. Used for plotting
#' and reporting, and determing units of [SpawningOutput()].
#'
#' @slot MeanAtAge `r MeanAtAge_param()`
#' @slot MeanAtLength `r MeanAtLength_param()`
#' @slot Classes `r Classes_param()`
#' @slot Misc `r Misc_param()`
#'
#' @seealso `r See_Also('fecundity', c('FecundityModels', 'Check'))`
#' @name Fecundity
#' @rdname Fecundity
#' @docType class
#' @example man-examples/Fecundity-class.R
#'
#' @export
setClass("fecundity",
         contains= c("ParsClass",
                     'MeanAtAgeClass',
                     'MeanAtLengthClass',
                     'ClassesClass',
                     'MiscClass')
)

setValidity('fecundity', isValidObject)

setMethod("initialize", "fecundity", function(.Object,
                                              Pars=list(L50=NA, L50_95=NA, MaxFec=NA),
                                              Model=NULL,
                                              Units='eggs',
                                              MeanAtAge=NULL,
                                              MeanAtLength=NULL,
                                              Classes=NULL,
                                              Timing=0,
                                              Misc=list()) {
  
  .Object@Pars <- CheckPars(Pars)
  .Object@Model <- Model
  .Object <- PopulateModel(.Object)
  .Object@MeanAtAge <- MeanAtAge
  .Object@MeanAtLength <- MeanAtLength
  .Object@Classes <- Classes
  .Object@Units <- Units
  .Object@Misc <- Misc
  #   .Object@Created <- Sys.time()
  .Object
})

#' @describeIn Fecundity Create a new `Fecundity` object
#' @param Pars `r Pars_param()`
#' @param Model `r Pars_param()`
#' @param Units Character. Length 1. The units corresponding to `MeanAtAge` and
#' `MeanAtLength`. Defaults to `eggs` if not specified. Used for plotting,
#' reporting, and determining units of [SpawningOutput()].
#' @param MeanAtAge `r MeanAtAge_param()`
#' @param MeanAtLength `r MeanAtLength_param()`
#' @param Classes `r Classes_param()`
#' @param Misc `r Misc_param()`
#' @export
Fecundity <- function(Pars=list(L50=NA, L50_95=NA, MaxFec=NA),
                      Model=NULL,
                      Units='eggs',
                      MeanAtAge=NULL,
                      MeanAtLength=NULL,
                      Classes=NULL,
                      Misc=list()) {
  if (methods::is(Pars, 'stock'))
    return(Pars@Fecundity)
  
  .Object <- methods::new('fecundity',
                          Pars=Pars,
                          Model=Model,
                          Units=Units,
                          MeanAtAge=MeanAtAge,
                          MeanAtLength=MeanAtLength,
                          Classes=Classes,
                          Timing=Timing,
                          Misc=Misc)
  validObject(.Object)
  .Object
}


#' @describeIn Fecundity Assign an `Fecundity` object to a [Stock()] object
#' @param x A [stock()] object
#' @param value A `Fecundity` object to assign to `x`
#' @export
`Fecundity<-` <- function(x, value) {
  assignSlot(x, value, 'Fecundity')
}

