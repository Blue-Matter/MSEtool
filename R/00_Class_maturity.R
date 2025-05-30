
## Maturity ----

#' Maturity Object
#' 
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#' 
#' The `Maturity` function is used to create S4 class `maturity` objects or to access or
#' assign `maturity` objects to [Stock()] class objects
#'
#' @details
#' ## About the `maturity` Class
#' Objects of class `maturity` contain information relating to the weight-at-age
#' of a stock.
#'
#' ## Creating New Objects
#' `r Creating_New_Objects('maturity')`
#'
#' ## Accessing and Assigning Slots
#' `r Accessing_Assigning_Slots('maturity')`
#'
#' ## Parameters (`Pars`)
#' The `Pars` slot is used to store the parameters for the `Model` that generates
#' the maturity-at-age or maturity-at-length schedule. The `MeanAtAge` and `MeanAtLength` slots
#' are populated internally from `Pars` and `Model`.
#'
#' `Pars` should be a named list, with the names corresponding to the parameters
#' for a valid length-at-age model (see [MaturityModels()]).
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
#' @slot Model `r Model_param()`
#' @slot Units `r Units_param(variable="MeanAtAge", class='Length', default='mm')`
#' @slot MeanAtAge `r MeanAtAge_param('Maturity')`
#' @slot MeanAtLength `r MeanAtLength_param('Maturity')`
#' @slot Classes `r Classes_param()`
#' @slot Semelparous Logical. Do animals die after spawning?
#' @slot Misc `r Misc_param()`
#'
#' @seealso `r See_Also('maturity', c('ValidUnits', 'Check', 'MaturityModels', 'Populate'))`
#'
#' @name Maturity
#' @rdname Maturity
#' @docType class
#'
#' @example man-examples/Maturity-class.R
#' @export
setClass("maturity",
         contains= c("ParsClassNoUnits",
                     'MeanAtAgeClass',
                     'MeanAtLengthClass',
                     'ClassesClass',
                     'SemelparousClass',
                     'MiscClass')
)

setValidity('maturity', isValidObject)

setMethod("initialize", "maturity", function(.Object,
                                             Model=NULL,
                                             Pars=list(L50=NA, L50_95=NA),
                                             MeanAtAge=NULL,
                                             MeanAtLength=NULL,
                                             Classes=NULL,
                                             Semelparous=FALSE,
                                             Misc=list()) {
  .Object@Pars <- Pars
  
  if (!is.null(Model))
    .Object@Model <- Model
  
  if (length(Pars)>0 &
      !is.null(names(Pars)) &
      all(!is.na(unlist(Pars))) &
      is.null(Model))
    .Object@Model <- FindModel(.Object)
  
  .Object@MeanAtAge <- MeanAtAge
  .Object@MeanAtLength <- MeanAtLength
  .Object@Classes <- Classes
  .Object@Semelparous <- Semelparous
  .Object@Misc <- Misc
  #   .Object@Created <- Sys.time()
  .Object
})


#' @describeIn Maturity Create a new `maturity` class object
#' @param Pars `r Pars_param()`
#' @param Model `r Model_param()`
#' @param MeanAtAge `r MeanAtAge_param('Maturity')`
#' @param MeanAtLength `r MeanAtLength_param('Maturity')`
#' @param Classes `r Classes_param()`
#' @param Semelparous Logical. Do animals die after spawning?
#' @param Misc `r Misc_param()`
#' @export
Maturity <- function(Pars=list(L50=NA, L50_95=NA),
                     Model=NULL,
                     MeanAtAge=NULL,
                     MeanAtLength=NULL,
                     Classes=NULL,
                     Semelparous=FALSE,
                     Misc=list()) {
  if (methods::is(Pars, 'stock'))
    return(Pars@Maturity)
  
  methods::new('maturity',
               Pars=Pars,
               MeanAtAge=MeanAtAge,
               MeanAtLength=MeanAtLength,
               Classes=Classes,
               Model=Model,
               Semelparous=Semelparous,
               Misc=Misc)
}


#' @describeIn Maturity Assign an `maturity` class object to a [Stock()] object
#' @param x A [Stock()] object
#' @param value A `maturity` object to assign to `x`
#' @export
`Maturity<-` <- function(x, value) {
  assignSlot(x, value, 'Maturity')
}

