


#' NaturalMortality Object
#'
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#' 
#' The `NaturalMortality` function is used to create S4 class `naturalmortality`
#' objects or to access or assign `naturalmortality` class objects to [Stock()]
#' class objects
#'
#' @details
#' ## About the `naturalmortality` Class
#' Objects of class `naturalmortality` contain information relating to the
#' natural mortality-at-age  of a stock.
#'
#' ## Creating New Objects
#' `r Creating_New_Objects('naturalmortality')`
#'
#' ## Accessing and Assigning Slots
#' `r Accessing_Assigning_Slots('naturalmortality')`
#'
#' ## Parameters (`Pars`)
#' The `Pars` slot is used to store the parameters for the `Model` that generates
#' the natural mortality-at-age or -length schedules.
#' The `MeanAtAge` and `MeanAtLength` slots are populated internally from `Pars` and `Model`.
#'
#' `Pars` should be a named list, with the names corresponding to the parameters
#' for a valid length-at-age model (see [NaturalMortalityModels()]).
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
#' @slot Units `r Units_param(variable="MeanAtAge", class='NaturalMortality', default='year')`
#' @slot MeanAtAge `r MeanAtAge_param('Natural Mortality')`
#' @slot MeanAtLength `r MeanAtLength_param('Natural Mortality')`
#' @slot Random `r Random_param()`
#' @slot Classes `r Classes_param()`
#' @slot Misc `r Misc_param()`

#'
#' @seealso `r See_Also('naturalmortality', c('ValidUnits', 'Check', 'NaturalMortalityModels', 'Populate'))`
#'
#' @name NaturalMortality
#' @rdname NaturalMortality
#' @docType class
#'
#' @example man-examples/NaturalMortality-class.R
#' @export
setClass("naturalmortality",
         contains= c("ParsClass",
                     'MeanAtAgeClass',
                     'MeanAtLengthClass',
                     'RandomClass',
                     'ClassesClass',
                     'MiscClass')
)


setValidity('naturalmortality', isValidObject)

setMethod("initialize", "naturalmortality", function(.Object,
                                                     Pars=list(M=NA),
                                                     Model=NULL,
                                                     Units='year',
                                                     MeanAtAge=NULL,
                                                     MeanAtLength=NULL,
                                                     Random=NULL,
                                                     Classes=NULL,
                                                     Misc=list()) {
  
  .Object@Pars <- Pars
  if (!is.null(Model))
    .Object@Model <- Model
  if (length(Pars)>0 &
      !is.null(names(Pars)) &
      all(!is.na(unlist(Pars))) &
      is.null(Model))
    .Object@Model <- FindModel(.Object)
  
  .Object@Units <- Units
  .Object@MeanAtAge <- MeanAtAge
  .Object@MeanAtLength <- MeanAtLength
  .Object@Random <- Random
  .Object@Classes <- Classes
  .Object@Misc <- Misc
  #   .Object@Created <- Sys.time()
  .Object
})

#' @describeIn NaturalMortality Create a new `naturalmortality` class object
#' @param Pars `r Pars_param()`
#' @param Model `r Model_param()`
#' @param Units `r Units_param(variable="MeanAtAge", class='NaturalMortality', default='year')`
#' @param MeanAtAge `r MeanAtAge_param('Natural Mortality')`
#' @param MeanAtLength `r MeanAtLength_param('Natural Mortality')`
#' @param Random `r Random_param()`
#' @param Classes `r Classes_param()`
#' @param Misc `r Misc_param()`
#' @export
NaturalMortality <- function(Pars=list(M=NA),
                             Model=NULL,
                             Units='year',
                             MeanAtAge=NULL,
                             MeanAtLength=NULL,
                             Random=NULL,
                             Classes=NULL,
                             Misc=list()) {
  if (methods::is(Pars, 'stock'))
    return(Pars@NaturalMortality)
  
  methods::new('naturalmortality',
               Pars=Pars,
               Model=Model,
               Units=Units,
               MeanAtAge=MeanAtAge,
               MeanAtLength=MeanAtLength,
               Random=Random,
               Classes=Classes,
               Misc=Misc)
}


#' @describeIn NaturalMortality Assign an `naturalmortality` class object to a [Stock()] object
#' @param x A [Stock()] class object
#' @param value A `naturalmortality` class object to assign to `x`
#' @export
`NaturalMortality<-` <- function(x, value) {
  assignSlot(x, value, 'NaturalMortality')
}


