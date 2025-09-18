
## Weight ----

#' Weight Object
#'
#' The `Weight` function is used to create S4 class `weight` objects or to access or
#' assign `weight` objects to [Stock()] class objects
#' 
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#' 
#' @details
#' ## About the `weight` Class
#' Objects of class `weight` contain information relating to the weight-at-age
#' of a stock.
#'
#' ## Creating New Objects
#' `r Creating_New_Objects('weight')`
#'
#' ## Accessing and Assigning Slots
#' `r Accessing_Assigning_Slots('weight')`
#'
#' ## Parameters (`Pars`)
#' The `Pars` slot is used to store the parameters for the `Model` that generates
#' the mean weight-at-age growth curve. The `MeanAtAge` slot is populated internally
#' from `Pars` and `Model`.
#'
#' `Pars` should be a named list, with the names corresponding to the parameters
#' for a valid length-at-age model (see [WeightModels()]).
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
#' @slot MeanAtAge `r MeanAtAge_param('Length')`
#' @slot MeanAtLength `r MeanAtLength_param('Weight')`
#' @slot CVatAge `r CVatAge_param('length')`
#' @slot Dist `r Dist_param()`
#' @slot TruncSD `r TruncSD_param()`
#' @slot Timing `r Timing_param()`
#' @slot Random `r Random_param()`
#' @slot ASK `r ASK_param()`
#' @slot Classes `r Classes_param()`
#' @slot Misc `r Misc_param()``
#'
#' @seealso `r See_Also('weight', c('ValidUnits', 'WeightModels', 'Populate'))`
#'
#' @name Weight
#' @rdname Weight
#' @docType class
#'
#' @example man-examples/Weight-class.R
#' @export
setClass("weight",
         contains= c("ParsClass",
                     'MeanAtAgeClass',
                     'MeanAtLengthClass',
                     'DistClass',
                     'Timing',
                     'RandomClass',
                     'ASKClass',
                     'ClassesClass',
                     'MiscClass')
)

setValidity('weight', isValidObject)


setMethod("initialize", "weight", function(.Object,
                                           Pars=list(a=NA, b=NA),
                                           Model=NULL,
                                           Units='g',
                                           MeanAtAge=NULL,
                                           MeanAtLength=NULL,
                                           CVatAge=NULL,
                                           Dist='lognormal',
                                           TruncSD=2,
                                           Timing=0,
                                           Random=NULL,
                                           ASK=NULL,
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
  .Object@CVatAge <- CVatAge
  .Object@Dist <- Dist
  .Object@TruncSD <- TruncSD
  .Object@Timing <- Timing
  .Object@Random <- Random
  .Object@ASK <- ASK
  .Object@Classes <- Classes
  .Object@Misc <- Misc
  #   .Object@Created <- Sys.time()
  
  .Object
})

#' @describeIn Weight Create a new `weight` class object
#' @param Pars `r Pars_param()`
#' @param Model `r Model_param()`
#' @param Units `r Units_param(variable="MeanAtAge", class='Weight', default='g')`
#' @param MeanAtAge `r MeanAtAge_param('Weight')`
#' @param MeanAtLength `r MeanAtLength_param('Weight')`
#' @param CVatAge `r CVatAge_param('weight')`. Only used to generate the `ASK` and
#' generate weight-at-age data. Set to `NULL` if the age-size key is not required.
#' @param Dist `r Dist_param()`
#' @param TruncSD `r TruncSD_param()`
#' @param Timing `r Timing_param()`
#' @param Random `r Random_param()`
#' @param ASK `r ASK_param()`
#' @param Classes `r Classes_param()`
#' @param Misc `r Misc_param()`
#' @export
Weight <- function(Pars=list(Alpha=NA, Beta=NA),
                   Model=NULL,
                   Units='g',
                   MeanAtAge=NULL,
                   MeanAtLength=NULL,
                   CVatAge=NULL,
                   Dist='lognormal',
                   TruncSD=2,
                   Timing=0,
                   Random=NULL,
                   ASK=NULL,
                   Classes=NULL,
                   Misc=list()) {
  if (methods::is(Pars, 'stock'))
    return(Pars@Weight)
  
  if (methods::is(Pars, 'fleet'))
    return(Pars@Weight)
  
  methods::new('weight',
               Pars=Pars,
               Model=Model,
               Units=Units,
               MeanAtAge=MeanAtAge,
               MeanAtLength=MeanAtLength,
               CVatAge=CVatAge,
               Dist=Dist,
               TruncSD=TruncSD,
               Timing=Timing,
               Random=Random,
               ASK=ASK,
               Classes=Classes,
               Misc=Misc)
}


#' @describeIn Weight Assign an `weight` class object to a [Stock()] object
#' @param x A [Stock()] class object
#' @param value A `weight` class object to assign to `x`
#' @export
`Weight<-` <- function(x, value) {
  assignSlot(x, value, 'Weight')
}