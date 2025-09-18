


## Length ----

#' Length Object
#'
#' The `Length` function is used to create S4 class `length` objects or to access or
#' assign `length` objects to [Stock()] class objects
#'
#' @details
#' ## About the `length` Class
#' Objects of class `length` contain information relating to the length-at-age
#' of a stock.
#'
#' ## Creating New Objects
#' `r Creating_New_Objects('length')`
#'
#' ## Accessing and Assigning Slots
#' `r Accessing_Assigning_Slots('length')`
#'
#' ## Parameters (`Pars`)
#' The `Pars` slot is used to store the parameters for the `Model` that generates
#' the mean length-at-age growth curve. The `MeanAtAge` slot is populated internally
#' from `Pars` and `Model`.
#'
#' `Pars` should be a named list, with the names corresponding to the parameters
#' for a valid length-at-age model (see [LengthModels()]).
#'
#' The elements in `Pars` can be structured several different ways:
#'
#' - **Constant value over all simulations and time steps**: Numeric length 1
#' - **Uniformly distributed over simulations, constant all time steps**: Numeric length 2,
#' representing the lower and upper bounds of a uniform distribution.
#' - **Log-normally distributed over time steps**: `SD` appended to a previously
#' specified parameter (e.g., `LinfSD`) representing the lower and upper bound
#' of a uniform distribution for the log-normally distributed inter-annual variation.
#' - **Non-uniform distribution over simulations**: Numeric vector of length `nSim` (must be >2).
#' - **Time-varying**: Numeric matrix with either 1 or `nSim` rows and `nTS` columns,
#' where `nTS` is the total number of time steps. See note below.
#'
#'
#' TODO - this has changed now - use dimnames not attributes
#' **Note:** For time-varying parameters, if the parameter only varies in particular
#' time steps (rather than every time step), the number of columns in the matrix
#' can be equal to the number of change points instead of `nTS`. The time step
#' corresponding with each change point can be specified using `attributes`, see `Examples`.
#' 
#'
#' @slot Pars `r Pars_param()`
#' @slot Model `r Model_param()`
#' @slot Units `r Units_param(variable="MeanAtAge", class='Length', default='mm')`
#' @slot MeanAtAge `r MeanAtAge_param('Length')`
#' @slot CVatAge `r CVatAge_param('length')`
#' @slot Dist `r Dist_param()`
#' @slot TruncSD `r TruncSD_param()`
#' @slot Timing `r Timing_param()`
#' @slot Random `r Random_param()`
#' @slot ASK `r ASK_param()`
#' @slot Classes `r Classes_param()`
#' @slot Misc `r Misc_param()`
#'
#' @seealso `r See_Also('length', c('ValidUnits', 'LengthModels', 'Populate'))`
#'
#' @name Length
#' 
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#' 
#' @example man-examples/Length-class.R
#' @export

setClass("length",
         contains= c("ParsClass",
                     'MeanAtAgeClass',
                     'DistClass',
                     'Timing',
                     'RandomClass',
                     'ASKClass',
                     'ClassesClass',
                     'MiscClass')
         
)

setValidity('length', isValidObject)

setMethod("initialize", "length", function(.Object,
                                           Pars=list(Linf=NA, K=NA, t0=NA),
                                           Model=NULL,
                                           Units='mm',
                                           MeanAtAge=NULL,
                                           CVatAge=0.1,
                                           Dist='normal',
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

#' @describeIn Length Create a new `length` class object
#' @param Pars `r Pars_param()`
#' @param Model `r Model_param()`
#' @param Units `r Units_param(variable="MeanAtAge", class='Length', default='mm')`
#' @param MeanAtAge `r MeanAtAge_param('Length')`
#' @param CVatAge `r CVatAge_param('length')`
#' @param Dist `r Dist_param()`
#' @param TruncSD `r TruncSD_param()`
#' @param Timing `r Timing_param()`
#' @param Random `r Random_param()`
#' @param ASK `r ASK_param()`
#' @param Classes `r Classes_param()`
#' @param Misc `r Misc_param()`
#' @export
Length <- function(Pars=list(Linf=NA, K=NA, t0=NA),
                   Model=NULL,
                   Units='mm',
                   MeanAtAge=NULL,
                   CVatAge=0.1,
                   Dist='normal',
                   TruncSD=2,
                   Timing=0,
                   Random=NULL,
                   ASK=NULL,
                   Classes=NULL,
                   Misc=list()) {
  if (methods::is(Pars, 'stock'))
    return(Pars@Length)
  
  methods::new('length',
               Pars=Pars,
               Model=Model,
               Units=Units,
               MeanAtAge=MeanAtAge,
               CVatAge=CVatAge,
               Dist=Dist,
               TruncSD=TruncSD,
               Timing=Timing,
               Random=Random,
               ASK=ASK,
               Classes=Classes,
               Misc=Misc)
}


#' @describeIn Length Assign an `length` class object to a [Stock()] object
#' @param x A [Stock()] class object
#' @param value A `length` class object to assign to `x`
#' @export
`Length<-` <- function(x, value) {
  assignSlot(x, value, 'Length')
}

