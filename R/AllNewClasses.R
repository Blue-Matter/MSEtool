#' @include Class_definitions.R


isValidObject <- function(object) {
  chk <- Check(object)
  if (chk@empty) return(TRUE)
  if (length(chk@errors)>0) return(chk@errors)
  TRUE
}

# S3 classes ----
# methods::setOldClass("ASK")

# R6 Classes ----
R6array <- R6::R6Class("R6array", list(
  value = NULL,
  initialize = function(vals) {
    self$value <- vals
  },
  set_value = function(val) {
    self$value <- val
    invisible(self)
  }
))

# Custom Class Unions ----
methods::setClassUnion(name="ASK.null", members=c("NULL", 'array'))
methods::setClassUnion(name="fun.char", members=c("character", "function", "NULL"))
methods::setClassUnion(name="char.null", members=c("character", "NULL"))
methods::setClassUnion(name="char.num", members=c('character', "numeric", 'NULL'))
methods::setClassUnion(name="char.list", members=c('character', "list", 'NULL'))
methods::setClassUnion(name="array.null", members=c("array", "NULL"))
methods::setClassUnion(name="array.char.null", members=c("array", 'character', "NULL"))
methods::setClassUnion(name="num.array.char.null", members=c('numeric', "array", 'character', "NULL"))
methods::setClassUnion(name="list.null", members=c('list', "NULL"))
methods::setClassUnion(name="num.null", members=c("numeric", "NULL"))
methods::setClassUnion(name="num.list.null", members=c("numeric", 'list', "NULL"))
methods::setClassUnion(name="num.array", members=c("numeric", "array", "NULL"))
methods::setClassUnion(name="num.array.df", members=c("numeric", "array", 'data.frame', "NULL"))
methods::setClassUnion(name="num.log", members=c("numeric", "logical", "NULL"))
methods::setClassUnion(name="logical.array", members=c("logical", "array", "NULL"))
methods::setClassUnion(name="num.Date", members=c("numeric", "Date", 'character', "NULL", 'POSIXct'))
methods::setClassUnion("missingOrcharacter", c("missing", "character"))
methods::setClassUnion("logical_list", c("logical", "list"))
methods::setClassUnion("obs.list", c("Obs", "list"))
methods::setClassUnion("imp.list", c("Imp", "list"))

# Child classes ----
setClass("ParsClassNoUnits",
         slots=c(Pars='list',
                 Model='fun.char'
         )
)

setClass("ParsClass",
         slots=c(Pars='list',
                 Model='fun.char',
                 Units='char.null'
         )
)

setClass("DistClass",
         slots=c(CVatAge='num.array',
                 Dist='character',
                 TruncSD='num.null'
         )
)

setClass("MeanAtAgeClass",
         slots=c(MeanAtAge='num.array')
)



setClass("MeanAtLengthClass",
         slots=c(MeanAtLength='num.array')
)

setClass("RandomClass",
         slots=c(Random='num.array')
)

setClass("ASKClass",
         slots=c(ASK='ASK.null')
)

setClass("ClassesClass",
         slots=c(Classes='num.null')
)

setClass("SemelparousClass",
         slots=c(Semelparous='logical.array')
)

setClass("Timing",
         slots=c(Timing='num.null')
)

setClass("MiscClass",
         slots=c(Misc='list')
)


setClass('Created_ModifiedClass',
         slots=c(Created='num.Date',
                 Modified='num.Date')
)





# Stock Sub Classes ----

## Ages ----

#' Ages Object
#'
#' The `Ages` function is used to create S4 class `ages` objects or to access or
#' assign `ages` objects to [Stock()] class objects.
#'
#' @details
#' ## About the `ages` Class
#' `ages` is an S4 class used in [Stock()] class objects. It contains information
#' relating to the age classes of the stock.
#'
#' ## Creating New Objects
#' `r Creating_New_Objects('ages')`
#'
#' ## Accessing and Assigning Slots
#' `r Accessing_Assigning_Slots('ages')`
#'
#' @slot MaxAge `r MaxAge_param()`
#' @slot Units `r Units_param()`
#' @slot PlusGroup `r Plusgroup_param()`
#' @slot Classes `r AgeClasses_param()`
#' @slot Created `r Created_param()`
#' @slot Modified `r Modified_param()`
#'
#' @seealso `r See_Also('ages', c('ValidUnits', 'Check'))`
#'
#' @name Ages
#' @rdname Ages
#' @docType class
#'
#' @example man-examples/Ages-class.R
#' @export
setClass('ages',
         slots=c(MaxAge='numeric',
                 Units='character',
                 PlusGroup='logical'),
         contains = c('ClassesClass', 'Created_ModifiedClass')
)

setValidity('ages', isValidObject)

setMethod("initialize", "ages", function(.Object,
                                         MaxAge=NA,
                                         Units='year',
                                         PlusGroup=TRUE) {
  if (!is.na(MaxAge)) {
    .Object@MaxAge <- MaxAge
    .Object@Classes <- 0:MaxAge
  }

  .Object@Units <- Units
  .Object@PlusGroup <- PlusGroup
  .Object@Created <- Sys.time()
  .Object
})

#' @describeIn Ages Create a new `ages` class object
#' @param MaxAge `r MaxAge_param()`
#' @param Units `r Units_param()`
#' @param PlusGroup `r Plusgroup_param()`
#' @param Classes `r AgeClasses_param()`
#' @export
Ages <- function(MaxAge=NA,
                 Units='year',
                 PlusGroup=TRUE) {
  if (methods::is(MaxAge, 'stock'))
    return(MaxAge@Ages)

  .Object <- methods::new('ages',
                          MaxAge=MaxAge,
                          Units=Units,
                          PlusGroup=PlusGroup)

  validObject(.Object)
  .Object
}

#' @describeIn Ages Assign an `ages` class object to a [Stock()] object
#' @param x A [Stock()] class object
#' @param value An `ages` class object to assign to `x`
#' @export
`Ages<-` <- function(x, value) {
  assignSlot(x, value, 'Ages')
}


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
#' @slot Created `r Created_param()`
#' @slot Modified `r Modified_param()`
#'
#' @seealso `r See_Also('length', c('ValidUnits', 'Check', 'LengthModels', 'Populate'))`
#'
#' @name Length
#' @rdname Length
#' @docType class
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
                     'MiscClass',
                     'Created_ModifiedClass')

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
  .Object@Created <- Sys.time()


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



## Weight ----

#' Weight Object
#'
#' The `Weight` function is used to create S4 class `weight` objects or to access or
#' assign `weight` objects to [Stock()] class objects
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
#' @slot Misc `r Misc_param()`
#' @slot Created `r Created_param()`
#' @slot Modified `r Modified_param()`
#'
#' @seealso `r See_Also('weight', c('ValidUnits', 'Check', 'WeightModels', 'Populate'))`
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
                     'MiscClass',
                     'Created_ModifiedClass')
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
  .Object@Created <- Sys.time()

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

## NaturalMortality ----

#' NaturalMortality Object
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
#' @slot Created `r Created_param()`
#' @slot Modified `r Modified_param()`
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
                     'MiscClass',
                     'Created_ModifiedClass')
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
  .Object@Created <- Sys.time()
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



## Maturity ----

#' Maturity Object
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
#' @slot Created `r Created_param()`
#' @slot Modified `r Modified_param()`
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
                     'MiscClass',
                     'Created_ModifiedClass')
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
  .Object@Created <- Sys.time()
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


## Fecundity ----

#' Fecundity Object
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
#' @slot Created `r Created_param()`
#' @slot Modified `r Modified_param()`
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
                     'MiscClass',
                     'Created_ModifiedClass')
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
  .Object@Created <- Sys.time()
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

## SRR ----

#' SRR Object
#'
#' The `SRR` function is used to create S4 class `srr` objects or to
#' access or assign `srr` objects to [Stock()] class objects
#'
#' @details
#' ## About the `srr` Class
#' Objects of class `SRR` contain information relating to stock-recruitment
#' relationship (SRR).
#'
#'
#' ## Creating New Objects
#' `r Creating_New_Objects('srr')`
#'
#' ## Accessing and Assigning Slots
#' `r Accessing_Assigning_Slots('srr')`
#'
#' ## Parameters (`Pars`)
#' The `Pars` slot is used to store the parameters for the `Model` that generates
#' the expected stock-recruit curve.
#'
#' `Pars` should be a named list, with the names corresponding to the parameters
#' for a valid length-at-age model (see [SRRModels()]).
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
#' @slot Model A named list of parameters for a model to generate
#' the expected stock-recruit curve. See `Parameters` section in `Details`
#' @slot SD Numeric vector. The standard deviation of the recruitment deviations in log-space.
#' Either length 1 (constant across simulations), length 2
#' (uniform distribution across simulations), or length `nSim`.
#' See `Parameters` section in `Details`.
#' @slot AC Numeric vector. The lag-1 autocorrelation factor of the recruitment deviations in log-space.
#' Same structure as `SD`.
#' @slot TruncSD The number of standard deviations to truncated the log-normal distribution
#' used to generate recruitment deviations. Defaults to 2.
#' @slot RecDevInit Optional. Numeric matrix with dimensions: `c(nSim, MaxAge)`.
#' The recruitment deviations for the age classes in the initial time step. Populated
#' internally from `SD` and `AC` if not specified.
#' @slot RecDevHist Optional. Numeric matrix with dimensions: `c(nSim, nHistTS)`.
#' The recruitment deviations for the historical time steps. Populated
#' internally from `SD` and `AC` if not specified.
#' @slot RecDevProj Optional. Numeric matrix with dimensions: `c(nSim, nProjectionTS)`.
#' The recruitment deviations for the projecion time steps. Populated
#' internally from `SD` and `AC` if not specified.
#' @slot SpawnTimeFrac Numeric value between 0 (default) and 1. The relative time in between
#' the time steps when spawning occurs, with 0 indicating the beginning of the time step.
#' @slot Misc `r Misc_param()`
#' @slot Created `r Created_param()`
#' @slot Modified `r Modified_param()`
#'
#' @seealso `r See_Also('srr', c('SRRModels', 'Check'))`
#' @name SRR
#' @rdname SRR
#' @docType class
#' @example man-examples/SRR-class.R
#'
#' @export
setClass("srr",
         slots=c(Pars='list',
                 Model='fun.char',
                 R0='num.array',
                 SD='num.array',
                 AC='num.array',
                 SPFrom='char.num',
                 TruncSD='num.null',
                 RecDevInit='num.array',
                 RecDevHist='num.array',
                 RecDevProj='num.array',
                 SpawnTimeFrac='numeric',
                 RelRecFun="fun.char",
                 Misc='list'
         ),
         contains='Created_ModifiedClass'
)

setValidity('srr', isValidObject)


setMethod("initialize", "srr", function(.Object,
                                        Pars=list(h=NA),
                                        Model='BevertonHolt',
                                        R0=array(),
                                        SD=array(),
                                        AC=array(),
                                        SPFrom=NULL,
                                        TruncSD=2,
                                        RecDevInit=array(),
                                        RecDevHist=array(),
                                        RecDevProj=array(),
                                        SpawnTimeFrac=0,
                                        RelRecFun=NULL,
                                        Misc=list()) {
  .Object@Pars <- Pars
  .Object@Model <- Model
  .Object@R0 <- R0
  .Object@SD <- SD
  .Object@AC <- AC
  .Object@SPFrom <- SPFrom
  .Object@TruncSD <- TruncSD
  .Object@RecDevInit <- RecDevInit
  .Object@RecDevHist <- RecDevHist
  .Object@RecDevProj <- RecDevProj
  .Object@SpawnTimeFrac <- SpawnTimeFrac
  .Object@RelRecFun <- RelRecFun
  .Object@Misc <- Misc
  .Object@Created <- Sys.time()

  if (length(Pars)>0 &
      !is.null(names(Pars)) &
      all(!is.na(unlist(Pars))) &
      is.null(Model))
    .Object@Model <- FindModel(.Object)

  .Object
})

#' @describeIn SRR Create a new `SRR` object
#' @param Pars `r Pars_param()`
#' @param Model A named list of parameters for a model to generate
#' the expected stock-recruit curve. See `Parameters` section in `Details`
#' @param SD Numeric vector. The standard deviation of the recruitment deviations in log-space.
#' Either length 1 (constant across simulations), length 2
#' (uniform distribution across simulations), or length `nSim`.
#' See `Parameters` section in `Details`.
#' @param AC Numeric vector. The lag-1 autocorrelation factor of the recruitment deviations in log-space.
#' Same structure as `SD`.
#' @param TruncSD The number of standard deviations to truncated the log-normal distribution
#' used to generate recruitment deviations. Defaults to 2.
#' @param RecDevInit Optional. Numeric matrix with dimensions: `c(nSim, MaxAge)`.
#' The recruitment deviations for the age classes in the initial time step. Populated
#' internally from `SD` and `AC` if not specified.
#' @param RecDevHist Optional. Numeric matrix with dimensions: `c(nSim, nHistTS)`.
#' The recruitment deviations for the historical time steps. Populated
#' internally from `SD` and `AC` if not specified.
#' @param RecDevProj Optional. Numeric matrix with dimensions: `c(nSim, nProjectionTS)`.
#' The recruitment deviations for the projecion time steps. Populated
#' internally from `SD` and `AC` if not specified.
#' @param SpawnTimeFrac Numeric value between 0 (default) and 1. The relative time in between
#' the time steps when spawning occurs, with 0 indicating the beginning of the time step.
#' @param Misc `r Misc_param()`
#' @export
SRR <- function(Pars=list(h=NA),
                Model='BevertonHolt',
                R0=array(),
                SD=array(),
                AC=array(),
                SPFrom=NULL,
                TruncSD=2,
                RecDevInit=array(),
                RecDevHist=array(),
                RecDevProj=array(),
                SpawnTimeFrac=0,
                RelRecFun=NULL,
                Misc=list()) {

  if (methods::is(Pars, 'stock'))
    return(Pars@SRR)

  methods::new('srr',
               Pars=Pars,
               Model=Model,
               R0=R0,
               SD=SD,
               AC=AC,
               SPFrom=SPFrom,
               TruncSD=TruncSD,
               RecDevInit=RecDevInit,
               RecDevHist=RecDevHist,
               RecDevProj=RecDevProj,
               SpawnTimeFrac=SpawnTimeFrac,
               RelRecFun=RelRecFun,
               Misc=Misc)
}


#' @describeIn SRR Assign an `SRR` object to a [Stock()] object
#' @param x A [stock()] object
#' @param value A `SRR` object to assign to `x`
#' @export
`SRR<-` <- function(x, value) {
  assignSlot(x, value, 'SRR')
}


## Spatial ----

#' Spatial Object
#'
#' The `Spatial` function is used to create S4 class `spatial` objects or to
#' access or assign `spatial` objects to [Stock()] class objects
#'
#' @details
#' ## About the `spatial` Class
#' Objects of class `spatial` contain information relating to spatial distribution
#' and movement of the stock.
#'
#' `Spatial` is only required for spatial operating models. Leave it empty for non-spatial models.
#'
#' ## Creating New Objects
#' `r Creating_New_Objects('spatial')`
#'
#' ## Accessing and Assigning Slots
#' `r Accessing_Assigning_Slots('spatial')`
#'
#' ## Two Area
#' For operating models with 2 spatial areas, `UnfishedDist` and `ProbStaying`
#' represent the the fraction of the unfished biomass and the probability of remaining
#' in Area 1 respectively.
#'
#' The fraction of unfished biomass in Area 2 is `1-UnfishedDist`. The probability
#' of remaining in Area 2 is calculated by an optimization routine
#' that solves for the specified fraction in Area 1 and the probability of remaining
#' in Area 1.
#'
#' `UnfishedDist` and `ProbStaying` can be one of the following:
#'
#' 1. **Numeric length 1**: Constant across all simulations and age classes;
#' 2. **Numeric length 2**: Lower and upper bounds of a uniform distribution;
#' 3. **Numeric 3D array**: Dimensions `c(nSim, nArea, nAge)`. Movement by age.
#' Must sum to 1 across areas for each simulation and age class.
#' 4. **Numeric 4D array**: Dimensions `c(nSim, nArea, nAge, nTS)`. Movement by age and time step.
#' Must sum to 1 across areas for each simulation, age class, and time step.
#'
#'
#' ## More than Two Areas
#'
#' For operating models with more than 2 spatial areas, `UnfishedDist` must be a
#' numeric array with at least 2 dimensions: `nsim` and `nArea`. It must sum to 1
#' across areas.
#'
#' Add additional dimensions for movement by age (`c(nSim, nArea, nAge)`) and
#' by age and time step (`c(nSim, nArea, nAge, nTS)`).
#'
#' With the exception of `nArea`, each dimension can either be length 1 or length corresponding
#' to the number of simulations, number of age class, and total number of time steps
#' respectively for `nSim`, `nAge`, and `nTS`.
#'
#' `ProbStaying` is the desired probability of staying within each area. The optimization
#' routine solves `ProbStaying` to find the movement matrix that is closted to the
#' specified unfished distribution (`UnfishedDist`; see `?FitMovement` for details).
#'
#' `FracOther` is required for models with more than 2 areas. It must be a numeric
#' array with at least dimensions `c(nSim, nArea, nArea)`, with option for `nAge`
#' and `nTS` dimensions as described above.
#'
#' The diagonal values of `FracOther` (within each simulation, age class, and time step)
#' represent the probability of remaining in an area and therefore should be set to `NA`,
#' as `ProbStaying` is solved as described above.
#'
#' The off-diagonal values represent, for each row, the relative probability of
#' moving from Area in row and column `i` to Area in row `i` and column `j`.
#'
#' ## RelativeSize
#'
#' `RelativeSize` is used for calculating the density of biomass in each area.
#'
#' For 2 area models, `RelativeSize` is the relative size of Area 1. IT should
#' either be a single numeric value, a numeric vector length 2 reperesenting the
#' lower and upper bounds of a uniform distribution, or a numeric vector of length `nSim`.
#'
#' For more than 2 areas, `RelativeSize` should be a numeric vector length `nArea`,
#' or a numeric matrix with dimensions `nSim` by `nArea`.
#'
#' If `RelativeSize` is not specified, all areas are assumed to be the same size.
#'
#' ## Movement
#' `Movement` is a numeric array  with dimensions `nSim`, `nArea`, `nArea`, `nAge`, and
#' `nTS`. The last two dimensions are optional.
#'
#' `Movement` is calculated internally from `UnfishedDist` and `ProbStaying`. If it is
#' provided, the asymptotic unfished distribution will be calculated and assigned
#' to `UnfishedDist` (over-writing any values that are in `UnfishedDist`).
#'
#' @slot UnfishedDist Numeric or numeric array. The relative distribution of the stock over areas.
#' See `Details`.
#' @slot ProbStaying Numeric or numeric array. The probability of remaining in an area in a given time step. See `Details`.#'
#' @slot RelativeSize Numeric or numeric array. The relative size of each area. See `RelativeSize` in `Details`.
#' @slot Movement A numeric matrix or array. See `Movement` in `Details`.
#' @slot FracOther See `More than Two Areas` in `Details`.
#' @slot Arrangement A numeric matrix with the layout ofthe areas. Used for plotting only.
#' @slot CVDist The logit CV associated with `UnfishedDist` (used as a penalty when optimizing for `UnfishedDist`). See `?FitMovement` for details.
#' @slot CVStay The logit CV associated with `ProbStaying` (used as a penalty when optimizing for diagonal (`ProbStaying`)). See `?FitMovement` for details.
#' @slot Misc `r Misc_param()`
#' @slot Created `r Created_param()`
#' @slot Modified `r Modified_param()`
#'
#' @seealso `r See_Also('spatial', c('CalcMovement', 'Check'))`
#' @name Spatial
#' @rdname Spatial
#' @docType class
#' @example man-examples/Spatial-class.R
#' @export
setClass('spatial',
         slots=c(UnfishedDist='num.array',
                 ProbStaying='num.array',
                 RelativeSize='num.array.char.null',
                 Movement='array.null',
                 FracOther='array.null',
                 Arrangement='array.null',
                 CVDist='numeric',
                 CVStay='numeric',
                 Misc='list'
         ),
         contains='Created_ModifiedClass'
)


setValidity('spatial', isValidObject)

setMethod("initialize", "spatial", function(.Object,
                                            UnfishedDist=NULL,
                                            ProbStaying=NULL,
                                            RelativeSize=NULL,
                                            Movement=NULL,
                                            FracOther=NULL,
                                            CVDist=0.1,
                                            CVStay=1,
                                            Misc=list()) {
  .Object@RelativeSize <- RelativeSize
  .Object@ProbStaying <- ProbStaying
  .Object@UnfishedDist <- UnfishedDist
  .Object@Movement <- Movement
  .Object@FracOther <- FracOther
  .Object@CVDist <- CVDist
  .Object@CVStay <- CVStay
  .Object@Misc <- Misc
  .Object@Created <- Sys.time()
  .Object
})


#' @describeIn Spatial Create a new `spatial` class object
#' @param UnfishedDist Numeric or numeric array. The relative distribution of the stock over areas.
#' See `Details`.
#' @param ProbStaying Numeric or numeric array. The probability of remaining in an area in a given time step. See `Details`.#'
#' @param RelativeSize Numeric or numeric array. The relative size of each area. See `RelativeSize` in `Details`.
#' @param Movement A numeric matrix or array. See `Movement` in `Details`.
#' @param FracOther See `More than Two Areas` in `Details`.
#' @param Arrangement A numeric matrix with the layout ofthe areas. Used for plotting only.
#' @param CVDist The logit CV associated with `UnfishedDist` (used as a penalty when optimizing for `UnfishedDist`). See `?FitMovement` for details.
#' @param CVStay The logit CV associated with `ProbStaying` (used as a penalty when optimizing for diagonal (`ProbStaying`)). See `?FitMovement` for details.
#' @param Misc `r Misc_param()`
#' @export
Spatial <- function(UnfishedDist=NULL,
                    ProbStaying=NULL,
                    RelativeSize=NULL,
                    Movement=NULL,
                    FracOther=NULL,
                    CVDist=0.1,
                    CVStay=1,
                    Misc=list()) {
  if (methods::is(UnfishedDist, 'stock'))
    return(UnfishedDist@Spatial)

  methods::new('spatial',
               UnfishedDist=UnfishedDist,
               ProbStaying=ProbStaying,
               RelativeSize=RelativeSize,
               Movement=Movement,
               FracOther=FracOther,
               CVDist=CVDist,
               CVStay=CVStay,
               Misc=Misc)
}


#' @describeIn Spatial Assign a `spatial` class object to a [Stock()] object
#' @param x A [Stock()] object
#' @param value A `spatial` object to assign to `x`
#' @export
`Spatial<-` <- function(x, value) {
  assignSlot(x, value, 'Spatial')
}


## Depletion ----

#' Depletion Object
#'
#' The `Depletion` function is used to create S4 class `depletion` objects or to access or
#' assign `depletion` objects to [Stock()] class objects
#'
#' @details
#' ## About the `depletion` Class
#' `depletion` is an S4 class used in [Stock()] class objects. It describes the
#' `Initial` depletion (at beginning of historical period) and the `Final` depletion
#' (at the end of the historical period).
#'
#' Depletion is defined as stock biomass divided by the unfished biomass; i.e.,
#' the lower the depletion value the lower the stock's biomass is related to it's
#' unfished level.
#'
#' ### `Initial`
#'
#' The stock at the beginning of the historical period is assumed to be in an
#' unfished state unless `Initial` is populated.
#'
#' When `Initial` is populated, an optimization routine adjusts the mean
#' recruitment deviations for the initial age classes such that the biomass relative
#' to `Reference` in the first time step is equal to `Initial`.
#'
#' ### `Final`
#'
#' `Final` is used to specify the depletion level in the last historical time step.
#' It is only required if the `Catchability` slot (`q`) in [FishingMortality()] is
#' not populated. In this case, an optimization routine will calculate the `q`
#' value for each simulation that results in the biomass relative to `Reference`
#' in the last historical time step to be equal to `Final`.
#'
#' ### `Reference`
#'
#' `Reference` describes the reference point used to calculate depletion.
#'
#' It can be a character string of either `B0` (default) or `BMSY`,
#' where `B0` is the equilibrium unfished biomass and `BMSY` the equilibrium
#' biomass corresponding the maximum sustainable yield.
#'
#' How are `B0` and `BMSY` calculated? Good question. That hasn't been documented
#' yet. Bug us to update the documentation!
#'
#' It can also be a numeric matrix with dimensions `(nSim, 2)` where the first
#' column is the absolute value for `Initial` and the second column the absolute
#' value for `Final`. Like all objects, the number of rows can be `nSim` if `Initial`
#' or `Final` vary over simulations, or otherwise a value of `1`.
#'
#' ## Creating New Objects
#' `r Creating_New_Objects('depletion')`
#'
#' ## Accessing and Assigning Slots
#' `r Accessing_Assigning_Slots('depletion')`
#'
#' @slot Initial A numeric of length `nSim` or length 1 specifying the biomass
#'  relative to `Reference` in the first historical time step. See `Details`.
#' @slot Final A numeric of length `nSim` or length 1 specifying the biomass
#'  relative to `Reference` in the last historical time step.  See `Details`.
#' @slot Reference The reference point used to calculate Depletion.  See `Details`.
#' @slot Misc `r Misc_param()`
#' @slot Created `r Created_param()`
#' @slot Modified `r Modified_param()`
#'
#' @seealso `r See_Also('depletion', c('Check'))`
#'
#' @name Depletion
#' @rdname Depletion
#' @docType class
#' @example man-examples/Depletion-class.R
#'
#' @export
setClass('depletion',
         slots=c(Initial='num.null',
                 Final='num.null',
                 Reference='array.char.null'),
         contains = c('MiscClass', 'Created_ModifiedClass')
)

setValidity('depletion', isValidObject)

setMethod("initialize", "depletion", function(.Object,
                                              Initial=numeric(),
                                              Final=numeric(),
                                              Reference='B0') {
  .Object@Initial <- Initial
  .Object@Final <- Final
  .Object@Reference <- Reference
  .Object@Created <- Sys.time()
  .Object
})


#' @describeIn Depletion Create a new `Depletion` object
#' @param Initial A numeric of length `nSim` or length 1 specifying the biomass
#'  relative to `Reference` in the first historical time step. See `Details`.
#' @param Final A numeric of length `nSim` or length 1 specifying the biomass
#'  relative to `Reference` in the last historical time step.  See `Details`.
#' @param Reference The reference point used to calculate Depletion.  See `Details`.
#' @export
Depletion <- function(Initial=numeric(),
                      Final=numeric(),
                      Reference='B0') {

  if (methods::is(Initial, 'stock'))
    return(Initial@Depletion)

  .Object <- methods::new('depletion',
                          Initial=Initial,
                          Final=Final,
                          Reference=Reference)

  validObject(.Object)
  .Object
}

#' @describeIn Depletion Assign an `Depletion` object to a [Stock()] object
#' @param x A [Stock()] class object
#' @param value An `depletion` class object to assign to `x`
#' @export
`Depletion<-` <- function(x, value) {
  assignSlot(x, value, 'Depletion')
}


# Stock Class ----

#' Stock Object
#'
#'
#' @details
#' ## About the `stock` Class
#' ...
#'
#' ## Creating New Objects
#' `r Creating_New_Objects('stock')`
#'
#' ## Accessing and Assigning Slots
#' `r Accessing_Assigning_Slots('stock')`
#'
#' @slot Name Character string. Unique name for this stock
#' @slot CommonName Common name of the stock
#' @slot Species Scientific name (genus and species)
#' @slot Ages A [Ages()] object. *Required*.
#' @slot Length A [Length()] object.
#' @slot Weight A [Weight()] object.
#' @slot NaturalMortality A [NaturalMortality()] object.
#' @slot Maturity A [Maturity()] object.
#' @slot Fecundity A [Fecundity()] object. Optional.
#' @slot SRR A [SRR()] object.
#' @slot Spatial A [Spatial()] object. Optional.
#' @slot Depletion A [Depletion()] object. Optional.
#' @slot nSim The number of simulations. Numeric. Positive integer `nSim=1` will
#' produce a deterministic operating model. Can be left empty and will be populated internally.
#' @slot CurrentTime Numeric value specifying the last historical time step. Must be in `TimeSteps`.
#' @slot TimeSteps Numeric vector specifying the time steps
#'
#' @slot Misc `r Misc_param()`
#' @slot Log A list. Used internally for logging and debugging.
#' @slot Created `r Created_param()`
#' @slot Modified `r Modified_param()`
#'
#' @seealso `r See_Also('stock', c('Check'))`
#'
#' @name Stock
#' @rdname Stock
#' @docType class
#'
#' @example man-examples/Stock-class.R
#' @export
setClass('stock',
         slots=c(Name='char.null',
                 CommonName='char.null',
                 Species='char.null',
                 Ages='ages',
                 Length='length',
                 Weight='weight',
                 NaturalMortality='naturalmortality',
                 Maturity='maturity',
                 Fecundity='fecundity',
                 SRR='srr',
                 Spatial='spatial',
                 Depletion='depletion',
                 nYear='num.null',
                 pYear='num.null',
                 nSim='num.null',
                 CurrentYear='num.null',
                 TimeUnits='char.null',
                 TimeSteps='num.null',
                 TimeStepsPerYear='num.null',
                 Misc='list',
                 Log='list'),
         contains='Created_ModifiedClass'
)

setMethod("initialize", "stock", function(.Object,
                                          Name=NULL,
                                          CommonName=NULL,
                                          Species=NULL,
                                          Ages=new('ages'),
                                          Length=new('length'),
                                          Weight=new('weight'),
                                          NaturalMortality=new('naturalmortality'),
                                          Maturity=new('maturity'),
                                          Fecundity=new('fecundity'),
                                          SRR=new('srr'),
                                          Spatial=new('spatial'),
                                          Depletion=new('depletion'),
                                          nYear=20,
                                          pYear=30,
                                          nSim=48,
                                          CurrentYear=as.numeric(format(Sys.Date(), '%Y')),
                                          TimeUnits='year',
                                          Misc=list()) {
  .Object@Name <- Name
  .Object@CommonName <- CommonName
  .Object@Species <- Species
  .Object@Ages <- Ages
  .Object@Length <- Length
  .Object@Weight <- Weight
  .Object@NaturalMortality <- NaturalMortality
  .Object@Maturity <- Maturity
  .Object@Fecundity <- Fecundity
  .Object@SRR <- SRR
  .Object@Spatial <- Spatial
  .Object@Depletion <- Depletion
  .Object@nYear <- nYear
  .Object@pYear <- pYear
  .Object@nSim <- nSim
  .Object@CurrentYear <- CurrentYear

  .Object@TimeStepsPerYear <- TSperYear(TimeUnits)
  .Object@TimeSteps <- CalcTimeSteps(nYear, pYear, CurrentYear, TimeUnits)
  .Object@TimeUnits <- TimeUnits
  .Object@Misc <- Misc
  .Object@Created <- Sys.time()
  .Object
})

setValidity('stock', isValidObject)


#' @describeIn Stock Create a new `stock` class object
#' @param Name Character string. Unique name for this stock
#' @param CommonName Common name of the stock
#' @param Species Scientific name (genus and species)
#' @param Ages A [Ages()] object. *Required*.
#' @param Length A [Length()] object.
#' @param Weight A [Weight()] object.
#' @param NaturalMortality A [NaturalMortality()] object.
#' @param Maturity A [Maturity()] object.
#' @param Fecundity A [Fecundity()] object. Optional.
#' @param SRR A [SRR()] object.
#' @param Spatial A [Spatial()] object. Optional.
#' @param Depletion A [Depletion()] object. Optional.
#' @param nSim The number of simulations. Numeric. Positive integer `nSim=1` will
#' produce a deterministic operating model. Can be left empty and will be populated internally.
#' @param CurrentYear The last historical year of the operating model. Defaults
#' to the year the Operating Model object is built. Must include `CurrentYear` but can be
#' in units other than `year`. See `ValidUnits('Ages')`
#' @param TimeSteps Numeric vector of the time steps.
#'
#' @param Misc `r Misc_param()`
#' @export
Stock <- function(Name=NULL,
                  CommonName=NULL,
                  Species=NULL,
                  Ages=new('ages'),
                  Length=new('length'),
                  Weight=new('weight'),
                  NaturalMortality=new('naturalmortality'),
                  Maturity=new('maturity'),
                  Fecundity=new('fecundity'),
                  SRR=new('srr'),
                  Spatial=new('spatial'),
                  Depletion=new('depletion'),
                  Misc=list(),
                  ...) {

  if (methods::is(Name, 'om')) {
    if (methods::is(Name@Stock, 'list')) {
      if (methods::is(CommonName, 'numeric')) {
        if (CommonName > nStock(Name)) {
          if (nStock(Name)==1) {
            cli::cli_abort('OM has only {.val {nStock(Name)}} stock')
          } else {
            cli::cli_abort('OM has only {.val {nStock(Name)}} stocks')
          }
        } else {
          return(Name@Stock[[CommonName]])
        }
      } else {
        cli::cli_inform('`Stock` is a list. Returning stock list. \n Use `Stock(OM, x)` to access stock `x`')
        return(Name@Stock)
      }
    }
    return(Name@Stock)
  }
      
  dots <- list(...)
  nYear <- 20
  pYear <- 30
  nSim <- 48
  CurrentYear <- as.numeric(format(Sys.Date(), '%Y'))
  TimeUnits <- 'year'
  for (nm in names(dots)) 
    assign(nm, dots[[nm]])
  
  methods::new('stock',
               Name=Name,
               CommonName=CommonName,
               Species=Species,
               Ages=Ages,
               Length=Length,
               Weight=Weight,
               NaturalMortality=NaturalMortality,
               Maturity=Maturity,
               Fecundity=Fecundity,
               SRR=SRR,
               Spatial=Spatial,
               Depletion=Depletion,
               nYear=nYear,
               pYear=pYear,
               nSim=nSim,
               CurrentYear=CurrentYear,
               TimeUnits=TimeUnits,
               Misc=Misc)
}


#' @describeIn Stock Assign an `stock` class object to a [OM()] object
#' @param x A [OM()] class object
#' @param value An `stock` class object to assign to `x`
#' @export
`Stock<-` <- function(x, value) {
  assignSlot(x, value, 'Stock')
}


# Fleet Sub Classes ----

## FishingMortality ----

#' FishingMortality Object
#'
#' FishingMortality
#'
#' @slot Created `r Created_param()`
#' @slot Modified `r Modified_param()`
#'
#' @seealso `r See_Also('fishingmortality', c('Check'))`
#'
#' @name FishingMortality
#' @rdname FishingMortality
#' @docType class
#' @example man-examples/FishingMortality-class.R
#' @export
setClass('fishingmortality',
         slots=c(ApicalF='array.null',
                 DeadAtAge='array.null',
                 RetainAtAge='array.null'
         ),
         contains = c('MiscClass', 'Created_ModifiedClass')
)

setMethod("initialize", "fishingmortality", function(.Object,
                                                     ApicalF=NULL,
                                                     DeadAtAge=NULL,
                                                     RetainAtAge=NULL,
                                                     Misc=list()) {
  .Object@ApicalF <- ApicalF
  .Object@DeadAtAge <- DeadAtAge
  .Object@RetainAtAge <- RetainAtAge
  .Object@Misc <- Misc
  .Object@Created <- Sys.time()
  .Object
})

#' @describeIn FishingMortality Create a new `fishingmortality` class object
#' @export
FishingMortality <- function(ApicalF=NULL,
                             DeadAtAge=NULL,
                             RetainAtAge=NULL,
                             Misc=list()) {
  if (methods::is(ApicalF, 'fleet'))
    return(ApicalF@FishingMortality)

  methods::new('fishingmortality',
               ApicalF=ApicalF,
               DeadAtAge=DeadAtAge,
               RetainAtAge=RetainAtAge,
               Misc=Misc)
}

#' @describeIn FishingMortality Assign an `FishingMortality` object to a [Fleet()] object
#' @param x A [Fleet()] class object
#' @param value A `FishingMortality` object to assign to `x`
#' @export
`FishingMortality<-` <- function(x, value) {
  assignSlot(x, value, 'FishingMortality')
}

## Effort ----

#' Effort Object
#'
#' Historical fishing effort
#'
#' DETAILS
#'
#' @slot Vessels Numeric array. The average number of fishing vessels per time step.
#' @slot Trips Numeric array. The average number of trips per vessel per time step.
#' @slot Misc `r Misc_param()`
#' @slot Created `r Created_param()`
#' @slot Modified `r Modified_param()`
#'
#' @seealso `r See_Also('effort', c('Check'))`
#'
#' @name Effort
#' @rdname Effort
#' @docType class
#' @example man-examples/Effort-class.R
#' @export
setClass('effort',
         slots=c(Effort='num.array.df',
                 Catchability='num.array',
                 qCV='num.array',
                 qInc='num.array',
                 Vessels='num.array.df',
                 Trips='num.array',
                 MaxVessels='num.array',
                 MaxTrips='num.array',
                 Units='char.null'
                 ),
         contains = c('MiscClass', 'Created_ModifiedClass')
)

setValidity('effort', isValidObject)

setMethod("initialize", "effort", function(.Object,
                                           Effort=NULL,
                                           Catchability=NULL,
                                           Vessels=NULL,
                                           Trips=NULL,
                                           MaxVessels=NULL,
                                           MaxTrips=NULL,
                                           Units=c('Vessels', 'Trips'),
                                           Misc=list()) {
  .Object@Effort <- Effort
  .Object@Catchability <- Catchability
  .Object@Vessels <- Vessels
  .Object@Trips <- Trips
  .Object@MaxVessels <- MaxVessels
  .Object@MaxTrips <- MaxTrips
  .Object@Units <- Units
  .Object@Misc <- Misc
  .Object@Created <- Sys.time()
  .Object
})

#' @describeIn Effort Create a new `effort` class object
#' @export
Effort <- function(Effort=NULL,
                   Catchability=NULL,
                   Vessels=NULL,
                   Trips=NULL,
                   MaxVessels=NULL,
                   MaxTrips=NULL,
                   Units=c('Vessels', 'Trips'),
                   Misc=list()) {
  if (methods::is(Effort, 'fleet'))
    return(Effort@Effort)

  methods::new('effort',
               Effort=Effort,
               Catchability=Catchability,
               Vessels=Vessels,
               Trips=Trips,
               MaxVessels=MaxVessels,
               MaxTrips=MaxTrips,
               Units=Units,
               Misc=Misc)
}

#' @describeIn Effort Assign an `Effort` object to a [Fleet()] object
#' @param x A [Fleet()] class object
#' @param value A `Effort` object to assign to `x`
#' @export
`Effort<-` <- function(x, value) {
  assignSlot(x, value, 'Effort')
}

## DiscardMortality ----

#' DiscardMortality Object
#'
#' Discard mortality
#'
#' DETAILS
#'
#' @slot Misc `r Misc_param()`
#' @slot Created `r Created_param()`
#' @slot Modified `r Modified_param()`
#'
#' @seealso `r See_Also('discardmortality', c('Check'))`
#'
#' @name DiscardMortality
#' @rdname DiscardMortality
#' @docType class
#' @example man-examples/DiscardMortality-class.R
#' @export
setClass("discardmortality",
         contains= c(
           'MeanAtAgeClass',
           'MeanAtLengthClass',
           'ClassesClass',
           'MiscClass',
           'Created_ModifiedClass')
)

setValidity('discardmortality', isValidObject)

setMethod("initialize", "discardmortality", function(.Object,
                                                     MeanAtAge=NULL,
                                                     MeanAtLength=NULL,
                                                     Classes=NULL,
                                                     Misc=list()) {

  .Object@MeanAtAge <- MeanAtAge
  .Object@MeanAtLength <- MeanAtLength
  .Object@Classes <- Classes
  .Object@Misc <- Misc
  .Object@Created <- Sys.time()
  .Object
})

#' @describeIn DiscardMortality Create a new `DiscardMortality` object
#' @export
DiscardMortality <- function(MeanAtAge=NULL,
                             MeanAtLength=NULL,
                             Classes=NULL,
                             Misc=list()) {

  if (methods::is(MeanAtAge, 'fleet'))
    return(MeanAtAge@DiscardMortality)

  methods::new('discardmortality',
               MeanAtAge=MeanAtAge,
               MeanAtLength=MeanAtLength,
               Classes=Classes,
               Misc=Misc)
}

#' @describeIn DiscardMortality Assign an `DiscardMortality` object to a [Fleet()] object
#' @param x A [Fleet()] class object
#' @param value A `DiscardMortality` object to assign to `x`
#' @export
`DiscardMortality<-` <- function(x, value) {
  assignSlot(x, value, 'DiscardMortality')
}



## Selectivity ----

#' Selectivity Object
#'
#' Discard mortality
#'
#' DETAILS
#'
#' @slot Misc `r Misc_param()`
#' @slot Created `r Created_param()`
#' @slot Modified `r Modified_param()`
#'
#' @seealso `r See_Also('selectivity', c('Check'))`
#'
#' @name Selectivity
#' @rdname Selectivity
#' @docType class
#' @example man-examples/Selectivity-class.R
#' @export
setClass("selectivity",
         slots=c(Pars='list',
                 Model='fun.char'),
         contains= c('MeanAtAgeClass',
                     'MeanAtLengthClass',
                     'ClassesClass',
                     'MiscClass',
                     'Created_ModifiedClass'))

setValidity('selectivity', isValidObject)

setMethod("initialize", "selectivity", function(.Object,
                                                     Pars=list(),
                                                     Model=NULL,
                                                     MeanAtAge=NULL,
                                                     MeanAtLength=NULL,
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

  .Object@MeanAtAge <- MeanAtAge
  .Object@MeanAtLength <- MeanAtLength
  .Object@Classes <- Classes
  .Object@Misc <- Misc
  .Object@Created <- Sys.time()
  .Object
})

#' @describeIn Selectivity Create a new `Selectivity` object
#' @export
Selectivity <- function(Pars=list(),
                             Model=NULL,
                             MeanAtAge=NULL,
                             MeanAtLength=NULL,
                             Classes=NULL,
                             Misc=list()) {

  if (methods::is(Pars, 'fleet'))
    return(Pars@Selectivity)

  methods::new('selectivity',
               Pars=Pars,
               Model=Model,
               MeanAtAge=MeanAtAge,
               MeanAtLength=MeanAtLength,
               Classes=Classes,
               Misc=Misc)
}

#' @describeIn Selectivity Assign an `Selectivity` object to a [Fleet()] object
#' @param x A [Fleet()] class object
#' @param value A `Selectivity` object to assign to `x`
#' @export
`Selectivity<-` <- function(x, value) {
  assignSlot(x, value, 'Selectivity')
}


## Retention ----


#' Retention Object
#'
#' Discard mortality
#'
#' DETAILS
#'
#' @slot Misc `r Misc_param()`
#' @slot Created `r Created_param()`
#' @slot Modified `r Modified_param()`
#'
#' @seealso `r See_Also('retention', c('Check'))`
#'
#' @name Retention
#' @rdname Retention
#' @docType class
#' @example man-examples/Retention-class.R
#' @export
setClass("retention",
         slots=c(Pars='list',
                 Model='fun.char'),
         contains= c('MeanAtAgeClass',
                     'MeanAtLengthClass',
                     'ClassesClass',
                     'MiscClass',
                     'Created_ModifiedClass'))

setValidity('retention', isValidObject)

setMethod("initialize", "retention", function(.Object,
                                                Pars=list(),
                                                Model=NULL,
                                                MeanAtAge=NULL,
                                                MeanAtLength=NULL,
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

  .Object@MeanAtAge <- MeanAtAge
  .Object@MeanAtLength <- MeanAtLength
  .Object@Classes <- Classes
  .Object@Misc <- Misc
  .Object@Created <- Sys.time()
  .Object
})

#' @describeIn Retention Create a new `Retention` object
#' @export
Retention <- function(Pars=list(),
                        Model=NULL,
                        MeanAtAge=NULL,
                        MeanAtLength=NULL,
                        Classes=NULL,
                        Misc=list()) {

  if (methods::is(Pars, 'fleet'))
    return(Pars@Retention)

  methods::new('retention',
               Pars=Pars,
               Model=Model,
               MeanAtAge=MeanAtAge,
               MeanAtLength=MeanAtLength,
               Classes=Classes,
               Misc=Misc)
}

#' @describeIn Retention Assign an `Retention` object to a [Fleet()] object
#' @param x A [Fleet()] class object
#' @param value A `Retention` object to assign to `x`
#' @export
`Retention<-` <- function(x, value) {
  assignSlot(x, value, 'Retention')
}


## ---- Distribution ----

#' Distribution Object
#'
#' Spatial Distribution of fishing fleet
#'
#' DETAILS
#'
#' @slot Misc `r Misc_param()`
#' @slot Created `r Created_param()`
#' @slot Modified `r Modified_param()`
#'
#' @seealso `r See_Also('distribution', c('Check'))`
#'
#' @name Distribution
#' @rdname Distribution
#' @docType class
#' @example man-examples/Distribution-class.R
#' @export
setClass('distribution',
         slots=c(Closure='num.array',
                 Cost='num.array',
                 Targeting='num.array',
                 Catchability='num.array',
                 Effort='num.array',
                 Misc='list'
         ),
         contains='Created_ModifiedClass')

setValidity('distribution', isValidObject)

setMethod("initialize", "distribution", function(.Object,
                                                 Closure=NULL,
                                                 Cost=NULL,
                                                 Targeting=NULL,
                                                 Catchability=NULL,
                                                 Effort=NULL,
                                                 Misc=list()) {
  .Object@Closure <- Closure
  .Object@Cost <- Cost
  .Object@Targeting <- Targeting
  .Object@Catchability <- Catchability
  .Object@Effort <- Effort
  .Object@Misc <- Misc
  .Object@Created <- Sys.time()
  .Object
})

#' @describeIn Distribution Create a new `Distribution` object
#' @export
Distribution <- function(Closure=NULL,
                         Cost=NULL,
                         Targeting=NULL,
                         Catchability=NULL,
                         Effort=NULL,
                         Misc=list()) {

  if (methods::is(Closure, 'fleet'))
    return(Closure@Distribution)

  methods::new('distribution',
               Closure=Closure,
               Cost=Cost,
               Targeting=Targeting,
               Catchability=Catchability,
               Effort=Effort,
               Misc=Misc)
}

#' @describeIn Distribution Assign an `Distribution` object to a [Fleet()] object
#' @param x A [Fleet()] class object
#' @param value A `Distribution` object to assign to `x`
#' @export
`Distribution<-` <- function(x, value) {
  assignSlot(x, value, 'Distribution')
}

# Fleet Class ----

#' Fleet Object
#'
#' Fleet Object
#'
#' DETAILS
#'
#' @slot Misc `r Misc_param()`
#' @slot Created `r Created_param()`
#' @slot Modified `r Modified_param()`
#'
#' @seealso `r See_Also('fleet', c('Check'))`
#'
#' @name Fleet
#' @rdname Fleet
#' @docType class
#' @example man-examples/Fleet-class.R
#' @export
setClass('fleet',
         slots=c(Name='char.null',
                 FishingMortality='fishingmortality',
                 DiscardMortality='discardmortality',
                 Effort='effort',
                 Selectivity='selectivity',
                 Retention='retention',
                 Distribution='distribution',
                 Weight='array.null',
                 BioEconomic='list',
                 Misc='list'
         ),
         contains='Created_ModifiedClass'
)


setMethod("initialize", "fleet", function(.Object,
                                          Name=NULL,
                                          FishingMortality=new('fishingmortality'),
                                          DiscardMortality=new('discardmortality'),
                                          Effort=new('effort'),
                                          Selectivity=new('selectivity'),
                                          Retention=new('retention'),
                                          Distribution=new('distribution'),
                                          Weight=array(),
                                          BioEconomic=list(),
                                          Misc=list()) {

  .Object@Name <- Name
  .Object@FishingMortality <- FishingMortality
  .Object@DiscardMortality <- DiscardMortality
  .Object@Effort <- Effort
  .Object@Selectivity <- Selectivity
  .Object@Retention <- Retention
  .Object@Distribution <- Distribution
  .Object@Weight <- Weight
  .Object@BioEconomic <- BioEconomic
  .Object@Misc <- Misc

  .Object@Created <- Sys.time()
  # methods::validObject(.Object)
  .Object
})

#' @describeIn Fleet Create a new `Fleet` object
#' @export
Fleet <- function(Name=NULL,
                  FishingMortality=new('fishingmortality'),
                  DiscardMortality=new('discardmortality'),
                  Effort=new('effort'),
                  Selectivity=new('selectivity'),
                  Retention=new('retention'),
                  Distribution=new('distribution'),
                  Weight=array(),
                  BioEconomic=list(),
                  Misc=list()) {

  if (methods::is(Name, 'om'))
    return(Name@Fleet)

  methods::new('fleet',
               Name=Name,
               FishingMortality=FishingMortality,
               DiscardMortality=DiscardMortality,
               Effort=Effort,
               Selectivity=Selectivity,
               Retention=Retention,
               Distribution=Distribution,
               Weight=array(),
               BioEconomic=list(),
               Misc=list())
}

#' @describeIn fleet Assign an `Fleet` object to an [OM()] object
#' @param x An [OM()] class object
#' @param value A `Fleet` object, or a list of `Fleet` objects, to assign to `x`
#' @export
`Fleet<-` <- function(x, value) {
  assignSlot(x, value, 'Fleet')
}






# Observation (obs) ----


# Implementation (imp) ----


# Data Sub Classes ----

## Biology ----

#' Biology Object
#'
#' @name Biology
#' @rdname Biology
#' @docType class
#'
#' @export
setClass("biology"
         # contains=c('ages',
         #            'length',
         #            'weight',
         #            'naturalmortality',
         #            'maturity',
         #            'fecundity',
         #            'srr',
         #            'spatial',
         #            'depletion')
)



# Selectivity


# dimensions - timesteps by nfleets

setClass("data_catch",
         slots=c(Value='array.null',
                 SD='array.null',
                 Units='char.null',
                 Name='char.null',
                 Selectivity='array.char.null'
         )
)

setClass("data_effort",
         slots=c(Value='array.null',
                 SD='array.null',
                 Units='char.null',
                 Name='char.null'
         )
)

# dim = timesteps
setClass("data_recruitment",
         slots=c(Value='num.null',
                 SD='num.null',
                 Units='char.null'
         )
)


# Indices
# dim - timesteps by nfleets
# units - 'biomass', 'number'
setClass("data_indices",
         slots=c(Value='array.null',
                 SD='array.null',
                 Units='char.null',
                 Timing='num.null',
                 Name='char.null',
                 Selectivity='array.char.null'
         )
)

# dim - timesteps by nage by nfleets

setClass("data_age_composition",
         slots=c(Classes='num.null',
                 Units='char.null',
                 Value='array.null',
                 ESS='array.null',
                 Name='char.null',
                 Selectivity='array.char.null'
         )
)

setClass("data_length_composition",
         slots=c(Classes='num.null',
                 Units='char.null',
                 Value='array.null',
                 ESS='array.null',
                 Name='char.null',
                 Selectivity='array.char.null'
         )
)




# ReferencePoints
#
# Misc
#
# Log
#
# Created




# Data  Class  ----

#' Data Object
#'
#' The `Data` function is used to create S4 class `data` objects or to access or
#' assign `data` or `datalist` objects to [OM()] class objects
#'
#' @details
#' ## About the `data` Class
#' `data` is an S4 class used in [OM()] class objects. It contains either real
#' or simulated fishery data
#'
#'
#' ## Creating New Objects
#' `r Creating_New_Objects('data')`
#'
#' ## Accessing and Assigning Slots
#' `r Accessing_Assigning_Slots('data')`
#'
#' @slot MaxAge `r MaxAge_param()`
#' @slot Units `r Units_param()`
#'
#' @seealso `r See_Also('data', c('Check'))`
#'
#' @name Data
#' @rdname Data
#' @docType class
#'
#' @example man-examples/Data-class.R
#' @export
setClass('data',
         slots=c(Name='char.null',
                 CommonName='char.null',
                 Species='char.null',
                 Agency='char.null',
                 Author='char.null',
                 Email='char.null',
                 Region='char.null',
                 Latitude='num.null',
                 Longitude='num.null',
                 Sponsor='char.null',
                 CurrentYear='num.null',
                 Biology='biology'
         ),


         contains = c('Created_ModifiedClass')
)



#' @describeIn Data Create a new `Data` object
#' @param MaxAge `r MaxAge_param()`
#' @export
Data <- function(Name=NA, ...) {
  if (methods::is(Name, 'om'))
    return(Name@Data)

  .Object <- methods::new('data')

  validObject(.Object)
  .Object
}

validDataObject <- function(object) {
  TRUE
}
setValidity('data', validDataObject)

setMethod("initialize", "data", function(.Object) {


  .Object@Created <- Sys.time()
  .Object
})

#' @describeIn Data Assign an `Data` object to a [OM()] object
#' @param x An [OM()] class object
#' @param value An `data` class object to assign to `x`
#' @export
`Data<-` <- function(x, value) {
  assignSlot(x, value, 'Data')
}



# SexPars ----

#' SexPars Object
#'
#' @name SexPars
#' @rdname SexPars
#' @docType class
#'
#' @export
setClass("sexpars",
         slots=c(SPFrom='array.null',
                 Herm='list.null',
                 SharePar='num.log',
                 Misc='list.null'
         ),
         contains='Created_ModifiedClass'
)

setValidity('sexpars', isValidObject)


setMethod("initialize", "sexpars", function(.Object,
                                            SPFrom=NULL,
                                            Herm=NULL,
                                            SharePar=TRUE,
                                            Misc=NULL) {
  .Object@SPFrom <- SPFrom
  .Object@Herm <- Herm
  .Object@SharePar <- SharePar
  if (!is.null(Misc)) {
    .Object@Misc <- Misc
  } else {
    .Object@Misc <- list()
    .Object@Misc$Stock <- c('Depletion', 'SRR')
    .Object@Misc$Fleet <- c('Effort', 'Distribution')
    .Object@Misc$Obs <- TRUE
    .Object@Misc$Imp <- TRUE
  }

  .Object@Created <- Sys.time()
  .Object
})

#' @describeIn SexPars Create a new `SexPars` object
#' @export
SexPars <- function(SPFrom=NULL,
                    Herm=list(),
                    SharePar=TRUE,
                    Misc=NULL) {
  
  if (methods::is(SPFrom, 'om'))
    return(SPFrom@SexPars)
  
  methods::new('sexpars',
               SPFrom=SPFrom,
               Herm=Herm,
               SharePar=SharePar,
               Misc=Misc)
}


#' @describeIn SexPars Assign an `SexPars` object to an [OM()] object
#' @param x An [OM()] object
#' @param value A `SexPars` object to assign to `x`
#' @export
`SexPars<-` <- function(x, value) {
  assignSlot(x, value, 'SexPars')
}


# ---- OM Class ----

setClassUnion(name="StockList", members=c("stock", 'Stock',  "list", 'NULL'))
setClassUnion(name="StockFleetList", members=c("fleet", 'Fleet', "list", 'NULL'))
setClassUnion(name="FleetList", members=c("fleet", 'Fleet', "list", 'NULL'))
setClassUnion(name="DataList", members=c("data", "list", 'NULL'))
# setClassUnion(name="obs.list", members=c('Obs', "obs", "list", 'NULL'))

#' Operating Model Object
#'
#' Objects of class `om` contain all the parameters needed to simulate a
#' historical fishery and project it forward for closed-loop simulation testing.
#'
#' @slot Name Name of the Operating Model. Character string.
#' @slot Agency Name of the agency responsible for the management of the fishery.
#' Character string. Supports Markdown.
#' @slot Author Name(s) of author(s) of the operating model. Character string
#' with length corresponding to the number of authors.
#' @slot Email Email address(es) for the author(s) of the operating model.
#' Character string of with length equal to `length(Author)`. Supports Markdown.
#' @slot Region Name of the general geographic region of the fishery. Character string.
#' @slot Latitude Latitude (decimal degrees) to indicate the center of `Region`.
#' Negative values represent the South of the Equator. Numeric. Single value.
#' @slot Longitude Longitude (decimal degrees) to indicate the center of `Region`.
#' Negative values represent the West of the Prime Meridian. Numeric. Single value.
#' @slot Sponsor Name of the organization who sponsored the development of the
#' Operating Model. Character string. Supports Markdown.
#' @slot CurrentYear The last historical year of the operating model. Defaults
#' to the year the Operating Model object is built.
#' @slot nSim The number of simulations. Numeric. Positive integer `nSim=1` will
#' produce a deterministic operating model.
#' @slot nYear The number of historical years. Typically corresponds to the
#' year the fishery was first (assumed to be) exploited. For multi-stock models,
#' `nYear` should be the earliest exploitation year of all stocks. Numeric.
#' Single value. Historical years are calculated as `rev(seq(CurrentYear, by=-1, length.out=nYear))`
#' @slot pYear The number of projection years. Numeric. Single value.
#' Projection years are calculated as `seq(CurrentYear+1, length.out=pYear)`.
#' @slot Stock A [Stock()] object or a list of [Stock()] objects
#' for multi-stock models.
#' @slot Complexes For multi-stock models only. A list of stock complexes.
#' Each position is a vector of stock numbers (as they appear in `Stock` list)
#' for which data and management recommendation should be aggregated; e.g.,
#' TAC will be split among stocks according to vulnerable biomass.
#' @slot Relations For multi-stock models only. A list of biological and/or
#' ecological relationships among stocks in `Stock`. For MICE models. Needs
#' more documentation so bug us if you get stuck.
#' @slot SexPars For multi-stock models only. A named list that controls
#' sex-specific dynamics, i.e., sex-specific spawning and hermaphroditism.
#' More generally, controls spawning and moving abundance between stocks. See `Details`.
#' @slot Data A [data-class()] object or a list of [data-class()] objects (up to
#' `length(Stock)`) containing the real fishery data.
#' if `Complexes` is specified, `Data` follows the same structure, i.e., elements
#' in `Data` list will be assumed to be aggregated according to `Complexes`.
#' @slot Interval The management update interval. Management will be implemented
#' in the first projection time step, and then every `Interval` time step, with
#' management remaining unchanged in the interim. A single numeric value for the
#' same interval for all managemement procedures. For MP-specific management
#' intervals , `Interval` can be a named numeric vector with length corresponding
#' to the number of management procedures used in [runMSE()] or [Project()].
#' @slot nReps Number of samples of the management recommendation for each method.
#' Only for management procedures that generate stochastic management advice (i.e.,
#' account for uncertainty in the data). Defaults to 1, which produces deterministic
#' management advice.
#' @slot pStar The percentile of the sample of the management recommendation for each method.
#' Defaults to 0.5 (median). Only for management procedures that generate
#' stochastic management advice (i.e., account for uncertainty in the data).
#' To ensure the management advice matches the correct percentile, `reps` should
#' be a largish value (i.e., >>1, but exact value depends on the degree of uncertainty in the data)
#' @slot Seed Optional numeric value for the seed for the random number generator.
#' Only required to over-ride the default seed (calculated internally based on
#' the contents of the `OM` object)
#' @slot Control A named list of settings. See `Details`
#' @slot Misc A list for storing additional things that don't have anywhere else to go.
#' Mainly for development purposes.
#' @slot Source Character string. Can be used to reference websites, articles, etc
#' with relevant information. Supports Markdown.
#' @slot Created `r Created_param()`
#' @slot Modified `r Modified_param()`
#'
#' @details
#'
#' ## About the `om` Class
#' `om` is a new S4 class that is designed to supersede [OM-class()] and [MOM-class()]
#' objects. In time, [OM-class()] and [MOM-class()] will be deprecated
#' and eventually removed from the package.
#'
#' ## Creating New Objects
#' `r Creating_New_Objects('om')`
#'
#' ## Accessing and Assigning Slots
#' `r Accessing_Assigning_Slots('om')`
#'
#' ## SexPars
#' The following are valid names for `SexPars`:
#'
#' - `SPfrom`: A `nstock` x `nstock` matrix, where `nstock` is `length(Stock`)
#' that specifies the proportion of the spawning output of the row `p` stock for
#' the column `p'` stock. A diagonal matrix means each stock is responsible for
#' its own recruitment.
#' - `Herm`: A list with each entry containing a matrix with
#' dimensions `c(nSim, MaxAge + 1)` that specifies the proportion at age that move from
#' stock `p` to `p'` (sequential hermaphroditism). The names of the list should be
#' of the form "H_p'_p" where `p` and `p'` are integers that identify the stocks
#' in the `Stock` list. For time-varying values, arrays with dimensions
#'  `c(nSim, (MaxAge + 1),nHistTS + nProjTS)` can be used. `MaxAge` is the maximum age
#'  for the both stock `p` and stock `p'`. `nHistTS` and `nProjTS` are equal to
#'  `nYear` and `pYear` respectively, unless `Units` in the stocks' [Ages()]
#'  object are not `year` (i.e, a higher resolution time step).
#' - `SharePar`: Optional. Logical to indicate whether stock-recruit, depletion,
#' and observation/implementation parameters are mirrored between stocks. By default, `TRUE`.
#'
#' ## Control
#' The following are valid names for `Control`:
#'
#' TODO
#'
#' @seealso `r See_Also('om')`
#'
#' @name OM
#' @rdname OM
#' @docType class
#'
#' @example man-examples/om-class.R
#' @export
setClass("om",
         slots=c(Name='char.null',
                 Agency='char.null',
                 Author='char.null',
                 Email='char.null',
                 Region='char.null',
                 Latitude='num.null',
                 Longitude='num.null',
                 Sponsor='char.null',
                 nSim='num.null',
                 nYear='num.null',
                 pYear='num.null',
                 CurrentYear='num.null',
                 Stock='StockList',
                 Fleet='StockFleetList',
                 Obs='obs.list',
                 Imp='imp.list',
                 Data='DataList',
                 CatchFrac='list',
                 Allocation='list',
                 Efactor='list',
                 Complexes='list',
                 SexPars='sexpars',
                 Relations='list',
                 Interval='numeric',
                 nReps='numeric',
                 pStar='numeric',
                 maxF='numeric',
                 Seed='num.null',
                 TimeUnits='char.null',
                 TimeStepsPerYear='num.null',
                 TimeSteps='num.null',
                 Control='list.null',
                 Misc='list',
                 Source='char.list'),
         contains = 'Created_ModifiedClass'
)
#' @describeIn OM Create a new object of class `om`
#' @param Name Name of the Operating Model. Character string.
#' @param Agency Name of the agency responsible for the management of the fishery.
#' Character string. Supports Markdown.
#' @param Author Name(s) of author(s) of the operating model. Character string
#' with length corresponding to the number of authors.
#' @param Email Email address(es) for the author(s) of the operating model.
#' Character string of with length equal to `length(Author)`. Supports Markdown.
#' @param Region Name of the general geographic region of the fishery. Character string.
#' @param Latitude Latitude (decimal degrees) to indicate the center of `Region`.
#' Negative values represent the South of the Equator. Numeric. Single value.
#' @param Longitude Longitude (decimal degrees) to indicate the center of `Region`.
#' Negative values represent the West of the Prime Meridian. Numeric. Single value.
#' @param Sponsor Name of the organization who sponsored the development of the
#' Operating Model. Character string. Supports Markdown.
#' @param CurrentYear The last historical year of the operating model. Defaults
#' to the year the Operating Model object is built.
#' @param nSim The number of simulations. Numeric. Positive integer `nSim=1` will
#' produce a deterministic operating model.
#' @param nYear The number of historical years. Typically corresponds to the
#' year the fishery was first (assumed to be) exploited. For multi-stock models,
#' `nYear` should be the earliest exploitation year of all stocks. Numeric.
#' Single value. Historical years are calculated as `rev(seq(CurrentYear, by=-1, length.out=nYear))`
#' @param pYear The number of projection years. Numeric. Single value.
#' Projection years are calculated as `seq(CurrentYear+1, length.out=pYear)`.
#' @param Stock A [Stock] object or a list of [Stock()] objects
#' for multi-stock models.
#' @param Complexes For multi-stock models only. A list of stock complexes.
#' Each position is a vector of stock numbers (as they appear in `Stock` list)
#' for which data and management recommendation should be aggregated; e.g.,
#' TAC will be split among stocks according to vulnerable biomass.
#' @param Relations For multi-stock models only. A list of biological and/or
#' ecological relationships among stocks in `Stock`. For MICE models. Needs
#' more documentation so bug us if you get stuck.
#' @param SexPars For multi-stock models only. A named list that controls
#' sex-specific dynamics, i.e., sex-specific spawning and hermaphroditism.
#' More generally, controls spawning and moving abundance between stocks. See `Details`.
#' @param Data A [data-class()] object or a list of [data-class()] objects (up to
#' `length(Stock)`) containing the real fishery data.
#' if `Complexes` is specified, `Data` follows the same structure, i.e., elements
#' in `Data` list will be assumed to be aggregated according to `Complexes`.
#' @param Interval The management update interval. Management will be implemented
#' in the first projection time step, and then every `Interval` time step, with
#' management remaining unchanged in the interim. A single numeric value for the
#' same interval for all managemement procedures. For MP-specific management
#' intervals , `Interval` can be a named numeric vector with length corresponding
#' to the number of management procedures used in [runMSE()] or [Project()].
#' @param nReps Number of samples of the management recommendation for each method.
#' Only for management procedures that generate stochastic management advice (i.e.,
#' account for uncertainty in the data). Defaults to 1, which produces deterministic
#' management advice.
#' @param pStar The percentile of the sample of the management recommendation for each method.
#' Defaults to 0.5 (median). Only for management procedures that generate
#' stochastic management advice (i.e., account for uncertainty in the data).
#' To ensure the management advice matches the correct percentile, `reps` should
#' be a largish value (i.e., >>1, but exact value depends on the degree of uncertainty in the data)
#' @param Seed Optional numeric value for the seed for the random number generator.
#' Only required to over-ride the default seed (calculated internally based on
#' the contents of the `OM` object)
#' @param Control A named list of settings. See `Details`
#' @param Misc A list for storing additional things that don't have anywhere else to go.
#' Mainly for development purposes.
#' @param Source Character string. Can be used to reference websites, articles, etc
#' with relevant information. Supports Markdown.
#' @export
OM <- function(Name='A new `OM` object',
               Agency='',
               Author='',
               Email='',
               Region='',
               Latitude=NULL,
               Longitude=NULL,
               Sponsor='',
               nSim=48,
               nYear=20,
               pYear=30,
               CurrentYear=as.numeric(format(Sys.Date(), '%Y')),
               TimeUnits='year',
               Stock=NULL,
               Fleet=NULL,
               Obs=list(),
               Imp=list(),
               Complexes=list(),
               Relations=list(),
               SexPars=new('sexpars'),
               Data=NULL,
               Interval=1,
               nReps=1,
               pStar=0.5,
               maxF=3,
               Seed=NULL,
               Control=NULL,
               Misc=list(),
               Source=list()) {

  .Object <- new('om')
  .Object@Name <- Name
  .Object@Agency <- Agency
  .Object@Author <- Author
  .Object@Email <- Email
  .Object@Region <- Region
  .Object@Latitude <- Latitude
  .Object@Longitude <- Longitude
  .Object@Sponsor <- Sponsor

  .Object@nSim <- nSim
  .Object@nYear <- nYear
  .Object@pYear <- pYear

  .Object@CurrentYear <- CurrentYear
  .Object@TimeUnits <- TimeUnits

  .Object@TimeStepsPerYear <- TSperYear(TimeUnits)
  .Object@TimeSteps <- CalcTimeSteps(nYear, pYear, CurrentYear, TimeUnits)

  .Object@Stock <- Stock
  .Object@Fleet <- Fleet
  .Object@Obs <- Obs
  .Object@Imp <- Imp
  .Object@Complexes <- Complexes
  .Object@Relations <- Relations
  .Object@SexPars <- SexPars
  .Object@Data <- Data
  .Object@Interval <- Interval
  .Object@nReps <- nReps
  .Object@pStar <- pStar
  .Object@maxF <- maxF
  .Object@Seed <- Seed
  
 
  if (!is.null(Control)) {
    .Object@Control <- Control
  } else {
    .Object@Control <- ControlDefault
  }
  
  .Object@Misc <- Misc
  .Object@Source <- Source
  .Object@Created <- Sys.time()

  methods::validObject(.Object)
  .Object
}

validOMobject <- function(object) {
  chk <- Check(object)
  if (chk@empty) return(TRUE)
  if (length(chk@errors)>0) return(chk@errors)
  TRUE
}

setValidity('om', validOMobject)









# Other Classes ----

setClass("CheckList",
                      slots=c(object='character',
                              errors='list',
                              warnings='list',
                              messages='list',
                              complete='logical_list',
                              populated='logical_list',
                              empty='logical_list')
)

setMethod("initialize", "CheckList", function(.Object) {
  .Object@empty <- TRUE
  .Object@complete <- FALSE
  .Object@populated <- FALSE
  .Object@empty <- TRUE
  .Object
})

CheckList <- function(object) {
  ll <- new('CheckList')
  ll@object <- class(object)
  ll@populated <- as.logical(PopulatedObject(object))
  ll@empty <- as.logical(EmptyObject(object))
  ll
}



# ---- Internal Classes ----

# Equilibrium values for a given F 
setClass("curves",
         slots=c(FValues='numeric',
                 NPR='list',
                 NPRS='list',
                 SPR='list',
                 YPR='list',
                 RPR='list',
                 RelRec='list',
                 Recruit='list',
                 Yield='list',
                 Removal='list',
                 Biomass='list',
                 SBiomass='list',
                 SP='list',
                 Misc='list'
         ),
         contains='Created_ModifiedClass'
)

setClass("unfishedrefpoints",
         slots=c(
           N0='list',
           B0='list',
           SB0='list',
           SP0='list',
           Misc='list'
         ),
         contains='Created_ModifiedClass'
)

setClass("refpoints",
         slots=c(Curves='curves',
                 SPR0='list',
                 MSY='list',
                 FMSY='list',
                 BMSY='list',
                 SBMSY='list',
                 SPMSY='list',
                 SPRMSY='list',
                 F01='list',
                 FMax='list',
                 FCrash='list',
                 SPRcrash='list',
                 MGT='list',
                 RefYield='list',
                 BLow='list',
                 Equilibrium='unfishedrefpoints',
                 Dynamic='unfishedrefpoints',
                 Misc='list'
         ),
         contains='Created_ModifiedClass'
)


setClass("popdynamics",
         slots=c(Number='list',
                 Biomass='list',
                 SBiomass='list',
                 SProduction='list',
                 Misc='list'
         ),
         contains='Created_ModifiedClass'
)

setClass("unfished",
         slots=c(Equilibrium='popdynamics',
                 Dynamic='popdynamics',
                 Misc='list'
         ),
         contains='Created_ModifiedClass'
)


# Hist Class ----

# OM - Operating Model
# Reference Points
# Unfished
# PopulationDynamics
# FleetDynamics


#' @export
setClass("hist",
         contains=c('om', 'Created_ModifiedClass'),
         slots=c(Unfished='unfished',
                 RefPoints='refpoints',
                 Number='list', # sim, age, ts, area
                 Biomass='list',
                 SBiomass='list',
                 SProduction='list',
                 Removal='list',
                 Retain='list'
                 
         )
)





