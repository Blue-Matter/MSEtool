
## SRR ----

#' SRR Object
#'
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#' 
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
                 RecDevInit='num.array.list',
                 RecDevHist='num.array.list',
                 RecDevProj='num.array.list',
                 SpawnTimeFrac='numeric',
                 RelRecFun="fun.char",
                 Misc='list'
         )
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
  #   .Object@Created <- Sys.time()
  
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

