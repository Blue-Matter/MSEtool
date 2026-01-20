
## SRR ----

#' SRR Class and Constructor
#'

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
#'
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
#'
#' @name SRR
#' @rdname SRR
#' 
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#' @example man-examples/SRR-class.R
#'
#' @export
setClass("srr",
         slots=c(Pars='list',
                 Model='fun.char',
                 R0='num.array.null',
                 SD='num.array.null',
                 AC='num.array.null',
                 SPFrom='char.num',
                 TruncSD='num.null',
                 RecDevInit='num.array.list',
                 RecDevHist='num.array.list',
                 RecDevProj='num.array.list',
                 SpawnTimeFrac='numeric',
                 RelRecFun="fun.char",
                 Units='numeric',
                 Misc='list'
         )
)


setValidity('srr', function(object) {
  # TODO 
  TRUE
})

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
                                        Units=1,
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
  .Object@Units <- Units
  .Object@Misc <- Misc

  if (length(Pars)>0 &
      !is.null(names(Pars)) &
      all(!is.na(unlist(Pars))) &
      is.null(Model))
    .Object@Model <- FindModel(.Object)
  
  .Object
})


