#' SRR
#'
#' Construct a [srr-class] object defining the stock-recruit relationship (SRR)
#' associated with a [Stock()].
#'
#' @param Pars A named list of parameters defining the expected stock–recruit
#' curve. Parameter names and interpretation depend on the chosen `Model`.
#'
#' @param Model Character string or function identifying the SRR model used to
#' calculate expected recruitment (e.g. `"BevertonHolt"`).
#'
#' @param R0 Numeric value or array giving unfished recruitment.
#'
#' @param SD Numeric vector or array giving the standard deviation of recruitment
#' deviations in log space. May be:
#' 
#'  * length 1 (constant across simulations)
#'  * length 2 (uniform distribution bounds)
#'  * length `nSim`.
#' 
#'
#' @param AC Numeric vector or array giving the lag-1 autocorrelation of recruitment
#' deviations. Same structure rules as `SD`.
#'
#' @param SPFrom Character or numeric value indicating the stock to use as the 
#' spawning production source for this stock. Defaults to the same stock.
#'
#' @param TruncSD Number of standard deviations used to truncate the lognormal
#' distribution of recruitment deviations. Defaults to 2.
#'
#' @param RecDevInit Optional numeric matrix (`nSim × MaxAge`) of recruitment
#' deviations for the initial age structure.
#'
#' @param RecDevHist Optional numeric matrix (`nSim × nHistTS`) of recruitment
#' deviations for historical time steps.
#'
#' @param RecDevProj Optional numeric matrix (`nSim × nProjectionTS`) of recruitment
#' deviations for projection time steps.
#'
#' @param SpawnTimeFrac Numeric value between 0 and 1 giving the fraction of the
#' time step when spawning occurs. Defaults to 0 (start of time step).
#'
#' @param RelRecFun Optional function defining relative recruitment.
#'
#' @param Units Numeric scaling factor for recruitment.
#'
#' @param Misc Miscellaneous list.
#'
#' @details
#' The `SRR()` constructor defines both the deterministic stock–recruit
#' relationship and the stochastic recruitment deviations used in an operating
#' model.
#'
#' Recruitment deviations may be supplied directly via `RecDevInit`,
#' `RecDevHist`, and `RecDevProj`, or generated internally from `SD` and `AC`
#' during model setup.
#'
#' A `SRR` object can be attached to a [Stock()] using `SRR(Stock) <- MySRR` and
#' retrieved using `MyLSRR <- SRR(Stock)`
#'
#' Individual components may be accessed or modified using accessor and
#' replacement functions such as [Pars()], [Model()], and [R0()].
#'
#' @return A  [srr-class] object.
#' 
#' @example man-examples/SRR-class.R
#'
#' @export
SRR <- function(Pars = list(h = NA),
                Model = "BevertonHolt",
                R0 = NULL,
                SD = NULL,
                AC = NULL,
                SPFrom = NULL,
                TruncSD = 2,
                RecDevInit = NULL,
                RecDevHist = NULL,
                RecDevProj = NULL,
                SpawnTimeFrac = 0,
                RelRecFun = NULL,
                Units = 1,
                Misc = list()) {
  
  if (!inherits(Pars, 'list')) {
    if (!'SRR' %in% slotNames(Pars))
      cli::cli_abort(c('x'='No slot {.val SRR} found in object class {.val {class(Pars)}}'))
    return(Pars@SRR)
  }
  
  
  if (inherits(Pars, 'stock'))
    return(Pars@SRR)
  
  obj <- new(
    "srr",
    Pars = Pars,
    Model = Model,
    R0 = R0,
    SD = SD,
    AC = AC,
    SPFrom = SPFrom,
    TruncSD = TruncSD,
    RecDevInit = RecDevInit,
    RecDevHist = RecDevHist,
    RecDevProj = RecDevProj,
    SpawnTimeFrac = SpawnTimeFrac,
    RelRecFun = RelRecFun,
    Units = Units,
    Misc = Misc
  )
  
  methods::validObject(obj)
  obj
}


#' @rdname SRR
#' @export
R0 <- function(x) {
  CheckClass(x, "srr", "x")
  x@R0
}

#' @rdname SRR
#' @export
`R0<-` <- function(x, value) {
  CheckClass(x, "srr", "x")
  x@R0 <- value
  methods::validObject(x)
  x
}


#' @rdname SRR
#' @export
SPFrom <- function(x) {
  CheckClass(x, "srr", "x")
  x@SPFrom
}

#' @rdname SRR
#' @export
`SPFrom<-` <- function(x, value) {
  CheckClass(x, "srr", "x")
  x@SPFrom <- value
  methods::validObject(x)
  x
}

#' @rdname SRR
#' @export
RecDevInit <- function(x) {
  CheckClass(x, "srr", "x")
  x@RecDevInit
}

#' @rdname SRR
#' @export
`RecDevInit<-` <- function(x, value) {
  CheckClass(x, "srr", "x")
  x@RecDevInit <- value
  methods::validObject(x)
  x
}

#' @rdname SRR
#' @export
RecDevHist <- function(x) {
  CheckClass(x, "srr", "x")
  x@RecDevHist
}

#' @rdname SRR
#' @export
`RecDevHist<-` <- function(x, value) {
  CheckClass(x, "srr", "x")
  x@RecDevHist <- value
  methods::validObject(x)
  x
}

#' @rdname SRR
#' @export
RecDevProj <- function(x) {
  CheckClass(x, "srr", "x")
  x@RecDevProj
}

#' @rdname SRR
#' @export
`RecDevProj<-` <- function(x, value) {
  CheckClass(x, "srr", "x")
  x@RecDevProj <- value
  methods::validObject(x)
  x
}

#' @rdname SRR
#' @export
SpawnTimeFrac <- function(x) {
  CheckClass(x, "srr", "x")
  x@SpawnTimeFrac
}

#' @rdname SRR
#' @export
`SpawnTimeFrac<-` <- function(x, value) {
  CheckClass(x, "srr", "x")
  x@SpawnTimeFrac <- value
  methods::validObject(x)
  x
}

#' @rdname SRR
#' @export
RelRecFun <- function(x) {
  CheckClass(x, "srr", "x")
  x@RelRecFun
}

#' @rdname SRR
#' @export
`RelRecFun<-` <- function(x, value) {
  CheckClass(x, "srr", "x")
  x@RelRecFun <- value
  methods::validObject(x)
  x
}

#' @rdname SRR
#' @export
`SRR<-`<- function(x, value) {
  CheckClass(x, "stock", "x")
  x@SRR <- value
  methods::validObject(x)
  x
}









