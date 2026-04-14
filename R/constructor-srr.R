#' Stock-Recruit Relationship
#'
#' Construct and manipulate a [srr-class] object defining the stock-recruit
#' relationship (SRR) associated with a [Stock()] object. An `SRR` object is
#' required for all [stock-class] objects.
#'
#' @param Pars Named list of parameters defining the expected stock-recruit
#'   curve. Parameter names and interpretation depend on `Model`. See
#'   [SRRModels()] for required parameters for each model. If `Pars` is an S4
#'   object with an `SRR` slot (e.g., a [stock-class] object), that slot is
#'   returned directly.
#' @param Model Character. Stock-recruit model identifier
#'   (e.g., `"BevertonHolt"`). Default `"BevertonHolt"`. See [SRRModels()]
#'   for available models.
#' @param R0 Numeric scalar or array. Unfished recruitment. Default `NULL`.
#' @param SD Numeric vector or array. Standard deviation of log-space
#'   recruitment deviations. May be:
#'   - length 1: constant across all simulations,
#'   - length 2: bounds of a uniform distribution sampled across simulations,
#'   - length `nSim`: one value per simulation.
#'
#'   Default `NULL`, in which case recruitment deviations are deterministic.
#' @param AC Numeric vector or array. Lag-1 autocorrelation of log-space
#'   recruitment deviations. Follows the same length conventions as `SD`.
#'   Default `NULL`.
#' @param SPFrom Character or numeric. Stock to use as the spawning production
#'   source for this stock's SRR. Defaults to the same stock (`NULL`).
#' @param TruncSD Numeric. Number of standard deviations at which to truncate
#'   the lognormal recruitment deviation distribution. Default `2`.
#' @param RecDevInit Numeric matrix (`nSim × MaxAge`). Recruitment deviations
#'   for the initial age structure. If `NULL` (default), generated internally
#'   from `SD` and `AC`.
#' @param RecDevHist Numeric matrix (`nSim × nHistTS`). Recruitment deviations
#'   for historical time steps. If `NULL` (default), generated internally from
#'   `SD` and `AC`.
#' @param RecDevProj Numeric matrix (`nSim × nProjectionTS`). Recruitment
#'   deviations for projection time steps. If `NULL` (default), generated
#'   internally from `SD` and `AC`.
#' @param SpawnTimeFrac Numeric. Fraction of the time step at which spawning
#'   occurs (0 = start, 1 = end). Default `0`.
#' @param RelRecFun Function. Optional function defining relative recruitment
#'   as a function of environmental or other covariates. Default `NULL`.
#' @param Units Numeric. Scaling factor applied to recruitment. Default `1`.
#' @param Misc List. Miscellaneous additional inputs. Default `list()`.
#' @param x A [srr-class] object for slot accessors, or a [stock-class] object
#'   for `SRR<-`.
#' @param value For `SRR<-`: a [srr-class] object. For slot replacement
#'   functions: the new value for the corresponding slot.
#'
#' @details
#' An [srr-class] object is required for all [stock-class] objects. It defines
#' both the deterministic stock-recruit relationship and the stochastic
#' recruitment deviations used in the operating model.
#'
#' ## Pass-Through Access
#'
#' If `Pars` is an S4 object with an `SRR` slot (e.g., a [stock-class]
#' object), `SRR()` returns that slot directly rather than constructing a new
#' object.
#'
#' ## Recruitment Deviations
#'
#' Recruitment deviations may be supplied directly via `RecDevInit`,
#' `RecDevHist`, and `RecDevProj`. If not supplied, they are generated
#' internally during model setup from `SD` and `AC`. If `SD` is `NULL`,
#' recruitment is deterministic.
#'
#' ## Attaching to a Stock
#'
#' An `SRR` object can be attached to a [Stock()] with
#' `SRR(Stock) <- MySRR` and retrieved with `SRR(Stock)`.
#'
#' Individual slots may be accessed or modified using [Pars()], [Model()],
#' [R0()], [SD()], [AC()], [SPFrom()], [TruncSD()], [RecDevInit()],
#' [RecDevHist()], [RecDevProj()], [SpawnTimeFrac()], and [RelRecFun()].
#'
#' `r TechManLink()`
#'
#' @return
#' - `SRR()` returns a [srr-class] object. If `Pars` is an S4 object with an
#'   `SRR` slot, that slot is returned.
#' - `SRR<-` returns `x` with the `SRR` slot replaced.
#' - Slot accessors return the value of the corresponding slot from `x`.
#' - Slot replacement functions return `x` with the corresponding slot
#'   updated.
#'
#' @seealso [srr-class], [Stock()], [SRRModels()], [Pars()], [Model()],
#'   [R0()], [RecDevHist()], [RecDevProj()], [SpawnTimeFrac()]
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
      cli::cli_abort(c('x'='No slot {.val SRR} found in object class {.cls {class(Pars)}}'))
    return(Pars@SRR)
  }
  

  obj <- methods::new(
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
  if (inherits(x, 'srr'))
    return(x@R0)
  
  if (inherits(x, 'hist')) {
    return(purrr::map(x@OM@Stock, \(stock)
                      ReduceDims(stock@SRR@R0, IncYear = TRUE) 
    ) |> List2Array("Stock", pos=2)
    )
  }
  if (inherits(x, 'om')) {
    return(purrr::map(x@Stock, \(stock)
                      ReduceDims(stock@SRR@R0, IncYear = TRUE) 
    ) |> List2Array("Stock", pos=2)
    ) 
  }
  
  
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
`SRR<-` <- function(x, value) {
  CheckClass(x, "stock", "x")
  x@SRR <- value
  methods::validObject(x)
  x
}









