#' SRR Constructor and Accessors
#'
#' Construct a [srr-class] object defining the stock-recruitment relationship
#' and recruitment variability for a [stock-class], or access and replace the
#' `SRR` slot of a [stock-class] and its individual slots. An `SRR` object is
#' required for all [stock-class] objects.
#'
#' @param Pars `list`. Named list containing the steepness parameter for the
#'   chosen SRR model. `R0` is never placed in `Pars` — it has its own
#'   dedicated slot. The required name depends on `Model`:
#'   - `BevertonHolt`: `list(h = ...)` where `0.2 < h < 1`.
#'   - `Ricker`: `list(hR = ...)`.
#'   - `HockeyStick`: `list(Shinge = ...)` where `0 < Shinge <= 1`.
#'   When `Pars` is a non-list S4 object with an `SRR` slot (e.g., a
#'   [stock-class]), `SRR()` acts as a pass-through accessor and returns that
#'   slot. See [Specifying Biological and Fleet Schedules](https://docs.openmse.com/concept-schedules.html)
#'   for accepted input formats (scalar, bounds vector, `nSim`-length vector).
#'   Default `list(h = NA)`.
#' @param Model `character(1)` or `function`. SRR model identifier. Must match
#'   one of the models listed in [SRRModels()], or be a custom R function.
#'   Default `"BevertonHolt"`.
#' @param R0 `numeric`, `array`, or `NULL`. Unfished equilibrium recruitment.
#'   May be:
#'   - A scalar: same value for all simulations and years.
#'   - A length-2 bounds vector: sampled from `Uniform(lower, upper)` once per
#'     simulation.
#'   - A length-`nSim` vector: one value per simulation, constant over years.
#'   - A `Sim x Year` array: deliberately time-varying, e.g. to model a
#'     regime shift in carrying capacity. See [srr-class] for how this
#'     differs from a constant `R0`.
#'   Interpreted in units of `Units`. Default `NULL`.
#' @param SD `numeric` or `NULL`. Log-space standard deviation of recruitment
#'   deviations. Follows the same length conventions as `R0`. Currently fixed
#'   across years (time-varying `SD` is not yet supported). When `NULL`,
#'   recruitment is deterministic (no process error). Default `NULL`.
#' @param AC `numeric` or `NULL`. Lag-1 autocorrelation of log-space
#'   recruitment deviations. Follows the same length conventions as `R0`.
#'   Currently fixed across years. Defaults to `0` (no autocorrelation) when
#'   `NULL`. Default `NULL`.
#' @param SPFrom `character(1)` or `numeric(1)`. Stock whose spawning
#'   production drives this stock's recruitment. Specify by stock name
#'   (`character`) or 1-based integer index (`numeric`). Defaults to self
#'   (the stock recruits from its own spawning production). Relevant only in
#'   multi-stock operating models. Default `NULL`.
#' @param TruncSD `numeric(1)`. Number of standard deviations at which the
#'   log-normal recruitment deviation distribution is truncated. Default `2`.
#' @param RecDevInit `matrix` or `NULL`. Pre-specified recruitment deviations
#'   (`Sim × Age`) for initialising the historical age structure, covering all
#'   age classes except the minimum age (whose deviation is the first column
#'   of `RecDevHist`). If `NULL` (default), generated internally from `SD`
#'   and `AC` during [Populate()].
#' @param RecDevHist `matrix` or `NULL`. Pre-specified recruitment deviations
#'   (`Sim × nHistTS`) for historical time steps. The first column corresponds
#'   to the minimum age class in the initial year. If `NULL` (default),
#'   generated internally from `SD` and `AC`.
#' @param RecDevProj `matrix` or `NULL`. Pre-specified recruitment deviations
#'   (`Sim × nProjTS`) for projection time steps. If `NULL` (default),
#'   generated internally from `SD` and `AC`.
#' @param SpawnTimeFrac `numeric(1)`. Fraction of the time step elapsed before
#'   spawning occurs. Determines how much mortality (`exp(-Z × SpawnTimeFrac)`)
#'   is applied to the population before spawning numbers are counted:
#'   - `0` (default): spawning at the start of the step; no mortality applied
#'     before spawning.
#'   - `0.5`: spawning mid-step; half of annual Z applied before spawning.
#'   - `1`: spawning at the end of the step; full within-step Z applied before
#'     spawning.
#'   Default `0`.
#' @param SpawnLag `numeric(1)` or `NULL`. Number of timesteps between the
#'   spawning production evaluation and recruitment. When `NULL` (default), the
#'   lag is derived automatically from `min(Ages@Classes) * Seasons`. Set
#'   explicitly when the spawn season differs from what `min(Ages@Classes)`
#'   implies (e.g. SS3 models where `Spawn_month` and `birthseas` are
#'   decoupled). Default `NULL`.
#' @param RelRecFun `function`, `character(1)`, or `NULL`. Relative recruitment
#'   function giving equilibrium recruitment relative to `R0` as a function of
#'   spawning-per-recruit (SPR). For built-in models, set automatically during
#'   [Populate()] (e.g., `"BevertonHolt_RelRec"` when `Model =
#'   "BevertonHolt"`). For custom SRR models, must be supplied with signature
#'   `function(Pars, SPR)`. Default `NULL`.
#' @param Units `numeric(1)`. Scaling factor for recruitment. `1` (default)
#'   means `R0` is in absolute numbers of fish; `1000` means `R0` is in
#'   thousands of fish. Does not affect internal calculations — used only to
#'   set the interpretation of numbers in [hist-class] output. Default `1`.
#' @param Misc `list`. Used internally. Default `list()`.
#' @param x A [srr-class] object for slot accessors, or a [stock-class] object
#'   for `SRR<-`.
#' @param value For `SRR<-`: a [srr-class] object. For slot replacement
#'   functions: the replacement value for the corresponding slot.
#'
#' @details
#' ## Specifying the SRR
#'
#' The SRR is defined by three components:
#'
#' 1. **The deterministic curve**: `Model` and `Pars` (steepness only; `R0`
#'    goes in its own slot). See [SRRModels()] for available models:
#'
#' ```r
#' # Beverton-Holt with fixed steepness
#' srr <- SRR(Pars = list(h = 0.7), R0 = 1000)
#'
#' # Beverton-Holt with stochastic steepness across simulations
#' srr <- SRR(Pars = list(h = c(0.6, 0.9)), R0 = 1000)
#'
#' # Ricker
#' srr <- SRR(Pars = list(hR = 0.7), Model = "Ricker", R0 = 1000)
#'
#' # Hockey-stick
#' srr <- SRR(Pars = list(Shinge = 0.3), Model = "HockeyStick", R0 = 1000)
#' ```
#'
#' 2. **Process error**: `SD` and `AC` control the magnitude and
#'    autocorrelation of log-space recruitment deviations. Both are currently
#'    fixed across years:
#'
#' ```r
#' # Stochastic recruitment: SD = 0.4, no autocorrelation
#' srr <- SRR(Pars = list(h = 0.7), R0 = 1000, SD = 0.4)
#'
#' # Stochastic recruitment: SD drawn from Uniform(0.3, 0.6), AC = 0.4
#' srr <- SRR(Pars = list(h = 0.7), R0 = 1000,
#'            SD = c(0.3, 0.6), AC = 0.4)
#' ```
#'
#' 3. **Recruitment deviations** (`RecDevInit`, `RecDevHist`, `RecDevProj`):
#'    generated automatically from `SD` and `AC` during [Populate()] when
#'    `NULL`. Supply directly to condition the operating model on observed
#'    recruitment indices or to reproduce a specific stochastic trajectory.
#'
#' ## Custom SRR Models
#'
#' A custom SRR function may be passed to `Model`. It must accept `S`, `S0`,
#' `R0`, and any named parameters in `Pars` as arguments. A matching
#' `RelRecFun` with signature `function(Pars, SPR)` must also be supplied:
#'
#' ```r
#' my_srr <- function(S, S0, R0, h) {
#'   # custom Beverton-Holt variant
#'   4 * h * R0 * S / (S0 * (1 - h) + S * (5 * h - 1))
#' }
#' my_rrf <- function(Pars, SPR) {
#'   h <- Pars$h
#'   CR <- 4 * h / (1 - h)
#'   pmax((CR * SPR - 1) / ((CR - 1) * SPR), 0)
#' }
#' srr <- SRR(Pars = list(h = 0.7), Model = my_srr,
#'            R0 = 1000, RelRecFun = my_rrf)
#' ```
#'
#' ## Spawning Production Source (`SPFrom`)
#'
#' In multi-stock models, one stock's recruitment may be driven by another
#' stock's spawning production. Specify by stock name or 1-based index:
#'
#' ```r
#' # Stock 2 recruits based on stock 1's spawning production
#' srr2 <- SRR(Pars = list(h = 0.7), R0 = 500, SPFrom = 1)
#' # equivalently by name:
#' srr2 <- SRR(Pars = list(h = 0.7), R0 = 500, SPFrom = "Stock1")
#' ```
#'
#' ## Pass-Through Access from a Stock
#'
#' When `Pars` is a non-list S4 object with an `SRR` slot, `SRR()` returns
#' that slot directly:
#'
#' ```r
#' SRR(my_stock)           # returns my_stock@SRR
#' SRR(my_stock) <- my_srr # replaces my_stock@SRR
#' ```
#'
#' ## Slot Accessors
#'
#' Individual slots can be read or replaced using generic functions matching
#' their names. All replacement functions re-validate the object:
#'
#' ```r
#' Pars(srr)          <- list(h = 0.8)
#' Model(srr)         <- "Ricker"
#' R0(srr)            <- 5000
#' SD(srr)            <- 0.4
#' AC(srr)            <- 0.3
#' TruncSD(srr)       <- 2
#' SpawnTimeFrac(srr) <- 0.5
#' SPFrom(srr)        <- 1
#' RelRecFun(srr)     <- "Ricker_RelRec"
#' RecDevHist(srr)    <- my_hist_matrix
#' RecDevProj(srr)    <- my_proj_matrix
#' RecDevInit(srr)    <- my_init_matrix
#' ```
#'
#' @return
#' - `SRR()` returns a [srr-class] object. If `Pars` is a non-list S4 object
#'   with an `SRR` slot, returns that slot.
#' - `SRR<-` returns the [stock-class] `x` with the `SRR` slot replaced and
#'   the object re-validated.
#' - `R0()`, `SPFrom()`, `RecDevInit()`, `RecDevHist()`, `RecDevProj()`,
#'   `SpawnTimeFrac()`, `RelRecFun()` return the value of the corresponding
#'   slot from `x`.
#' - All replacement variants return `x` with the named slot updated and the
#'   object re-validated.
#'
#' @seealso
#' - [srr-class] for the class definition and slot-level documentation.
#' - [SRRModels()] for available stock-recruitment models and their required
#'   parameters.
#' - [Populate()] for array population.
#' - [Stock()] for the enclosing stock constructor.
#' - [Fecundity()] for the spawning production used as input to the SRR.
#' - [Specifying Biological and Fleet Schedules](https://docs.openmse.com/concept-schedules.html) for
#'   accepted `Pars`, `R0`, `SD`, and `AC` input formats.
#'
#' @family srr
#'
#' @example man-examples/class-SRR.R
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
                SpawnLag = NULL,
                RelRecFun = NULL,
                Units = 1,
                Misc = list()) {
  
  if (.IsStockOrList(Pars)) 
    return(.ExtractStockSlot(Pars, "SRR"))
  
  if (is.null(Pars))
    return(NULL)
  
  obj <- methods::new(
    "srr",
    Pars          = Pars,
    Model         = Model,
    R0            = R0,
    SD            = SD,
    AC            = AC,
    SPFrom        = SPFrom,
    TruncSD       = TruncSD,
    RecDevInit    = RecDevInit,
    RecDevHist    = RecDevHist,
    RecDevProj    = RecDevProj,
    SpawnTimeFrac = SpawnTimeFrac,
    SpawnLag      = SpawnLag,
    RelRecFun     = RelRecFun,
    Units         = Units,
    Misc          = Misc
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
                      ReduceDims(stock@SRR@R0) 
    ) |> List2Array("Stock", pos=2)
    )
  }
  if (inherits(x, 'om')) {
    return(purrr::map(x@Stock, \(stock)
                      ReduceDims(stock@SRR@R0) 
    ) |> List2Array("Stock", pos=2)
    ) 
  }
  
  
}

#' @rdname SRR
#' @export
`R0<-` <- function(x, value) {
  .CheckClass(x, "srr", "x")
  x@R0 <- value
  methods::validObject(x)
  x
}


#' @rdname SRR
#' @export
SPFrom <- function(x) {
  .CheckClass(x, "srr", "x")
  x@SPFrom
}

#' @rdname SRR
#' @export
`SPFrom<-` <- function(x, value) {
  .CheckClass(x, "srr", "x")
  x@SPFrom <- value
  methods::validObject(x)
  x
}

#' @rdname SRR
#' @export
RecDevInit <- function(x) {
  .CheckClass(x, "srr", "x")
  x@RecDevInit
}

#' @rdname SRR
#' @export
`RecDevInit<-` <- function(x, value) {
  .CheckClass(x, "srr", "x")
  x@RecDevInit <- value
  methods::validObject(x)
  x
}

#' @rdname SRR
#' @export
RecDevHist <- function(x) {
  .CheckClass(x, "srr", "x")
  x@RecDevHist
}

#' @rdname SRR
#' @export
`RecDevHist<-` <- function(x, value) {
  .CheckClass(x, "srr", "x")
  x@RecDevHist <- value
  methods::validObject(x)
  x
}

#' @rdname SRR
#' @export
RecDevProj <- function(x) {
  .CheckClass(x, "srr", "x")
  x@RecDevProj
}

#' @rdname SRR
#' @export
`RecDevProj<-` <- function(x, value) {
  .CheckClass(x, "srr", "x")
  x@RecDevProj <- value
  methods::validObject(x)
  x
}

#' @rdname SRR
#' @export
SpawnTimeFrac <- function(x) {
  .CheckClass(x, "srr", "x")
  x@SpawnTimeFrac
}

#' @rdname SRR
#' @export
`SpawnTimeFrac<-` <- function(x, value) {
  .CheckClass(x, "srr", "x")
  x@SpawnTimeFrac <- value
  methods::validObject(x)
  x
}

#' @rdname SRR
#' @export
SpawnLag <- function(x) {
  .CheckClass(x, "srr", "x")
  x@SpawnLag
}

#' @rdname SRR
#' @export
`SpawnLag<-` <- function(x, value) {
  .CheckClass(x, "srr", "x")
  x@SpawnLag <- value
  methods::validObject(x)
  x
}

#' @rdname SRR
#' @export
RelRecFun <- function(x) {
  .CheckClass(x, "srr", "x")
  x@RelRecFun
}

#' @rdname SRR
#' @export
`RelRecFun<-` <- function(x, value) {
  .CheckClass(x, "srr", "x")
  x@RelRecFun <- value
  methods::validObject(x)
  x
}

#' @rdname SRR
#' @export
`SRR<-` <- function(x, value) {
  .AssignSlotRecursive(x, value, 'SRR')
}







