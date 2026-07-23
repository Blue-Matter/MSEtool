#' OM Constructor and Accessor
#'
#' Construct an [om-class] object defining the complete specification of an
#' operating model (OM) for use in Management Strategy Evaluation (MSE), or
#' extract the `OM` slot from an enclosing object.
#'
#' @param Name Character. Name of the operating model. Default
#'   `"A new OM object"`.
#'
#'   If `Name` is an S4 object with an `OM` slot (e.g. a [hist-class] or
#'   [mse-class] object), `OM()` returns that slot directly rather than
#'   constructing a new object. See *Pass-Through Access* in Details.
#' @param Agency Character. Name of the agency responsible for management.
#'   Supports Markdown. Default `""`.
#' @param Author Character vector. Name(s) of the author(s) of the operating
#'   model. Default `""`.
#' @param Email Character vector. Email address(es) corresponding to `Author`.
#'   Supports Markdown. Default `""`.
#' @param Region Character. Name of the geographic region of the fishery.
#'   Default `""`.
#' @param Latitude Numeric. Latitude (decimal degrees) of the centre of
#'   `Region`. Default `NULL`.
#' @param Longitude Numeric. Longitude (decimal degrees) of the centre of
#'   `Region`. Default `NULL`.
#' @param Sponsor Character. Organisation sponsoring development of the
#'   operating model. Supports Markdown. Default `""`.
#' @param nSim Positive integer. Number of stochastic simulations. Default
#'   `48`.
#' @param nYear Numeric. Number of historical years. Default `20`.
#' @param pYear Numeric. Number of projection years. Default `30`.
#' @param CurrentYear Integer. Final historical calendar year of the operating
#'   model. Default is the current system year.
#' @param Seasons Integer. Number of seasons per year. Default `1`.
#' @param RefSeason Integer vector or `NULL`. Only used for `Seasons > 1`.
#'   Season index/indices (`1..Seasons`) used as the reference snapshot(s) for
#'   reporting equilibrium biomass and spawning biomass (e.g. `BMSY`, `SBMSY`,
#'   `B0`, `SB0`, `BLow`). `NULL` (default) auto-detects, independently per
#'   simulation, which season(s) have nonzero spawning contribution, and
#'   averages the cross-sectional snapshot across them when more than one is
#'   detected. Default `NULL`.
#' @param RefEffortYears Numeric vector or `NULL`. Only used for `Seasons > 1`. 
#'   One or more historical calendar years whose relative seasonal
#'   effort/catchability pattern fixes the seasonal shape of fishing
#'   mortality during per-recruit and MSY reference point optimization,
#'   decoupled from the year used for biological parameters. `NULL` (default)
#'   reuses the same year as the biological parameters. When more than one
#'   year is given, the per-season effort is averaged across those years.
#'   Default `NULL`.
#' @param Stock A [stock-class] object or named list of [stock-class] objects.
#'   Default `NULL`. See [Stock()] for construction and pass-through access.
#' @param Fleet A hierarchical named list of [fleet-class] objects indexed by
#'   stock then fleet. Each stock must have the same number of fleets. Default
#'   `NULL`. See [Fleet()] for construction and pass-through access.
#' @param Obs A hierarchical named list of [obs-class] objects indexed by
#'   stock and fleet. Default `NULL`. See [Obs()] for construction and
#'   pass-through access.
#' @param Imp A hierarchical named list of [imp-class] objects indexed by
#'   stock and fleet. Default `NULL`. See [Imp()] for construction and
#'   pass-through access.
#' @param Data A [data-class] object or list of [data-class] objects
#'   associated with the operating model. Default `NULL`. See [Data()] for
#'   construction and pass-through access.
#' @param DataLag Integer. Number of time steps that data are lagged relative
#'   to management implementation. Default `0`.
#' @param CatchFrac List. Named list of length 0 or `nStock(OM)`. Each
#'   element is an `nSim` by `nFleet` matrix (or a single row recycled
#'   across sims) giving the fraction of catch taken by each fleet, with
#'   rows summing to 1. Only used when there is more than one fleet and a
#'   historical `Depletion@Final` target is set for at least one stock, in
#'   which case it is the target fleet split that catchability is
#'   calibrated to reproduce. If left unspecified for a stock, it is
#'   derived from relative Effort times Catchability in the final
#'   historical year. Default `NULL`.
#' @param Allocation List. Named list of length 0 or the number of stock
#'   complexes. Each element is an `nSim` by `nFleet` matrix, with rows
#'   summing to 1, controlling how the TAC is split among fleets during
#'   projection. If unspecified it falls back to `CatchFrac`, and then to
#'   the mean of removals over the last five historical years. Default
#'   `NULL`.
#' @param EFactor List. Effort or exploitation modifiers applied during
#'   projection. Default `NULL`.
#' @param Complexes List. Defines stock complexes for data aggregation and
#'   management. Default `NULL`.
#' @param Herm List. Defines hermaphroditism or movement between stocks.
#'   Default `NULL`.
#' @param SharePar Logical. Whether key parameters are shared among stocks.
#'   Default `NULL`.
#' @param Relations List. Biological or ecological relationships among stocks
#'   (e.g. predator-prey). Default `NULL`. Currently not used.
#' @param StockTargeting A [stocktargeting-class] object. Defines fleet-level
#'   stock targeting weights and deviations. Populated internally.
#'    See [StockTargeting()].
#'   
#' @param Interval Numeric scalar or named numeric vector. Management update
#'   interval in years. Default `1`.
#' @param MPStartYear Numeric or `NULL`. First calendar year in which MPs are
#'   applied - see *Interim Advice* in Details. Default `NULL`.
#' @param InterimAdvice A `data.frame` or `NULL`. Fixed or stochastic
#'   TAC/Effort values for years before `MPStartYear` - see *Interim Advice*
#'   in Details. Default `NULL`.
#' @param nReps Positive integer. Number of stochastic replicates used for
#'   generating management advice. Default `1`. Not currently used.
#' @param pStar Numeric in \eqn{[0, 1]}. Percentile applied to stochastic
#'   management advice. Default `0.5`. Not currently used.
#' @param maxF Numeric. Maximum allowable instantaneous fishing mortality.
#'   Applies to nominal fishing mortality for fish that interact with the
#'   fishing gear; actual effective F may be lower if some fish are discarded
#'   and survive. Default `3`.
#' @param Seed Integer. Random number generator seed for reproducibility.
#'   Default `101`.
#' @param Control Named list of internal operating model control settings.
#'   Default `list()`.
#' @param Misc List. Miscellaneous objects or developer-use components.
#'   Default `list()`.
#' @param Source Character. References to data sources or documentation.
#'   Supports Markdown. Default `NULL`.
#'
#' @details
#' ## Pass-Through Access
#'
#' When `Name` is an S4 object with an `OM` slot, `OM()` returns that slot
#' directly rather than constructing a new object. This allows a consistent
#' interface for both construction and retrieval:
#'
#' ```r
#' om  <- OM(hist_obj)    # extract from `hist` or `mse`
#' ```
#'
#' ## Derived Slots
#'
#' The `Years` slot is not a constructor parameter. It is derived
#' automatically from `nYear`, `pYear`, `CurrentYear`, and `Seasons` via
#' [CalcYears()] and is read-only. Use [Years()] to extract it from a
#' constructed object.
#'
#' ## Interim Advice
#'
#' When the historical period ends before MPs should actually start being
#' applied (e.g. the OM is conditioned on data to 2024 but MPs will not be
#' implemented until 2028), set `MPStartYear` to the first calendar year MPs
#' should run. Projection years before `MPStartYear` are "interim" years:
#' the MP is not called, `Imp` (implementation error) is bypassed since these
#' values represent actual/plausible realised catch or effort rather than a
#' management recommendation, and advice is instead taken from
#' `InterimAdvice`.
#'
#' `InterimAdvice` is a `data.frame` with one row per Year x Stock x
#' (optionally Fleet), with columns:
#' - `Year`: calendar year, in `[first projection year, MPStartYear - 1]`.
#' - `Stock`: stock (or complex) name. Optional when the OM has a single
#'   stock/complex, in which case every row applies to it; required when
#'   there is more than one.
#' - `Fleet`: optional; fleet name, or `NA` to apply as a stock total (TAC,
#'   allocated across fleets the same way MP-supplied TAC is) or an identical
#'   value across fleets (Effort).
#' - `Type`: `"TAC"` or `"Effort"`.
#' - `Mean`: point estimate, natural scale. Must be `>= 0` (e.g. `0` for a
#'   fleet with no historical catch, maintained as a closure in interim
#'   years). A row with `Mean == 0` is always deterministic -- a lognormal
#'   draw cannot be centred at `0` -- regardless of `SD`.
#' - `SD`: optional; natural-scale SD for a lognormal draw (sampled once per
#'   simulation, per row). `NA`/`0` (default), or `Mean == 0`, gives a fixed,
#'   deterministic value. If supplied, must be `>= 0`, and must be `NA`/`0`
#'   wherever `Mean == 0`.
#' - `TACType`, `TACUnit`: as in [Advice()]; only used for `"TAC"` rows.
#' - `EffType`: as in [Advice()]; only used for `"Effort"` rows.
#'
#' Any Year x Stock combination with no matching row falls back to freezing
#' effort at the last historical level (the same default used when an MP
#' itself returns no TAC/Effort).
#'
#' For seasonal OMs (`Seasons > 1`), the annual `Mean` for a row is spread
#' across seasons using that stock/fleet's own seasonal shape from the last
#' historical year (Landings for TAC rows, Effort for Effort rows), so a
#' historical seasonal pattern is retained rather than flattened.
#'
#' ## StockTargeting Initialisation
#'
#' The `StockTargeting` slot is initialised automatically by [StockTargeting()]
#' when `Stock` and `Fleet` are provided. It is not a user-facing parameter
#' and should not be set directly.
#'
#' ## Sub-Object Slots
#'
#' `Stock`, `Fleet`, `Obs`, `Imp`, and `Data` each have dedicated constructor
#' functions that also act as pass-through accessors when called with an
#' [om-class] argument. Replacement is handled by the corresponding
#' replacement functions in each constructor file:
#'
#' ```r
#' Stock(om)  <- Stock("MyStock", ...)
#' Fleet(om)  <- list(s1 = list(f1 = Fleet("MyFleet", ...)))
#' Obs(om)    <- Obs("MyObs", ...)
#' Imp(om)    <- Imp("MyImp", ...)
#' Data(om)   <- Data(...)
#' ```
#'
#' ## Accessing and Assigning Slots
#'
#' All remaining slots in [om-class] objects can be accessed or assigned
#' using functions matching the slot names. See [OM-accessors] for the full
#' list:
#'
#' ```r
#' Agency(om)
#' Agency(om) <- "DFO"
#' ```
#'
#' @return
#' - `OM()` returns a new [om-class] object when `Name` is a character string
#'   or `NULL`.
#' - `OM()` returns `Name@OM` (an [om-class] object) when `Name` is an S4
#'   object with an `OM` slot.
#'
#' @seealso
#' - [om-class] for the class definition.
#' - [OM-accessors] for slot accessor and replacement functions.
#' - [Stock()], [Fleet()], [Obs()], [Imp()], [Data()] for sub-object
#'   constructors and their pass-through accessors.
#' - [Years()], [CalcYears()] for the derived time-step vector.
#' - [StockTargeting()] for the stock targeting sub-object.
#' - [Log()] for accessing runtime diagnostics.
#' - [PopulateOM()], [runMSE()] for downstream use of the operating model.
#' - [ConvertOM()], [ConvertMOM()] for converting legacy objects.
#'
#' @family om
#'
#' @examples
#' om <- OM()
#' Agency(om)
#' Agency(om) <- "DFO"
#'
#' nSim(om)
#' nSim(om) <- 100
#'
#' Years(om)
#'
#' @export
OM <- function(Name        = "A new OM object",
               Agency      = "",
               Author      = "",
               Email       = "",
               Region      = "",
               Latitude    = NULL,
               Longitude   = NULL,
               Sponsor     = "",
               
               nSim        = 48,
               nYear       = 20,
               pYear       = 30,
               CurrentYear = as.numeric(format(Sys.Date(), "%Y")),
               Seasons     = 1,
               RefSeason      = NULL,
               RefEffortYears = NULL,

               Stock       = NULL,
               Fleet       = NULL,
               Obs         = NULL,
               Imp         = NULL,
               
               Data        = NULL,
               DataLag     = 0,
               
               CatchFrac   = NULL,
               Allocation  = NULL,
               EFactor     = NULL,
               
               Complexes   = NULL,
               Herm        = NULL,
               SharePar    = NULL,
               Relations   = NULL,
               
               StockTargeting = NULL,
               
               Interval    = 1,
               MPStartYear = NULL,
               InterimAdvice = NULL,
               nReps       = 1,
               pStar       = 0.5,
               maxF        = 3,
               Seed        = 101,
               
               Control     = NULL,
               Misc        = list(),
               Source      = NULL) {
  
  if (!inherits(Name, "character")) {
    if (!"OM" %in% slotNames(Name))
      cli::cli_abort(c("x" = "No slot {.val OM} found in object class {.val {class(Name)}}"))
    return(Name@OM)
  }
  
  .Object <- new("om")
  .Object@Name        <- Name
  .Object@Agency      <- Agency
  .Object@Author      <- Author
  .Object@Email       <- Email
  .Object@Region      <- Region
  .Object@Latitude    <- Latitude
  .Object@Longitude   <- Longitude
  .Object@Sponsor     <- Sponsor
  
  .Object@nSim        <- nSim
  .Object@nYear       <- nYear
  .Object@pYear       <- pYear
  .Object@CurrentYear <- CurrentYear
  .Object@Seasons     <- Seasons
  .Object@RefSeason      <- RefSeason
  .Object@RefEffortYears <- RefEffortYears
  .Object@Years       <- CalcYears(nYear, pYear, CurrentYear, Seasons)
  
  Stock     <- if (!is.null(Stock)) .ToNamedList(Stock, 'stock') else NULL
  stock_nms <- names(Stock)
  compx_nms <- names(Complexes)
  if (is.null(compx_nms))
    compx_nms <- stock_nms
  
  Fleet     <- .ToNestedList(Fleet, 'fleet', stock_nms)
  fleet_nms <- if (!is.null(Fleet)) purrr::map(Fleet, names) else NULL

  # Obs/Imp are indexed by complex, not stock. When Complexes is explicit,
  # map each complex to the fleet names of its first stock. Full validation of
  # Complexes (names, index coverage) happens in .PopulateComplexes(); here we
  # are permissive so that stocks/obs/complexes can be added incrementally.
  obs_fleet_nms <- if (!is.null(Complexes) && !is.null(fleet_nms) && length(stock_nms) > 0) {
    purrr::map(setNames(nm = compx_nms), function(cx) {
      first_stk_idx <- Complexes[[cx]][1]
      stk_nm        <- if (!is.null(first_stk_idx) && first_stk_idx <= length(stock_nms))
                         stock_nms[[first_stk_idx]] else NULL
      if (is.null(stk_nm)) NULL else fleet_nms[[stk_nm]]
    })
  } else {
    fleet_nms
  }

  Obs       <- .ToNestedList(Obs,   'obs',   compx_nms, obs_fleet_nms)
  Imp       <- .ToNestedList(Imp,   'imp',   compx_nms, obs_fleet_nms)
                          
  .Object@Stock       <- Stock
  .Object@Fleet       <- Fleet
  .Object@Obs         <- Obs
  .Object@Imp         <- Imp
  
  .Object@Data        <- Data
  .Object@DataLag     <- DataLag
  
  .Object@CatchFrac   <- CatchFrac
  .Object@Allocation  <- Allocation
  .Object@EFactor     <- EFactor
  
  .Object@Complexes   <- Complexes
  .Object@Herm        <- Herm
  .Object@SharePar    <- SharePar
  .Object@Relations   <- Relations
  
  if (is.null(StockTargeting))
    StockTargeting <- StockTargeting(.Object)
  .Object@StockTargeting <- StockTargeting
  
  .Object@Interval    <- Interval
  .Object@MPStartYear <- MPStartYear
  .Object@InterimAdvice <- InterimAdvice
  .Object@nReps       <- nReps
  .Object@pStar       <- pStar
  .Object@maxF        <- maxF
  .Object@Seed        <- Seed
  
  .Object@Control     <- if (!is.null(Control)) Control else list()
  
  .Object@Misc        <- Misc
  .Object@Source      <- Source
  
  methods::validObject(.Object)
  .Object
}


#' Access and Modify OM Slots
#'
#' Accessor and replacement functions for slots in [om-class] objects.
#' Each function retrieves or replaces the value of the corresponding slot.
#' All accessors also accept [hist-class] and [mse-class] objects, extracting
#' the embedded `OM` slot transparently.
#'
#' @param x An [om-class], [hist-class], or [mse-class] object.
#' @param value Replacement value for the corresponding slot.
#'
#' @details
#' ## Sub-Object Slots
#'
#' The `Stock`, `Fleet`, `Obs`, `Imp`, and `Data` slots are accessed and
#' replaced via their own constructor functions, which act as pass-through
#' accessors when called with an [om-class] argument. They are not listed
#' here. See [Stock()], [Fleet()], [Obs()], [Imp()], [Data()].
#'
#' ## Read-Only Slots
#'
#' `Years` is derived automatically from `nYear`, `pYear`, `CurrentYear`, and
#' `Seasons` and is read-only. Use [Years()] to extract it.
#'
#' @return
#' - Accessor functions return the value of the named slot.
#' - Replacement functions return `x` with the named slot updated.
#'
#' @seealso
#' - [OM()] for the constructor.
#' - [om-class] for the class definition.
#' - [Stock()], [Fleet()], [Obs()], [Imp()], [Data()] for sub-object
#'   pass-through accessors.
#' - [Years()] for the read-only time-step vector.
#' - [Log()] for runtime diagnostics.
#'
#' @family om
#'
#' @examples
#' om <- OM()
#'
#' Agency(om)
#' Agency(om) <- "DFO"
#'
#' nSim(om)
#' nSim(om) <- 100
#'
#' maxF(om)
#' maxF(om) <- 5
#'
#' @name OM-accessors
NULL


#' @rdname OM-accessors
#' @export
Agency <- function(x) .IsHist(x, "Agency")

#' @rdname OM-accessors
#' @export
`Agency<-` <- function(x, value) .AssignSlot(x, value, "Agency")

#' @rdname OM-accessors
#' @export
Author <- function(x) .IsHist(x, "Author")

#' @rdname OM-accessors
#' @export
`Author<-` <- function(x, value) .AssignSlot(x, value, "Author")

#' @rdname OM-accessors
#' @export
Email <- function(x) .IsHist(x, "Email")

#' @rdname OM-accessors
#' @export
`Email<-` <- function(x, value) .AssignSlot(x, value, "Email")

#' @rdname OM-accessors
#' @export
Region <- function(x) .IsHist(x, "Region")

#' @rdname OM-accessors
#' @export
`Region<-` <- function(x, value) .AssignSlot(x, value, "Region")

#' @rdname OM-accessors
#' @export
Latitude <- function(x) .IsHist(x, "Latitude")

#' @rdname OM-accessors
#' @export
`Latitude<-` <- function(x, value) .AssignSlot(x, value, "Latitude")

#' @rdname OM-accessors
#' @export
Longitude <- function(x) .IsHist(x, "Longitude")

#' @rdname OM-accessors
#' @export
`Longitude<-` <- function(x, value) .AssignSlot(x, value, "Longitude")

#' @rdname OM-accessors
#' @export
Sponsor <- function(x) .IsHist(x, "Sponsor")

#' @rdname OM-accessors
#' @export
`Sponsor<-` <- function(x, value) .AssignSlot(x, value, "Sponsor")

#' @rdname OM-accessors
#' @export
Source <- function(x) .IsHist(x, "Source")

#' @rdname OM-accessors
#' @export
`Source<-` <- function(x, value) .AssignSlot(x, value, "Source")


#' @rdname OM-accessors
#' @export
nYear <- function(x) .IsHist(x, "nYear")

#' @rdname OM-accessors
#' @export
`nYear<-` <- function(x, value) .AssignSlot(x, value, "nYear")

#' @rdname OM-accessors
#' @export
pYear <- function(x) .IsHist(x, "pYear")

#' @rdname OM-accessors
#' @export
`pYear<-` <- function(x, value) .AssignSlot(x, value, "pYear")

#' @rdname OM-accessors
#' @export
CurrentYear <- function(x) .IsHist(x, "CurrentYear")

#' @rdname OM-accessors
#' @export
`CurrentYear<-` <- function(x, value) .AssignSlot(x, value, "CurrentYear")

#' @rdname OM-accessors
#' @export
Seasons <- function(x) .IsHist(x, "Seasons")

#' @rdname OM-accessors
#' @export
`Seasons<-` <- function(x, value) .AssignSlot(x, value, "Seasons")

#' @rdname OM-accessors
#' @export
RefSeason <- function(x) .IsHist(x, "RefSeason")

#' @rdname OM-accessors
#' @export
`RefSeason<-` <- function(x, value) .AssignSlot(x, value, "RefSeason")

#' @rdname OM-accessors
#' @export
RefEffortYears <- function(x) .IsHist(x, "RefEffortYears")

#' @rdname OM-accessors
#' @export
`RefEffortYears<-` <- function(x, value) .AssignSlot(x, value, "RefEffortYears")


#' @rdname OM-accessors
#' @export
DataLag <- function(x) .IsHist(x, "DataLag")

#' @rdname OM-accessors
#' @export
`DataLag<-` <- function(x, value) .AssignSlot(x, value, "DataLag")

#' @rdname OM-accessors
#' @export
CatchFrac <- function(x) .IsHist(x, "CatchFrac")

#' @rdname OM-accessors
#' @export
`CatchFrac<-` <- function(x, value) .AssignSlot(x, value, "CatchFrac")

#' @rdname OM-accessors
#' @export
Allocation <- function(x) .IsHist(x, "Allocation")

#' @rdname OM-accessors
#' @export
`Allocation<-` <- function(x, value) .AssignSlot(x, value, "Allocation")

#' @rdname OM-accessors
#' @export
EFactor <- function(x) .IsHist(x, "EFactor")

#' @rdname OM-accessors
#' @export
`EFactor<-` <- function(x, value) .AssignSlot(x, value, "EFactor")


#' @rdname OM-accessors
#' @export
Complexes <- function(x) .IsHist(x, "Complexes")

#' @rdname OM-accessors
#' @export
`Complexes<-` <- function(x, value) .AssignSlot(x, value, "Complexes")

#' @rdname OM-accessors
#' @export
Herm <- function(x) .IsHist(x, "Herm")

#' @rdname OM-accessors
#' @export
`Herm<-` <- function(x, value) .AssignSlot(x, value, "Herm")

#' @rdname OM-accessors
#' @export
SharePar <- function(x) .IsHist(x, "SharePar")

#' @rdname OM-accessors
#' @export
`SharePar<-` <- function(x, value) .AssignSlot(x, value, "SharePar")

#' @rdname OM-accessors
#' @export
Relations <- function(x) .IsHist(x, "Relations")

#' @rdname OM-accessors
#' @export
`Relations<-` <- function(x, value) .AssignSlot(x, value, "Relations")


#' @rdname OM-accessors
#' @export
Interval <- function(x) .IsHist(x, "Interval")

#' @rdname OM-accessors
#' @export
`Interval<-` <- function(x, value) .AssignSlot(x, value, "Interval")

#' @rdname OM-accessors
#' @export
MPStartYear <- function(x) .IsHist(x, "MPStartYear")

#' @rdname OM-accessors
#' @export
`MPStartYear<-` <- function(x, value) .AssignSlot(x, value, "MPStartYear")

#' @rdname OM-accessors
#' @export
InterimAdvice <- function(x) .IsHist(x, "InterimAdvice")

#' @rdname OM-accessors
#' @export
`InterimAdvice<-` <- function(x, value) .AssignSlot(x, value, "InterimAdvice")

#' @rdname OM-accessors
#' @export
nReps <- function(x) .IsHist(x, "nReps")

#' @rdname OM-accessors
#' @export
`nReps<-` <- function(x, value) .AssignSlot(x, value, "nReps")

#' @rdname OM-accessors
#' @export
pStar <- function(x) .IsHist(x, "pStar")

#' @rdname OM-accessors
#' @export
`pStar<-` <- function(x, value) .AssignSlot(x, value, "pStar")

#' @rdname OM-accessors
#' @export
maxF <- function(x) .IsHist(x, "maxF")

#' @rdname OM-accessors
#' @export
`maxF<-` <- function(x, value) .AssignSlot(x, value, "maxF")

#' @rdname OM-accessors
#' @export
Seed <- function(x) .IsHist(x, "Seed")

#' @rdname OM-accessors
#' @export
`Seed<-` <- function(x, value) .AssignSlot(x, value, "Seed")

#' @rdname OM-accessors
#' @export
Control <- function(x) .IsHist(x, "Control")

#' @rdname OM-accessors
#' @export
`Control<-` <- function(x, value) .AssignSlot(x, value, "Control")


.IsHist <- function(x, slot_name) {
  if (inherits(x, "hist") || inherits(x, "mse"))
    x <- x@OM
  .AccessSlot(x, slot_name)
}

.ToNamedList <- function(x, cls) {
  if (inherits(x, cls))
    return(setNames(list(x), x@Name))
  if (is.null(names(x)) || any(names(x) == ""))
    names(x) <- purrr::map_chr(x, ~ .x@Name)
  x
}

.ToNestedList <- function(x, cls, stock_nms, fleet_nms = NULL) {
  if (is.null(x)) return(NULL)

  is_flat <- inherits(x, cls) ||
    (is.list(x) && all(purrr::map_lgl(x, ~ inherits(.x, cls))))

  if (is_flat) {
    if (inherits(x, cls)) x <- list(x)

    return(purrr::map(setNames(nm = stock_nms), function(stk) {
      flt_nms <- fleet_nms[[stk]]
      if (!is.null(flt_nms) && length(x) == 1) {
        setNames(rep(x, length(flt_nms)), flt_nms)
      } else {
        setNames(x, purrr::map_chr(x, ~ .x@Name))
      }
    }))
  }

  if (is.null(names(x))) names(x) <- stock_nms[seq_along(x)]

  purrr::imap(x, function(inner, stk) {
    if (inherits(inner, cls))
      inner <- setNames(list(inner), inner@Name)
    if (is.null(names(inner)) || any(names(inner) == ""))
      names(inner) <- (fleet_nms[[stk]] %||% purrr::map_chr(inner, ~ .x@Name))[seq_along(inner)]
    inner
  })
}
