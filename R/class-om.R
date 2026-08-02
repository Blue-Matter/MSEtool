#' The `om` S4 Class
#'
#' The [om-class] defines a complete operating model (OM) specification for
#' use in Management Strategy Evaluation (MSE).
#'
#' @slot Name Character. Name of the operating model. See [OM()].
#' @slot Agency Character. Name of the agency responsible for management.
#'   Supports Markdown. See [OM()].
#' @slot Author Character vector. Name(s) of the author(s). See [OM()].
#' @slot Email Character vector. Email address(es) corresponding to `Author`.
#'   Supports Markdown. See [OM()].
#' @slot Region Character. Name of the geographic region of the fishery.
#'   See [OM()].
#' @slot Latitude Numeric. Latitude (decimal degrees) of the centre of
#'   `Region`. See [OM()].
#' @slot Longitude Numeric. Longitude (decimal degrees) of the centre of
#'   `Region`. See [OM()].
#' @slot Sponsor Character. Organisation sponsoring development of the
#'   operating model. Supports Markdown. See [OM()].
#'
#' @slot nSim Positive integer. Number of stochastic simulations. See [OM()].
#' @slot nYear Numeric. Number of historical years. See [OM()].
#' @slot pYear Numeric. Number of projection years. See [OM()].
#' @slot CurrentYear Integer. Final historical calendar year. See [OM()].
#' @slot Seasons Integer. Number of seasons per year. See [OM()].
#' @slot Years Numeric vector. Model time steps (including seasonal resolution
#'   if applicable). Derived from `nYear`, `pYear`, `CurrentYear`, and
#'   `Seasons` via [CalcYears()]. Read-only; see [Years()].
#' @slot RefSeason Integer vector or `NULL`. Only used for seasonal models 
#'   (`Seasons > 1`). Season index/indices (`1..Seasons`) used as the
#'   reference snapshot(s) for reporting equilibrium reference points. 
#'   `NULL` (default)  auto-detects, independently per simulation, which 
#'   season(s) have nonzero spawning contribution and averages the 
#'   cross-sectional snapshot across them when more than one is detected. See [OM()].
#' @slot RefEffortYears Numeric vector or `NULL`. Only used for seasonal models
#'  (`Seasons > 1`). One or more historical calendar years whose relative
#'   seasonal effort/catchability pattern is used to fix the seasonal shape
#'   of fishing mortality during per-recruit and MSY reference point
#'   optimization, decoupled from the year used for biological parameters.
#'   `NULL` (default) reuses the same year as the biological parameters. 
#'   When more than one year is given, the
#'   per-season effort is averaged across those years before use. See [OM()].
#'
#' @slot Stock A [stock-class] object or named list of [stock-class] objects.
#'   See [Stock()].
#' @slot Fleet A hierarchical named list of [fleet-class] objects indexed by
#'   stock then fleet. Each stock must have the same number of fleets.
#'   See [Fleet()].
#' @slot Obs A hierarchical named list of [obs-class] objects indexed by stock
#'   and fleet. See [Obs()].
#' @slot Imp A hierarchical named list of [imp-class] objects indexed by stock
#'   and fleet. See [Imp()].
#'
#' @slot Data A [data-class] object or list of [data-class] objects associated
#'   with the operating model. See [Data()].
#' @slot DataLag Integer. Number of time steps that data are lagged relative
#'   to management implementation. See [OM()].
#'
#' @slot CatchFrac List. Named list of length 0 or `nStock(OM)`. Each element
#'   is an `nSim` by `nFleet` matrix (or a single row recycled across sims)
#'   giving the fraction of catch taken by each fleet, with rows summing to
#'   1. Only used when there is more than one fleet and a historical
#'   `Depletion@Final` target is set for at least one stock, in which case
#'   it is the target fleet split that catchability is calibrated to
#'   reproduce. If left unspecified for a stock, it is derived from relative
#'   Effort times Catchability in the final historical year. See [OM()].
#' @slot Allocation List. Named list of length 0 or the number of stock
#'   complexes. Each element is an `nSim` by `nFleet` matrix, with rows
#'   summing to 1, controlling how the TAC is split among fleets during
#'   projection. If unspecified it falls back to `CatchFrac`, and then to
#'   the mean of removals over the last five historical years. See [OM()].
#' @slot EFactor List. Effort or exploitation modifiers applied during
#'   projection. See [OM()].
#'
#' @slot Complexes List. Defines stock complexes for data aggregation and
#'   management. See [OM()].
#' @slot Herm List. Defines hermaphroditism or movement between stocks.
#'   See [OM()].
#' @slot SharePar Logical. Whether key parameters are shared among stocks.
#'   See [OM()].
#' @slot Relations List. Biological or ecological relationships among stocks.
#'   See [OM()].
#'
#' @slot StockTargeting A [stocktargeting-class] object. Defines fleet-level
#'   stock targeting weights and deviations. Initialised automatically by
#'   [OM()] via [StockTargeting()].
#'
#' @slot Interval Numeric scalar or named numeric vector. Management update
#'   interval in years. See [OM()].
#' @slot MPStartYear Numeric or `NULL`. First calendar year in which MPs are
#'   applied. Projection years before `MPStartYear` are "interim" years -
#'   the MP is not called, and advice is instead built from `InterimAdvice`
#'   (falling back to freezing effort at the last historical level where no
#'   matching entry exists). `NULL` (default) means MPs start in the first
#'   projection year. See [OM()].
#' @slot InterimAdvice A `data.frame` or `NULL`. Analyst-supplied fixed or
#'   stochastic TAC/Effort values for interim years (before `MPStartYear`).
#'   See [OM()] for the required columns.
#' @slot nReps Positive integer. Number of stochastic replicates for
#'   management advice. See [OM()]. Not currently used.
#' @slot pStar Numeric. Percentile applied to stochastic management advice.
#'   See [OM()]. Not currently used.
#' @slot maxF Numeric. Maximum allowable instantaneous fishing mortality.
#'   See [OM()].
#' @slot Seed Integer. Random number generator seed. See [OM()].
#'
#' @slot Control Named list of operating model control settings. See [OM()].
#' @slot Misc List. Miscellaneous objects or developer-use components.
#' @slot Log `list`. Internal named list storing diagnostics, warnings, and
#'   assumptions recorded during processing. See [Log()]. Not intended for
#'   direct user access.
#' @slot Source Character. References to data sources or documentation.
#'   Supports Markdown. See [OM()].
#'
#' @details
#' Direct construction via [methods::new()] is not recommended; use [OM()]
#' instead, which populates defaults, derives `Years`, and initialises
#' `StockTargeting` automatically.
#'
#' All slots can be accessed or replaced using functions matching the slot
#' name. See [OM-accessors] for the full list. Slots belonging to sub-objects
#' (`Stock`, `Fleet`, `Obs`, `Imp`, `Data`) are accessed via their own
#' constructors, which also serve as pass-through accessors when called with
#' an [om-class] argument.
#'
#' @seealso
#' - [OM()] for the constructor and [OM-accessors] for slot accessors.
#' - [Stock()], [Fleet()], [Obs()], [Imp()], [Data()] for sub-object
#'   constructors and their pass-through accessors.
#' - [Years()], [CalcYears()] for the derived time-step vector.
#' - [StockTargeting()] for the stock targeting sub-object.
#' - [Log()] for accessing runtime diagnostics.
#' - [runMSE()], [PopulateOM()] for downstream use of the operating model.
#'
#' @family om
#'
#' @include class-stock.R
#' @include class-fleet.R
#' @include class-data.R
#' @include class-obs.R
#' @include class-imp.R
#' @include class-stock-targeting.R
#' @include zz_Class_definitions.R
#' @name om-class
NULL

methods::setClassUnion(name = "StockList",      members = c("stock", "Stock", "list", "NULL"))
methods::setClassUnion(name = "StockFleetList",  members = c("fleet", "Fleet", "list", "NULL"))
methods::setClassUnion(name = "FleetList",       members = c("fleet", "Fleet", "list", "NULL"))
methods::setClassUnion(name = "DataList",        members = c("data",  "list",  "NULL"))
methods::setClassUnion(name = "ObsList",         members = c("Obs",   "obs",   "list", "NULL"))
methods::setClassUnion(name = "ImpList",         members = c("Imp",   "imp",   "list", "NULL"))

setClass(
  "om",
  slots = c(
    Name='char.null',
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
    Seasons='num.null',
    RefSeason='num.null',
    RefEffortYears='num.null',

    Stock='StockList',
    Fleet='StockFleetList',
    Obs='ObsList',
    Imp='ImpList',
    
    Data='DataList',
    DataLag='numeric',
    
    CatchFrac='list.null',
    Allocation='list.null',
    EFactor='list.null',
    
    Complexes='list.null',
    Herm='list.null',
    SharePar='num.log',
    Relations='list.null',
    
    StockTargeting = 'stocktargeting',
    
    Interval='numeric',
    MPStartYear='num.null',
    InterimAdvice='df.null',
    nReps='numeric',
    pStar='numeric',
    maxF='numeric',
    Seed='num.null',
    
    Years='num.null',
    Control='list.null',
    Misc='list',
    Log='list',
    Source='char.list'
  )
)

setValidity("om", function(object) {
  errors <- character()

  if (!is.null(object@MPStartYear)) {
    if (length(object@MPStartYear) != 1)
      errors <- c(errors, "`MPStartYear` must be a single numeric year")
    if (!is.null(object@CurrentYear) && object@MPStartYear <= object@CurrentYear)
      errors <- c(errors, "`MPStartYear` must be after `CurrentYear`")
  }

  if (!is.null(object@InterimAdvice)) {
    required_cols <- c("Year", "Type", "Mean")
    if (length(object@Stock) > 1)
      required_cols <- c(required_cols, "Stock")
    missing_cols  <- setdiff(required_cols, names(object@InterimAdvice))
    if (length(missing_cols))
      errors <- c(errors, paste0(
        "`InterimAdvice` is missing required column(s): ",
        paste(missing_cols, collapse = ", ")
      ))
    if (!length(missing_cols) && !all(object@InterimAdvice$Type %in% c("TAC", "Effort")))
      errors <- c(errors, "`InterimAdvice$Type` must be `\"TAC\"` or `\"Effort\"`")

    if (!length(missing_cols)) {
      mean_vals <- object@InterimAdvice$Mean
      if (any(is.na(mean_vals) | mean_vals < 0))
        errors <- c(errors, "`InterimAdvice$Mean` must be `>= 0` (natural-scale TAC/Effort values); found `NA` or negative value(s)")

      if ("SD" %in% names(object@InterimAdvice)) {
        sd_vals <- object@InterimAdvice$SD
        if (any(!is.na(sd_vals) & sd_vals < 0))
          errors <- c(errors, "`InterimAdvice$SD` must be `NA`, `0`, or positive; found negative value(s)")
        if (any(!is.na(mean_vals) & mean_vals == 0 & !is.na(sd_vals) & sd_vals > 0))
          errors <- c(errors, "`InterimAdvice$Mean` is `0` for some row(s) with `SD > 0`; a lognormal draw cannot be centred at `0` -- use `SD = NA`/`0` for a deterministic closure instead")
      }
    }
  }

  if (!is.null(object@RefSeason) && !is.null(object@Seasons)) {
    if (any(object@RefSeason != round(object@RefSeason)) ||
        any(object@RefSeason < 1) || any(object@RefSeason > object@Seasons))
      errors <- c(errors, "`RefSeason` must contain whole numbers between `1` and `Seasons`")
  }

  if (!is.null(object@RefEffortYears) && !is.null(object@Years))
    if (any(object@RefEffortYears < min(object@Years)) ||
        any(object@RefEffortYears > max(object@Years)))
      errors <- c(errors, "`RefEffortYears` must be within the range of `Years`")

  if (length(errors)) errors else TRUE
})
