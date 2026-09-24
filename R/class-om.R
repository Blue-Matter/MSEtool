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
#'   cross-sectional snapshot across them when more than one is detected. See
#'   [RefSeason()].
#' @slot RefEffortYears Numeric vector or `NULL`. Only used for seasonal models
#'  (`Seasons > 1`). One or more historical calendar years whose relative
#'   seasonal effort/catchability pattern is used to fix the seasonal shape
#'   of fishing mortality during per-recruit and MSY reference point
#'   optimization, decoupled from the year used for biological parameters.
#'   `NULL` (default) reuses the same year as the biological parameters.
#'   When more than one year is given, the
#'   per-season effort is averaged across those years before use. See
#'   [RefSeason()].
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
#' @slot DataLag Integer. Number of years that data are lagged relative
#'   to management implementation. See [DataLag()].
#'
#' @slot CatchFrac List. Named list of length 0 or `nStock(OM)`. Each element
#'   is an `nSim` by `nFleet` matrix (or a single row recycled across sims)
#'   giving the fraction of catch taken by each fleet, with rows summing to
#'   1. Only used when there is more than one fleet and a historical
#'   `Depletion@Final` target is set for at least one stock, in which case
#'   it is the target fleet split that catchability is calibrated to
#'   reproduce. If left unspecified for a stock, it is derived from relative
#'   Effort times Catchability in the final historical year. See
#'   [FleetAllocation()].
#' @slot FleetAllocation List. Named list of length 0 or the number of stock
#'   complexes. Each element is an `nSim` by `nFleet` matrix, with rows
#'   summing to 1, controlling how the TAC is split among fleets during
#'   projection. If unspecified it falls back to `Allocation` (retained for
#'   backwards compatibility - see below), then `CatchFrac`, then the mean of
#'   removals over the last five historical years. See [FleetAllocation()].
#' @slot Allocation List. Deprecated alias for `FleetAllocation`, retained
#'   only so that objects saved before the rename still carry their data. If
#'   `FleetAllocation` is unset and `Allocation` is, its value is copied over
#'   during `Simulate()`/`Project()`. Not user-facing, set `FleetAllocation`
#'   directly instead.
#' @slot HistoricalWeight List. Named list of length 0 or the number of
#'   stock complexes. Each element is a named numeric vector over fleet
#'   names, values in `[0,1]`, giving the weight placed on the historical
#'   seasonal pattern (vs. the population abundance pattern) when
#'   `SeasonalAllocation` is derived. `1` (default) uses the historical
#'   pattern only. See [SeasonalAllocation()].
#' @slot SeasonalAllocation List. Named list of length 0 or the number of
#'   stock complexes. Each element is an `nSim` (or 1, recycled) by
#'   `Seasons` by `nFleet` array, each `[sim, , fleet]` column summing to 1,
#'   controlling how a periodically-set TAC/Effort is split across the
#'   seasons of the interval it covers. If unspecified it is derived from
#'   `HistoricalWeight`. See [SeasonalAllocation()].
#' @slot EffortAllocation List. Named list of length 0 or the number of
#'   stock complexes. Each element is an `nSim` (or 1, recycled) by `nFleet`
#'   matrix, with rows summing to 1, splitting a scalar absolute Effort
#'   recommendation among fleets. If unspecified it falls back to to mean
#'   relative effort over the last five historical years. See
#'   [EffortAllocation()].
#' @slot EFactor List. Deprecated alias for `EffortAllocation`, retained
#'   only so that objects saved before the rename still carry their data. If
#'   `EffortAllocation` is unset and `EFactor` is, its value is copied over
#'   during `Simulate()`/`Project()`. Set `EffortAllocation` directly
#'   instead.
#'
#' @slot Complexes List. Defines stock complexes for data aggregation and
#'   management. See [OM()].
#' @slot Herm A list of [stocktransition-class] objects (built via [Herm()]),
#'   one per `From`/`To` stock pair, defining age-dependent reclassification
#'   of individuals between stocks (e.g. sequential hermaphroditism). See
#'   [OM()].
#' @slot Relations List. Biological or ecological relationships among stocks.
#'   See [OM()].
#'
#' @slot StockTargeting A [stocktargeting-class] object. Defines fleet-level
#'   stock targeting weights and deviations. Initialised automatically by
#'   [OM()] via [StockTargeting()].
#'
#' @slot Interval Numeric scalar or named numeric vector. Management update
#'   interval in years (regardless of `Seasons` - e.g. `Interval = 1` always
#'   means "once a year"), optionally per-MP via
#'   names. MPs that need to be called at every timestep of a
#'   seasonal OM (e.g. because they return a season-specific value derived
#'   directly from history) should declare `attr(mp, 'EverySeason') <- TRUE`
#'   instead of relying on a small `Interval` value. 
#' @slot MPStartYear Numeric or `NULL`. First calendar year in which MPs are
#'   applied. Projection years before `MPStartYear` are "interim" years -
#'   the MP is not called, and advice is instead built from `InterimAdvice`
#'   (falling back to freezing effort at the last historical level where no
#'   matching entry exists). `NULL` (default) means MPs start in the first
#'   projection year. See [InterimAdvice()].
#' @slot InterimAdvice A `data.frame` or `NULL`. Analyst-supplied fixed or
#'   stochastic TAC/Effort values for interim years (before `MPStartYear`).
#'   See [InterimAdvice()] for the required columns.
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
#' @include class-stocktransition.R
#' @include zz_Class_definitions.R
#' @name om-class
NULL

methods::setClassUnion(name = "StockList",      members = c("stock", "Stock", "list", "NULL"))
methods::setClassUnion(name = "StockFleetList",  members = c("fleet", "Fleet", "list", "NULL"))
methods::setClassUnion(name = "FleetList",       members = c("fleet", "Fleet", "list", "NULL"))
methods::setClassUnion(name = "DataList",        members = c("data",  "list",  "NULL"))
methods::setClassUnion(name = "ObsList",         members = c("Obs",   "obs",   "list", "NULL"))
methods::setClassUnion(name = "ImpList",         members = c("Imp",   "imp",   "list", "NULL"))
methods::setClassUnion(name = "StockTransitionList", members = c("stocktransition", "list", "NULL"))

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
    FleetAllocation='list.null',
    Allocation='list.null',
    HistoricalWeight='list.null',
    SeasonalAllocation='list.null',
    EffortAllocation='list.null',
    EFactor='list.null',
    
    Complexes='list.null',
    Herm='StockTransitionList',
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

      if ("SD" %in% names(object@InterimAdvice))
        errors <- c(errors, "`InterimAdvice$SD` is not supported; use `CV` (coefficient of variation) instead")

      if ("CV" %in% names(object@InterimAdvice)) {
        cv_vals <- object@InterimAdvice$CV
        if (any(!is.na(cv_vals) & cv_vals < 0))
          errors <- c(errors, "`InterimAdvice$CV` must be `NA`, `0`, or positive; found negative value(s)")
        pos <- !is.na(mean_vals) & mean_vals > 0
        if (any(pos)) {
          stk <- if ("Stock" %in% names(object@InterimAdvice)) object@InterimAdvice$Stock else ""
          grp <- paste(object@InterimAdvice$Year, stk, object@InterimAdvice$Type, sep = "\r")
          cv0 <- ifelse(is.na(cv_vals), 0, cv_vals)
          n_cv <- tapply(cv0[pos], grp[pos], function(x) length(unique(x)))
          if (any(n_cv > 1))
            errors <- c(errors, "`InterimAdvice$CV` must be identical for all rows with `Mean > 0` within a Year x Stock x Type")
        }
      }

      if ("Max" %in% names(object@InterimAdvice)) {
        max_vals <- object@InterimAdvice$Max
        if (any(!is.na(max_vals) & !is.na(mean_vals) & max_vals < mean_vals))
          errors <- c(errors, "`InterimAdvice$Max` must be `NA` or `>= Mean`")
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
