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
#' @slot CatchFrac List. Controls catch fraction allocation among fleets or
#'   stocks. See [OM()].
#' @slot Allocation List. Controls effort or catch allocation among fleets or
#'   stocks. See [OM()].
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
#' @slot nReps Positive integer. Number of stochastic replicates for
#'   management advice. See [OM()].
#' @slot pStar Numeric. Percentile applied to stochastic management advice.
#'   See [OM()].
#' @slot maxF Numeric. Maximum allowable instantaneous fishing mortality.
#'   See [OM()].
#' @slot Seed Integer. Random number generator seed. See [OM()].
#'
#' @slot Control Named list of operating model control settings. See [OM()].
#' @slot Misc List. Miscellaneous objects or developer-use components.
#' @slot Log List. Internal diagnostic and runtime information. See [Log()].
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
  # TODO: structural consistency checks
  TRUE
})
