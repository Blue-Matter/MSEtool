
#' `OM` Object
#'
#' The `om` class defines an operating model. See [OM()] for more details.
#'
#' @slot Name Name of the operating model. Character string.
#' @slot Agency Optional. Name of the agency responsible for management.
#' Character string. Supports Markdown.
#' @slot Author Optional. Name(s) of author(s) of the operating model.
#' Character vector.
#' @slot Email Optional. Email address(es) corresponding to `Author`.
#' Character vector. Supports Markdown.
#' @slot Region Optional. Name of the geographic region of the fishery.
#' Character string.
#' @slot Latitude Optional. Latitude (decimal degrees) representing the
#' center of `Region`. Numeric scalar.
#' @slot Longitude Optional. Longitude (decimal degrees) representing the
#' center of `Region`. Numeric scalar.
#' @slot Sponsor Optional. Organization sponsoring development of the
#' operating model. Character string. Supports Markdown.
#'
#' @slot nSim Number of stochastic simulations. Positive integer.
#' @slot nYear Number of historical years. Numeric scalar.
#' @slot pYear Number of projection years. Numeric scalar.
#' @slot CurrentYear Final historical year of the operating model.
#' Integer scalar.
#' @slot Seasons Number of seasons per year. Numeric scalar.
#' @slot Years Vector of model years (including seasonal resolution if
#' applicable). Numeric.
#'
#' @slot Stock A [Stock()] object or list of [Stock()] objects.
#' @slot Fleet A hierarchical list of [Fleet()] objects by stock and fleet. 
#' Each stock must have the same number of fleets.
#' 
#' @slot Obs A hierarchical list of [Obs()] objects by stock/complex and fleet.
#' @slot Imp A hierarchical list of [Imp()] objects by stock/complex and fleet.
#'
#' @slot Data A [Data()] object or list of [Data()] objects associated with
#' the operating model.
#' @slot DataLag Integer specifying the number of time steps that data are
#' lagged relative to management implementation.
#'
#' @slot CatchFrac Optional list controlling catch fraction allocation.
#' @slot Allocation Optional list controlling fleet or stock allocation.
#' @slot EFactor Optional list of effort or exploitation modifiers.
#'
#' @slot Complexes Optional list defining stock complexes for data aggregation
#' and management.
#' @slot Herm Optional list defining hermaphroditism or movement between stocks.
#' @slot SharePar Logical indicating whether key parameters are shared among
#' stocks.
#' @slot Relations Optional list defining biological or ecological relationships
#' among stocks.
#'
#' @slot Interval Management update interval. Numeric scalar or named numeric
#' vector.
#' @slot nReps Number of stochastic replicates for management advice.
#' @slot pStar Percentile applied to stochastic management advice.
#' @slot maxF Maximum allowable fishing mortality. Applies to nominal fishing mortality
#' for fish that interact with the fishing gear, although actual effective F might be 
#' lower if some fish are discarded and survive. 
#' @slot Seed Optional random number generator seed.
#'
#' @slot Control Named list of operating model control settings.
#' @slot Misc List for miscellaneous objects or developer-use components.
#' @slot Log List used internally to store diagnostic or runtime information.
#' @slot Source Optional character string referencing data sources or
#' documentation. Supports Markdown.
#'
#' ## Accessing and Assigning Slots
#'
#' Slots in [om-class] objects can be accessed or assigned using
#' functions matching the slot names (e.g., `Agency(om)` or
#' `Agency(om) <- "DFO"`). 
#' 
#' @seealso [OM()], [Stock()], [Fleet()], [Obs()], [Imp()]
#' 
#' @include class-stock.R
#' @include class-fleet.R
#' @include class-data.R
#' @include class-obs.R
#' @include class-imp.R
#' @include zz_Class_definitions.R
#' @name om-class
NULL 

setClassUnion(name="StockList", members=c("stock", 'Stock',  "list", 'NULL'))
setClassUnion(name="StockFleetList", members=c("fleet", 'Fleet', "list", 'NULL'))
setClassUnion(name="FleetList", members=c("fleet", 'Fleet', "list", 'NULL'))
setClassUnion(name="DataList", members=c("data", "list", 'NULL'))

setClassUnion(name="ObsList", members=c('Obs', "obs", "list", 'NULL'))
setClassUnion(name="ImpList", members=c('Imp', "imp", "list", 'NULL'))


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
