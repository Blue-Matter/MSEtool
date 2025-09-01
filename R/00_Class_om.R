

setClassUnion(name="StockList", members=c("stock", 'Stock',  "list", 'NULL'))
setClassUnion(name="StockFleetList", members=c("fleet", 'Fleet', "list", 'NULL'))
setClassUnion(name="FleetList", members=c("fleet", 'Fleet', "list", 'NULL'))
setClassUnion(name="DataList", members=c("data", "list", 'NULL'))

setClassUnion(name="obs.list", members=c('Obs', "obs", "list", 'NULL'))
setClassUnion(name="imp.list", members=c('Imp', "imp", "list", 'NULL'))

#' Class `om`: Operating Model Object 
#'
#' @include Class_definitions.R
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#' @include 00_Class_stock.R
#' @include 00_Class_fleet.R
#' @include 00_Class_sexpars.R
#' @include 00_Class_obs.R
#' @include 00_Class_imp.R
#' 
#' @slot Name Name of the Operating Model. Character string.
#' 
#' @slot Agency Optional. Name of the agency responsible for the management of the fishery.
#' Character string. Supports Markdown. 
#' 
#' @slot Author Optional. Name(s) of author(s) of the operating model. Character string
#' with length corresponding to the number of authors.
#' 
#' @slot Email Optional. Email address(es) for the author(s) of the operating model.
#' Character string of with length equal to `length(Author)`. Supports Markdown.
#' 
#' @slot Region Optional. Name of the general geographic region of the fishery. Character string.
#' 
#' @slot Latitude Optional. Latitude (decimal degrees) to indicate the center of `Region`.
#' Negative values represent the South of the Equator. Numeric. Single value.
#' 
#' @slot Longitude Optional. Longitude (decimal degrees) to indicate the center of `Region`.
#' Negative values represent the West of the Prime Meridian. Numeric. Single value.
#' 
#' @slot Sponsor Optional. Name of the organization who sponsored the development of the
#' Operating Model. Character string. Supports Markdown.
#' 
#' @slot nSim The number of simulations. Positive integer length 1. `nSim=1` will
#' produce a deterministic operating model.
#' 
#' @slot nYear The number of historical years. Typically corresponds to the
#' year the fishery was first (assumed to be) exploited. For multi-stock models,
#' `nYear` should be the earliest exploitation year of all stocks. Numeric.
#' Single value. Historical years are calculated as `rev(seq(CurrentYear, by=-1, length.out=nYear))`
#' 
#' @slot pYear The number of projection years. Numeric. Single value.
#' Projection years are calculated as `seq(CurrentYear+1, length.out=pYear)`.
#' 
#' @slot CurrentYear The last historical year of the operating model.
#' Defaults to the year the Operating Model object is built. Integer. 
#' 
#' @slot Stock A list of [Stock()] object(s). For single-stock OMs, a 
#' [Stock()] object can be used for this slot (i.e., not a list), which will then
#' be converted internally to a named list length 1. 
#' 
#' @slot Fleet A hierarchical list of [Fleet()] object(s) for each [Stock()] and each [Fleet()].
#' Level 1 is `Stock` and Level 2 `Fleet`. For single-stock/fleet models, this can be a single
#' [Fleet()] object (i.e., not a list), which will then be converted internally to a named list.
#' 
#' @slot Obs A hierarchical list of [Obs()] object(s) for each [Stock()] and each [Fleet()].
#' Level 1 is `Stock` and Level 2 `Fleet`. For single-stock/fleet models, this can be a single
#' [Obs()] object (i.e., not a list), which will then be converted internally to a named list.
#' 
#' @slot Imp A hierarchical list of [Imp()] object(s) for each [Stock()] and each [Fleet()].
#' Level 1 is `Stock` and Level 2 `Fleet`. For single-stock/fleet models, this can be a single
#' [Imp()] object (i.e., not a list), which will then be converted internally to a named list.
#' 
#' @slot Data A [Data()] object or a list of [Data()] objects (up to
#' `length(Stock)`) containing the real fishery data. If `Complexes` is specified,
#'  `Data` follows the same structure, i.e., elements
#' in `Data` list will be assumed to be aggregated according to `Complexes`.
#' 
#' @slot DataLag 
#' 
#' 
#' 
#' 
#' @slot Complexes For multi-stock models only. A list of stock complexes for
#'  which data and management recommendation should be aggregated. 
#' Each position is a vector of stock numbers (as they appear in `Stock` list)
#' for which data and management recommendation should be aggregated; e.g.,
#' TAC will be split among stocks according to vulnerable biomass.
#' 
#' @slot Relations For multi-stock models only. A list of biological and/or
#' ecological relationships among stocks in `Stock`. For MICE models. Needs
#' more documentation so bug us if you get stuck.
#' 
#' @slot SexPars For multi-stock models only. A named list that controls
#' sex-specific dynamics, i.e., sex-specific spawning and hermaphroditism.
#' More generally, controls spawning and moving abundance between stocks. See `Details`.

#' 
#' @slot Interval The management update interval. Management will be implemented
#' in the first projection time step, and then every `Interval` time step, with
#' management remaining unchanged in the interim. A single numeric value for the
#' same interval for all managemement procedures. For MP-specific management
#' intervals , `Interval` can be a named numeric vector with length corresponding
#' to the number of management procedures used in [runMSE()] or [Project()].
#' @slot nReps Number of samples of the management recommendation for each method.
#' Only for management procedures that generate stochastic management advice (i.e.,
#' account for uncertainty in the data). Defaults to 1, which produces deterministic
#' management advice.
#' 
#' @slot pStar The percentile of the sample of the management recommendation for each method.
#' Defaults to 0.5 (median). Only for management procedures that generate
#' stochastic management advice (i.e., account for uncertainty in the data).
#' To ensure the management advice matches the correct percentile, `reps` should
#' be a largish value (i.e., >>1, but exact value depends on the degree of uncertainty in the data)
#' 
#' @slot Seed Optional numeric value for the seed for the random number generator.
#' Only required to over-ride the default seed (calculated internally based on
#' the contents of the `OM` object)
#' 
#' @slot Control A named list of settings. See `Details`
#' 
#' @slot Misc A list for storing additional things that don't have anywhere else to go.
#' Mainly for development purposes.
#' 
#' @slot Source Character string. Can be used to reference websites, articles, etc
#' with relevant information. Supports Markdown.
#'
#' @details
#'
#' ## About the `om` Class
#' `om` is a new S4 class that is designed to supersede [OM-class()] and [MOM-class()]
#' objects. In time, [OM-class()] and [MOM-class()] will be deprecated
#' and eventually removed from the package.
#'
#' ## Creating New Objects
#' `r Creating_New_Objects('om')`
#'
#' ## Accessing and Assigning Slots
#' `r Accessing_Assigning_Slots('om')`
#'
#' ## SexPars
#' The following are valid names for `SexPars`:
#'
#' - `SPfrom`: A `nstock` x `nstock` matrix, where `nstock` is `length(Stock`)
#' that specifies the proportion of the spawning output of the row `p` stock for
#' the column `p'` stock. A diagonal matrix means each stock is responsible for
#' its own recruitment.
#' - `Herm`: A list with each entry containing a matrix with
#' dimensions `c(nSim, MaxAge + 1)` that specifies the proportion at age that move from
#' stock `p` to `p'` (sequential hermaphroditism). The names of the list should be
#' of the form "H_p'_p" where `p` and `p'` are integers that identify the stocks
#' in the `Stock` list. For time-varying values, arrays with dimensions
#'  `c(nSim, (MaxAge + 1),nHistTS + nProjTS)` can be used. `MaxAge` is the maximum age
#'  for the both stock `p` and stock `p'`. `nHistTS` and `nProjTS` are equal to
#'  `nYear` and `pYear` respectively, unless `Units` in the stocks' [Ages()]
#'  object are not `year` (i.e, a higher resolution time step).
#' - `SharePar`: Optional. Logical to indicate whether stock-recruit, depletion,
#' and observation/implementation parameters are mirrored between stocks. By default, `TRUE`.
#'
#' ## Control
#' The following are valid names for `Control`:
#'
#' TODO
#'
#' @seealso `r See_Also('om')`
#'
#' @rdname class-om
#'
#' @example man-examples/om-class.R
#' @export
setClass("om",
         slots=c(Name='char.null',
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
                 
                 Stock='StockList',
                 Fleet='StockFleetList',
                 Obs='obs.list',
                 Imp='imp.list',
                 
                 Data='DataList',
                 DataLag='numeric',
                 
                 CatchFrac='list',
                 Allocation='list',
                 Efactor='list',
                 
                 Complexes='list',
                 SexPars='sexpars',
                 Relations='list',
                 
                 Interval='numeric',
                 
                 nReps='numeric',
                 pStar='numeric',
                 maxF='numeric',
                 Seed='num.null',
                 
                 TimeUnits='char.null',
                 TimeStepsPerYear='num.null',
                 TimeSteps='num.null',
                 
                 Control='list.null',
                 Misc='list',
                 Log='list',
                 Source='char.list')
)


#' @rdname class-om
#' 
#' @param Name Name of the Operating Model. Character string.
#' @param Agency Name of the agency responsible for the management of the fishery.
#' Character string. Supports Markdown.
#' @param Author Name(s) of author(s) of the operating model. Character string
#' with length corresponding to the number of authors.
#' @param Email Email address(es) for the author(s) of the operating model.
#' Character string of with length equal to `length(Author)`. Supports Markdown.
#' @param Region Name of the general geographic region of the fishery. Character string.
#' @param Latitude Latitude (decimal degrees) to indicate the center of `Region`.
#' Negative values represent the South of the Equator. Numeric. Single value.
#' @param Longitude Longitude (decimal degrees) to indicate the center of `Region`.
#' Negative values represent the West of the Prime Meridian. Numeric. Single value.
#' @param Sponsor Name of the organization who sponsored the development of the
#' Operating Model. Character string. Supports Markdown.
#' @param CurrentYear The last historical year of the operating model. Defaults
#' to the year the Operating Model object is built.
#' @param nSim The number of simulations. Numeric. Positive integer `nSim=1` will
#' produce a deterministic operating model.
#' @param nYear The number of historical years. Typically corresponds to the
#' year the fishery was first (assumed to be) exploited. For multi-stock models,
#' `nYear` should be the earliest exploitation year of all stocks. Numeric.
#' Single value. Historical years are calculated as `rev(seq(CurrentYear, by=-1, length.out=nYear))`
#' @param pYear The number of projection years. Numeric. Single value.
#' Projection years are calculated as `seq(CurrentYear+1, length.out=pYear)`.
#' @param Stock A [Stock] object or a list of [Stock()] objects
#' for multi-stock models.
#' @param Complexes For multi-stock models only. A list of stock complexes.
#' Each position is a vector of stock numbers (as they appear in `Stock` list)
#' for which data and management recommendation should be aggregated; e.g.,
#' TAC will be split among stocks according to vulnerable biomass.
#' @param Relations For multi-stock models only. A list of biological and/or
#' ecological relationships among stocks in `Stock`. For MICE models. Needs
#' more documentation so bug us if you get stuck.
#' @param SexPars For multi-stock models only. A named list that controls
#' sex-specific dynamics, i.e., sex-specific spawning and hermaphroditism.
#' More generally, controls spawning and moving abundance between stocks. See `Details`.
#' @param Data A [data-class()] object or a list of [data-class()] objects (up to
#' `length(Stock)`) containing the real fishery data.
#' if `Complexes` is specified, `Data` follows the same structure, i.e., elements
#' in `Data` list will be assumed to be aggregated according to `Complexes`.
#' @param Interval Integer length 1. The management update interval. Management will be implemented
#' in the first projection time step, and then every `Interval` time step, with
#' management remaining unchanged in the interim. A single numeric value for the
#' same interval for all managemement procedures. For MP-specific management
#' intervals , `Interval` can be a named numeric vector with length corresponding
#' to the number of management procedures used in [runMSE()] or [Project()].
#' @param DataLag Integer length 1. The number of time steps to lag the data. 
#' Default `DataLage=0` means data is generated up to the time step before an MP is implemented.
#' E.g., if management advice is being produced for `TimeStep=2025`, the data provided to the MP
#' will be up to and including `2024`. 
#' @param nReps Number of samples of the management recommendation for each method.
#' Only for management procedures that generate stochastic management advice (i.e.,
#' account for uncertainty in the data). Defaults to 1, which produces deterministic
#' management advice.
#' @param pStar The percentile of the sample of the management recommendation for each method.
#' Defaults to 0.5 (median). Only for management procedures that generate
#' stochastic management advice (i.e., account for uncertainty in the data).
#' To ensure the management advice matches the correct percentile, `reps` should
#' be a largish value (i.e., >>1, but exact value depends on the degree of uncertainty in the data)
#' @param Seed Optional numeric value for the seed for the random number generator.
#' Only required to over-ride the default seed (calculated internally based on
#' the contents of the `OM` object)
#' @param Control A named list of settings. See `Details`
#' @param Misc A list for storing additional things that don't have anywhere else to go.
#' Mainly for development purposes.
#' @param Source Character string. Can be used to reference websites, articles, etc
#' with relevant information. Supports Markdown.
#' 
#' 
#' @export
OM <- function(Name='A new `OM` object',
               Agency='',
               Author='',
               Email='',
               Region='',
               Latitude=NULL,
               Longitude=NULL,
               Sponsor='',
               nSim=48,
               nYear=20,
               pYear=30,
               CurrentYear=as.numeric(format(Sys.Date(), '%Y')),
               TimeUnits='year',
               Stock=NULL,
               Fleet=NULL,
               Obs=list(),
               Imp=list(),
               Complexes=list(),
               Relations=list(),
               SexPars=new('sexpars'),
               Data=NULL,
               DataLag=0,
               Interval=1,
               nReps=1,
               pStar=0.5,
               maxF=3,
               Seed=NULL,
               Control=NULL,
               Misc=list(),
               Log=list(),
               Source=list()) {
  
  .Object <- new('om')
  .Object@Name <- Name
  .Object@Agency <- Agency
  .Object@Author <- Author
  .Object@Email <- Email
  .Object@Region <- Region
  .Object@Latitude <- Latitude
  .Object@Longitude <- Longitude
  .Object@Sponsor <- Sponsor
  
  .Object@nSim <- nSim
  .Object@nYear <- nYear
  .Object@pYear <- pYear
  
  .Object@CurrentYear <- CurrentYear
  .Object@TimeUnits <- TimeUnits
  
  .Object@TimeStepsPerYear <- TSperYear(TimeUnits)
  .Object@TimeSteps <- CalcTimeSteps(nYear, pYear, CurrentYear, TimeUnits)
  
  .Object@Stock <- Stock
  .Object@Fleet <- Fleet
  .Object@Obs <- Obs
  .Object@Imp <- Imp
  .Object@Complexes <- Complexes
  .Object@Relations <- Relations
  .Object@SexPars <- SexPars
  .Object@Data <- Data
  .Object@DataLag <- DataLag
  .Object@Interval <- Interval
  .Object@nReps <- nReps
  .Object@pStar <- pStar
  .Object@maxF <- maxF
  .Object@Seed <- Seed 
  if (!is.null(Control)) {
    .Object@Control <- Control
  } else {
    .Object@Control <- ControlDefault
  }
  
  .Object@Misc <- Misc
  .Object@Source <- Source
  #   .Object@Created <- Sys.time()
  
  methods::validObject(.Object)
  .Object
}

validOMobject <- function(object) {
  # chk <- Check(object)
  # if (chk@empty) return(TRUE)
  # if (length(chk@errors)>0) return(chk@errors)
  TRUE
}

setValidity('om', validOMobject)





