

# Fleet Class ----

#' Fleet Object
#'
#' Fleet Object
#'
#' @include 00_Class_effort.R
#' @include 00_Class_discardmortality.R
#' @include 00_Class_selectivity.R
#' @include 00_Class_retention.R
#' 
#'
#' @slot Misc `r Misc_param()`
#'
#' @seealso `r See_Also('fleet')`
#'
#' @name FleetClass
#' @export
setClass('fleet',
         slots=c(Name='char.null',
                 Effort='effort',
                 Catchability='catchability',
                 Selectivity='selectivity',
                 Retention='retention',
                 DiscardMortality='discardmortality',
                
                 Closure='num.array', # historical spatial closures
                 Targeting='num.array', # spatial targeting - not currently used
                 
                 WeightFleet='array.null',
                 BioEconomic='list',
                 
                 nYear='num.null',
                 pYear='num.null',
                 nSim='num.null',
                 CurrentYear='num.null',
                 Years='num.null',
                 Seasons='num.null'
         ),
         contains = c('MiscClass')
)


setMethod("initialize", "fleet", function(.Object,
                                          Name=NULL,
                                          Effort=new('effort'),
                                          Catchability=new('catchability'),
                                          Selectivity=new('selectivity'),
                                          Retention=new('retention'),
                                          DiscardMortality=new('discardmortality'),
                                          Closure=array(),
                                          Targeting=array(),
                                          WeightFleet=array(),
                                          BioEconomic=list(),
                                          Misc=list()) {
  
  .Object@Name <- Name
  .Object@Effort <- Effort
  .Object@Catchability <- Catchability
  
  .Object@Selectivity <- Selectivity
  .Object@Retention <- Retention
  .Object@DiscardMortality <- DiscardMortality
  
  .Object@Closure <- Closure
  .Object@Targeting <- Targeting
  
  .Object@WeightFleet <- WeightFleet
  .Object@BioEconomic <- BioEconomic
  .Object@Misc <- Misc
  
  .Object
})

#' @describeIn FleetClass Create a new `Fleet` object
#' @export
Fleet <- function( Name=NULL,
                   Effort=new('effort'),
                   Catchability=new('catchability'),
                   Selectivity=new('selectivity'),
                   Retention=new('retention'),
                   DiscardMortality=new('discardmortality'),
                   Closure=array(),
                   Targeting=array(),
                   WeightFleet=array(),
                   BioEconomic=list(),
                   Misc=list()) {
  
  if (methods::is(Name, 'om'))
    return(Name@Fleet)
  
  methods::new('fleet',
               Name=Name,
               Effort=Effort,
               Catchability=Catchability,
               Selectivity=Selectivity,
               Retention=Retention,
               DiscardMortality=DiscardMortality,
               Closure=Closure,
               Targeting=Targeting,
               WeightFleet=WeightFleet,
               BioEconomic=BioEconomic,
               Misc=Misc)
}

#' @describeIn FleetClass Assign an `Fleet` object to an [OM()] object
#' @param x An [OM()] class object
#' @param value For single stock OMs, a`Fleet` object or a list of `Fleet` objects to assign to `x`.
#'    For multi-stock OMs, a nested list (length `nStock`) each element containing a `Fleet` object or a list of `Fleet` objects to assign to `x`.
#' @export
`Fleet<-` <- function(x, value) {
  CheckClass(x)
  
  stocknames <- StockNames(x)
  
  if (is.null(stocknames)) {
    cli::cli_abort("Add `Stock` object(s) to `OM` first")
  }
  
  nstocks <- nStock(x)
  
  if (inherits(value, 'fleet')) {
    value <- MakeNamedList(value@Name, value)
  }
  if (!inherits(value, 'list')) {
    cli::cli_abort("`value` must be a list of `Fleet` objects")
  }
  
  class(value) <- 'FleetList'
  
  x@Fleet <- MakeNamedList(stocknames, value)
  class(x@Fleet) <- 'StockFleetList'
  x
}

  
  



