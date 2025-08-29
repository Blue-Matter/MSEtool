
# Fleet Class ----

#' Fleet Object
#'
#' Fleet Object
#'
#' @include 00_Class_fishingmortality.R
#' @include 00_Class_discardmortality.R
#' @include 00_Class_effort.R
#' @include 00_Class_selectivity.R
#' @include 00_Class_retention.R
#' @include 00_Class_distribution.R
#' 
#'
#' @slot Misc `r Misc_param()`
#'
#' @seealso `r See_Also('fleet', c('Check'))`
#'
#' @name FleetClass
#' @example man-examples/Fleet-class.R
#' @export
setClass('fleet',
         slots=c(Name='char.null',
                 FishingMortality='fishingmortality',
                 DiscardMortality='discardmortality',
                 Effort='effort',
                 Selectivity='selectivity',
                 Retention='retention',
                 Distribution='distribution',
                 WeightFleet='array.null',
                 BioEconomic='list',
                 nYear='num.null',
                 pYear='num.null',
                 nSim='num.null',
                 CurrentYear='num.null',
                 TimeUnits='char.null',
                 TimeSteps='num.null',
                 TimeStepsPerYear='num.null',
                 Misc='list'
         )
)


setMethod("initialize", "fleet", function(.Object,
                                          Name=NULL,
                                          FishingMortality=new('fishingmortality'),
                                          DiscardMortality=new('discardmortality'),
                                          Effort=new('effort'),
                                          Selectivity=new('selectivity'),
                                          Retention=new('retention'),
                                          Distribution=new('distribution'),
                                          WeightFleet=array(),
                                          BioEconomic=list(),
                                          Misc=list()) {
  
  .Object@Name <- Name
  .Object@FishingMortality <- FishingMortality
  .Object@DiscardMortality <- DiscardMortality
  .Object@Effort <- Effort
  .Object@Selectivity <- Selectivity
  .Object@Retention <- Retention
  .Object@Distribution <- Distribution
  .Object@WeightFleet <- WeightFleet
  .Object@BioEconomic <- BioEconomic
  .Object@Misc <- Misc
  
  #   .Object@Created <- Sys.time()
  # methods::validObject(.Object)
  .Object
})

#' @describeIn FleetClass Create a new `Fleet` object
#' @export
Fleet <- function(Name=NULL,
                  FishingMortality=new('fishingmortality'),
                  DiscardMortality=new('discardmortality'),
                  Effort=new('effort'),
                  Selectivity=new('selectivity'),
                  Retention=new('retention'),
                  Distribution=new('distribution'),
                  WeightFleet=array(),
                  BioEconomic=list(),
                  Misc=list()) {
  
  if (methods::is(Name, 'om'))
    return(Name@Fleet)
  
  methods::new('fleet',
               Name=Name,
               FishingMortality=FishingMortality,
               DiscardMortality=DiscardMortality,
               Effort=Effort,
               Selectivity=Selectivity,
               Retention=Retention,
               Distribution=Distribution,
               WeightFleet=array(),
               BioEconomic=list(),
               Misc=list())
}

#' @describeIn FleetClass Assign an `Fleet` object to an [OM()] object
#' @param x An [OM()] class object
#' @param value A `Fleet` object, or a list of `Fleet` objects, to assign to `x`
#' @export
`Fleet<-` <- function(x, value) {
  assignSlot(x, value, 'Fleet')
}


