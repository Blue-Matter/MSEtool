

# Fleet Class ----

#' Fleet Object
#'
#' Fleet Object
#'
#' @include 00_Class_discardmortality.R
#' @include 00_Class_effort.R
#' @include 00_Class_selectivity.R
#' @include 00_Class_retention.R
#' 
#'
#' @slot Misc `r Misc_param()`
#'
#' @seealso `r See_Also('fleet')`
#'
#' @name FleetClass
#' @example man-examples/Fleet-class.R
#' @export
setClass('fleet',
         slots=c(Name='char.null',
                 Effort='effort',
                 Catchability='catchability',
                 Selectivity='selectivity',
                 Retention='retention',
                 DiscardMortality='discardmortality',
                
                 Closure='num.array',
                 Targetting='num.array',
                 
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
                                          Effort=new('effort'),
                                          Catchability=new('catchability'),
                                          Selectivity=new('selectivity'),
                                          Retention=new('retention'),
                                          DiscardMortality=new('discardmortality'),
                                          Closure=array(),
                                          Targetting=array(),
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
  .Object@Targetting <- Targetting
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
                  Effort=new('effort'),
                  Catchability=new('catchability'),
                  Selectivity=new('selectivity'),
                  Retention=new('retention'),
                  DiscardMortality=new('discardmortality'),
                  Closure=array(),
                  Targetting=array(),
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
               Targetting=Targetting,
               WeightFleet=WeightFleet,
               BioEconomic=BioEconomic,
               Misc=Misc)
}

#' @describeIn FleetClass Assign an `Fleet` object to an [OM()] object
#' @param x An [OM()] class object
#' @param value A `Fleet` object, or a list of `Fleet` objects, to assign to `x`
#' @export
`Fleet<-` <- function(x, value) {
  assignSlot(x, value, 'Fleet')
}


