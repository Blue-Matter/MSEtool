

# Fleet Class ----

#' Fleet Object
#'
#' Fleet Object
#'
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
                 Effort='num.array.df',
                 Units='char.null', # effort units
                 Distribution='num.array',
                 
                 Catchability='num.array',
                 qCV='num.array',
                 qInc='num.array',
                 qArea='num.array',
                 
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
                                          Effort=array(),
                                          Units=NULL,
                                          Distribution=array(),
                                          Catchability=array(),
                                          qCV=NULL,
                                          qInc=NULL,
                                          qArea=array(),
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
  .Object@Units <- Units
  .Object@Distribution <- Distribution
  
  .Object@Catchability <- Catchability
  .Object@qCV <- qCV
  .Object@qInc <- qInc
  .Object@qArea <- qArea
  
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
                   Effort=array(),
                   Units=NULL,
                   Distribution=array(),
                   Catchability=array(),
                   qCV=NULL,
                   qInc=NULL,
                   qArea=array(),
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
               Units=Units,
               Distribution=Distribution,
               Catchability=Catchability,
               qCV=qCV,
               qInc=qInc,
               qArea=qArea,
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
#' @param value A `Fleet` object, or a list of `Fleet` objects, to assign to `x`
#' @export
`Fleet<-` <- function(x, value) {
  assignSlot(x, value, 'Fleet')
}


