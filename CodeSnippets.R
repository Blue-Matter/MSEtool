# ---- CalcFatAge ----
setGeneric('CalcFatAge', function(x, TimeSteps=NULL, apicalF=NULL)
  standardGeneric('CalcFatAge')
)

setMethod('CalcFatAge', c('om', 'ANY'), function(x, TimeSteps=NULL, apicalF=NULL) {
  purrr::map(x, CalcFatAge, TimeSteps, apicalF)
})

setMethod('CalcFatAge', c('StockFleetList', 'ANY'),
          function(x, TimeSteps=NULL, apicalF=NULL) {
            out <- purrr::map(x, CalcFatAge, TimeSteps, apicalF)
            class(out) <- 'StockFleetList'
            out
          })


setMethod('CalcFatAge', c('FleetList', 'ANY'), 
          function(x, TimeSteps=NULL, apicalF=NULL) {
            out <- purrr::map(x, CalcFatAge, TimeSteps, apicalF)
            class(out) <- 'FleetList'
            out
          })

setMethod('CalcFatAge', c('FleetList', 'ANY', 'list'), 
          function(x, TimeSteps=NULL, apicalF=NULL) {
            out <- purrr::map2(x, apicalF, \(x, apicalF)
                               CalcFatAge(x, TimeSteps = TimeSteps, apicalF)
            )
            class(out) <- 'FleetList'
            out
          })

setMethod('CalcFatAge', c('fleet', 'ANY'),
          function(x, TimeSteps=NULL, apicalF=NULL) {
            
            # x = OM@Fleet[[1]][[1]]
            # TimeSteps <- TimeSteps(OM, 'Historical')
            
            if (is.null(apicalF)) {
              x <- CalcApicalF(x, TimeSteps)
              apicalF <- GetApicalF(x, TimeSteps, array=FALSE)
            }
            
            selectivity <- GetSelectivityAtAge(x, TimeSteps)
            retention <- GetRetentionAtAge(x, TimeSteps)
            discardmortality <- GetDiscardMortalityAtAge(x, TimeSteps)
            DimSelectivity <- dim(selectivity)
            DimapicalF <- dim(apicalF)
            
            # add age dimension
            apicalF <- apicalF |> AddDimension('Age') |> aperm(c(1,3,2))
            
            FInteract <- ArrayMultiply(apicalF, selectivity)
            if (is.null(retention)) {
              FRetain <- retention <- FInteract
              retention[] <- 1
            } else {
              FRetain <- ArrayMultiply(FInteract, retention)   
            }
            
            FDiscardTotal <- ArraySubtract(FInteract, FRetain)
            
            if (is.null(discardmortality)) {
              discardmortality <- FInteract
              discardmortality[] <- 0
              FDiscardDead <- discardmortality
            } else {
              FDiscardDead <- ArrayMultiply(FDiscardTotal, discardmortality)
            }
            
            FDead <- FRetain + FDiscardDead  
            
            DeadApicalF <- AddDimension(apply(FDead, c(1,3), max), 'Age') |> aperm(c(1,3,2)) 
            InteractDeadRatio <- ArrayDivide(apicalF, DeadApicalF)
            InteractDeadRatio[!is.finite(InteractDeadRatio)] <- 1
            if (!all(InteractDeadRatio>0.99)) {
              # Inflate ApicalF to account for discard mortality
              # FishingMortality@apicalF is the apicalF of Dead Fish;
              # If passed as an argument, `apicalF` is first calculated for 'Caught' or 'Interacted' fish,
              # i.e., assuming it's proportion to Effort.
              # Here it's adjusted so that the actual apicalF on dead fish is equal to `apicalF`
              
              
              apicalF <- ArrayMultiply(apicalF, InteractDeadRatio)
              FInteract <- ArrayMultiply(apicalF, selectivity)
              FRetain <- ArrayMultiply(FInteract, retention)
              FDiscardTotal <- ArraySubtract(FInteract, FRetain)
              FDiscardDead <- ArrayMultiply(FDiscardTotal, discardmortality)
              FDead <- FRetain + FDiscardDead 
            }
            
            ArrayFill(x@FishingMortality@ApicalF) <- apply(FDead, c(1,3), max)
            ArrayFill(x@FishingMortality@DeadAtAge) <- FDead
            ArrayFill(x@FishingMortality@RetainAtAge) <- FRetain
            
            x
          })

# ---- CalcFTotal ----
setGeneric('CalcFTotal', function(x, TimeSteps=NULL)
  standardGeneric('CalcFTotal')
)

setMethod('CalcFTotal', c('om', 'ANY'), function(x, TimeSteps=NULL) {
  purrr::map(x, CalcFTotal, TimeSteps)
})

setMethod('CalcFTotal', c('StockFleetList', 'ANY'),
          function(x, TimeSteps=NULL) {
            out <- purrr::map(x, CalcFTotal, TimeSteps=TimeSteps)
            class(out) <- 'StockFleetList'
            out
          })


setMethod('CalcFTotal', c('FleetList', 'ANY'), 
          function(x, TimeSteps=NULL) {
            FFleet <- purrr::map(x, GetFatAgeArray, TimeSteps=TimeSteps)
            array <- array(unlist(FFleet), dim=c(dim(FFleet[[1]]), length(FFleet)))
            array <- apply(array, 1:3, sum)
            dd <- dim(array)
            dimnames(array) <- dimnames(FFleet[[1]])
            class(array) <- 'FTotal'
            array
          })





# Calculates apicalF (dead) from Effort
# accounting for discarding and discard mortality

# ---- CalcFatAge ----
setGeneric('CalcApicalF', function(x, TimeSteps=NULL)
  standardGeneric('CalcApicalF')
)

setMethod('CalcApicalF', c('FleetList', 'ANY'),
          function(x, TimeSteps=NULL) {
            purrr::map(x, CalcApicalF, TimeSteps=TimeSteps)
          })


setMethod('CalcApicalF', c('fleet', 'ANY'),
          function(x, TimeSteps=NULL) {
            selectivity <- GetSelectivityAtAge(x, TimeSteps)
            retention <- GetRetentionAtAge(x, TimeSteps)
            discardmortality <- GetDiscardMortalityAtAge(x, TimeSteps)
            effort <- GetEffort(x, TimeSteps)
            
            effort <- effort |> AddDimension('Age') |> aperm(c(1,3,2))
            interact <- ArrayMultiply(effort, selectivity)
            retain <- ArrayMultiply(interact, retention)
            discard <- ArraySubtract(interact,retain)
            deaddiscard <- ArrayMultiply(discard, discardmortality)
            
            apicalF <- apply(ArrayAdd(retain, deaddiscard), c(1,3), max)
            ArrayFill(x@FishingMortality@ApicalF) <- apicalF
            x
          })






#' FishingMortality Object
#'
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#'
#'
#' @seealso `r See_Also('fishingmortality')`
#'
#' @name FishingMortality
#' @rdname FishingMortality
#' @docType class
#' @example man-examples/FishingMortality-class.R
#' @export
setClass('fishingmortality',
         slots=c(ApicalF='array.null',
                 DeadAtAge='array.null',
                 RetainAtAge='array.null'
         ),
         contains = c('MiscClass')
)

setMethod("initialize", "fishingmortality", function(.Object,
                                                     ApicalF=NULL,
                                                     DeadAtAge=NULL,
                                                     RetainAtAge=NULL,
                                                     Misc=list()) {
  .Object@ApicalF <- ApicalF
  .Object@DeadAtAge <- DeadAtAge
  .Object@RetainAtAge <- RetainAtAge
  .Object@Misc <- Misc
  #   .Object@Created <- Sys.time()
  .Object
})

#' @describeIn FishingMortality Create a new `fishingmortality` class object
#' @export
FishingMortality <- function(ApicalF=NULL,
                             DeadAtAge=NULL,
                             RetainAtAge=NULL,
                             Misc=list()) {
  if (inherits(ApicalF, 'fleet'))
    return(ApicalF@FishingMortality)
  
  methods::new('fishingmortality',
               ApicalF=ApicalF,
               DeadAtAge=DeadAtAge,
               RetainAtAge=RetainAtAge,
               Misc=Misc)
}

#' @describeIn FishingMortality Assign an `FishingMortality` object to a [Fleet()] object
#' @param x A [Fleet()] class object
#' @param value A `FishingMortality` object to assign to `x`
#' @export
`FishingMortality<-` <- function(x, value) {
  assignSlot(x, value, 'FishingMortality')
}

#' Distribution Object
#'
#' Spatial Distribution of fishing fleet
#'
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#'
#' @slot Misc `r Misc_param()`
#'
#' @seealso `r See_Also('distribution')`
#'
#' @name Distribution
#' @rdname Distribution
#' @docType class
#' @example man-examples/Distribution-class.R
#' @export
setClass('distribution',
         slots=c(Closure='num.array',
                 Cost='num.array.list',
                 Misc='list'
         ))

setValidity('distribution', isValidObject)

setMethod("initialize", "distribution", function(.Object,
                                                 Closure=NULL,
                                                 Cost=NULL,
                                                 Misc=list()) {
  .Object@Closure <- Closure
  .Object@Cost <- Cost
  .Object@Misc <- Misc
  .Object
})

#' @describeIn Distribution Create a new `Distribution` object
#' @export
Distribution <- function(Closure=NULL,
                         Cost=NULL,
                         Misc=list()) {
  
  if (methods::is(Closure, 'fleet'))
    return(Closure@Distribution)
  
  methods::new('distribution',
               Closure=Closure,
               Cost=Cost,
               Misc=Misc)
}

#' @describeIn Distribution Assign an `Distribution` object to a [Fleet()] object
#' @param x A [Fleet()] class object
#' @param value A `Distribution` object to assign to `x`
#' @export
`Distribution<-` <- function(x, value) {
  assignSlot(x, value, 'Distribution')
}




#' Effort Object
#'
#' Historical fishing effort
#'
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#'
#' @slot Vessels Numeric array. The number of fishing vessels per time step.
#' @slot Trips Numeric array. The number of trips per vessel per time step.
#' @slot Misc `r Misc_param()`
#'
#' @seealso `r See_Also('effort')`
#'
#' @name Effort
#' @rdname Effort
#' @docType class
#' @example man-examples/Effort-class.R
#' @export
setClass('effort',
         slots=c(Vessels='num.array.df',
                 Trips='num.array.list',
                 MaxVessels='num.array.list',
                 MaxTrips='num.array.list',
                 Distribution='num.array.list',
                 Units='char.null'
         ),
         contains = c('MiscClass')
)

setValidity('effort', isValidObject)

setMethod("initialize", "effort", function(.Object,
                                           Vessels=NULL,
                                           Trips=NULL,
                                           MaxVessels=NULL,
                                           MaxTrips=NULL,
                                           Distribution=NULL,
                                           Units=c('Vessels', 'Trips'),
                                           Misc=list()) {
  
  .Object@Vessels <- Vessels
  .Object@Trips <- Trips
  .Object@MaxVessels <- MaxVessels
  .Object@MaxTrips <- MaxTrips
  .Object@Distribution <- Distribution
  .Object@Units <- Units
  .Object@Misc <- Misc
  #   .Object@Created <- Sys.time()
  .Object
})

#' @describeIn Effort Create a new `effort` class object
#' @export
Effort <- function(Vessels=NULL,
                   Trips=NULL,
                   MaxVessels=NULL,
                   MaxTrips=NULL,
                   Distribution=NULL,
                   Units=c('Vessels', 'Trips'),
                   Misc=list()) {
  if (methods::is(Effort, 'fleet'))
    return(Effort@Effort)
  
  methods::new('effort',
               Vessels=Vessels,
               Trips=Trips,
               MaxVessels=MaxVessels,
               MaxTrips=MaxTrips,
               Distribution=Distribution,
               Units=Units,
               Misc=Misc)
}

#' @describeIn Effort Assign an `Effort` object to a [Fleet()] object
#' @param x A [Fleet()] class object
#' @param value A `Effort` object to assign to `x`
#' @export
`Effort<-` <- function(x, value) {
  assignSlot(x, value, 'Effort')
}





#' Catchability Object
#'
#' Historical fishing effort
#'
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#'
#' @slot Q Numeric array.
#' @slot QArea Numeric array. 
#' @slot Misc `r Misc_param()`
#'
#' @seealso `r See_Also('catchability')`
#'
#' @name Catchability
#' @rdname Catchability
#' @docType class
#' @example man-examples/Catchability-class.R
#' @export
setClass('catchability',
         slots=c(Q='num.array',
                 qCV='num.array',
                 qInc='num.array',
                 qArea='num.array'
         ),
         
)



Fleet@FishingMortality <- CombineFishingMortality(lapply(FleetList, slot, 
                                                         'FishingMortality'),
                                                  nSim, nAges, TimeSteps)

if (!silent)
  cli::cli_progress_update(id=id)
