
CalcCatch_Biomass <- function(object, type=c("Landings", "Discards")) {
  CheckClass(object, c('hist', 'mse'), 'object')
  
  type <- match.arg(type, c('Landings', 'Discards'))
  
  if (type=='Landings') {
    NumberList <- object@LandingsAtAge
  } else {
    NumberList <- object@DiscardsAtAge
  }
  
  StockFleetList <- object@OM@Fleet
  
  isMSE <- inherits(object,'mse')
  
  purrr::map2(NumberList, StockFleetList,   
              \(num, fleetlist) {
                nArea <- dim(num)[5]
                FleetWeight <- purrr::map(fleetlist, \(fleet) {
                  weight <- fleet@WeightFleet |> 
                    AddDimension("Area") |>
                    ExtendAreas(1:nArea)
                  
                  if (isMSE) {
                    weight <- AddDimension(weight, "MP")
                  }
                  weight
                }) |> List2Array(pos=4)
                
                if (length(dimnames(num)[["Sim"]])==1) {
                  dimnames(num)[["Sim"]] <- "1"
                }
               
                if (length(dimnames(FleetWeight)[["Sim"]])==1) {
                  dimnames(FleetWeight)[["Sim"]] <- "1"
                }
          
                ArrayMultiply(num, FleetWeight)
              })
  
}

GetCatch <- function(object, 
                     Units=c('Biomass', 'Number'),
                     type=c('Landings', 'Discards', 'Removals'),
                     byAge=FALSE,
                     byFleet=FALSE,
                     byArea=FALSE) {
  CheckClass(object, c('hist', 'mse'), 'object')
  Units <- match.arg(Units, c('Biomass', 'Number'))
  type <- match.arg(type, c('Landings', 'Discards', 'Removals'))
  
  if (Units == 'Biomass' && byAge==FALSE && byArea==FALSE) {
    landings <- object@Landings
    discards <- object@Discards
    
    if (!byFleet) {
      landings <- SumOverFleet(landings)
      discards <- SumOverFleet(discards)
    }
    
    if (type=='Landings') {
      return(landings)
    } else if (type=='Discards') {
      return(discards)
    } else {
      return(ArraySum(landings, discards))  
    }
  }
  
  if (Units == 'Number') {
    landings <- object@LandingsAtAge
    discards <- object@LandingsAtAge
  } else {
    landings <- CalcCatch_Biomass(object, "Landings")
    discards <- CalcCatch_Biomass(object, "Discards")
  }

  if (!byAge) {
    landings <- purrr::map(landings, SumOverAge)
    discards <- purrr::map(discards, SumOverAge)
  }
  
  if (!byFleet) {
    landings <- purrr::map(landings, SumOverFleet)
    discards <- purrr::map(discards, SumOverFleet)
  }
  
  if (!byArea) {
    landings <- purrr::map(landings, SumOverArea)
    discards <- purrr::map(discards, SumOverArea)
  }
  
  if (byAge) {
    if (type=='Landings') {
      return(landings)
    } else if (type=='Discards') {
      return(discards)
    } else {
      removals <- purrr::map2(landings, discards, ArraySum)
      return(removals)
    }
  }
  
  if (type=='Landings') {
    landings <- List2Array(landings, "Stock", pos=2)
    return(landings)
  } else if (type=='Discards') {
    discards <- List2Array(discards, "Stock", pos=2)
    return(discards)
  }
  
  landings <- List2Array(landings, "Stock", pos=2)
  discards <- List2Array(discards, "Stock", pos=2)
  
  ArraySum(landings, discards)
  
}