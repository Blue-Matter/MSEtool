

Get <- function(object, slots, TimeSteps=NULL, df=FALSE) {
  
  cl <- class(object)
  for (i in seq_along(slots)) {
    object <- slot(object, slots[i])
  }
  if (is.null(object) | all(is.na(object)))
    return(NULL)
  
  object <- ArraySubsetTimeStep(object, TimeSteps)
  
  if (df) 
    object <- as.data.frame.table(object, stringsAsFactors =FALSE,
                                  responseName =slots[length(slots)]) |>
    dplyr::mutate_if(is.character, as.numeric)
  
  
  object
}

# Stock -----
GetStockAtAge <- function(object, slots, TimeSteps=NULL, df=FALSE) {
  if (methods::is(object, 'om')) 
    object <- object@Stock
  
  if (isS4(object))
    return(Get(object, slots, TimeSteps, df))
  
  if (methods::is(object, 'StockList')) 
    return(
      purrr::map(object, Get, slots=slots, TimeSteps=TimeSteps, df=df)
    )
  Get(object, slots, TimeSteps, df)
}


GetLengthAtAge <- function(object, TimeSteps=NULL, df=FALSE) {
  GetStockAtAge(object,  slots=c('Length', 'MeanAtAge'), TimeSteps, df)
}


`SetLengthAtAge<-` <- function(x, value) {
  x@Length@MeanAtAge <- value
  x
}


GetWeightAtAge <- function(object, TimeSteps=NULL, df=FALSE) {
  GetStockAtAge(object,  c('Weight', 'MeanAtAge'), TimeSteps, df)
}

`SetWeightAtAge<-` <- function(x, value) {
  x@Weight@MeanAtAge <- value
  x
}

GetNMortalityAtAge <- function(object, TimeSteps=NULL, df=FALSE) {
  GetStockAtAge(object,  c('NaturalMortality', 'MeanAtAge'), TimeSteps, df)
}

`SetGetNMortalityAtAge<-` <- function(x, value) {
  x@NaturalMortality@MeanAtAge <- value
  x
}

GetMovementAtAge <- function(object, TimeSteps=NULL, df=FALSE) {
  GetStockAtAge(object,  c('Spatial', 'Movement'), TimeSteps, df)
}

GetUnfishedDist <- function(object, TimeSteps=NULL, df=FALSE) {
  GetStockAtAge(object,  c('Spatial', 'UnfishedDist'), TimeSteps, df)
}


GetMaturityAtAge <- function(object, TimeSteps=NULL, df=FALSE) {
  GetStockAtAge(object,  c('Maturity', 'MeanAtAge'), TimeSteps, df)
}

`SetMaturityAtAge<-` <- function(x, value) {
  x@Maturity@MeanAtAge <- value
  x
}

GetSemelparous <- function(object, TimeSteps=NULL, df=FALSE) {
  GetStockAtAge(object,  c('Maturity', 'Semelparous'), TimeSteps, df)
}



GetFecundityAtAge <- function(object, TimeSteps=NULL, df=FALSE) {
  GetStockAtAge(object, c('Fecundity', 'MeanAtAge'), TimeSteps, df)
}

`SetFecundityAtAge<-` <- function(x, value) {
  x@Fecundity@MeanAtAge <- value
  x
}


GetPlusGroup <- function(object, TimeSteps=NULL, df=FALSE) {
  GetStockAtAge(object,  c('Ages', 'PlusGroup'), TimeSteps, df)
}

GetR0 <- function(object, TimeSteps=NULL, df=FALSE) {
  GetStockAtAge(object,  c('SRR', 'R0'), TimeSteps, df)
}

GetSPFrom <- function(object, TimeSteps=NULL, df=FALSE) {
  GetStockAtAge(object,  c('SRR', 'SPFrom'), TimeSteps, df)
}


GetSpawnTimeFrac <- function(object, TimeSteps=NULL, df=FALSE) {
  GetStockAtAge(object,  c('SRR', 'SpawnTimeFrac'), TimeSteps, df)
}

GetRecDevInit <- function(object, TimeSteps=NULL, df=FALSE) {
  GetStockAtAge(object,  c('SRR', 'RecDevInit'), TimeSteps, df)
}

`SetRecDevInit<-` <- function(x, value) {
  x@SRR@RecDevInit <- value
  x
}

GetRecDevHist <- function(object, TimeSteps=NULL, df=FALSE) {
  GetStockAtAge(object,  c('SRR', 'RecDevHist'), TimeSteps, df)
}

`SetRecDevHist<-` <- function(x, value) {
  x@SRR@RecDevHist <- value
  x
}


GetRecDevProj <- function(object, TimeSteps=NULL, df=FALSE) {
  GetStockAtAge(object,  c('SRR', 'RecDevProj'), TimeSteps, df)
}

`SetRecDevProj<-` <- function(x, value) {
  x@SRR@RecDevProj <- value
  x
}

GetRelativeSize <- function(object, TimeSteps=NULL, df=FALSE) {
  out <- GetStockAtAge(object,  c('Spatial', 'RelativeSize'), TimeSteps, df)
  if (is.null(out)) {
    out <- array(1, dim=c(1,1))
    dimnames(out) <- list(Sim=1,
                          Area=1)
  }
  out
}


# Fleet -----

GetFleetAtAge <- function(object, slots, TimeSteps=NULL, df=FALSE) {
  if (methods::is(object, 'om')) 
    object <- object@Fleet
  
  if (isS4(object))
    return(Get(object, slots, TimeSteps, df))
  
  if (methods::is(object, 'FleetList')) {
      object <- purrr::map(object, GetFleetAtAge, slots=slots, TimeSteps=TimeSteps, df=df) 
      return(object)
  }
  object
}


GetApicalF <- function(object, TimeSteps=NULL, df=FALSE, array=TRUE) {
  out <- GetFleetAtAge(object, slots=c('FishingMortality', 'ApicalF'), TimeSteps, df)
  if (array && inherits(out, 'list'))
    return(List2Array(out))
  out
}

GetEffort <- function(object, TimeSteps=NULL, df=FALSE) {
  if (inherits(object, 'hist')) 
    return(
      purrr::map(object@Fleet, GetEffort, TimeSteps)
    )

  out <- GetFleetAtAge(object, slots=c('Effort', 'Effort'), TimeSteps, df)
  if (inherits(out, 'list'))
    return(List2Array(out))
  out
}





GetCatchability <- function(object, TimeSteps=NULL, df=FALSE) {
  out <- GetFleetAtAge(object, slots=c('Effort', 'Catchability'), TimeSteps, df)
  if (inherits(out, 'list'))
    return(List2Array(out))
  out
}



GetDiscardMortalityAtAge <- function(object, TimeSteps=NULL, df=FALSE) {
  out <- GetFleetAtAge(object,  c('DiscardMortality', 'MeanAtAge'), TimeSteps, df)
  
  if (inherits(out, 'list')) {
    out <- purrr::map(out, \(x) {
      if (!is.null(x))
        return(x)
      array(0, dim=c(1,1,1), 
            dimnames=list(Sim=1,
                          Age=0,
                          `Time Step`=TimeSteps[1]))
        
    })
    return(List2Array(out))
  }
    
  if (!is.null(out))
    return(out)
  
  array(0, dim=c(1,1,1), 
        dimnames=list(Sim=1,
                      Age=0,
                      `Time Step`=TimeSteps[1]))
}

`SetDiscardMortalityAtAge<-` <- function(x, value) {
  x@DiscardMortality@MeanAtAge <- value
  x
}

GetSelectivityAtAge <- function(object, TimeSteps=NULL, df=FALSE) {
  out <- GetFleetAtAge(object,  c('Selectivity', 'MeanAtAge'), TimeSteps, df)
  if (inherits(out, 'list'))
    return(List2Array(out))
  out
}

`SetSelectivityAtAge<-` <- function(x, value) {
  x@Selectivity@MeanAtAge <- value
  x
}

GetRetentionAtAge <- function(object, TimeSteps=NULL, df=FALSE) {
  out <- GetFleetAtAge(object,  c('Retention', 'MeanAtAge'),TimeSteps, df)

  if (inherits(out, 'list')) {
    out <- purrr::map(out, \(x) {
      if (!is.null(x))
        return(x)
      array(1, dim=c(1,1,1), 
            dimnames=list(Sim=1,
                          Age=0,
                          `Time Step`=TimeSteps[1]))
      
    })
    return(List2Array(out))
  }
  
  if (!is.null(out))
    return(out)
  
  array(1, dim=c(1,1,1), 
        dimnames=list(Sim=1,
                      Age=0,
                      `Time Step`=TimeSteps[1]))
  
}

`SetRetentionAtAge<-` <- function(x, value) {
  x@Retention@MeanAtAge <- value
  x
}

GetEmpiricalWeightAtAge <- function(object, TimeSteps=NULL, df=FALSE) {
  GetFleetAtAge(object,  slots=c('Weight'),TimeSteps, df)
}

`SetFleetWeightAtAge<-` <- function(x, value) {
  x@Weight <- value
  x
}

GetFleetWeightAtAge <- function(Stock, FleetList, TimeSteps=NULL) {
  nFleet <- length(FleetList)
  FleetEmpiricalWeightatAge <- GetEmpiricalWeightAtAge(FleetList, TimeSteps=TimeSteps) 
  StockWeightatAge <- GetWeightAtAge(Stock, TimeSteps)
  FleetWeightatAge <- replicate(nFleet, StockWeightatAge)
  l <- dimnames(StockWeightatAge)
  l$Fleet <- names(FleetList)
  dimnames(FleetWeightatAge) <- l
  
  for (fl in seq_along(FleetEmpiricalWeightatAge)) {
    if (is.null(FleetEmpiricalWeightatAge[[fl]])) {
      array <- AddDimension(StockWeightatAge, 'Fleet')
      dimnames(array)$Fleet <- l$Fleet[fl]
      ArrayFill(FleetWeightatAge) <- array
    } else {
      array <- AddDimension(FleetEmpiricalWeightatAge[[fl]], 'Fleet')
      dimnames(array)$Fleet <- l$Fleet[fl]
      ArrayFill(FleetWeightatAge) <- array
    }
  }
  FleetWeightatAge
}

# ---- Hist ----

# GetHistAtAge <- function(object, slots, TimeSteps=NULL, df=FALSE) {
# 
#   if (isS4(object))
#     return(Get(object, slots, TimeSteps, df))
#   
#   if (methods::is(object, 'StockList')) 
#     return(
#       purrr::map(object, Get, slots=slots, TimeSteps=TimeSteps, df=df)
#     )
#   Get(object, slots, TimeSteps, df)
# }


GetNumberAtAge <- function(Hist, TimeSteps=NULL) {
  if (is.null(TimeSteps))
    TimeSteps <- TimeSteps(Hist)
  
  purrr::map(Hist@Number, \(x)
             ArraySubsetTimeStep(x, TimeSteps=TimeSteps)
  )
}

GetBiomassAtAge <- function(Hist, TimeSteps=NULL) {
  if (is.null(TimeSteps))
    TimeSteps <- TimeSteps(Hist)
  
  Number <- purrr::map(Hist@Number, \(x)
                  ArraySubsetTimeStep(x, TimeSteps=TimeSteps))
  Weight <- GetWeightAtAge(Hist, TimeSteps=TimeSteps) |>
    purrr::map(AddDimension,'Area')
  
  purrr::map2(Number, Weight, ArrayMultiply)

}

GetRemovalAtAge <- function(Hist, TimeSteps=NULL) {
  if (is.null(TimeSteps))
    TimeSteps <- TimeSteps(Hist)
  
  purrr::map(Hist@Removal, \(x)
             ArraySubsetTimeStep(x, TimeSteps=TimeSteps)
  )
}

GetSpawnBiomassAtAge <- function(Hist, TimeSteps=NULL) {
  if (is.null(TimeSteps))
    TimeSteps <- TimeSteps(Hist)
  
  biomass <- GetBiomassAtAge(Hist, TimeSteps)
  maturity <- GetMaturityAtAge(Hist@Stock, TimeSteps) |>
    purrr::map(AddDimension, 'Area')
  
  purrr::map2(biomass, maturity, ArrayMultiply)
  
}

GetSProductionAtAge <- function(Hist, TimeSteps=NULL) {
  if (is.null(TimeSteps))
    TimeSteps <- TimeSteps(Hist)
  
  out <- vector('list', nStock(Hist))
  names(out) <- StockNames(Hist)
  
  for (st in 1:nStock(Hist)) {
    SpawnTimeFrac <- GetSpawnTimeFrac(Hist@Stock[[st]])
    fecundity <- GetFecundityAtAge(Hist@Stock[[st]], TimeSteps)
    NBegin <- ArraySubsetTimeStep(Hist@Number[[st]], TimeSteps) |>
      apply(1:3, sum) # sum over areas
    
    if (SpawnTimeFrac==0) {
      out[[st]] <- ArrayMultiply(NBegin, fecundity)
    } else {
      # calculate number alive at SpawnTimeFrac of the time step
      # before aging
      fishingmortality <- GetFatAgeArray(Hist@Fleet[[st]], TimeSteps) |>
        apply(c('Sim', 'Age', 'Time Step'), sum) # sum over fleets
      naturalmortality <- GetNMortalityAtAge(Hist@Stock[[st]], TimeSteps) 
      totalmortality <- ArrayAdd(fishingmortality, naturalmortality)
      Nspawn <- NBegin * exp(-totalmortality*SpawnTimeFrac)
      out[[st]] <- ArrayMultiply(Nspawn, fecundity)
    }
  }
  
  for (st in 1:nStock(Hist)) {
    SPfrom <- GetSPFrom(Hist@Stock[[st]])
    if (!is.null(SPfrom))
      stop('not done yet!')
  }
  out
}


GetDensity <- function(Hist, TimeSteps=NULL) {
  purrr::map(Hist@Density, \(x)
             ArraySubsetTimeStep(x, TimeSteps=TimeSteps)
  )
}

GetEffortArea <- function(Hist, TimeSteps=NULL) {
  purrr::map(Hist@EffortArea, \(x)
             ArraySubsetTimeStep(x, TimeSteps=TimeSteps)
  )
}

