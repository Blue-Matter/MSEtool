
#' Populate an Object 
#' 
#' @name Populate
NULL



# Obs ----

StructureObs <- function(OM) {
  
  StockNames <- StockNames(OM)
  FleetNames <- FleetNames(OM)
  
  # Obs should be list length `nStock`
  # with each element a list `nFleet`
  
  # Recycles over both stocks and fleets
  if (inherits(OM@Obs,'obs')) {
    OM@Obs <- MakeNamedList(StockNames, MakeNamedList(FleetNames, OM@Obs))
  }
  
  if (!is.list(OM@Obs)) {
    cli::cli_abort("`OM@Obs` must be a list or an object of class `obs`")
  }
  
  # if (length(StockNames)>1)
  #   warning("Multi-stock/fleet Obs not done")
  # 
  # TODO 
  
  
  
  # if (length(OM@Obs)!=nStock(OM)) {
  #   cli::cli_abort('`OM@Obs` must be a list length `nStock(OM)`')
  # }
  # names(OM@Obs) <- StockNames(OM)
  
  OM
}

PopulateObs <- function(OM) {
  
  if (EmptyObject(OM@Obs)) {
    # initialize Obs object for conditioning
    if(is.null(FleetNames(OM)))
      return(OM)
    OM@Obs <- MakeNamedList(StockNames(OM),
                            MakeNamedList(FleetNames(OM),new('obs')))
    return(OM)
  }
 
  OM <- StructureObs(OM)
  HistYears <- Years(OM,'H')
  ProjYears <- Years(OM,'P')
  
  for (st in 1:length(OM@Obs)) {
    for (fl in 1:length(OM@Obs[[1]])) {
      SetSeed(OM@Obs[[st]][[fl]], OM@Seed)
      
      OM@Obs[[st]][[fl]]@Effort <- PopulateEffortObs(Effort=OM@Obs[[st]][[fl]]@Effort, 
                                                     nSim=OM@nSim, 
                                                     HistYears,
                                                     ProjYears)
      
      OM@Obs[[st]][[fl]]@Landings <- PopulateCatchObs(Catch=OM@Obs[[st]][[fl]]@Landings, 
                                                      nSim=OM@nSim, 
                                                      HistYears,
                                                      ProjYears)
      
      OM@Obs[[st]][[fl]]@Discards <- PopulateCatchObs(Catch=OM@Obs[[st]][[fl]]@Discards, 
                                                      nSim=OM@nSim, 
                                                      HistYears,
                                                      ProjYears)
      
      OM@Obs[[st]][[fl]]@CPUE <- PopulateIndexObs(Index=OM@Obs[[st]][[fl]]@CPUE, 
                                                  nSim=OM@nSim, 
                                                  HistYears,
                                                  ProjYears)
    
      OM@Obs[[st]][[fl]]@Survey <- PopulateIndexObs(Index=OM@Obs[[st]][[fl]]@Survey, 
                                                    nSim=OM@nSim, 
                                                    HistYears,
                                                    ProjYears)
      
      OM@Obs[[st]][[fl]]@CAA
      
      OM@Obs[[st]][[fl]]@CAL
      
    }
    
    
  
  }
    
    
  OM
}


getACF <- function(Value) {
  acf(Value, plot=FALSE)[[1]][2,1,1]
}

PopulateIndexObs <- function(Index, nSim, HistYears, ProjYears) {
  Index@CV <- PopulateObsCV(Index@CV, nSim)
  Index@Error <- PopulateObsError(Index, nSim, c(HistYears, ProjYears))
  Index@Beta # TODO - currently not implemented
  Index@Ref <- PopulateObsRef(Index@Ref, nSim)

  if (length(Index@Years)<1)
    Index@Years <- HistYears
  
  # TODO implement AC if specified 
  if (!is.null(Index@AC)) {
    
  }
 
  Index@Selectivity
  Index@Type
  Index@q
  Index
  
}

PopulateEffortObs <- function(Effort, nSim, HistYears, ProjYears) {
  if (EmptyObject(Effort))
    return(Effort)
  Effort@CV <- PopulateObsCV(Effort@CV, nSim)
  Effort@Error <- PopulateObsError(Effort, nSim, c(HistYears, ProjYears))
  Effort@Bias <- PopulateObsBias(Effort, nSim)
  
  # if (length(Effort@Years)<1)
  #   Effort@Years <- Years
  Effort
}

PopulateCatchObs <- function(Catch, nSim, HistYears, ProjYears) {
  if (EmptyObject(Catch))
    return(Catch)

  Catch@CV <- PopulateObsCV(Catch@CV, nSim)
  Catch@Error <- PopulateObsError(Catch, nSim, c(HistYears, ProjYears))
  Catch@Bias <- PopulateObsBias(Catch, nSim)
  Catch@Ref <- PopulateObsRef(Catch@Ref, nSim)
  
  if (length(Catch@Years)<1)
    Catch@Years <- HistYears
  
  if (!Catch@Type %in% c('Removals', 'Landings'))
    cli::cli_abort(message="Valid values for `Obs@Catch@Type` are: {.val {c('Removals', 'Landings')}} ")
  
  Catch
}

PopulateObsRef <- function(Ref, nSim) {
  if (!length(Ref)) {
    return(Ref)
  }
  
  if (!is.null(dimnames(Ref))) {
    return(Ref[1:nSim])
  }
  
  if (nSim!=2 && length(Ref)==nSim)
    return(Ref)

  CV <- StructurePars(list(Ref), nSim)[[1]] |> 
    ExtendSims(nSim) |>
    DropDimension("Year", FALSE)
  
  Error <- array(rlnorm(nSim,
                        mconv(1, CV),
                        sdconv(1, CV)),
                 nSim)
  
  dimnames(Error) <- list(Sim=1:nSim)
  Error
}

PopulateObsCV <- function(CV, nSim) {
  if  (!length(CV)) {
    return(CV)
  }
  
  if (!is.null(dimnames(CV))) {
    return(CV[1:nSim])
  }
  CV <- StructurePars(list(CV), nSim)[[1]] |> 
    ExtendSims(nSim) |>
    DropDimension("Year", FALSE)
  CV
}


PopulateObsError <- function(object, nSim, Years) {
  nTS <- length(Years)
  if (length(object@Error)<1) {
    if (is.null(object@CV))
      return(object@Error)
    Error <- array(rlnorm(nTS * nSim,
                          mconv(1, rep(object@CV, nTS)),
                          sdconv(1, rep(object@CV, nTS))),
                   c(nSim, nTS))
    
    dimnames(Error) <- list(Sim=1:nSim,
                            Year=Years)
    object@Error <- Error
  } else {
    if (!inherits(object@Error, 'array'))
      cli::cli_abort("`object@Error` must be an array with `nSim` rows and `nTS` columns")
    
    chk1 <- nrow(object@Error) != nSim
    
    if (chk1 & nrow(object@Error) > nSim) {
      object@Error <- object@Error[1:nSim,]
      chk1 <- !chk1
    }
    
    
    chk2 <- ncol(object@Error) != length(Years)
    if (chk1 & chk2) 
      cli::cli_abort("`object@Error` must be an array with `nSim` rows and `nTS` columns")
    if (chk1 & !chk2) 
      cli::cli_abort("`object@Error` must be an array with `nSim` rows")
    
    if (!chk1 & chk2) 
      cli::cli_abort("`object@Error` must be an array with `nTS` columns")
    
    dimnames(object@Error) <- list(Sim=1:nSim,
                                   Year=Years)
    
  }
  object@Error
}

PopulateObsBias <- function(object, nSim) {
  if (length(object@Bias)<1) {
    object@Bias <- array(1, dim=nSim, dimnames = list(Sim=1:nSim)) 
    return(object@Bias)
  }
  
  if (any(object@Bias<=0))
    cli::cli_abort("`Bias` must be positive values")
  
  if (length(object@Bias) != nSim) {
    object@Bias <- array(rlnorm(nSim, mconv(1, object@Bias), sdconv(1, object@Bias)),
                         dim=nSim, 
                         dimnames = list(Sim=1:nSim)) 
    return(object@Bias)
  }
    
  if (length(object@Bias) > nSim) {
    object@Bias <- object@Bias[1:nSim]
  } 
  
  if (length(object@Bias) == nSim) {
    object@Bias <- array(object@Bias, dim=nSim, dimnames = list(Sim=1:nSim)) 
    return(object@Bias)
  }
  cli::cli_abort("`Catch@Bias` must be length 1, 2, or `nSim`")
  object@Bias
}
