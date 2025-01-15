
SetHistRel <- function(OM) {
  # Ignore MICE in historical period
  if (isFALSE(OM@Control$HistRel))
    return(list())
  Relations(OM) 
}

nSimUpdate <- function(OM, nSim=NULL, messages='default') {
  if (is.null(nSim))
    return(OM)
  if (nSim==OM@nSim)
    return(OM)
    
  msg <- SetMessages(messages)
  if (nSim>OM@nSim) {
    if (isTRUE(msg$alert==TRUE))
      cli::cli_alert_info('Argument `nSim` ({.val {nSim}}) is greater than `nSim(OM)` ({.val {nSim(OM)}}). Ignoring.')
    return(OM)                      
  }
  if (isTRUE(msg$alert==TRUE))
    cli::cli_alert_info('Setting {.val nSim(OM) <-  {nSim}}')
  OM@nSim <- nSim
  
  if (length(OM@CatchFrac)>0) 
    OM@CatchFrac <- lapply(OM@CatchFrac, function(x)
      x[1:OM@nSim, , drop = FALSE])
  OM
}

CheckClass <- function(object, class='om', name='OM', type='Argument') {
  if (isFALSE(methods::is(object, class)))
     cli::cli_abort('{type} {.var {name}} must be class {.var {class}}')
  invisible(object)
}

ConvertToList <- function(x) {
  if (methods::is(x, 'om')) {
    if (methods::is(x@Stock, 'stock'))
      x@Stock <- list(x@Stock)
    if (methods::is(x@Fleet, 'fleet'))
      x@Fleet <- list(list(x@Fleet))
  }
  if (methods::is(x, 'stock')) 
    x <- list(x)
  if (methods::is(x, 'fleet')) 
    x <- list(list(x))      
  x
}

ConvertFromList <- function(OM) {
  
}



CalcUnfishedSurvival <- function(Stock, SP=FALSE) {
  M_at_Age <- Stock@NaturalMortality@MeanAtAge
  PlusGroup <- Stock@Ages@PlusGroup
  if (SP) {
    SpawnTimeFrac <- Stock@SRR@SpawnTimeFrac  
  } else {
    SpawnTimeFrac <- NULL
  }
  CalcSurvival(M_at_Age, PlusGroup, SpawnTimeFrac)
}

#' @describeIn runMSE Development version of `Simulate`
#' @export
SimulateDEV <- function(OM=NULL, 
                        parallel=FALSE, 
                        messages='default',
                        nSim=NULL, 
                        silent=FALSE, 
                        ...) {
  
  if (isTRUE(silent)) 
    messages <- FALSE

  # ---- Initial Checks and Setup ----
  chk <- OM |> CheckClass() |> Check() # TODO OM checks
  
  OM <- OM |> nSimUpdate(nSim, messages) |>
    Populate(messages=messages) |>
    ConvertToList() 
  
  # TODO
  # OM@Allocation - dimension length
  # OM@Efactor - dimension length
  # Hermaphroditism do in Populate
  
  # new Hist object 
  
  setClass("AgeArray",
           slots=c(NatAge='list',
                   SNatAge='list',
                   BatAge='list',
                   SBatAge='list',
                   SPatAge='list',
                   Misc='list'
           ),
           contains='Created_ModifiedClass'
  )
  
  setClass("unfished",
           slots=c(Equilibrium='AgeArray',
                   Dynamic='AgeArray',
                   Misc='list'
           ),
           contains='Created_ModifiedClass'
  )
  
  
  setClass("hist",
           slots=c(OM='om',
                   Unfished='unfished',
                   RefPoints='list',
                   BatAge='list',
                   SBatAge='list',
                   SPatAge='list',
                   Misc='list'
           ),
           contains='Created_ModifiedClass'
  )

  hist <- new('hist')
  hist@Unfished@Equilibrium@SBatAge

  # OM - Operating Model
  # Reference Points
  # Unfished
  # PopulationDynamics
  # FleetDynamics
  
  
  # Hist |> Unfished() |> Equilibrium() |> NatAge() # TODO
  
  
  # ---- Calculate Unfished Dynamics ----
  
  CalcUnfishedDynamics <- function(OM,
                                   parallel=FALSE, 
                                   messages='default',
                                   nSim=NULL,
                                   ...) {
    
    
    
    
    # Calculates Equilibrium and Dynamic Unfished Population Dynamics
    
    OM <- OM |> nSimUpdate(nSim, messages) |>
      Populate(messages=messages) |>
      ConvertToList()
    
    ######################################
    # TODO                               #
    ######################################
    # - !! add Spatial Dimension !!      
    # - add Herm
    
    not <- function(val) !val
    
    Unfished <- new('unfished') 
    
    # Equilibrium 
    UnfishedSurvival <- purrr::map(OM@Stock, CalcUnfishedSurvival)
    UnfishedSurvivalSP <- purrr::map(OM@Stock, CalcUnfishedSurvival, SP=TRUE)
    R0 <- purrr::map(OM@Stock, GetR0)
    WeightatAge <- purrr::map(OM@Stock, GetWeightAtAge)
    MaturityatAge <- purrr::map(OM@Stock, GetMaturityAtAge)
    FecundityatAge <- purrr::map(OM@Stock, GetFecundityAtAge)
    # not sure if this will work for all cases
    ind <- lapply(FecundityatAge, is.null) |> unlist() |> not() |> which() 
    
    NatAge <- purrr::map2(R0, UnfishedSurvival, MultiplyArrays, structure=TRUE)
    BatAge <- purrr::map2(NatAge, WeightatAge, MultiplyArrays)
    
    SNatAge <- purrr::map2(R0, UnfishedSurvivalSP, MultiplyArrays, 
                           structure=TRUE) |>
      purrr::map2(MaturityatAge, MultiplyArrays)
    SBatAge <- purrr::map2(SNatAge, WeightatAge, MultiplyArrays)
    SPatAge <- purrr::map2(SNatAge[ind], FecundityatAge[ind], MultiplyArrays)
    
    # Distribute over areas
    if (nArea(OM) == 1) {
      # add spatial dimension only
    } else {
      # distribution over areas
      NatAge$`Red Snapper` |> dim()
      
      OM@Stock[[1]]@Spatial@UnfishedDist 
      
      NatAge$`Red Snapper`[1,1,1]
      
      NatAge$`Red Snapper`[1,1,1] * 
        OM@Stock[[1]]@Spatial@UnfishedDist[1,,1,1]
      
      OM@Stock[[1]]@Spatial@Movement[1,,,1,1] 
      
      t = CalcAsymptoticDist(OM@Stock[[1]]@Spatial@Movement[1,,,1,1])
      
      OM@Stock[[1]]@Spatial@UnfishedDist[1,,21,1]
      # for SAMMC Red snapper - initial distribution by age is
      # not the same in this version as previous method where it was calculated
      # by projecting out equilibrium
    }
    
  
    
    
    Unfished@Equilibrium@NatAge <- NatAge
    Unfished@Equilibrium@SNatAge <- SNatAge
    Unfished@Equilibrium@BatAge <- BatAge
    Unfished@Equilibrium@SBatAge <- SBatAge
    Unfished@Equilibrium@SPatAge <- SPatAge
    
    # Dynamic 
    RecDevInit <- purrr::map(OM@Stock, GetRecDevInit)
    RecDevHist <- purrr::map(OM@Stock, GetRecDevHist)
    RecDevProj <- purrr::map(OM@Stock, GetRecDevProj)
    
    c(dim(RecDevInit),
      dim(RecDevHist),
      dim(RecDevProj))
    
    ## UP TO HERE #####
    purrr::map2(RecDevInit, NatAge, MultiplyArrays)
    
    N[1,1,,1,] |> rowSums()
    
    NatAge[[1]][1,,1] * RecDevInit[[1]][1,]
    
    
   
   
    
  
    Unfished
  }
  

  

  
  
  
  # Calc dynamic unfished
  
  # Calc SPR
  
  
  
 
  
  
 
  


  
  # ---- Calculate Reference Points ----
  
  CalcReferencePoints <- function(OM, 
                                  parallel=FALSE, 
                                  messages='default',
                                  nSim=NULL, 
                                  ...) {
    
    OM <- OM |> nSimUpdate(nSim, messages) |>
      Populate(messages=messages) |>
      ConvertToList() |>
      StartMessages(messages)
    
  }
  
  

  
  
    # convert this to object later and add to initialize
    
    OM@Control$ReferencePoints <- list()
    
    OM@Control$ReferencePoints$UnfishedEq <- TRUE
    OM@Control$ReferencePoints$UnfishedDyn <- TRUE
    
    OM@Control$ReferencePoints$Unfished <- TRUE
    
    
    # if (!isFALSE(OM@Control$ReferencePoints$Unfished)) {
    #   
    # }
    
    
    

    
    CalcR0 <- function(Stock) {
      R0 <- Stock@SRR@Pars$R0
      if (!is.null(R0)) 
        
        return(R0)
      stop('R0 not in SRR@Pars. Calculate from parameters?')
    }
    
    EquilibriumUnfished 
  

    
    

    
    R0 <- lapply(OM@Stock, CalcR0)
    
    
    
    N0
    B0
    SN0
    SB0
    
    
    
    
    SpawningPerRecruit
    
  
    
    
  
  
  
  
  
  ## ---- Unfished Equilibrium ----
  
  ## ---- Per-Recruit Reference Points ----
  
  ## ---- Unfished Reference Points ----
  
  ## ---- Dynamic Unfished Reference Points ---- 
  
  ## ---- MSY Reference Points ----
  
  ## ---- Calculate Spawning Potential Ratio ----
  
  ## ---- Mean Generation Time ----
  
  ## ---- Reference Yield ----
  
  
  
  # ---- Optimize Rec Devs for Initial Depletion ----
  
  
  # ---- Non-Equilibrium Initial Year ----
  
  # ---- Optimize catchability (q) to fit depletion ----
  # (if needed)
  
  # ---- Run Historical Simulations ----
  
  # ---- Calculate Historical Catch ----
  
  # ---- Condition Observation Object on Real Fishery Data ----
  
  # ---- Simulate Fishery Data ----
  
  # ---- Return `hist` Object ----
  
  
  
  # # --- Sample Obs Parameters ----
  # # TODO - updated Obs object
  # 
  # ObsPars[[p]] <- lapply(1:nf, function(f) {
  #   SampleObsPars(MOM@Obs[[p]][[f]], nsim,
  #                 cpars = SampCpars[[p]][[f]],
  #                 Stock = StockPars[[p]],
  #                 nyears, proyears)
  # })
  # 
  # # --- Sample Imp Parameters ----
  # # TODO - updated Imp object
  # 
  # ImpPars[[p]] <- lapply(1:nf, function(f) {
  #   SampleImpPars(MOM@Imps[[p]][[f]], nsim,
  #                 cpars = SampCpars[[p]][[f]],
  #                 nyears, proyears)
  # })
  
  

  
  
  
  HistRel <- SetHistRel(OM) 
    
  
  
  
                        
  
  
  
  
}


