
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

#' @describeIn runMSE Development version of `Simulate`
#' @export
SimulateDEV <- function(OM=NULL, 
                        parallel=FALSE, 
                        messages='default',
                        nSim=NULL, 
                        silent=FALSE, 
                        ...) {
  
  if (isTRUE(silent)) 
    messages <- 'FALSE'

  # ---- Initial Checks and Setup ----
  chk <- OM |> CheckClass() |> Check() # TODO OM checks
  
  OM <- OM |> nSimUpdate(nSim, messages) |>
    Populate(messages=messages) |>
    ConvertToList() |>
    StartMessages(messages)
  
  # TODO
  # OM@Allocation - dimension length
  # OM@Efactor - dimension length
  # Hermaphroditism do in Populate
  
  
  CalcUnfishedSurvival <- function(Stock, SP=FALSE) {
    M_at_Age <- Stock@NaturalMortality@MeanAtAge
    PlusGroup <- Stock@Ages@PlusGroup
    if (SP) {
      SpawnTimeFrac <- Stock@SRR@SpawnTimeFrac  
    } else {
      SpawnTimeFrac <- NULL
    }
    CalcSurvival(M_at_Age, PlusGroup, SpawnTimeFrac)
    
    # UnfishedSurvival <- list()
    # # spawning population; SP = Spawning Production
    # UnfishedSurvivalSP <- list()
    # 
    # for (st in 1:nStock(OM)) {
    #   M_at_Age <- OM@Stock[[st]]@NaturalMortality@MeanAtAge
    #   PlusGroup <- OM@Stock[[st]]@Ages@PlusGroup
    #   SpawnTimeFrac <- OM@Stock[[st]]@SRR@SpawnTimeFrac
    #   
    #   UnfishedSurvival[[st]] <- CalcSurvival(M_at_Age, PlusGroup)
    #   if (any(SpawnTimeFrac!=0) && length(SpawnTimeFrac)>0) 
    #     UnfishedSurvivalSP[[st]] <- CalcSurvival(M_at_Age, PlusGroup, SpawnTimeFrac)
    # }
    # names(UnfishedSurvival) <- StockNames(OM)
    # 
    # if (length(UnfishedSurvivalSP)>0) 
    #   names(UnfishedSurvivalSP) <- StockNames(OM)
    # 
    # list(Population=UnfishedSurvival,
    #      Spawning=UnfishedSurvivalSP)
  }
  

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
  
    CalcN0 <- function(R0, UnfishedSurvival) {
      # R0 dimensions! 
      # should have at least nsim 
      # Add R0 to SRR object and remove from Pars!
      # UP TO HERE 
      
      MultiplyArrays(array1=R0[[1]], 
                     array2=UnfishedSurvival[[1]])
    
    }
    
    
    UnfishedSurvival <- lapply(OM@Stock, CalcUnfishedSurvival)
    UnfishedSurvivalSP <- lapply(OM@Stock, CalcUnfishedSurvival, SP=TRUE)
    
    R0 <- lapply(OM@Stock, CalcR0)
    
    
    
    N0
    B0
    SN0
    SB0
    
    
    
    
    SpawningPerRecruit
    
  
    
    
  }
  
  
  
  
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
  
  

  
  
  
  HistRel <- SetHistRel(OM) {
    
  }
  
  
                        
  
  
  
  
}


