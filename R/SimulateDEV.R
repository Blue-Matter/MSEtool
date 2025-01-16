# 2025/01/16 
# - fix errors in testOM.R - line 6 Convert(testOM)
# - then test and compare SimulateDEV with Hist from Simulate 


# New Classes (temp) ----

setClass("hist",
         contains=c('om', 'Created_ModifiedClass'),
         slots=c(Unfished='unfished',
                 RefPoints='refpoints'
                 
         )
)


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
    messages <- FALSE

  # ---- Initial Checks and Setup ----
  chk <- OM |> CheckClass() |> Check() # TODO OM checks
  
  OM <- OM |> 
    nSimUpdate(nSim, messages) |>
    Populate(messages=messages) |>
    ConvertToList() # converts OM@Stock and OM@Fleet to lists
  
  
  Hist <- new('hist') # new Hist object to return 
  
  # ---- Calculate Unfished Dynamics ----
  Hist@Unfished <- CalcUnfishedDynamics(OM, parallel, messages)
  
  
  # ---- Calculate Reference Points ----
  Hist@RefPoints <- CalcReferencePoints(OM, parallel, messages,
                                        Unfished = Hist@Unfished)

  

  
    
    
  

  
  
  
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


