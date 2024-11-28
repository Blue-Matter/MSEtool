# default: info, progress, warnings
# FALSE: no messages or warnings
# minimal: 

SetMessages <- function(messages='default') {
  msg <- list()
  if (isFALSE(messages)) 
    return(msg)

  msg$info <- TRUE
  msg$alert <- TRUE
  msg$progress <- TRUE
  msg$warning <- TRUE

  msg
}

StartMessages <- function(OM, messages='default') {
  msg <- SetMessages(messages)
  
  # Allocation
  if(!length(MOM@Allocation)) {
    MOM@Allocation <- CatchFrac
    if (!silent) message_info("Slot @Allocation of MOM object not specified. Setting slot ",
                              "@Allocation equal to slot @CatchFrac - current catch fractions")
  }
  
  if(!length(MOM@Efactor)) {
    MOM@Efactor <- lapply(1:np, function(x) matrix(1, nsim, nf))
    if (!silent) message_info("Slot @Efactor of MOM object not specified. Setting slot @Efactor ",
                              "to current effort for all fleets")
  }
  
  
  if (nStock(OM)==1)
    return(NULL)
  
  if (!length(OM@Relations) && !length(MOM@SexPars)) {
    if (isTRUE(msg$alert==TRUE)) {
      cli::cli_alert_info("You have specified more than one stock but no MICE relationships (`Relations(OM)`) or sex-specific relationships (`SexPars(OM)`) among these. \nAs they are independent, consider doing MSE for one stock at a time for computational efficiency\n")
    }
  }
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
    CatchFrac <- lapply(OM@CatchFrac, function(x) x[1:OM@nSim, , drop = FALSE])
  OM
}

CheckClass <- function(object, class='om', name='OM', type='Argument') {
  if (isFALSE(methods::is(object, class)))
     cli::cli_abort('{type} {.var {name}} must be class {.var {class}}')
}

ConvertToList <- function(OM) {
  if (methods::is(OM@Stock, 'stock'))
    OM@Stock <- list(OM@Stock)
  if (methods::is(OM@Fleet, 'fleet'))
    OM@Fleet <- list(list(OM@Fleet))
  OM
}

ConvertFromList <- function(OM) {
  
}

#' @describeIn runMSE Development version of `Simulate`
#' @export
SimulateDEV <- function(OM=NULL, parallel=FALSE, messages='default', nSim=NULL) {

  # ---- Initial Checks and Setup ----
  CheckClass(OM)
  
  OM <- nSimUpdate(OM, nSim, messages) |>
    Populate(messages=messages) |>
    ConvertToList()
  
  OMCheck <- Check(OM)   # TODO OM checks
  
  
  # ---- Unfished Equilibrium ----
  UnfishedSurvival <- list()
  # spawning population; SO = Spawning Output
  UnfishedSurvival_SO <- list()
  
  for (st in 1:nStocks) {
    M_at_Age <- OM@Stock[[st]]@NaturalMortality@MeanAtAge
    PlusGroup <- OM@Stock[[st]]@Ages@PlusGroup
    SpawnTimeFrac <- OM@Stock[[st]]@SRR@SpawnTimeFrac
    
    UnfishedSurvival[[st]] <- CalcSurvival(M_at_Age, PlusGroup)
    if (any(SpawnTimeFrac!=0) && length(SpawnTimeFrac)>0) 
      UnfishedSurvival_SO[[st]] <- CalcSurvival(M_at_Age, PlusGroup, SpawnTimeFrac)
    
  }
  
  
  
  
                        
  
  
  
  
}


