

SubsetSim <- function(object, Sim=1, drop=FALSE) {
  # this is a piece of black magic that subsets all arrays with a Sim
  # dimension
  # Sim is a numeric vector, can be length > 1
  # If Sim includes values greater then the length of the `Sim` dimension in
  # an array, the last value in that dimension will be returned
  # All Sim dimension should be length 1 or length nsim, so that shouldn't be 
  # a problem, but you've been warned!
  if (isS4(object)) {
    slots <- slotNames(object)
    for (i in seq_along(slots)) {
      obj <- slot(object, slots[i])
      slot(object, slots[i]) <- Recall(obj, Sim)
    }
    if ('nSim' %in% slotNames(object)) 
      object@nSim <- length(Sim)

    return(object)
  }
  
  if (inherits(object, c('StockList',  'StockFleetList', 'FleetList', 'list'))) {
    outlist <- object 
    for (j in seq_along(object)) {
      if (is.null(object[[j]])) {
        outlist[[j]] <- object[[j]]
      } else {
        outlist[[j]] <- Recall(object[[j]], Sim)
      }
      
    }
    if (length(outlist)<1) 
      outlist <- object
    return(outlist)
  }
  
  if (inherits(object, 'array')) {
    if ("Sim" %in% names(dimnames(object))) {
      object <- ArraySubsetSim(object, Sim, drop)
    }
    return(object)
  }
  
  object
}


OptimCatchability <- function(OMList) {
  # OMList <- rlang::duplicate(OMList)
  nsims <- length(OMList)
  
  nStock <- length(OMList[[1]]$Ages)
  nFleet <- dim(OMList[[1]]$FishingMortality$DeadAtAge[[1]])[3]
  
  if (nStock>1)
    cli::cli_abort('Optimizing catchability not currently working for multiple stocks', .internal=TRUE)
  
  if (nFleet>1)
    cli::cli_abort('Optimizing catchability not currently working for multiple fleets', .internal=TRUE)
  
  bounds <- c(1e-03, 15)
 
  st <- 1
  fl <- 1
  
  OMList <- purrr::map(OMList, \(x) {
    
    q1 <- x$Effort$Catchability[[st]][1,fl]
    if (q1>tiny)
      return(x)
    
    doOpt <- optimize(OptCatchability,
                      log(bounds), 
                      x=x,
                      tol=1e-2)
    logQ <- doOpt$minimum
    qval <- exp(doOpt$minimum)
    # TODO add qinc etc
    x$Effort$Catchability[[st]][,fl] <- qval
    x
    
  }, .progress = list(
    type = "iterator", 
    format = "Optimizing catchability (q) for Final Depletion {cli::pb_bar} {cli::pb_percent}",
    clear = TRUE))
  
 
  OMList
}

OptCatchability <- function(logQ, x) {
 
  # TODO update for multiple stocks and fleets
  st <- 1
  DepletionTarget <- x$Depletion$Final[[st]]
  DepletionReference <- x$Depletion$Reference[[st]]
  
  if (length(DepletionTarget)<1)
    cli::cli_abort("`Effort@Catchability` not set for first time step and no value set for `Depletion@Final`")

  x$Effort$Catchability[[st]][] <- exp(logQ)
  TimeStepsAll <- x$TimeSteps[[st]]
  TimeStepsHist <- x$TimeStepsHist[[st]]
  TermInd <- match(max(TimeStepsHist),TimeStepsAll)
  
  PopDynamicsHistorical <- CalcPopDynamics_(x, TimeStepsHist)

 
  
  if (DepletionReference=='B0') {
    Bterminal <- PopDynamicsHistorical$Biomass[[1]][TermInd]
    depRef <- x$B0[[st]][TermInd]  
  } else if (DepletionReference=='SB0') {
    Bterminal <- PopDynamicsHistorical$SBiomass[[1]][TermInd]
    depRef <- x$SB0[[st]][TermInd] 
  } else {
    cli::cli_abort("Currently only accepts`Depletion@Reference = 'B0' or 'SB0'")
  }
  
  sum((Bterminal/depRef - DepletionTarget)^2)
  
}


GetDepletion <- function(object, Reference=NULL) {
  Biomass <- rowSums(object$PopulationList$BiomassArea[[1]] )
  Bterminal <- Biomass[length(Biomass)]
  
  # TODO Unfished B shouldn't be by age 
  B0 <- object$PopulationList$Unfished@Equilibrium@Biomass[[1]] |>
    apply(c("Sim",'TimeStep'), sum)
  Bterminal/B0[1,length(TimeSteps)]
  
    
  

}
