OptimRefYield <- function(ProjSimList) {
  
  nStock <- nStock(ProjSimList[[1]]@OM)
  nFleet <- nFleet(ProjSimList[[1]]@OM)
  
  if (nStock>1)
    cli::cli_abort('Optimizing Reference Catch not currently working for multiple stocks', .internal=TRUE)
  
  if (nFleet>1)
    cli::cli_abort('Optimizing Reference Catch not currently working for multiple fleets', .internal=TRUE)
  
  bounds <- c(0.1, 15)
  
  st <- 1
  fl <- 1
  # Retained Catch (Landings)
  ProjSimList <- purrr::imap(ProjSimList, \(ProjSim, idx) {
    
    doOpt <- optimize(OptRefYield,
                      log(bounds), 
                      ProjSim=ProjSim,
                      tol=1e-2)
    
    ProjSim@RefPoints@RefYield <- array(-doOpt$objective, dimnames=list(Sim=idx))
    ProjSim
  }, .progress = list(
    type = "iterator", 
    format = "Calculating Reference Yield {cli::pb_bar} {cli::pb_percent}",
    clear = TRUE))
 
  ProjSimList
}


OptRefYield <- function(logEffort, ProjSim) {
  
  # TODO update for multiple stocks and fleets
  st <- 1
  fl <- 1
  
  TimeStepsProj <- TimeSteps(ProjSim@OM, 'Projection')
  TimeSteps <- TimeSteps(ProjSim@OM)
  projind <- match(TimeStepsProj,TimeSteps)
  
  ProjSim@Effort[st,projind,fl] <- ProjSim@Effort[st,min(projind)-1,fl] * exp(logEffort)
  PopDynamicsProject <- SimulateDynamics_(ProjSim, TimeStepsProj)
  
  Landings <- PopDynamicsProject@Landings[[st]] |> List2Array("TimeStep")

  lastnTS <- ProjSim@OM@Control$RefYield$lastnTS
  if (is.null(lastnTS))
    lastnTS <- 5
  
  dd <- dim(Landings)
  nTS <- dd[4]
  if (lastnTS >nTS)
    lastnTS <- nTS
  
  TSmean <- (nTS-lastnTS+1):nTS
  -mean(apply(Landings[,,,TSmean,drop=FALSE], c('TimeStep'), sum))
}
