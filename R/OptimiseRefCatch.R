OptimRefCatch <- function(HistSimList) {
  
  nStock <- nStock(HistSimList[[1]]@OM)
  nFleet <- nFleet(HistSimList[[1]]@OM)
  
  if (nStock>1)
    cli::cli_abort('Optimizing Reference Catch not currently working for multiple stocks', .internal=TRUE)
  
  if (nFleet>1)
    cli::cli_abort('Optimizing Reference Catch not currently working for multiple fleets', .internal=TRUE)
  
  bounds <- c(0.1, 15)
  
  st <- 1
  fl <- 1
  # Retained Catch
  OMList <- purrr::map(OMList, \(x) {
    
    doOpt <- optimize(OptRefCatch,
                      log(bounds), 
                      x=x,
                      tol=1e-2)
    
    x$RefCatch <- -doOpt$objective
    x
  }, .progress = list(
    type = "iterator", 
    format = "Calculating Reference Yield {cli::pb_bar} {cli::pb_percent}",
    clear = TRUE))
  OMList

}


OptRefCatch <- function(logEffort, x, lastnTS=12) {
  
  # TODO update for multiple stocks and fleets
  st <- 1
  fl <- 1
  TimeSteps <- x$TimeSteps[[st]]
  TimeStepsProj <- x$TimeStepsProj[[st]]
  
  projind <- match(TimeStepsProj, TimeSteps)
  x$Effort$Effort[[st]][projind,fl] <-  x$Effort$Effort[[st]][max(projind)-1,fl] * exp(logEffort)
  
  PopDynamicsProject <- CalcPopDynamics_(x, TimeStepsProj)

  retained <- PopDynamicsProject$RetainBiomassAtAge[[st]] |> List2Array("TimeStep")
  
  dd <- dim(retained)
  nTS <- dd[2]
  if (lastnTS >nTS)
    lastnTS <- nTS
  
  TSmean <- (nTS-lastnTS+1):nTS
  -mean(apply(retained[,TSmean, ,drop=FALSE], 2, sum))
}
