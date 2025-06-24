

#' Calculate Reference Yield
#' 
#' Highest Landings or Removals over the projection period with a constant F 
#' policy
#' 
#' @export
CalcRefLandings <- function(MSE, type=c('Landings', 'Removals')) {
  type <- match.arg(type)
  CheckClass(MSE, c('mse', 'hist'), 'MSE')
  if (inherits(MSE, 'mse')) {
    Hist <- MSE2Hist(MSE)
  } else if (inherits(MSE, 'hist')) {
    Hist <- MSE
    stop('TODO...')
  }
  
  TimeStepsHist <- TimeSteps(MSE@OM, 'Historical')
  TimeStepsProj <- TimeSteps(MSE@OM, 'Projection')
  TimeSteps <- c(TimeStepsHist, TimeStepsProj)
  projind <- match(TimeStepsProj,TimeSteps)
  
  Proj <- ExtendHist(Hist, TimeSteps)
  ProjSimList <- Hist2HistSimList(Proj)
  LastHistTS <- tail(TimeStepsHist,1)
  ProjSimList <- purrr::map(ProjSimList, \(ProjSim) PopulateNumberNext_(ProjSim, LastHistTS))
  
  nStock <- nStock(MSE@OM)
  nFleet <- nFleet(MSE)
  
  # TODO 
  if (nStock>1)
    cli::cli_abort('Optimizing Reference Catch not currently working for multiple stocks', .internal=TRUE)
  
  if (nFleet>1)
    cli::cli_abort('Optimizing Reference Catch not currently working for multiple fleets', .internal=TRUE)
  
  bounds <- c(0.01, 1)
  
  RefYieldList <- purrr::imap(ProjSimList, \(ProjSim, idx) {
    doOpt <- optimize(OptRefLandings,
                      log(bounds),
                      ProjSim=ProjSim,
                      TimeStepsProj=TimeStepsProj,
                      projind=projind,
                      type=type,
                      tol=1e-2)
    
    array(-doOpt$objective, dimnames=list(Sim=idx))
  }, .progress = list(
    type = "iterator",
    caller = environment(),
    format = "Calculating Reference {type} {cli::pb_bar} {cli::pb_percent}",
    clear = TRUE))
  
  RefYield <- List2Array(RefYieldList) 
  dimnames(RefYield) <- list(Stock=StockNames(MSE@OM),
                             Sim=1:MSE@OM@nSim)
  
  RefYield <- RefYield |>  aperm(c("Sim", 'Stock'))
  
  
  if (type=='Landings') {
    MSE@RefPoints@RefLandings <- RefYield
  } else {
    MSE@RefPoints@RefRemovals <- RefYield
  }
  MSE 
}

#' @describeIn CalcRefLandings Calculate Reference Removals
#' @export
CalcRefRemovals <- function(MSE, type=c('Landings', 'Removals')) {
  CalcRefLandings(MSE, type)
}

OptRefLandings <- function(logF, ProjSim, TimeStepsProj, projind, type=c('Landings', 'Removals')) {
   
  type <- match.arg(type)
  # TODO update for multiple stocks and fleets
  st <- 1
  fl <- 1
  
  ProjSim@Effort[st,projind,fl] <- exp(logF) # ProjSim@Effort[st,min(projind)-1,fl] * exp(logEffort)
  ProjSim@OM@Fleet[[st]]@Effort@Catchability[] <- 1

  PopDynamicsProject <- SimulateDynamics_(ProjSim, TimeStepsProj)
  
  if (type=='Landings') {
    Yield <- PopDynamicsProject@Landings[[st]] |> List2Array("TimeStep")
  } else {
    Yield <- PopDynamicsProject@Removals[[st]] |> List2Array("TimeStep")
  }
  

  lastnTS <- ProjSim@OM@Control$RefYield$lastnTS
  if (is.null(lastnTS))
    lastnTS <- 5
  
  dd <- dim(Yield)
  nTS <- dd[4]
  if (lastnTS >nTS)
    lastnTS <- nTS
  
  TSmean <- (nTS-lastnTS+1):nTS
  
  -mean(apply(Yield[,,,TSmean,drop=FALSE], c('TimeStep'), sum))
  
}

