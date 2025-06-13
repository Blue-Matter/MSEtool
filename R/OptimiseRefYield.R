OptimRefYield <- function(ProjSimList, silent=FALSE, .lapply=NULL, parallel=TRUE) {
  
  if (is.null(.lapply))
    .lapply <- define.lapply(silent) 
  
  nStock <- nStock(ProjSimList[[1]]@OM)
  nFleet <- nFleet(ProjSimList[[1]]@OM)
  
  if (nStock>1)
    cli::cli_abort('Optimizing Reference Catch not currently working for multiple stocks', .internal=TRUE)
  
  if (nFleet>1)
    cli::cli_abort('Optimizing Reference Catch not currently working for multiple fleets', .internal=TRUE)
  
  bounds <- c(0.01, 1)
  
  TimeStepsProj <- TimeSteps(ProjSimList[[1]]@OM, 'Projection')
  TimeSteps <- TimeSteps(ProjSimList[[1]]@OM)
  projind <- match(TimeStepsProj,TimeSteps)
  
  st <- 1
  fl <- 1
  
  
  # Retained Catch (Landings)
  # tictoc::tic()
  # ProjSimList <- purrr::imap(ProjSimList, \(ProjSim, idx) {
  # 
  #   doOpt <- optimize(OptRefYield,
  #                     log(bounds),
  #                     ProjSim=ProjSim,
  #                     TimeStepsProj=TimeStepsProj,
  #                     projind=projind,
  #                     tol=1e-2)
  # 
  #   ProjSim@RefPoints@RefYield <- array(-doOpt$objective, dimnames=list(Sim=idx))
  #   ProjSim
  # }, .progress = list(
  #   type = "iterator",
  #   format = "Calculating Reference Yield {cli::pb_bar} {cli::pb_percent}",
  #   clear = TRUE))
  
  # tictoc::toc()
  
  # if (parallel) {
  # 
  #   optRefYieldFunction <- function(ProjSim, bounds, TimeStepsProj, projind) {
  #     doOpt <- optimize(MSEtool:::OptRefYield,
  #                       log(bounds),
  #                       ProjSim=ProjSim,
  #                       TimeStepsProj=TimeStepsProj,
  #                       projind=projind,
  #                       tol=1e-2)
  # 
  #     ProjSim@RefPoints@RefYield <- array(-doOpt$objective)
  #     ProjSim
  #   }
  # 
  #   cli::cli_progress_message("Calculating Reference Yield")
  #   
  #   snowfall::sfExport('bounds', 'TimeStepsProj', 'projind')
  #   
  #   ProjSimList <- .lapply(ProjSimList, optRefYieldFunction, bounds, TimeStepsProj, projind)
  #   cli::cli_progress_done()
  # 
  # } else {
  #   ProjSimList <- purrr::imap(ProjSimList, \(ProjSim, idx) {
  # 
  #     doOpt <- optimize(OptRefYield,
  #                       log(bounds),
  #                       ProjSim=ProjSim,
  #                       TimeStepsProj=TimeStepsProj,
  #                       projind=projind,
  #                       tol=1e-2)
  # 
  #     ProjSim@RefPoints@RefYield <- array(-doOpt$objective, dimnames=list(Sim=idx))
  #     ProjSim
  #   }, .progress = list(
  #     type = "iterator",
  #     format = "Calculating Reference Yield {cli::pb_bar} {cli::pb_percent}",
  #     clear = TRUE))
  # }
  # 
  
  ProjSimList <- purrr::imap(ProjSimList, \(ProjSim, idx) {
    
    doOpt <- optimize(OptRefYield,
                      log(bounds),
                      ProjSim=ProjSim,
                      TimeStepsProj=TimeStepsProj,
                      projind=projind,
                      tol=1e-2)
    
    ProjSim@RefPoints@RefYield <- array(-doOpt$objective, dimnames=list(Sim=idx))
    ProjSim
  }, .progress = list(
    type = "iterator",
    format = "Calculating Reference Yield {cli::pb_bar} {cli::pb_percent}",
    clear = TRUE))

  # Fastest 
  #
  # setup()
  # library(snowfall)
  # # snowfall::sfExport()
  # sfExport("bounds", "TimeStepsProj", "projind")
  # # sfLibrary( "MSEtool", character.only=TRUE )
  # 
  # tictoc::tic()

  # tictoc::toc()
  
  
  # Very slow
  # future::plan(future::multisession, workers = 24)
  # tictoc::tic()
  # HistSimList <- furrr::future_map(ProjSimList, \(ProjSim) {
  #     doOpt <- optimize(MSEtool:::OptRefYield,
  #                       log(bounds), 
  #                       ProjSim=ProjSim,
  #                       TimeStepsProj=TimeStepsProj, 
  #                       projind=projind,
  #                       tol=1e-2)
  #     
  #     ProjSim@RefPoints@RefYield <- array(-doOpt$objective)
  #     ProjSim
  #   })
  # 
  # tictoc::toc()
  
  
 
  ProjSimList
}

OptRefYield <- function(logF, ProjSim, TimeStepsProj, projind) {
  
  # TODO update for multiple stocks and fleets
  st <- 1
  fl <- 1
  
  ProjSim@Effort[st,projind,fl] <- exp(logF) # ProjSim@Effort[st,min(projind)-1,fl] * exp(logEffort)
  ProjSim@OM@Fleet[[st]]@Effort@Catchability[] <- 1
  tictoc::tic()
  
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

