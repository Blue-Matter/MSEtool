
#' @describeIn runMSE Development version of `Simulate`
#' @export
SimulateDEV <- function(OM=NULL, 
                        messages='default',
                        parallel=FALSE, 
                        silent=FALSE,
                        ...) {
  
  MSEtool:::CheckClass(OM)
  
  if (isTRUE(silent)) 
    messages <- FALSE

  # ---- Initial Checks and Setup ----
  chk <- Check(OM) # TODO OM checks
  
  OM <- MSEtool:::StartUp(OM, messages) 
  
  # ---- Calculate Equilbrium Unfished Dynamics ----
  Unfished <- CalcEquilibriumUnfished(OM)
  
  # ---- Make OM List ----
  OMList <- MakeOMList(OM, Unfished)
  
  # ---- Calculate Dynamic Unfished ----
  # TODO 
  # update Unfished: create array sizes on initization
  # Create from OMList 
  
  # CalcDynamicUnfished <- function(OMList, Unfished) {
  #   
  #   OMListUnfished <- purrr::map(OMList, \(x, idx, Unfished=Unfished) {
  #     x$Effort$Catchability <- purrr::map(x$Effort$Catchability , \(y) {
  #       y[] = tiny
  #       y
  #     })
  #     unfished <- CalcPopDynamics(x, Period="All")  
  #     
  #     
  #   },
  #   .progress = list(
  #     type = "iterator", 
  #     format = "Calculating Unfished {cli::pb_bar} {cli::pb_percent}",
  #     clear = TRUE))
  #   
  #   OMListUnfished[[1]]$NumberAtAgeArea[[1]] |> dim()
  #   
  #   Unfished@Dynamic@Number |> dim()
  #   
  # } 
  

  # ---- Calculate Reference Points ----
  # TODO speed up
  # RefPoints <- CalcRefPoints(OM, Unfished)

  # ---- Optimize for Final Depletion ----
  OMList <- OptimCatchability(OMList)
  
  
  # ---- Historical Population Dynamics ----
  OMListDone <- purrr::map(OMList, \(x) 
                       CalcPopDynamics(x),
                       .progress = list(
                         type = "iterator", 
                         format = "Simulating Historical Fishery {cli::pb_bar} {cli::pb_percent}",
                         clear = TRUE))
  

  OMListDone <- purrr::map(OMListDone, AddDimNamesOMListSim)
  
  # ---- Calculate Reference Catch ----
  OMListDone <- OptimRefCatch(OMListDone) 
  


  # TODO
  # - make MICE Case Study and test 
  # - make data 
  # - make obs
  # - make imp
  
  # ---- Condition Observation Object on Real Fishery Data ----
  
  # ---- Simulate Fishery Data ----
  
  # ---- Return `hist` Object ----
  
  # make Hist object
  OMListDone
}


setClass('advice',
         slots=c(Removal='numeric',
                 Retain='numeric',
                 Effort='effort',
                 Distribution='distribution',
                 Selectivity='selectivity',
                 Retention='retention',
                 DiscardMortality='discardmortality',
                 Misc='list'
         ),
         contains='Created_ModifiedClass'
)

Advice <- function(DataList=NULL) {
  # TODO - populate selectivity model parameters etc
  new('advice')
}



ProjectDEV <- function(OMList, MPs=NULL, parallel = FALSE, silent = FALSE, 
                       options=NULL, output=c('MSE', 'MSEList')) {
  
  output <- match.arg(output)
  
  # class(Hist) # OMList or hist
  
  OMList <- rlang::duplicate(OMList)
  
  MSEList <- purrr:::map(MPs, \(mm) {
    ProjectMP(OMList, MP=mm)
  })

  names(MSEList) <- MPs
  MSEList <- ReverseList(MSEList)
  class(MSEList) <- 'MSEList'
  # TODO - add Hist, Unfished, Ref Points, etc 
  if (output == 'MSEList')
    return(MSEList)
  
  # Create MSE object 
  # MSE 
  # OM
  # Hist
  # RefPoints
  
  MSEList2MSE(MSEList)
}


# Convert MSEList object to `mse` object
MSEList2MSE <- function(MSEList) {
  
  Number <- purrr::map(MSEList$NumberAtAgeArea, \(mm) {
    purrr::map(mm, \(st) {
      apply(st, c('Sim', 'Time Step'), sum)
    }) |>
      List2Array("Stock")
  }) |>  List2Array("MP") |>
    aperm(c("Sim", "Stock", "MP", "Time Step"))
  
  Biomass <- List2Array(MSEList$Biomass, 'MP') |>
    aperm(c("Sim", "Stock", "MP", "Time Step"))
  
  SBiomass <- List2Array(MSEList$SBiomass, 'MP') |>
    aperm(c("Sim", "Stock", "MP", "Time Step"))
  
  SProduction <- List2Array(MSEList$SProduction, 'MP') |>
    aperm(c("Sim", "Stock", "MP", "Time Step"))
  
  Removal <- purrr::map(MSEList$Removal, \(mm) {
    purrr::map(mm, \(st) {
      apply(st, c('Sim', 'Time Step', 'Fleet'), sum)
    }) |>
      List2Array("Stock")
  }) |>  List2Array("MP") |>
    aperm(c("Sim", "Stock", "MP", "Time Step", "Fleet"))
  
  Retain <- purrr::map(MSEList$Retain, \(mm) {
    purrr::map(mm, \(st) {
      apply(st, c('Sim', 'Time Step', 'Fleet'), sum)
    }) |>
      List2Array("Stock")
  }) |>  List2Array("MP") |>
    aperm(c("Sim", "Stock", "MP", "Time Step", "Fleet"))
 
  list(Number=Number,
       Biomass=Biomass,
       SBiomass=SBiomass,
       SProduction=SProduction,
       Removal=Removal,
       Retain=Retain) 
}
  
# TODO Pre-MP function

# TimeStep <- '2026'
# MP <- "CloseArea_6"

# DataList list of Data by Stock
# TODO multi-stock
# TODO multi-fleet
# TODO data
ApplyMP <- function(OMListSim, Data=NULL, MP, TimeStep) {
  
  TimeStepsAll <- OMListSim$TimeSteps[[1]]
  Data <- list()
  Data$TimeStepCurrent <- as.numeric(TimeStep)
  Data$TimeStepLastHist <- max(OMListSim$TimeStepsHist[[1]])
  ind <- which(TimeStepsAll==Data$TimeStepCurrent)
  
  Data$TimeSteps <- TimeStepsAll[1:(ind-1)]

  
  Dims <- dim(OMListSim$NumberAtAgeArea[[1]])
  nTS <- Dims[2]
  nAreas <- Dims[3]
  nFleet <- dim(OMListSim$VBiomassArea[[1]])[2]
  
  LastHistindex <-  match(Data$TimeStepLastHist, TimeStepsAll)
  TSindex <- match(TimeStep, TimeStepsAll)
  nTS <- length(TimeStepsAll)
  
  # Apply the MP
  intervalTS <- TRUE
  if (intervalTS) {
    MPfun <- getMP(MP)
    Advice <- tryCatch(MPfun(Data))  
  } else {
    Advice <- Advice()
  }
  
  # Update Effort 
  OMListSim <- UpdateEffort(OMListSim, Advice, TSindex)
  
  # Update Distribution (Spatial Closures)
  OMListSim <- UpdateDistribution(OMListSim, Advice, TSindex)
  
  # Update Selectivity 
  if (!EmptyObject(Advice@Selectivity)) {
    cli::cli_abort("Selectivity Management not done")
  }
  
  # Update Retention 
  if (!EmptyObject(Advice@Retention)) {
    cli::cli_abort("Retention Management not done")
  }
  
  # Calculate Effort from Removals or Retain
  
  # Apply Effort constraint if applicable 
  
  OMListSim
}

UpdateEffort <- function(OMListSim, Advice, TSindex) {
  LastHistindex <- length(OMListSim$TimeStepsHist[[1]])
  
  effortTS <- Advice@Effort@Effort
  
  if (!is.null(effortTS)) {
    if (length(effortTS)!=nFleet)
      cli::cli_abort("`Advice@Effort@Effort` must be length `nFleet`: {.val {nFleet}}")
    OMListSim$Effort$Effort[[1]][TSindex,] <- OMListSim$Effort$Effort[[1]][LastHistindex,] * effortTS
    
  } else {
    # effort same as last time step
    OMListSim$Effort$Effort[[1]][TSindex,] <- OMListSim$Effort$Effort[[1]][TSindex-1,]
  }
  OMListSim
}

UpdateDistribution <- function(OMListSim, Advice, TSindex) {
  LastHistindex <- length(OMListSim$TimeStepsHist[[1]])
  Dims <- dim(OMListSim$NumberAtAgeArea[[1]])
  nTS <- Dims[2]
  nAreas <- Dims[3]
  nFleet <- dim(OMListSim$VBiomassArea[[1]])[2]
  
  closure <- Advice@Distribution@Closure
  
  if (!is.null(closure)) {
    if (length(closure) != nAreas)
      cli::cli_abort("`Advice@Distribution@Closure` must be length `nAreas`: {.val {nAreas}}")
    if (nFleet>1)
      stop('Closure multi fleet not done!')
    OMListSim$Distribution$Closure[[1]][TSindex,1,] <- closure
  } else {
    # same as last time step
    OMListSim$Distribution$Closure[[1]][TSindex,,] <- OMListSim$Distribution$Closure[[1]][TSindex-1,,]
  }
  OMListSim
}
















