
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
  # TODO keep dimnames in output from C++
  OMListDone <- purrr::map(OMList, \(x) 
                       CalcPopDynamics(x),
                       .progress = list(
                         type = "iterator", 
                         format = "Simulating Historical Fishery {cli::pb_bar} {cli::pb_percent}",
                         clear = TRUE))
  

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



ProjectDEV <- function(Hist, MPs=NULL, parallel = FALSE, silent = FALSE, 
                       options=NULL, output='MSE') {
  
  # class(Hist) # OMList or hist
  
  MPs <- c('CloseArea_1_12', 'CloseArea_6')
  mm <- 1
  # Create MSE object 
  
  # MSE 
  # OM
  # Hist
  # RefPoints

  Sims <- names(OMList) |> as.numeric()
  StockNames <- names(OMList[[1]]$NumberAtAgeArea)
  FleetNames <- names(dimnames(OMList[[1]]$NumberAtAgeArea[[1]]))
  TimeStepsProj <- OMList[[1]]$TimeStepsProj[[1]]
  
  nSim <- length(Sims)
  nStock <- length(StockNames)
  nTS <- length(TimeStepsProj)
  nMP <- length(MPs)
  
  #  sim, stock, MP, time step
  Number <- array(NA, dim=c(nSim, nStock, nMP, nTS), 
                  dimnames=list('Sim'=Sims, 
                                'Stock'=StockNames,
                                'MP'=MPs,
                                'Time Step'=TimeStepsProj)
  )
  
  
  Biomass <- Number
  SBiomass <- Number
  Removal <- ListArraySimAgeTimeMPFleetArea(OM, "All", MPs)  # sim, age, time step, mp, fleet, area
  Retain <- Removal
  
  TimeStepsAll <- OMList[[1]]$TimeSteps[[1]]
  TimeStepsProj <- OMList[[1]]$TimeStepsProj[[1]]
 
  MSEList <- list()
  # MSEList$OM <- OM
  
  OMList = OMListDone
  
  tt <- List2Array(OMListDone[[1]]$RemovalAtAgeArea[[1]], 'Time Step')
  tt[,1,,120]
  
  myremovals = OMListDone[[1]]$RemovalAtAgeArea[[1]]$`2030.92`[,1,] 
  myremovals
  
  OMListDone[[1]]$RemovalAtAgeArea[[1]]$`2030.92`[,1,] 
  OMList[[1]]$RemovalAtAgeArea[[1]]$`2030.92`[,1,] 
  
  r = ProjectMP(OMList, MP=MPs[1])
  
  tt <- purrr:::map(MPs, \(mm) {
    ProjectMP(OMList, MP=mm)
  })
  names(tt) <- MPs
  tt <- ReverseList(tt)
  tt$NumberAtAgeArea$CloseArea_1_12[[1]][1,,60,]
  tt$NumberAtAgeArea$CloseArea_6[[1]][1,,60,]

  
  # return MSElist = return(tt)
  Number <- purrr::map(tt$NumberAtAgeArea, \(mm) {
    purrr::map(mm, \(st) {
      apply(st, c('Sim', 'Time Step'), sum)
    }) |>
      List2Array("Stock")
  }) |>  List2Array("MP") |>
    aperm(c("Sim", "Stock", "MP", "Time Step"))

  dimnames(Number)
  


  ProjectMP <- function(OMList, MP) {
    start <- Sys.time()

    MPList <- tryCatch(
      purrr::map(OMList, CalcPopDynamics, Period="Projection", MP=MP,
                 .progress = list(
                   type = "tasks",
                   format = "Running MP {cli::pb_bar} {cli::pb_percent}",
                   clear = TRUE
                 )
      )
    )
    
    end <- Sys.time() 
    elapsed <- round(as.numeric(difftime(time1 = end, time2 = start, units = "secs")),2)
    cli::cli_alert_success("{.val {MP}} ({elapsed} seconds)")
    
    
    NumberAtAgeArea <- purrr::map(MPList, \(x)
                                  purrr::map(x$NumberAtAgeArea, \(y) 
                                             ArraySubsetTimeStep(y, TimeStepsProj))) |> 
      ReverseList() |>
      purrr::map(List2Array, 'Sim') |>
      purrr::map(\(x) aperm(x, c("Sim", "Age", "Time Step", "Area")))
    
    
    BiomassArea <- purrr::map(MPList, \(x)
                              purrr::map(x$BiomassArea, \(y) 
                                         ArraySubsetTimeStep(y, TimeStepsProj))) |> 
      ReverseList() |>
      purrr::map(List2Array, 'Sim') |>
      purrr::map(\(x) aperm(x, c("Sim", "Time Step", "Area")))
    
    # TODO 
    # add spawning biomass
    # add removals and retained
    
    list(NumberAtAgeArea=NumberAtAgeArea,
         BiomassArea=BiomassArea)
    
  }
  

    
  
    
    
  
  #     aperm(c("Sim", "Stock", "Time Step", "Area"))
  #   
  #   t$`Day octopus`$`1` |> dimnames()
  #   
  # 
  #                         
  #   
  #   Number[,,mm,] <-  purrr::map(MPList, \(x)
  #                                purrr::map(x$BiomassArea, \(y)
  #                                           ArraySubsetTimeStep(y, TimeStepsProj) |>  
  #                                             rowSums() |>
  #                                             array(dimnames=list("Time Step"=TimeStepsProj))
  #                                ) ) |>
  #     ReverseList() |>
  #     purrr::map(List2Array, "Sim") |>
  #     List2Array("Stock") |>
  #     aperm(c("Sim", "Stock", "Time Step"))
  #   
  #   
  #   r |> dimnames()
  #   
  #   MPList[[1]]$BiomassArea[[1]] |> ArraySubsetTimeStep(TimeStepsProj)
  #   r = MPList[[1]]$Length$MeanAtAge$`Day octopus` |> ArraySubsetTimeStep(TimeSteps=TimeStepsProj)
  #   
  #   # TODO add spawn biomass
  #   
  #   # TODO speed up opt D 
  #   
  #   for (st in 1:nStock(OM)) {
  #     
  #     Removal[[st]][,,,mm,,] |> dim()
  #     
  #     purrr::map(MPList, \(x)
  #                x$RemovalAtAgeArea[[st]]
  #                )
  #   }
  #   st = 1
  #   r = List2Array(MPList[[1]]$RemovalAtAgeArea[[st]])
  #   
  #   
  #   MPList[[1]]$BiomassArea[[st]] |> dim()
  #   SBiomass
  # }
  
  
  
  
} 


# TODO Pre-MP function

# TimeStep <- '2026'
# MP <- "CloseArea_6"

# DataList list of Data by Stock
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
  effortTS <- Advice@Effort@Effort
  if (is.null(effortTS)) {
    effortTS <- rep(1, nFleet)
  } 
  if (length(effortTS)!=nFleet)
    cli::cli_abort("`Advice@Effort@Effort` must be length `nFleet`: {.val {nFleet}}")
  
  OMListSim$Effort$Effort[[1]][TSindex,] <-  OMListSim$Effort$Effort[[1]][LastHistindex,] * effortTS
  
  # Update Spatial Closure
  closure <- Advice@Distribution@Closure
  if (is.null(closure)) {
    closure <- rep(1, nAreas)
  } 
  if (length(closure) != nAreas)
    cli::cli_abort("`Advice@Distribution@Closure` must be length `nAreas`: {.val {nAreas}}")
  
  OMListSim$Distribution$Closure[[1]][TSindex,1,] <- closure
  
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















