aperm <- function(a, perm, ...) {
  if (is.null(a))
    return(a)
  base::aperm(a, perm, ...)
}

# TODO check where

#' @describeIn runMSE Development version of `Simulate`
#' @export
SimulateDEV <- function(OM=NULL, 
                        parallel=FALSE, 
                        silent=FALSE,
                        ...) {
  
  CheckClass(OM)
  
  # if (isTRUE(silent)) 
  #   messages <- FALSE

  # ---- Initial Checks and Setup ----
  chk <- Check(OM) # TODO OM checks
  
  OM <- StartUp(OM) 
  

  # ---- Make Hist Object ----
  Hist <- Hist(OM, silent)

  # ---- Calculate Equilibrium Unfished ----
  Hist@Unfished@Equilibrium <- CalcEquilibriumUnfished(OM)
  
  # ---- Calculate Number-at-Age for Initial TimeStep ----
  Hist@Number[,,,1,] <- CalcInitialTimeStep(Hist)
  

  Hist@OM@Fleet$Female@Effort@Effort
  
  Hist@OM@Fleet$Female@Distribution@Effort
  
  # ---- Calculate Unfished Equilibrium and Dynamic ----
  Unfished <- CalcUnfished(OM)

 

  # ---- Convert OM to OMList -----
  OMList <- MakeOMList(OM)
  
  # ---- Calculate Unfished Equilibrium and Dynamic ----
  if (CheckSimsUnique(OMList, ignore=c('Sim', 'RecDevs'))) {
    # identical across Sims
    temp <- CalcEquilibriumUnfished(OMList[[1]])
    for (i in seq_along(OMList)) {
      OMList[[i]]$N0atAge <- temp$N0atAge
      OMList[[i]]$B0 <- temp$B0
      OMList[[i]]$SB0 <- temp$SB0
      OMList[[i]]$SP0 <- temp$SP0
    }
  } else {
    OMList <- purrr::map(OMList, CalcEquilibriumUnfished)  
  }
  Unfished <- CalcUnfished(OMList)
    
  # ---- Calculate Reference Points ----
  # TODO speed up - see below
  RefPoints <- CalcRefPoints(OM, Unfished)
  
  # ---- Calculate Initial TimeStep ----
  OMList <- purrr::map(OMList, CalcInitialTimeStep) 
  
  
  
  
 t = ArrayExpand(OM@Stock$Female@NaturalMortality@MeanAtAge, OM@nSim, 50, TimeSteps)
  
 t = ArrayExpand(OM@Stock$Female@Weight@MeanAtAge, OM@nSim, 30, TimeSteps)
 
 t <- lapply(OM@Stock, GetMeanAtAge, TimeSteps=TimeSteps)
 dimnames(t$Female)
 
 # GetMeanAtAge(OM@Stock, TimeSteps)$Male |> dimnGetMeanAtAge(OM@Stock, TimeSteps)$Male |> dimnGetMeanAtAge(OM@Stock, TimeSteps)$Male |> dimnames()
 
 r <- lapply(OM@Stock, 'Weight')
 r$Female@MeanAtAge
 r$Male@MeanAtAge
 class(r$Female)

 

  FSearch <- OMListSim$CurvesFSearch
  
 
  
  # Calculate Number Per Recruit for a Given F 
  OMListSim <- OMList$`1`
  
  
  TimeSteps <- OMListSim$TimeSteps
  
  dd <- dim(OMListSim$SelectivityMeanAtAge[[1]])
  nAge <- dd[1]
  nTS <- dd[2]
  nFleet <- dd[3]
  stocks <- names(OMListSim$Length$MeanAtAge)
  nstock <- length(stocks)
  
  nFs <- length(FSearch)
  
  NPR <- array(NA, dim=c(nStock, nTS, nFs), dimnames = list(Stock=stocks, TimeStep=TimeSteps,
                                                            F=FSearch))
  YPR <- SPR_0 <- SPR_F <- SPR <- NPR 
   
  

  
  
  # Distribute F over fleets
  RelativeFbyFleet <- OMListSim$Catchability * OMListSim$Effort # nStock, nTS, nFleet
  TotalFbyFleet <- apply(RelativeFbyFleet, c('Stock', 'TimeStep'), sum) 
  RelativeFbyFleet <- ArrayDivide(RelativeFbyFleet,AddDimension(TotalFbyFleet, 'Fleet')) |>
    Array2List(1)
  
  # UP TO HERE 
  # TODO  - check if faster to loop over Fs 

  tictoc::tic()
  for (st in 1:nStock) {
    Ind <- expand.grid(F=1:nFs, Age=1:nAge, TimeStep=1:nTS, Fleet=1:nFleet) |> as.matrix()
    
    dd <- dim(OMListSim$SelectivityMeanAtAge[[st]])
    nAge <- dd[1]
    apicalFs <- array(FSearch, dim=c(nFs, nAge, nTS, nFleet))
    
    FInteract <- array(NA, dim=c(nFs, nAge, nTS, nFleet))
    FDead <- FRetain <- FDiscard <- FDeadDiscard <- FInteract
    
    
    FInteract[Ind] <- RelativeFbyFleet[[st]][Ind[,3:4]] * apicalFs[Ind] * 
      OMListSim$SelectivityMeanAtAge[[st]][Ind[,2:4]]  # Age, TimeStep, Fleet
    FRetain[Ind] <- FInteract[Ind] * OMListSim$RetentionMeanAtAge[[st]][Ind[,2:4]] # Age, TimeStep, Fleet
    FDiscard[] <- FInteract - FRetain
    FDeadDiscard[] <- FDiscard * OMListSim$DiscardMortalityMeanAtAge[[st]][Ind[,2:4]]
    FDead[] <- FDeadDiscard + FRetain
    apicalFCurr <- apply(FDead, c(1:3), sum) |> apply(c(1,3), max)
    
  }
  tictoc::toc()
  
  # Update apical F to account for discard mortality 
  # to make sure apicalF = apicalF for Dead rather than Interact
  
  
  
  
  
  st <- 1
  
  
  RelF <- RelF |> 
    AddDimension('Age', 0) |> 
    ExpandAges(length(OMListSim$Ages[[st]]@Classes)) |>
    aperm(c('Stock', 'Age', 'TimeStep', 'Fleet'))
  
  totalF <- apply(RelF, c('Stock', 'TimeStep', 'Age'), sum) 
  totalF <- replicate(nFleet, totalF)
  names(dimnames(totalF))[4] <- 'Fleet'
  dimnames(totalF)[['Fleet']] <- dimnames(RelF)[['Fleet']] 
  totalF <- totalF |> aperm(c('Stock', 'Age', 'TimeStep', 'Fleet'))

  RelF <- RelF/totalF
  RelF <- Array2List(RelF, 1)
 
  
  FInteract <- array(NA, dim=c(nAge, nTS, nFleet))
  FRetain <- FDeadDiscard <- FDead<- FInteract
  
  tstApicalF <- apicalF
  FInteract <- RelF[[st]] * tstApicalF * OMListSim$SelectivityMeanAtAge[[st]] # Age, TimeStep, Fleet
  FRetain <- FInteract * OMListSim$RetentionMeanAtAge[[st]] # Age, TimeStep, Fleet
  FDiscard <- FInteract - FRetain
  FDeadDiscard <- FDiscard * OMListSim$DiscardMortalityMeanAtAge[[st]]
  FDead <- FDeadDiscard + FRetain
  apicalFCurr <- apply(FDead, c('TimeStep', 'Age'), sum) |> apply(c('TimeStep'), max)
  tstApicalF[Ind] <- apicalF[Ind] * apicalF[Ind]/apicalFCurr[Ind[,2]]
  
  FInteract <- RelF[[st]] * tstApicalF * OMListSim$SelectivityMeanAtAge[[st]] # Age, TimeStep, Fleet
  FRetain <- FInteract * OMListSim$RetentionMeanAtAge[[st]] # Age, TimeStep, Fleet
  FDiscard <- FInteract - FRetain
  FDeadDiscard <- FDiscard * OMListSim$DiscardMortalityMeanAtAge[[st]]
  FDead <- FDeadDiscard + FRetain
  
  # Number
  NaturalMortalityAtAge <- OMListSim$NaturalMortalityMeanAtAge[[st]]
  FishingMortalityAtAge <- apply(FDead, c('Age', 'TimeStep'), sum)
  PlusGroup <- OMListSim$Ages[[st]]@PlusGroup
  SpawnTimeFrac <- OMListSim$SpawnTimeFrac[st]
  Semelparous <- OMListSim$MaturitySemelparous[[st]]
  
  # Number per Recruit
  NPR <- CalcSurvival(NaturalMortalityAtAge,
                    FishingMortalityAtAge,
                    PlusGroup,
                    SpawnTimeFrac=NULL,
                    Semelparous)
  if (SpawnTimeFrac==0) {
    NPR_SP <- NPR
  } else {
    NPR_SP <- CalcSurvival(NaturalMortalityAtAge,
                           FishingMortalityAtAge,
                           PlusGroup,
                           SpawnTimeFrac,
                           Semelparous)
  }
 
  # Yield per Recruit
  ZDeadTotal <- NaturalMortalityAtAge + FishingMortalityAtAge
  ZDeadTotalFleet <- AddDimension(ZDeadTotal, 'Fleet') 
  FishingDead <- ArrayDivide(FDead, ZDeadTotalFleet)
  FishingRetain <- ArrayDivide(FRetain, ZDeadTotalFleet)
  NDead <- ArrayMultiply(NPR, (1-exp(-ZDeadTotal)))

  Removal <- ArrayMultiply(FishingDead, AddDimension(NDead, 'Fleet')) * OMListSim$FleetWeightAtAge[[st]]
  Retain <- ArrayMultiply(FishingRetain, AddDimension(NDead, 'Fleet')) * OMListSim$FleetWeightAtAge[[st]]
  
  RemovalPerRecruit <- apply(Removal, 'TimeStep', sum)
  RetainPerRecruit <- apply(Retain, 'TimeStep', sum)
  
  # Spawning per Recruit 
  SPR_F <- apply(NPR_SP * OMListSim$FecundityMeanAtAge[[st]], 'TimeStep', sum)
  SPR_0 <- OMListSim$SP0[st]/OMListSim$R0[st]
  SPR <- SPR_F/SPR_0
  
  
  
  
  
   # Age, TimeStep, Fleet
  
  t = CalcCatch_(OMListSim, OMListSim$TimeSteps)
  t$Effort |> dimnames()
  

  
  NaturalMortalityAtAge <- OMListSim$NaturalMortalityMeanAtAge
  PlusGroup <- lapply(OMListSim$Ages, slot, 'PlusGroup')
  SpawnTimeFrac <- as.list(OMListSim$SpawnTimeFrac)
  Semelparous <- OMListSim$MaturitySemelparous
  
  SelectivityAtAge <- OMListSim$SelectivityMeanAtAge
  RetentionAtAge <- OMListSim$RetentionMeanAtAge
  DiscardMortalityAtAge <- OMListSim$DiscardMortalityMeanAtAge
  
  
  F <- FSearch[i]
  
  FishingMortalityAtAge <- F * SelectivityAtAge[[st]]
  
  
  
  Survival <- purrr::pmap(list(NaturalMortalityAtAge,
                                       PlusGroup,
                                       SpawnTimeFrac,
                                       Semelparous),
                                  CalcSurvival)
  
  CalcSurvival()
  
  
  
  RefPoints <- new('refpoints')
  

 
  
  CalcSPR0 <- function(OMListSim) {
    
  }
  
  
  RefPoints@Curves
  
  

  
 
  # - reference point - update
  # - SimulateFisheryDynamics_ 
  #   - add option to calculate Catch
  #   - check that all objects in OMList are updated and returned
  
  


  # ---- Optimize for Final Depletion ----
  OMList <- OptimizeCatchability(OMList)

  # ---- Historical Population Dynamics ----
  HistTimeSteps <- TimeSteps(OM, 'Historical')
  
  # Calculate Population Dynamics (Fishing Mortality & Number by Area)
  OMList <- purrr::map(OMList, \(x) 
                       SimulateFisheryDynamics_(x, HistTimeSteps, MP=NULL, CalcCatch = 0),
                       .progress = list(
                         type = "iterator", 
                         format = "Simulating Historical Fishery {cli::pb_bar} {cli::pb_percent}",
                         clear = TRUE))
  
  
  OMList[[1]]$RemovalBiomassAtAge$Albacore[,2,]
  
  # Calculate Removals, Landings
  OMList <- purrr::map(OMList, \(x) 
                       CalcCatch_(x, HistTimeSteps))
  
  # Calculate Aggregate F 
  OMList <- purrr::map(OMList, \(x) 
                           CalcAggregateF_(x, HistTimeSteps))
  
  # Condition Observation Model on Fishery Data
  
  # Generate Simulated Fishery Data
  
  # OMList <- MakeOMList(OM, Unfished
  
  OMListSim <- OMList$`1`
  

  
  
  
  
  

  
  OMListSim$TimeSteps$Albacore
  t = CalcFisheryDynamics(OMListSim)
  
# 
  CheckClass
  
  # TODO -
  # fix catches, Fs, etc over time 
  # not updating
  ts <- 2
  OMList$`1`$EffortArea$Albacore[1,,]
  
  OMList[[1]]$FDeadAtAgeArea$Albacore[[ts]] 
  
  OMList[[1]]$FDeadAtAge$Albacore[,ts,] 
  
  
  OMList[[1]]$RemovalAtAgeArea$Albacore[[ts]]
  

  
  
  OMList <- purrr::map(OMList, AddDimNamesOMListSim)
  
  
  # ---- Calculate Reference Catch ----
  OMList <- OptimRefCatch(OMList) 
  
  OMList$`1`$RefCatch
  


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
         )
)

#' @export
Advice <- function(DataList=NULL) {
  # TODO - populate selectivity model parameters etc
  new('advice')
}



# MPs=c('Open', 
#       'Closed_1', 
#       'Closed_2',
#       'Closed_3',
#       'Closed_6',
#       'Closed_12')
# OMList <- OMListHist

#' @export
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
      apply(st, c('Sim', 'TimeStep'), sum)
    }) |>
      List2Array("Stock")
  }) |>  List2Array("MP") |>
    aperm(c("Sim", "Stock", "MP", "TimeStep"))
  
  Biomass <- List2Array(MSEList$Biomass, 'MP') |>
    aperm(c("Sim", "Stock", "MP", "TimeStep"))
  
  SBiomass <- List2Array(MSEList$SBiomass, 'MP') |>
    aperm(c("Sim", "Stock", "MP", "TimeStep"))
  
  SProduction <- List2Array(MSEList$SProduction, 'MP') |>
    aperm(c("Sim", "Stock", "MP", "TimeStep"))
  
  Removal <- purrr::map(MSEList$Removal, \(mm) {
    purrr::map(mm, \(st) {
      apply(st, c('Sim', 'TimeStep', 'Fleet'), sum)
    }) |>
      List2Array("Stock")
  }) |>  List2Array("MP") |>
    aperm(c("Sim", "Stock", "MP", "TimeStep", "Fleet"))
  
  Retain <- purrr::map(MSEList$Retain, \(mm) {
    purrr::map(mm, \(st) {
      apply(st, c('Sim', 'TimeStep', 'Fleet'), sum)
    }) |>
      List2Array("Stock")
  }) |>  List2Array("MP") |>
    aperm(c("Sim", "Stock", "MP", "TimeStep", "Fleet"))
 
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
ApplyMP <- function(OMListSim, Data=NULL, MP=NULL, TimeStep) {
  if (is.null(MP))
    return(OMListSim)
  
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
  Dims <- dim(OMListSim$NumberAtAgeArea[[1]])
  nTS <- Dims[2]
  nAreas <- Dims[3]
  nFleet <- dim(OMListSim$VBiomassArea[[1]])[2]
  
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
















