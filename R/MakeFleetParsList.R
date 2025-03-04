
MakeFleetList <- function(OM, Period='Historical') {
 
  List <- list()
  List$FishingMortality <- MakeFishingMortalityList(OM, Period)
  List$DiscardMortality <- MakeFleetSlotList(OM, 'DiscardMortality', Period=Period)
  List$Effort <- MakeEffortList(OM, Period)
  List$Selectivity <- MakeFleetSlotList(OM, 'Selectivity', Period)
  List$Retention <- MakeFleetSlotList(OM, 'Retention', Period)
  List$Distribution <-  MakeDistributionList(OM, Period)

  ## Arrays to be filled
  List$EffortArea <- ArraySimStockAgeTimeFleetArea(OM, Period) |>
    DropDimension('Age', warn=FALSE) 
  
  List$DensityArea <- List$EffortArea 
  List$VBiomassArea <- List$EffortArea 
  
  
  List$FDeadAtAgeArea <-  ArraySimStockAgeTimeFleetArea(OM, Period)
  List$FRetainAtAgeArea <- List$FDeadAtAgeArea
  
  List$RemovalAtAgeArea <- List$FDeadAtAgeArea
  List$RetainAtAgeArea <- List$FDeadAtAgeArea
  
  
  List$RemovalNumberAtAge <- ArraySimStockAgeTimeFleet(OM, Period)
  List$RetainNumberAtAge <- List$RemovalNumberAtAge
  
  List$RemovalBiomassAtAge <- List$RemovalNumberAtAge 
  List$RetainBiomassAtAge <- List$RemovalNumberAtAge
  
  List$FleetWeightAtAge <- MakeFleetWeightList(OM)
  List
}


MakeFleetSlotList <- function(OM, slot='Selectivity', Period='Historical',
                              TimeSteps=NULL) {
  
  sNames <- slotNames(slot(OM@Fleet[[1]][[1]], slot))
  
  meta <- GetMetaData(OM, Period, TimeSteps)
  nSim <- meta$nSim
  nAges <- meta$nAges
  TimeSteps <- meta$TimeSteps
  nStock <- length(meta$StockNames)
  nFleet <- length(meta$FleetNames)
  nArea <- meta$nAreas
  
  
  List <- list()
  
  if ('Pars' %in% sNames) 
    List[['Pars']] <- purrr::map(OM@Fleet, \(x)
                                               purrr::map(x, \(y) 
                                               y|> 
                                                 slot(slot) |>
                                                 slot('Pars'))
    )
  
  if ('Model' %in% sNames) 
    List[['Model']] <- purrr::map(OM@Fleet, \(x)
                                                purrr::map(x, \(y) 
                                                           y|> 
                                                             slot(slot) |>
                                                             slot('Model'))
    )
  
  if ('MeanAtAge' %in% sNames) {
    fun <- get(paste0('Get', slot, 'AtAge'))
    List[['MeanAtAge']] <- purrr::map(OM@Fleet, \(x) fun(x, TimeSteps)) |>
      List2Array('Stock') |>
      aperm(c('Sim', 'Stock', 'Age', 'Time Step', 'Fleet')) |>
      ArrayExpand(nSim, nAges, TimeSteps) |>
      ReplaceTiny(1) 
  }
  
  if ('MeanAtLength' %in% sNames) {
    fun <- get(paste0('Get', slot, 'AtLength'))
    List[['MeanAtLength']] <- purrr::map(OM@Fleet, \(x) fun(x, TimeSteps)) |>
      List2Array('Stock') |>
      aperm(c('Sim', 'Stock', 'Class', 'Time Step', 'Fleet')) |>
      ArrayExpand(nSim, nAges, TimeSteps) 
  }
  
  
  if ('Classes' %in% sNames)
    List[['Classes']] <- purrr::map(OM@Fleet, \(x)
                                                  purrr::map(x, \(y) 
                                                             y|> 
                                                               slot(slot) |>
                                                               slot('Classes')) 
    ) 
 
  if ('Misc' %in% sNames)
    List[['Misc']] <- purrr::map(OM@Fleet, \(x)
                                               purrr::map(x, \(y) 
                                                          y|> 
                                                            slot(slot) |>
                                                            slot('Misc'))
    )
  List
}



MakeFishingMortalityList <- function(OM, Period='Historical', TimeSteps=NULL) {
  
  meta <- GetMetaData(OM, Period, TimeSteps)
  nSim <- meta$nSim
  nAges <- meta$nAges
  TimeSteps <- meta$TimeSteps
  nStock <- length(meta$StockNames)
  nFleet <- length(meta$FleetNames)
  nArea <- meta$nAreas
  
  List <- list()
  
  List$ApicalF <- ArraySimStockAgeTimeFleet(OM, Period) |>
    DropDimension('Age', warn=FALSE)
  
  List$DeadAtAge <- ArraySimStockAgeTimeFleet(OM, Period)
  List$RetainAtAge <- ArraySimStockAgeTimeFleet(OM, Period)
  
  List
}



MakeEffortList <- function(OM, Period='Historical', TimeSteps=NULL) {
  
  
  meta <- GetMetaData(OM, Period, TimeSteps)
  nSim <- meta$nSim
  nAges <- meta$nAges
  TimeSteps <- meta$TimeSteps
  nStock <- length(meta$StockNames)
  nFleet <- length(meta$FleetNames)
  nArea <- meta$nAreas
  
  List <- list()

  # Effort
  List$Effort <- purrr::map(OM@Fleet, \(x)
                            GetEffort(x, TimeSteps)) |>
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'Time Step', 'Fleet')) |>
    ArrayExpand(nSim, nAges, TimeSteps)
  
  # Catchability
  List$Catchability <- purrr::map(OM@Fleet, \(x)
                                  GetCatchability(x, TimeSteps)) |>
    List2Array('Stock')
  
  if (is.null(List$Catchability)) {
    List$Catchability <- ArraySimStockAgeTimeFleet(OM, Period) |>
      DropDimension('Age', warn=FALSE)
  } else {
    List$Catchability <- List$Catchability |>
      aperm(c('Sim', 'Stock', 'Time Step', 'Fleet')) |>
      ArrayExpand(nSim, nAges, TimeSteps) 
  }
  
  #TODO qCV, qInc, Vessels, Trips, MaxVessels, MaxTrips
  # do in Populate

  List
}



MakeDistributionList <- function(OM, Period='Historical', TimeSteps=NULL) {
  
  meta <- GetMetaData(OM, Period, TimeSteps)
  nSim <- meta$nSim
  nAges <- meta$nAges
  TimeSteps <- meta$TimeSteps
  nStock <- length(meta$StockNames)
  nFleet <- length(meta$FleetNames)
  nArea <- meta$nAreas
  
  List <- list()
  
  List$Closure <- purrr::map(OM@Fleet, \(x)
                             GetClosure(x, TimeSteps)) |>
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'Time Step', 'Area', 'Fleet')) |>
    ArrayExpand(nSim, nAges, TimeSteps)
  
  #TODO Cost 
  
  List
}


MakeFleetWeightList <- function(OM, Period='Historical', TimeSteps=NULL) {
  
  meta <- GetMetaData(OM, Period, TimeSteps)
  nSim <- meta$nSim
  nAges <- meta$nAges
  TimeSteps <- meta$TimeSteps
  nStock <- length(meta$StockNames)
  nFleet <- length(meta$FleetNames)
  nArea <- meta$nAreas
  
  FleetWeightAtAge <- purrr::map2(OM@Stock, OM@Fleet, \(x,y)
                                                GetFleetWeightAtAge(x,y, TimeSteps)
  ) |>
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'Age', 'Time Step', 'Fleet')) |>
    ArrayExpand(nSim, nAges, TimeSteps) 
  
  
 
  
  FleetWeightAtAge
}
