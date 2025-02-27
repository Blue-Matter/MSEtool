
MakeFleetParsList <- function(OM, Period='Historical') {
  nSim <- nSim(OM)
  nAges <- purrr::map(OM@Stock, nAge) |> unlist() |> max()
  TimeSteps <- TimeSteps(OM, Period)
  nStock <- nStock(OM)
  nFleet <- nFleet(OM)
  nArea <- nArea(OM@Stock[[1]])
  
  FleetParsList <- list()
  
  FleetParsList <- MakeEffortList(OM, nSim, nAges, TimeSteps, Period)
  FleetParsList <- c(FleetParsList, 
                     MakeFishingMortalityList(OM, nSim, nAges, TimeSteps, Period))
 
  FleetParsList <- c(FleetParsList, 
                     MakeFleetSlotList(OM, Period, 'DiscardMortality', nSim, nAges, TimeSteps),
                     MakeFleetSlotList(OM, Period, 'Selectivity', nSim, nAges, TimeSteps),
                     MakeFleetSlotList(OM, Period, 'Retention', nSim, nAges, TimeSteps))
  
                   
  
  # Distribution 
  FleetParsList$Closure <- purrr::map(OM@Fleet, \(x) {
    closure <- purrr::map(x, \(y) 
                          y@Distribution@Closure)
    List2Array(closure)
  }) |> 
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'Time Step', 'Area', 'Fleet')) 
  
  # Weight
  FleetParsList$FleetWeightAtAge <- purrr::map2(OM@Stock, OM@Fleet, \(x,y)
              GetFleetWeightAtAge(x,y, TimeSteps)
              ) |>
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'Age', 'Time Step', 'Fleet')) |>
    ArrayExpand(nSim, nAges, TimeSteps) 

  # BioEconomic
  # TODO
  
  FleetParsList$EffortArea <- ArraySimStockAgeTimeFleetArea(OM, Period) |>
    DropDimension('Age', warn=FALSE) 
  
  FleetParsList$FDeadArea <- ArraySimStockAgeTimeFleetArea(OM, Period)
    
  FleetParsList$FRetainArea <- FleetParsList$FDeadArea
  FleetParsList$RemovalArea <- FleetParsList$FDeadArea
  FleetParsList$RetainArea <- FleetParsList$FDeadArea
  
  class(FleetParsList) <- c('list', 'FleetParsList')
  FleetParsList
}


MakeFleetSlotList <- function(OM, Period='Historical', slot='Selectivity',
                              nSim, nAges, TimeSteps) {
  
  sNames <- slotNames(slot(OM@Fleet[[1]][[1]], slot))
  
  List <- list()
  
  if ('Pars' %in% sNames) 
    List[[paste0(slot, 'Pars')]] <- purrr::map(OM@Fleet, \(x)
                                               purrr::map(x, \(y) 
                                               y|> 
                                                 slot(slot) |>
                                                 slot('Pars'))
    )
  
  if ('Model' %in% sNames) 
    List[[paste0(slot, 'Model')]] <- purrr::map(OM@Fleet, \(x)
                                                purrr::map(x, \(y) 
                                                           y|> 
                                                             slot(slot) |>
                                                             slot('Model'))
    )
  
  if ('MeanAtAge' %in% sNames) {
    fun <- get(paste0('Get', slot, 'AtAge'))
    List[[paste0(slot, 'MeanAtAge')]] <- purrr::map(OM@Fleet, \(x) fun(x, TimeSteps)) |>
      List2Array('Stock') |>
      aperm(c('Sim', 'Stock', 'Age', 'Time Step', 'Fleet')) |>
      ArrayExpand(nSim, nAges, TimeSteps) |>
      ReplaceTiny(1) 
  }
  
  if ('MeanAtLength' %in% sNames) {
    fun <- get(paste0('Get', slot, 'AtLength'))
    List[[paste0(slot, 'MeanAtLength')]] <- purrr::map(OM@Fleet, \(x) fun(x, TimeSteps)) |>
      List2Array('Stock') |>
      aperm(c('Sim', 'Stock', 'Class', 'Time Step', 'Fleet')) |>
      ArrayExpand(nSim, nAges, TimeSteps) 
  }
  
  
  if ('Classes' %in% sNames)
    List[[paste0(slot, 'Classes')]] <- purrr::map(OM@Fleet, \(x)
                                                  purrr::map(x, \(y) 
                                                             y|> 
                                                               slot(slot) |>
                                                               slot('Classes')) 
    ) 
 
  if ('Misc' %in% sNames)
    List[[paste0(slot, 'Misc')]] <- purrr::map(OM@Fleet, \(x)
                                               purrr::map(x, \(y) 
                                                          y|> 
                                                            slot(slot) |>
                                                            slot('Misc'))
    )
  List
}



MakeFishingMortalityList <- function(OM, nSim, nAges, TimeSteps, Period) {
  List <- list()
  
  List$ApicalF <- purrr::map(OM@Fleet, \(x)
                            GetApicalF(x, TimeSteps)) |>
    List2Array('Stock') 
  
  if (is.null(List$ApicalF)) {
    List$ApicalF <- ArraySimStockAgeTimeFleet(OM, Period) |>
      DropDimension('Age', warn=FALSE) 
  } else {
    List$ApicalF <- List$ApicalF |>
      aperm(c('Sim', 'Stock', 'Time Step', 'Fleet')) |>
      ArrayExpand(nSim, nAges, TimeSteps) 
  }
  
  List$FDeadAtAge <- purrr::map(OM@Fleet, \(x)
                               GetFatAgeArray(x, TimeSteps)) |>
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock',  'Age', 'Time Step', 'Fleet')) |>
    ArrayExpand(nSim, nAges, TimeSteps) 
  
  List$FRetainAtAge <- purrr::map(OM@Fleet, \(x)
                               GetFatAgeArray(x, TimeSteps, type='Retain')) |>
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock',  'Age', 'Time Step', 'Fleet')) |>
    ArrayExpand(nSim, nAges, TimeSteps) |>
    ReplaceTiny() 

  List
}

MakeEffortList <- function(OM, nSim, nAges, TimeSteps, Period) {
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