SimulateDynamics <- function(HistSimList, TimeSteps=NULL, silent=FALSE) {
  
  if (inherits(HistSimList, 'hist')) {
    HistSimList <- Hist2HistSimList(HistSimList)
  } 
  
  if (is.null(TimeSteps))
    TimeSteps <- TimeSteps(HistSimList[[1]]@OM, 'Historical')
  
  
  sim <- 1
  HistSim = HistSimList[[sim]]
 
  r = SimulateDynamics_(HistSim, TimeSteps)
  
  
  
  # Add DimNames back in !

  
  
  r@EffortArea$`Day octopus`[,1,]
  
  r@TimeSeries@FDeadAtAgeArea$`Day octopus`[[2]][,1,]
  r@TimeSeries@FRetainAtAgeArea$`Day octopus`[[2]][,1,]
  

  HistSim@TimeSeries@SBiomass |> dim()
 
  t = HistSim@TimeSeries@FDeadAtAgeArea[[1]] |> Array2List(2)
  t$`2021` |> dim()
  HistSim@TimeSeries@FRetainAtAgeArea
  
  
  FDeadAtAgeAreaStock[TSindex] = FMortFleetArea["FDeadFleetArea"];
  FRetainAtAgeAreaStock[TSindex] = FMortFleetArea["FRetainFleetArea"];
  
  FDeadAtAgeAreaList[st] = FDeadAtAgeAreaStock;
  FRetainAtAgeAreaList[st] = FRetainAtAgeAreaStock;
  
  
  # Lists by Sim 
  
  # Stock 
  NumberAtAgeAreaList <- Array2List(Hist@Number, 'Sim') |> lapply(Array2List, 'Stock')
  WeightAtAge <- Array2List(Hist@OM@Stock@Weight@MeanAtAge, 'Sim') 
  
  # Fleet
  FleetWeightAtAgeList <- purrr::map(Hist@OM@Fleet, \(x) Array2List(x@WeightFleet, 'Sim')) |>
    ReverseList()
    
  FleetWeightAtAgeList$`1`$Female |> dimnames()
  
  
  SelectivityAtAgeList <- purrr::map(Hist@OM@Fleet, \(x) Array2List(x@Selectivity@MeanAtAge, 'Sim')) |>
    ReverseList()
  

  ClosureAreaList <- purrr::map(Hist@OM@Fleet, \(x) Array2List(x@Distribution@Closure, 'Sim')) |>
    ReverseList()

  EffortAreaList <- purrr::map(Hist@OM@Fleet, \(x) Array2List(x@Distribution@EffortArea, 'Sim')) |>
    ReverseList()
  
  Effort <- purrr::map(Hist@OM@Fleet, \(x) Array2List(x@Effort@Effort, 'Sim')) |>
    ReverseList() |>
    purrr::map(\(x) 
      List2Array(x, 'Stock') |>
        aperm(c('Stock', 'TimeStep', 'Fleet'))
    )
  
  TimeStepsAll <- Hist@OM@TimeSteps
  nAges <- lapply(Hist@OM@Stock@Ages@Classes, length) |> unlist()
  nStock <- nStock(OM)
  nArea <- nArea(OM)
  nFleet <- nFleet(OM)

  List <- list(NumberAtAgeAreaList=NumberAtAgeAreaList,
               FleetWeightAtAgeList,
               SelectivityAtAgeList,
               ClosureAreaList,
               EffortAreaList,
               WeightAtAge=WeightAtAge,
               Effort=Effort
               )
  
  tt <- purrr::pmap(List, 
                    SimulateDynamics_, 
                    TimeSteps=TimeSteps,
                    TimeStepsAll=TimeStepsAll,
                    nAges=nAges,
                    nStock=nStock,
                    nFleet=nFleet,
                    nArea=nArea)
  
  Effort$`1`[1,1,]
  tt[[1]]$EffortAreaList$Female[1,,]
  
  

  # Simulate Population
  
  NaturalMortalityatAgeList <- Array2List(Hist@OM@Stock@NaturalMortality@MeanAtAge, 1)
  
  NaturalMortalityatAgeList <- Array2List(Hist@OM@Stock@NaturalMortality@MeanAtAge,1)   
  SpawnTimeFrac <- Hist@OM@Stock@SRR@SpawnTimeFrac
  Hist@Biomass
  
  
  Hist@nsim
  Hist@nstock
  Hist@nfleet
  Hist@narea
  Hist@TimeSteps
  NaturalMortalityAtAge=NaturalMortalityatAgeList
  
}