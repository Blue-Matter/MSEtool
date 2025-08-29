# CalcReferencePoints <- function(OM, TimeSteps=NULL, silent=FALSE) {
#   
#   if (is.null(TimeSteps))
#     TimeSteps <- OM |> TimeSteps('Historical') |> tail(1)
#   
#   StockList <- PopulateStockList(OM) |> SubsetTimeStep(TimeSteps, AddPast = FALSE)
#   StockFleetList <- PopulateFleetList(OM, StockList) |> SubsetTimeStep(TimeSteps)
#   CatchFracList <- OM |> CheckCatchFrac() |> CatchFrac()
#   
#   Complexes <- OM@Complexes
#   
#   nAgesList <- purrr::map(StockList, \(Stock) 
#                           length(Stock@Ages@Classes))
#   
#   FleetList <- purrr::map2(StockFleetList, nAgesList, \(FleetList,nAges)
#                                Fleet2Hist(FleetList, nAges,
#                                           nSim=nSim(OM), 
#                                           TimeSteps=TimeSteps,
#                                           nArea(StockList[[1]]),
#                                           silent=TRUE)
#   )
#   
#   StockSimList <- purrr::map(1:nSim(OM), \(x) {
#     SubsetSim(StockList, Sim=x, drop=TRUE)
#   }, .progress = 'Building StockList')
#   names(StockSimList) <- 1:nSim(OM)
#   
#   FleetSimList <- purrr::map(1:nSim(OM), \(x) {
#     SubsetSim(FleetList, Sim=x, drop=TRUE)
#   }, .progress = 'Building FleetList')
#   names(FleetSimList) <- 1:nSim(OM)
#   
#   CatchFracSimList <- purrr::map(1:nSim(OM), \(x) {
#     SubsetSim(CatchFracList, Sim=x, drop=TRUE)
#   })
#   names(CatchFracSimList) <- 1:nSim(OM)
#   
# 
# }
# 
# CalcRefPointsSim <- function(StockSimList, FleetSimList, CatchFracSimList, Complexes, TimeSteps) {
#   IdenticalAcrossSims <- IdenticalSims(StockSimList, TimeSteps) &
#     IdenticalSims(FleetSimList, TimeSteps) &
#     IdenticalSims(CatchFracSimList, TimeSteps, EditSlots=FALSE)
#   
# 
#   RefPoints <- new('refpoints')
#   
# 
#   
#   
# 
#   
# }
# 
