# 
# MakeFleetList <- function(OM, Period='All') {
#  
#   List <- list()
#   List$FishingMortality <- MakeFishingMortalityList(OM, Period)
#   
#   discardmortality <- MakeFleetSlotList(OM, 'DiscardMortality', Period=Period)
#   List$DiscardMortalityMeanAtAge <- discardmortality$MeanAtAge
#   
#   effort <- MakeEffortList(OM, Period)
#   List$Effort <- effort$Effort
#   List$Catchability <- effort$Catchability
#   selectivity <- MakeFleetSlotList(OM, 'Selectivity', Period)
#   List$SelectivityMeanAtAge <- selectivity$MeanAtAge
#   
#   retention <- MakeFleetSlotList(OM, 'Retention', Period)
#   List$RetentionMeanAtAge <- retention$MeanAtAge
#   
#   distribution <- MakeDistributionList(OM, Period)
#   List$Closure <- distribution$Closure
# 
#   ## Arrays to be filled
#   List$EffortArea <- ListArraySimAgeTimeFleetArea(OM, Period) |>
#     purrr::map(\(x) DropDimension(x, 'Age', warn=FALSE)) 
#     
#   
#   List$DensityArea <- List$EffortArea 
#   List$VBiomassArea <- List$EffortArea 
#   
#   List$FDeadAtAgeArea <-  ListArraySimAgeTimeFleetArea(OM, Period) |>
#     purrr::map(Array2List, "TimeStep")
#   
#   List$FRetainAtAgeArea <- List$FDeadAtAgeArea
#   
#   List$FDeadAtAge <-  ListArraySimAgeTimeFleet(OM, Period) 
#   List$FRetainAtAge <- List$FDeadAtAge
#   
#   List$RemovalAtAgeArea <- List$FDeadAtAgeArea
#   List$RetainAtAgeArea <- List$FDeadAtAgeArea
#   
#   
#   List$RemovalNumberAtAge <- ListArraySimAgeTimeFleet(OM, Period)
#   List$RetainNumberAtAge <- List$RemovalNumberAtAge
#   
#   List$RemovalBiomassAtAge <- List$RemovalNumberAtAge 
#   List$RetainBiomassAtAge <- List$RemovalNumberAtAge
#   
#   List$FleetWeightAtAge <- MakeFleetWeightList(OM) 
#   List
# }
# 
# 
# MakeFleetSlotList <- function(OM, slot='Selectivity', Period='Historical',
#                               TimeSteps=NULL) {
#   
#   if (is.null(TimeSteps))
#     TimeSteps <- TimeSteps(OM, Period)
#   
#   sNames <- slotNames(slot(OM@Fleet[[1]][[1]], slot))
#   meta <- GetMetaData(OM)
#   List <- list()
#   
#   if ('Pars' %in% sNames) 
#     List$Pars <- purrr::map(OM@Fleet, \(x)
#                             purrr::map(x, \(y) 
#                                        y|> 
#                                          methods::slot(slot) |>
#                                          methods::slot('Pars'))
#     )
#   
#   if ('Model' %in% sNames) 
#     List$Model <- purrr::map(OM@Fleet, \(x)
#                                                 purrr::map(x, \(y) 
#                                                            y|> 
#                                                              slot(slot) |>
#                                                              slot('Model'))
#     ) |> purrr::map(\(x) if(!is.null(x) && is.character(x))
#       get(x))
#   
#   
#   if ('MeanAtAge' %in% sNames) {
#     fun <- get(paste0('Get', slot, 'AtAge'))
#     List$MeanAtAge <- purrr::map(OM@Fleet, \(x) fun(x, TimeSteps)) |>
#       purrr::imap(\(x, idx) {
#         ArrayExpand(x, OM@nSim, meta$nAges[[idx]],
#                     TimeSteps) 
#       })
#   }
# 
#   if ('MeanAtLength' %in% sNames) {
#     fun <- get(paste0('Get', slot, 'AtLength'))
#     List[['MeanAtLength']] <- purrr::map(OM@Fleet, \(x) fun(x, TimeSteps)) |>
#       purrr::imap(\(x, idx) {
#         ArrayExpand(x, OM@nSim, meta$nAges[[idx]],
#                     TimeSteps) 
#       }) 
#   }
#   
#   
#   if ('Classes' %in% sNames)
#     List[['Classes']] <- purrr::map(OM@Fleet, \(x)
#                                                   purrr::map(x, \(y) 
#                                                              y|> 
#                                                                slot(slot) |>
#                                                                slot('Classes')) 
#     ) 
#  
#   if ('Misc' %in% sNames)
#     List[['Misc']] <- purrr::map(OM@Fleet, \(x)
#                                                purrr::map(x, \(y) 
#                                                           y|> 
#                                                             slot(slot) |>
#                                                             slot('Misc'))
#     )
#   List
# }
# 
# 
# 
# MakeFishingMortalityList <- function(OM, Period='Historical', TimeSteps=NULL) {
#   
#   if (is.null(TimeSteps))
#     TimeSteps <- TimeSteps(OM, Period)
#   
#   List <- list()
# 
#   List$DeadAtAge <- ListArraySimAgeTimeFleet(OM, Period)
#   List$RetainAtAge <- List$DeadAtAge
#   
#   List
# }
# 
# 
# 
# MakeEffortList <- function(OM, Period='Historical', TimeSteps=NULL) {
#   meta <- GetMetaData(OM)
#   if (is.null(TimeSteps))
#     TimeSteps <- TimeSteps(OM, Period)
#   
#   List <- list()
# 
#   # Effort
#   List$Effort <- purrr::map(OM@Fleet, \(x)
#                             GetEffort(x, TimeSteps)) |>
#     purrr::map(\(x) ArrayExpand(x, OM@nSim, TimeSteps=TimeSteps))
#     
#   List$Effort  <- List2Array(List$Effort, 'Stock') |> aperm(c('Sim', 'Stock', 'TimeStep', 'Fleet'))
# 
#   # Catchability
#   List$Catchability <- purrr::map(OM@Fleet, \(x)
#                                   GetCatchability(x, TimeSteps)) |>
#     purrr::imap(\(x, idx) {
#       ArrayExpand(x, OM@nSim, meta$nAges[[idx]],
#                   TimeSteps)
#     })
#   
#   List$Catchability  <- List2Array(List$Catchability, 'Stock') |> aperm(c('Sim', 'Stock',  'TimeStep', 'Fleet'))
#   
#   #TODO qCV, qInc, Vessels, Trips, MaxVessels, MaxTrips
#   # do in Populate
# 
#   List
# }
# 
# 
# 
# MakeDistributionList <- function(OM, Period='Historical', TimeSteps=NULL) {
#   
#   if (is.null(TimeSteps))
#     TimeSteps <- TimeSteps(OM, Period)
#   List <- list()
#   
#   meta <- GetMetaData(OM)
#   
#   List$Closure <- purrr::map(OM@Fleet, \(x) {
#     GetClosure(x, TimeSteps) |>
#       aperm(c('Sim', 'TimeStep', 'Fleet', 'Area'))
#   }) |>
#     purrr::imap(\(x, idx) {
#       ArrayExpand(x, OM@nSim, meta$nAges[[idx]],
#                   TimeSteps) 
#     })       
#     
#   
#   #TODO Cost 
#   
#   List
# }
# 
# 
# MakeFleetWeightList <- function(OM, Period='Historical', TimeSteps=NULL) {
#   
#   meta <- GetMetaData(OM)
#   
#   if (is.null(TimeSteps))
#     TimeSteps <- TimeSteps(OM)
#   
#   List <- purrr::map2(OM@Stock, OM@Fleet, \(x,y)
#                                                 GetFleetWeightAtAge(x,y, TimeSteps) |>
#                                                   ExpandSims(OM@nSim)
#   ) |>
#     purrr::imap(\(x, idx) {
#       ArrayExpand(x, OM@nSim, meta$nAges[[idx]],
#                   TimeSteps)
#     })
# 
#   
#   List
# }
