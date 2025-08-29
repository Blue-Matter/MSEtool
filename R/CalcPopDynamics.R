

# 
# 
# AddDimNamesOMListSim <- function(OMListSim) {
#   
#   StockNames <- names(OMListSim$NumberAtAgeArea)
#   nStock <- length(StockNames)
#   FleetNames <- dimnames(OMListSim$Selectivity$MeanAtAge[[1]])[["Fleet"]]
#   nFleet <- length(FleetNames)
#   TimeStepsAll <- OMListSim$TimeSteps
#   
#   # Arrays including stock 
#   dimnames(OMListSim$Biomass) <- list('Stock'=StockNames,
#                                       'TimeStep'=TimeStepsAll[1:ncol(OMListSim$Biomass)])
#   
#   dimnames(OMListSim$SBiomass) <- list('Stock'=StockNames,
#                                        'TimeStep'=TimeStepsAll[1:ncol(OMListSim$SBiomass)])
#   
#   dimnames(OMListSim$SProduction) <- list('Stock'=StockNames,
#                                           'TimeStep'=TimeStepsAll[1:ncol(OMListSim$SProduction)])
#   
#   for (st in 1:nStock) {
#     ages <- OMListSim$Ages[[st]]@Classes
#     areas <- 1:dim(OMListSim$NumberAtAgeArea[[st]])[3]
#     
#     
#     dimnames(OMListSim$VBiomassArea[[st]]) <- list(
#       "TimeStep"= TimeStepsAll,
#       "Fleet" = FleetNames,
#       "Area" = areas
#     )
#     
#     dimnames(OMListSim$DensityArea[[st]]) <- list(
#       "TimeStep"= TimeStepsAll,
#       "Fleet" = FleetNames,
#       "Area" = areas
#     )
#     
#     dimnames(OMListSim$EffortArea[[st]]) <- list(
#       "TimeStep"= TimeStepsAll,
#       "Fleet" = FleetNames,
#       "Area" = areas
#     )
#     
#     OMListSim$FDeadAtAgeArea[[st]] <- purrr::map(OMListSim$FDeadAtAgeArea[[st]], \(x) {
#       dimnames(x) <- list(
#         "Age" = ages,
#         "Fleet" = FleetNames,
#         "Area" = areas
#       )
#       x
#     })
#     
#     OMListSim$FRetainAtAgeArea[[st]] <- purrr::map(OMListSim$FRetainAtAgeArea[[st]], \(x) {
#       dimnames(x) <- list(
#         "Age" = ages,
#         "Fleet" = FleetNames,
#         "Area" = areas
#       )
#       x
#     })
#     
#     OMListSim$RemovalAtAgeArea[[st]] <- purrr::map(OMListSim$RemovalAtAgeArea[[st]], \(x) {
#       dimnames(x) <- list(
#         "Age" = ages,
#         "Fleet" = FleetNames,
#         "Area" = areas
#       )
#       x
#     })
#     
#     OMListSim$RetainAtAgeArea[[st]] <- purrr::map(OMListSim$RetainAtAgeArea[[st]], \(x) {
#       dimnames(x) <- list(
#         "Age" = ages,
#         "Fleet" = FleetNames,
#         "Area" = areas
#       )
#       x
#     })
#     
#     dimnames(OMListSim$RemovalNumberAtAge[[st]]) <- list(
#       "Age" = ages,
#       "TimeStep"= TimeStepsAll,
#       "Fleet" = FleetNames
#     )
#     
#     dimnames(OMListSim$RetainNumberAtAge[[st]]) <- list(
#       "Age" = ages,
#       "TimeStep"= TimeStepsAll,
#       "Fleet" = FleetNames
#     )
#     
#     dimnames(OMListSim$RemovalBiomassAtAge[[st]]) <- list(
#       "Age" = ages,
#       "TimeStep"= TimeStepsAll,
#       "Fleet" = FleetNames
#     )
#     dimnames(OMListSim$RetainBiomassAtAge[[st]]) <- list(
#       "Age" = ages,
#       "TimeStep"= TimeStepsAll,
#       "Fleet" = FleetNames
#     )
#     
#     dimnames(OMListSim$FDeadAtAge[[st]]) <- list(
#       "Age" = ages,
#       "TimeStep"= TimeStepsAll,
#       "Fleet" = FleetNames
#     )
#     
#     dimnames(OMListSim$FRetainAtAge[[st]]) <- list(
#       "Age" = ages,
#       "TimeStep"= TimeStepsAll,
#       "Fleet" = FleetNames
#     )
#     
#     
#     dimnames(OMListSim$NumberAtAgeArea[[st]]) <- list(
#       "Age" = ages,
#       "TimeStep"= TimeStepsAll,
#       "Area" = areas
#     )
#   }
#   OMListSim
# }
# 



# OMList <- MakeOMList(OM, Unfished)
# OMListSim <- OMList[[1]]
# Period <- "Historical"
# MP=NULL
# DataListSim=NULL
# AddDimNames=TRUE

# CalcPopDynamics <- function(OMListSim, 
#                             Period=c("Historical", "Projection", "All"), 
#                             MP=NULL, 
#                             DataListSim=NULL,
#                             AddDimNames=TRUE) {
# 
#   
#   stop()
#   Period <- match.arg(Period)
#   
#   OMListSim <- rlang::duplicate(OMListSim)
#     
#   if (!inherits(OMListSim, "OMListSim"))
#     stop('Object must be class `OMListSim`')
#   
#   TimeStepsAll <- OMListSim$TimeSteps[[1]]
#   if (Period=="Historical") {
#     TimeSteps <- OMListSim$TimeStepsHist[[1]]
#   } else if (Period=="Projection") {
#     TimeSteps <- OMListSim$TimeStepsProj[[1]]
#   } else {
#     TimeSteps <- TimeStepsAll
#   }
#     
# 
#   nTStotal <- dim(OMListSim$NumberAtAgeArea[[1]])[2]
#   
#   # timestep <- TimeSteps[1]
#   for (timestep in TimeSteps) {
# 
#     ts <- match(timestep, TimeStepsAll)
#     TSindex <- ts - 1
# 
#     # Apply MICE 
#     
# 
#     # Biomass  beginning of this time step
#     OMListSim$Biomass = CalcBiomass_(
#       OMListSim$Biomass,
#       OMListSim$NumberAtAgeArea,
#       OMListSim$Weight$MeanAtAge,
#       TSindex
#     )
# 
#     # Apply MP
#     OMListSim <- ApplyMP(OMListSim,
#                          Data=NULL,
#                          MP=MP,
#                          TimeStep=timestep)
# 
# 
#     # VB by Area
#     OMListSim$VBiomassArea = CalcVBiomass_(
#       OMListSim$VBiomassArea,
#       OMListSim$NumberAtAgeArea,
#       OMListSim$FleetWeightAtAge,
#       OMListSim$Selectivity$MeanAtAge,
#       OMListSim$Distribution$Closure,
#       TSindex
#     )
# 
#     # # Relative VB Density by Area & Fleet
#     # OMListSim$DensityArea = CalcDensity_(
#     #   OMListSim$DensityArea,
#     #   OMListSim$VBiomassArea,
#     #   OMListSim$Spatial$RelativeSize,
#     #   TSindex
#     # )
#     # 
#  
#     
#     # Distribute Effort over Areas (currently proportional to VB)
#     OMListSim$EffortArea = DistEffort_(
#       OMListSim$EffortArea,
#       OMListSim$VBiomassArea,
#       OMListSim$Effort$Effort,
#       TSindex
#     )
# 
#     # Calculate F within each Area
#     List <- CalcFArea_(
#       OMListSim$FDeadAtAgeArea,
#       OMListSim$FRetainAtAgeArea,
#       OMListSim$EffortArea,
#       OMListSim$Spatial$RelativeSize,
#       OMListSim$Effort$Catchability,
#       OMListSim$Selectivity$MeanAtAge,
#       OMListSim$Retention$MeanAtAge,
#       OMListSim$DiscardMortality$MeanAtAge,
#       TSindex)
# 
#     OMListSim$FDeadAtAgeArea <- List$FDeadAtAgeArea
#     OMListSim$FRetainAtAgeArea <- List$FRetainAtAgeArea
# 
#     # Removals and Retained Number and Biomass by Area
#     List <- CalcCatch_(
#       OMListSim$RemovalAtAgeArea,
#       OMListSim$RetainAtAgeArea,
#       OMListSim$RemovalNumberAtAge,
#       OMListSim$RetainNumberAtAge,
#       OMListSim$RemovalBiomassAtAge,
#       OMListSim$RetainBiomassAtAge,
#       OMListSim$NaturalMortality$MeanAtAge,
#       OMListSim$FleetWeightAtAge,
#       OMListSim$NumberAtAgeArea,
#       OMListSim$FDeadAtAgeArea,
#       OMListSim$FRetainAtAgeArea,
#       TSindex)
# 
#     OMListSim$RemovalAtAgeArea <- List$RemovalAtAgeArea
#     OMListSim$RetainAtAgeArea <- List$RetainAtAgeArea
#     OMListSim$RemovalNumberAtAge <- List$RemovalNumberAtAge
#     OMListSim$RetainNumberAtAge <- List$RetainNumberAtAge
#     OMListSim$RemovalBiomassAtAge <- List$RemovalBiomassAtAge
#     OMListSim$RetainBiomassAtAge <- List$RetainBiomassAtAge
# 
#     # Calc overall F from catch and pop by Area
#     List <- CalcFfromCatch_(
#       OMListSim$FDeadAtAge,
#       OMListSim$FRetainAtAge,
#       OMListSim$NumberAtAgeArea,
#       OMListSim$RemovalNumberAtAge,
#       OMListSim$NaturalMortality$MeanAtAge,
#       OMListSim$Selectivity$MeanAtAge,
#       OMListSim$Retention$MeanAtAge,
#       OMListSim$DiscardMortality$MeanAtAge,
#       OMListSim$FDeadAtAgeArea,
#       OMListSim$FRetainAtAgeArea,
#       TSindex
#     )
# 
#     OMListSim$FDeadAtAge <- List$FDeadAtAge
#     OMListSim$FRetainAtAge <- List$FRetainAtAge
# 
#     # Calc Spawning Production and Spawning Biomass
#     List <-CalcSpawnProduction_(
#       OMListSim$SProduction,
#       OMListSim$SBiomass,
#       OMListSim$NumberAtAgeArea,
#       OMListSim$NaturalMortality$MeanAtAge,
#       OMListSim$Fecundity$MeanAtAge,
#       OMListSim$Weight$MeanAtAge,
#       OMListSim$Maturity$MeanAtAge,
#       OMListSim$SRR$SpawnTimeFrac,
#       OMListSim$SRR$SPFrom,
#       OMListSim$FDeadAtAge,
#       TSindex
#     )
# 
#     OMListSim$SProduction <- List$SProduction
#     OMListSim$SBiomass <- List$SBiomass
#     
#     # Calc Recruitment
#     Recruits <- CalcRecruitment_(
#       OMListSim$SProduction,
#       OMListSim$SP0,
#       OMListSim$SRR$R0,
#       OMListSim$SRR$RecDevs,
#       OMListSim$SRR$SRRModel,
#       OMListSim$SRR$SRRPars,
#       TSindex
#     )
#     
#     # Add Recruits
#     OMListSim$NumberAtAgeArea <- AddRecruits_(
#       OMListSim$NumberAtAgeArea,
#       Recruits,
#       OMListSim$Spatial$UnfishedDist,
#       TSindex
#     )
# 
# 
#     # Generate Data
#     if (ts<nTStotal) {
#       # Update Number beginning of next
#       OMListSim$NumberAtAgeArea <- CalcNumberNext_(
#         OMListSim$NumberAtAgeArea,
#         OMListSim$NaturalMortality$MeanAtAge,
#         OMListSim$FDeadAtAgeArea,
#         OMListSim$Maturity$Semelparous,
#         OMListSim$Ages,
#         TSindex
#       )
# 
#       OMListSim$NumberAtAgeArea <- MoveStock_(
#         OMListSim$NumberAtAgeArea,
#         OMListSim$Spatial$Movement,
#         TSindex+1
#       )
# 
# 
#     }
#   }
#   
#   if (AddDimNames) 
#     OMListSim <- AddDimNamesOMListSim(OMListSim)
#     
#   OMListSim
# }

