

# 
# UpdateApicalF <- function(ProjSim, MPAdvice, Year, TSIndex) {
#   if (is.null(MPAdvice))
#     return(ProjSim)
#   
#   if (length(MPAdvice@apicalF)<1)
#     return(ProjSim)
#   
#   apicalF <- MPAdvice@apicalF 
# 
#   FleetAllocationF <- CalcFleetAllocationF(ProjSim@OM@Fleet, Year)
#   
# 
#   apicalFAge <- apicalF * FleetAllocationF  |> 
#     AddDimension("Age") |> 
#     aperm(c('Stock', 'Age', 'Year', 'Fleet'))
#   
#   # TODO - currently only for stock = 1 
#   
#   SelectivityAtAge <- purrr::map(ProjSim@OM@Fleet, \(stock) stock@Selectivity@MeanAtAge[,TSIndex,, drop=FALSE] |> abind::adrop(2))
#   RetentionAtAge <- purrr::map(ProjSim@OM@Fleet, \(stock) stock@Retention@MeanAtAge[,TSIndex,, drop=FALSE] |> abind::adrop(2))
#   DiscardMortalityAtAge <- purrr::map(ProjSim@OM@Fleet, \(stock) stock@DiscardMortality@MeanAtAge[,TSIndex,, drop=FALSE] |> abind::adrop(2))
#   
#   st <- 1
# 
#   FInteract <- ArrayMultiply(apicalFAge[st,,1,, drop=FALSE] |> abind::adrop(c(1,3)),
#                              SelectivityAtAge[[st]])
#   
#   FRetain <- ArrayMultiply(FInteract, RetentionAtAge[[st]])  
#   FDiscardTotal <- ArraySubtract(FInteract, FRetain)
#   FDiscardDead <- ArrayMultiply(FDiscardTotal, DiscardMortalityAtAge[[st]])
#   FDead <- FRetain + FDiscardDead
#   FDeadTotal <- apply(FDead, 'Age',  sum) 
#   ActualApicalF <- max(FDeadTotal)  
#   if (abs(ActualApicalF/apicalF -1) > 1e-2) {
#     adjust <- apicalF/ActualApicalF
#     FInteract <- FInteract * adjust
#   }
#   
#   RequiredEffort <- apply(FInteract, 2, max) / ProjSim@OM@Fleet[[st]]@Effort@Catchability[TSIndex,] 
#   RequiredEffort[RequiredEffort<1E-5] <- 1E-5 
#   
#   ProjSim@Effort[st,TSIndex,] <- RequiredEffort
#   ProjSim
#   
# }








