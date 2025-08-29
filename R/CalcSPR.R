
CalcSPR0 <- function(HistSim, TimeSteps=NULL) {
  if (is.null(TimeSteps))
    TimeSteps <- HistSim@OM@TimeSteps
  
  
  SPR0 <- purrr::map(HistSim@OM@Stock, \(stock) CalcSPR0_Stock(stock, TimeSteps))
  
  SPFrom <- purrr::map(HistSim@OM@Stock, \(stock) stock@SRR@SPFrom)
  
  for (st in seq_along(SPR0)) {
    if (SPFrom[[st]] != st && SPFrom[[st]] != HistSim@OM@Stock[[st]]@Name)
      SPR0[[st]][] <- NA
  }
  
  SPR0 <- List2Array(SPR0, 'Stock', 'TimeStep') |> t() 
  dimnames(SPR0)[[2]] <- TimeSteps
  SPR0 |> ArrayReduceDims()
}

CalcSPR0_Stock <- function(Stock, TimeSteps) {
  FecundityAtAge <- Stock@Fecundity@MeanAtAge |> ArraySubsetTimeStep(TimeSteps)
  UnfishedSurvival <- CalcUnfishedSurvival(Stock, TRUE, TimeSteps)
  
  BySim <- "Sim" %in% names(dimnames(FecundityAtAge))
  if (BySim) {
    SPR0 <- ArrayMultiply(UnfishedSurvival, FecundityAtAge) |> apply(c('Sim', 'TimeStep'), sum)
  } else {
    SPR0 <- ArrayMultiply(UnfishedSurvival, FecundityAtAge) |> apply('TimeStep', sum) |>
      array(dimnames=list(TimeStep=TimeSteps))
    
  }
  SPR0
}


# 
# # ---- CalcSPRF -----
# 
# setGeneric('CalcSPRF', function(x, NPR=NULL, FSearch=NULL, TimeSteps=NULL)
#   standardGeneric('CalcSPRF')
# )
# 
# 
# setMethod('CalcSPRF', c('stock', 'FleetList',  'ANY'), function(x, NPR=NULL, FSearch=NULL, TimeSteps=NULL) {
#   
#   if (!is.null(x@SRR@SPFrom) && x@SRR@SPFrom!=x@Name)
#     return(NULL)
#   
#   out <- lapply(cli::cli_progress_along(seq_along(FSearch), 
#                                         format='Calculating Spawning-Per-Recruit {.val {x@Name}} {cli::pb_bar} {cli::pb_percent} '),
#                 function(i) {
#                   Fleet <- UpdateApicalF(NPR, FSearch[i],TimeSteps=TimeSteps) 
#                   FishedSurvival <- CalcFishedSurvival(x, Fleet, SP=TRUE, TimeSteps=TimeSteps)
#                   
#                   # fished egg production per recruit
#                   # TODO need to check if Fecundity@MeanAtAge always include maturity-at-age
#                   ArrayMultiply(array1=FishedSurvival, array2=x@Fecundity@MeanAtAge) |>
#                     apply(c(1,3), sum) |> process_cpars()
#                 })
#   l <- out[[1]]
#   if (is.null(l)) 
#     return(NULL)
#   DimNames <- dimnames(l)
#   DimNames$apicalF <- FSearch
#   array <- array(unlist(out), dim=c(dim(l), length(FSearch)))
#   dimnames(array) <- DimNames
#   array
# })
# 
# setMethod('CalcSPRF', c('StockList', 'StockFleetList',  'ANY'), 
#           function(x, NPR=NULL, FSearch=NULL, TimeSteps=NULL) {
#             purrr::map2(x, NPR, CalcSPRF, FSearch=FSearch, TimeSteps=TimeSteps)
#           })
# 
# setMethod('CalcSPRF', c('om', 'ANY',  'ANY'), 
#           function(x, NPR=NULL, FSearch=NULL, TimeSteps=NULL) {
#             if (is.null(FSearch))
#               FSearch <- x@Control$Curves$FSearch
#             purrr::map2(x@Stock, x@Fleet, CalcSPRF, FSearch=FSearch, TimeSteps=TimeSteps)
#           })
# 
# setMethod('CalcSPRF', c('stock', 'array',  'ANY'), 
#           function(x, NPR=NULL, FSearch=NULL, TimeSteps=NULL) {
#            
#             if (!is.null(x@SRR@SPFrom) && x@SRR@SPFrom!=x@Name)
#               return(NULL)
#             
#             fecundity <- AddDimension(GetFecundityAtAge(x, TimeSteps), 'apicalF')
#             ArrayMultiply(NPR, fecundity) |>
#               apply(c(1,3,4), sum) 
#           })
# 
# # ---- CalcSPR ---- 
# 
# CalcSPR <- function(OM, SPR0=NULL, NPR=NULL, FSearch=NULL, TimeSteps=NULL) {
#   # TODO add option to specify Time Steps to calculate
#   # currently does all
#  
#   # TODO  modify for herm species
#   if (is.null(SPR0)) 
#     SPR0 <- CalcSPR0(OM)
#   if (is.null(FSearch))
#     FSearch <- OM@Control$Curves$FSearch
#   if (is.null(NPR)) 
#     NPR <- CalcNPR(OM, FSearch=FSearch)
#   if (is.null(TimeSteps)) 
#     TimeSteps <- TimeSteps(OM, 'Historical')
#   
#   SPRF <- purrr::map2(OM@Stock, NPR, CalcSPRF, TimeSteps=TimeSteps)
#   
#   # add apicalF dimension for division
#   SPR0 <- purrr::map(SPR0, AddDimension, 'apicalF') 
#   purrr::map2(SPRF, SPR0, ArrayDivide)
#   
# }
# 
