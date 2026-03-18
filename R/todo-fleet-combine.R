# 
# #' Combine Multiple Fleets into a Single Fleet
# #'
# #' Combine several fleets into a new aggregated fleet within an operating model.
# #'
# #'
# #' @param OM An [OM()] object.
# #' @param FleetList A named list 
# #' @param silent Logical. Suppress informational messages.
# #'
# #' @return An updated `OM` object with the new fleet added for each stock.
# #'
# #' @export
# CombineFleets <- function(OM, FleetList, silent=FALSE) {
#   
#   CheckClass(OM)
#   
#   if (!is.list(FleetList)) 
#     cli::cli_abort("`FleetList` must be a list")
#   
#   if (is.null(names(FleetList)))
#     cli::cli_abort("`FleetList` must be a named list")
#   
#   fleetnames <- FleetNames(OM)
#   ind <- as.character(unlist(FleetList)) %in% fleetnames
#   if (any(!ind)) {
#     cli::cli_abort(c('x' = 'Names in `FleetList` do not match `FleetNames(OM)`',
#                      'i' = 'Invalid Fleets: {.val {as.character(unlist(FleetList))[!ind]}}'))
#   }
# 
#   OM <- Populate(OM, silent=TRUE)
#   
#   FleetIndList <- purrr::map(FleetList, \(Fleets) resolve_fleet_indices(OM, Fleets))
#  
#   if (!silent) 
#     cli::cli_alert_info("Combining fleets into aggregated fleet:")
#   
#   NamesList <- names(FleetList)
#   
#   # Combine Fleets
#   for (i in seq_along(FleetList)) {
#     if (!silent) 
#       cli::cli_li(
#         "{.val {FleetList[[i]]}} into new fleet: {.val {NamesList[i]}} "
#       )
#     
#     replaceFleet <- FleetIndList[[i]][1]
#     dropFleet <- FleetIndList[[i]][-1]
#     FleetInds <- FleetIndList[[i]]
#     Name <- NamesList[i]
#     
#     for (st in seq_len(nStock(OM))) {
#       OM@Fleet[[st]][[replaceFleet]] <- combine_fleets_stock(OM, st, Name, FleetInds)
#       names(OM@Fleet[[st]])[replaceFleet] <- Name 
#       
#     }
#     
#     # Data
#     for (i in seq_along(OM@Data)) {
#       
#       # OM@Data[[i]]@Landings@Name
#       # OM@Data[[i]]@Landings@Value
#       # OM@Data[[i]]@Landings@CV
#       
#     }
#     
#     # Obs 
#     
#     # Imb 
#       
#   }
#       
#       
# 
#   DropFleets <- lapply(FleetList, '[', -1) |> unlist()
#   for (st in seq_len(nStock(OM))) {
#     OM@Fleet[[st]][DropFleets] <- NULL
#   }
#   
#   OM
#   
# 
# }
# 
# resolve_fleet_indices <- function(OM, Fleets) {
#   
#   fleetnames <- FleetNames(OM)
#   
#   if (is.character(Fleets)) {
#     FleetInds <- match(Fleets, fleetnames)
#   } else {
#     FleetInds <- Fleets
#   }
#   
#   if (any(is.na(FleetInds)) ||
#       !all(FleetInds %in% seq_along(fleetnames))) {
#     
#     cli::cli_abort(c(
#       "x" = "Invalid `Fleets` supplied.",
#       "i" = "Fleets: {.val {Fleets}}",
#       "i" = "Existing Fleets: {.val {fleetnames}}"
#     ))
#   }
#   
#   FleetInds
# }
# 
# combine_fleets_stock <- function(OM, st, Name, FleetInds) {
#   
#   FleetList <- OM@Fleet[[st]][FleetInds]
#   NewFleet  <- Fleet(Name = Name)
#   
#   apicalFList   <- purrr::map(FleetList, \(fleet)
#                               ArrayMultiply(fleet@Effort@Effort,
#                                             fleet@Catchability@Efficiency)
#   )
#   
#   totalApicalF  <- Reduce(`+`, apicalFList)
#   
#   Efficiency <- FleetList[[1]]@Catchability@Efficiency
#   
#   Effort(NewFleet) <- Effort(Effort = ArrayDivide(totalApicalF, Efficiency))
#   
#   Catchability(NewFleet) <- Catchability(Efficiency = Efficiency)
#   
#   FInteractList <- purrr::map2(apicalFList, FleetList, \(apicalF, fleet) {
#     Fa_expanded <- apicalF |>
#       AddDimension("Age",  pos = 2) |>
#       AddDimension("Area", pos = 4)
#     
#     ArrayMultiply(Fa_expanded, fleet@Selectivity@MeanAtAge)
#   }) 
#   
#   FInteract <- Reduce(`+`, FInteractList)
#   
#   RetentionList <- purrr::map(FleetList,
#                               \(fleet) fleet@Retention@MeanAtAge
#   )
#   
#   FRetainList <- purrr::map2(FInteractList, RetentionList,
#                              \(Fint, ret)
#                              ArrayMultiply(Fint, ret)
#   )
#   FRetain <- Reduce(`+`, FRetainList)
#   
#   Selectivity(NewFleet) <- Selectivity(MeanAtAge = standardizeF(FInteract))
#   Retention(NewFleet) <- Retention(MeanAtAge = ArrayDivide(FRetain, FInteract))
#   
#   DiscardMortalityList <- purrr::map(FleetList, \(fleet) {
#     -log(1-fleet@DiscardMortality@MeanAtAge)
#   })
#   
#   relFList <- purrr::map(apicalFList, \(Ff) ArrayDivide(Ff, totalApicalF))
#   
#   weightedList <- purrr::map2(relFList, DiscardMortalityList, \(Frel, DiscM) {
#     Frel <- Frel |> AddDimension('Age', pos=2) |>
#       AddDimension('Area', pos=4)
#     temp <- ArrayMultiply(Frel, DiscM)
#     temp[!is.finite(temp)] <- Inf
#     temp
#   })
#   
#   MeanAtAge <- 1-exp(-Reduce(`+`, weightedList))
#  
#   DiscardMortality(NewFleet) <- DiscardMortality(MeanAtAge = MeanAtAge)
#   
#   # WeightFleet 
#   WeightFleetList <- purrr::map(FleetList, WeightFleet)
#   
#   weightedList <- purrr::map2(relFList, WeightFleetList, \(Frel, WF) {
#     Frel <- Frel |> AddDimension("Age", pos = 2)
#     ArrayMultiply(Frel, WF)
#   })
#   
#   WeightFleet(NewFleet) <- Reduce(`+`, weightedList)
# 
#   NewFleet
# }
# 
# 
# standardizeF <- function(Farray) {
#   nms <- names(dimnames(Farray))
#   age_ind <- which(nms=='Age')
#   maxF <- apply(Farray,  nms[-age_ind], max) |>
#     AddDimension("Age", pos = age_ind)
#   
#   ArrayDivide(Farray, maxF)
# }

