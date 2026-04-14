
#' Unfished Spawning Production Per Recruit
#'
#' Calculates the unfished spawning production per recruit (SPR0) — the
#' denominator of the spawning potential ratio — under equilibrium conditions.
#' SPR0 is computed as the element-wise ratio of unfished spawning production
#' [SP0()] to unfished recruitment [R0()], with array broadcasting handled by
#' [ArrayDivide()].
#'
#' @param object Either a [om-class] or [hist-class] object. If an [om-class]
#'   object is provided, the historical dynamics are populated internally via
#'   [Populate()] and [CalcUnfished_Equilibrium()], which may be
#'   computationally expensive. If a [hist-class] object is provided (the output
#'   of [Simulate()]), it is used directly.
#' @param silent Logical. If `TRUE`, suppresses progress messages during
#'   population and simulation. Only used when `object` is an [om-class].
#'   Default is `FALSE`.
#'
#' @return An array with dimensions `[Sim, Stock, Year]` containing the
#'   unfished spawning production per recruit. Dimensions where values are
#'   identical across simulations or years are collapsed by [ReduceDims()].
#'
#' @seealso [SP0()], [R0()], [ArrayDivide()]
#' @export
CalcSPR0 <- function(object, silent = FALSE) {
  if (inherits(object, 'om')) {
    object <- Populate(object, silent=silent)
    Hist <- OM2Hist(OM=object, silent=silent)
    Hist@Unfished@Equilibrium <- CalcUnfished_Equilibrium(object, silent)
    
  } else if (inherits(object, 'hist')) {
    Hist <- object
  } else {
    cli::cli_abort("`object` must be class `om` or class `hist`")
  }
  
  SP0 <- SP0(Hist)
  R0 <- R0(Hist)
  ArrayDivide(SP0,R0) |> ReduceDims()

}




#
# # ---- CalcSPRF -----
#
# setGeneric('CalcSPRF', function(x, NPR=NULL, FSearch=NULL, Years=NULL)
#   standardGeneric('CalcSPRF')
# )
#
#
# setMethod('CalcSPRF', c('stock', 'FleetList',  'ANY'), function(x, NPR=NULL, FSearch=NULL, Years=NULL) {
#
#   if (!is.null(x@SRR@SPFrom) && x@SRR@SPFrom!=x@Name)
#     return(NULL)
#
#   out <- lapply(cli::cli_progress_along(seq_along(FSearch),
#                                         format='Calculating Spawning-Per-Recruit {.val {x@Name}} {cli::pb_bar} {cli::pb_percent} '),
#                 function(i) {
#                   Fleet <- UpdateApicalF(NPR, FSearch[i],Years=Years)
#                   FishedSurvival <- CalcFishedSurvival(x, Fleet, SP=TRUE, Years=Years)
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
#           function(x, NPR=NULL, FSearch=NULL, Years=NULL) {
#             purrr::map2(x, NPR, CalcSPRF, FSearch=FSearch, Years=Years)
#           })
#
# setMethod('CalcSPRF', c('om', 'ANY',  'ANY'),
#           function(x, NPR=NULL, FSearch=NULL, Years=NULL) {
#             if (is.null(FSearch))
#               FSearch <- x@Control$Curves$FSearch
#             purrr::map2(x@Stock, x@Fleet, CalcSPRF, FSearch=FSearch, Years=Years)
#           })
#
# setMethod('CalcSPRF', c('stock', 'array',  'ANY'),
#           function(x, NPR=NULL, FSearch=NULL, Years=NULL) {
#
#             if (!is.null(x@SRR@SPFrom) && x@SRR@SPFrom!=x@Name)
#               return(NULL)
#
#             fecundity <- AddDimension(GetFecundityAtAge(x, Years), 'apicalF')
#             ArrayMultiply(NPR, fecundity) |>
#               apply(c(1,3,4), sum)
#           })
#
# # ---- CalcSPR ----
#
# CalcSPR <- function(OM, SPR0=NULL, NPR=NULL, FSearch=NULL, Years=NULL) {
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
#   if (is.null(Years))
#     Years <- Years(OM, 'Historical')
#
#   SPRF <- purrr::map2(OM@Stock, NPR, CalcSPRF, Years=Years)
#
#   # add apicalF dimension for division
#   SPR0 <- purrr::map(SPR0, AddDimension, 'apicalF')
#   purrr::map2(SPRF, SPR0, ArrayDivide)
#
# }
#
