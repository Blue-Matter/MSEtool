
# ---- CalcSPR0 -----
setGeneric('CalcSPR0', function(x)
  standardGeneric('CalcSPR0')
)

setMethod('CalcSPR0', 'stock', function(x) {
  fecundity <- GetFecundityAtAge(x)
  if (is.null(fecundity))
    return(NULL)
  
  if (!is.null(x@SRR@SPFrom) && x@SRR@SPFrom != x@Name)
    return(NULL)
  
  MultiplyArrays(CalcUnfishedSurvival(x, SP=TRUE), fecundity) |> 
  apply(c(1,3), sum) |> 
    process_cpars()
})

setMethod('CalcSPR0', 'StockList', function(x) {
  purrr::map(x, CalcSPR0)
})


setMethod('CalcSPR0', 'om', function(x) {
  CalcSPR0(x@Stock)
})



# ---- CalcSPRF -----

setGeneric('CalcSPRF', function(x, Fleet=NULL, FSearch=NULL)
  standardGeneric('CalcSPRF')
)


setMethod('CalcSPRF', c('stock', 'FleetList',  'ANY'), function(x, Fleet=NULL, FSearch=NULL) {
  
  if (!is.null(x@SRR@SPFrom) && x@SRR@SPFrom!=x@Name)
    return(NULL)
  
  out <- lapply(cli::cli_progress_along(FSearch, 
                                        'Calculating Equilibrium SPR'),
                function(i) {
                  Fleet <- UpdateApicalF(Fleet, FSearch[i]) 
                  FishedSurvival <- CalcFishedSurvival(x, Fleet, SP=TRUE)
                  
                  # fished egg production per recruit
                  # TODO need to check if Fecundity@MeanAtAge always include maturity-at-age
                  MultiplyArrays(array1=FishedSurvival, array2=x@Fecundity@MeanAtAge) |>
                    apply(c(1,3), sum) |> process_cpars()
                })
  l <- out[[1]]
  if (is.null(l)) 
    return(NULL)
  DimNames <- dimnames(l)
  DimNames$apicalF <- FSearch
  array <- array(unlist(out), dim=c(dim(l), length(FSearch)))
  dimnames(array) <- DimNames
  array
})

setMethod('CalcSPRF', c('StockList', 'StockFleetList',  'ANY'), 
          function(x, Fleet=NULL, FSearch=NULL) {
            purrr::map2(x, Fleet, CalcSPRF, FSearch=FSearch)
          })

setMethod('CalcSPRF', c('om', 'ANY',  'ANY'), 
          function(x, Fleet=NULL, FSearch=NULL) {
            purrr::map2(x@Stock, x@Fleet, CalcSPRF, FSearch=FSearch)
          })

# ---- CalcSPR ---- 

# setGeneric('CalcSPR', function(x, SPR0=NULL, FSearch=NULL)
#   standardGeneric('CalcSPR')
# )
# 
# 
# setMethod('CalcSPR', c('stock', 'FleetList') function(x, ) {
#   MultiplyArrays(CalcUnfishedSurvival(x, SP=TRUE), 
#                  GetFecundityAtAge(x)
#   )
# })


CalcSPR <- function(OM, SPR0=NULL, FSearch=NULL) {
  # TODO add option to specify Time Steps to calculate
  # currently does all
  
  # TODO  modify for herm species
  
  if (is.null(FSearch))
    FSearch <- OM@Control$Curves$FSearch
  if (is.null(SPR0)) 
    SPR0 <- CalcSPR0(OM)

  # add apicalF dimension for division
  SPR0 <- purrr::map(SPR0, AddDimension, 'apicalF') 
  
  SPRF <- CalcSPRF(OM, FSearch=FSearch)
  purrr::map2(SPRF, SPR0, DivideArrays)
}

