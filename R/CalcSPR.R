
# ---- CalcSPR0 -----
setGeneric('CalcSPR0', function(x)
  standardGeneric('CalcSPR0')
)

setMethod('CalcSPR0', 'stock', function(x) {
  MultiplyArrays(CalcUnfishedSurvival(x, SP=TRUE), 
                         GetFecundityAtAge(x)) |> 
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


setMethod('CalcSPRF', c('stock', 'FleetList',  'ANY'), 
          function(x, Fleet=NULL, FSearch=NULL) {
            out <- lapply(cli::cli_progress_along(FSearch, 
                                                  'Calculating Equilibrium SPR'),
                          function(i) {
                            FishedSurvival <- CalcFishedSurvival(x, Fleet, FSearch[i], SP=TRUE)
                            
                            # fished egg production per recruit
                            # TODO need to check if Fecundity@MeanAtAge always include maturity-at-age
                            MultiplyArrays(array1=FishedSurvival, array2=x@Fecundity@MeanAtAge) |>
                              apply(c(1,3), sum) |> process_cpars()
                          })
            l <- out[[1]]
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



CalcSPR <- function(OM, FSearch=NULL, SPR0=NULL) {
  if (is.null(SPR0)) 
    SPR0 <- CalcSPR0(OM) # unfished spawning production per recruit
  
  # add F dimension for division
  SPR0 <- purrr::map(SPR0, replicate, n=1) |>
    purrr::map(AddDimNames, c('Sim', 'Time Step', 'apicalF'), TimeSteps=TimeSteps(OM))
  
  SPRF <- CalcSPRF(OM, FSearch=FSearch)
  purrr::map2(SPRF, SPR0, DivideArrays)
}

