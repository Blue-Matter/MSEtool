
# ---- CalcSPR0 -----

CalcSPR0 <- function(OM) {
  SPR0 <- purrr::map2(CalcUnfishedSurvival(OM@Stock, SP=TRUE), 
                      GetFecundityAtAge(OM@Stock),
                      MultiplyArrays)
  
  SPR0 <- purrr::map(SPR0, function(x)
    apply(x, c(1,3), sum) |> process_cpars()
  )
  
  # Map to Stock number
  # TODO - move SPFrom to Stock object?
  if (!is.null(OM@SexPars@SPFrom)) {
    for (i in 1:nrow(OM@SexPars@SPFrom)) {
      ind <- which(OM@SexPars@SPFrom[i,]==1)
      if (i !=ind) {
        SPR0[[i]] <- ind
      }
    }
  }
  SPR0
}


# ---- CalcSPRF -----

setGeneric('CalcSPRF', function(x, Fleet=NULL, apicalF=NULL)
  standardGeneric('CalcSPRF')
)


setMethod('CalcSPRF', c('stock', 'FleetList',  'ANY'), 
          function(x, Fleet=NULL, apicalF=NULL) {
            FishedSurvival <- CalcFishedSurvival(x, Fleet, apicalF, SP=TRUE)
            
            # fished egg production per recruit
            # TODO need to check if Fecundity@MeanAtAge always include maturity-at-age
            MultiplyArrays(array1=FishedSurvival, array2=x@Fecundity@MeanAtAge) |>
              apply(c(1,3), sum) |> process_cpars()
          })

setMethod('CalcSPRF', c('StockList', 'StockFleetList',  'ANY'), 
          function(x, Fleet=NULL, apicalF=NULL) {
            purrr::map2(x, Fleet, CalcSPRF, apicalF=apicalF)
          })

setMethod('CalcSPRF', c('om', 'ANY',  'ANY'), 
          function(x, Fleet=NULL, apicalF=NULL) {
            purrr::map2(x@Stock, x@Fleet, CalcSPRF, apicalF=apicalF)
          })

# ---- CalcSPR ---- 

CalcSPR <- function(OM, apicalF=NULL, SPR0=NULL) {
  if (is.null(SPR0)) 
    SPR0 <- CalcSPR0(OM) # unfished spawning production per recruit
  SPRF <- CalcSPRF(OM, apicalF=apicalF)
  purrr::map2(SPRF, SPR0, DivideArrays)
}

