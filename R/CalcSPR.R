
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
  
  ArrayMultiply(CalcUnfishedSurvival(x, SP=TRUE), fecundity) |> 
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

setGeneric('CalcSPRF', function(x, NPR=NULL, FSearch=NULL)
  standardGeneric('CalcSPRF')
)


setMethod('CalcSPRF', c('stock', 'FleetList',  'ANY'), function(x, NPR=NULL, FSearch=NULL) {
  
  if (!is.null(x@SRR@SPFrom) && x@SRR@SPFrom!=x@Name)
    return(NULL)
  
  out <- lapply(cli::cli_progress_along(seq_along(FSearch), 
                                        format='Calculating Spawning-Per-Recruit {.val {x@Name}} {cli::pb_bar} {cli::pb_percent} '),
                function(i) {
                  Fleet <- UpdateApicalF(NPR, FSearch[i]) 
                  FishedSurvival <- CalcFishedSurvival(x, Fleet, SP=TRUE)
                  
                  # fished egg production per recruit
                  # TODO need to check if Fecundity@MeanAtAge always include maturity-at-age
                  ArrayMultiply(array1=FishedSurvival, array2=x@Fecundity@MeanAtAge) |>
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
          function(x, NPR=NULL, FSearch=NULL) {
            purrr::map2(x, NPR, CalcSPRF, FSearch=FSearch)
          })

setMethod('CalcSPRF', c('om', 'ANY',  'ANY'), 
          function(x, NPR=NULL, FSearch=NULL) {
            if (is.null(FSearch))
              FSearch <- x@Control$Curves$FSearch
            purrr::map2(x@Stock, x@Fleet, CalcSPRF, FSearch=FSearch)
          })

setMethod('CalcSPRF', c('stock', 'array',  'ANY'), 
          function(x, NPR=NULL, FSearch=NULL) {
            if (!is.null(x@SRR@SPFrom) && x@SRR@SPFrom!=x@Name)
              return(NULL)
            fecundity <- AddDimension(x@Fecundity@MeanAtAge, 'apicalF')
            ArrayMultiply(NPR, fecundity) |>
              apply(c(1,3,4), sum) 
          })

# ---- CalcSPR ---- 

CalcSPR <- function(OM, SPR0=NULL, NPR=NULL, FSearch=NULL) {
  # TODO add option to specify Time Steps to calculate
  # currently does all
  
  # TODO  modify for herm species
  if (is.null(SPR0)) 
    SPR0 <- CalcSPR0(OM)
  if (is.null(FSearch))
    FSearch <- OM@Control$Curves$FSearch
  if (is.null(NPR)) 
    NPR <- CalcNPR(OM, FSearch=FSearch)

  SPRF <- purrr::map2(OM@Stock, NPR, CalcSPRF)
  # add apicalF dimension for division
  SPR0 <- purrr::map(SPR0, AddDimension, 'apicalF') 
  purrr::map2(SPRF, SPR0, ArrayDivide)
}

