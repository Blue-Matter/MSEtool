
# ---- CalcSPR0 -----
setGeneric('CalcSPR0', function(x, TimeSteps=NULL)
  standardGeneric('CalcSPR0')
)

setMethod('CalcSPR0', c('stock', 'ANY'), function(x, TimeSteps=NULL) {
  fecundity <- GetFecundityAtAge(x, TimeSteps(x, 'Historical'))
  
  if (is.null(fecundity))
    return(NULL)
  
  if (!is.null(x@SRR@SPFrom) && x@SRR@SPFrom != x@Name)
    return(NULL)
  
  ArrayMultiply(CalcUnfishedSurvival(x, SP=TRUE), fecundity) |> 
  apply(c(1,3), sum) |> process_cpars()
})

setMethod('CalcSPR0', c('StockList', 'ANY'), function(x, TimeSteps=NULL) {
  purrr::map(x, CalcSPR0)
})


setMethod('CalcSPR0', c('om', 'ANY'), function(x, TimeSteps=NULL) {
  if (is.null(TimeSteps))
    TimeSteps <- TimeSteps(x, 'Historical')
  CalcSPR0(x@Stock, TimeSteps)
})



# ---- CalcSPRF -----

setGeneric('CalcSPRF', function(x, NPR=NULL, FSearch=NULL, TimeSteps=NULL)
  standardGeneric('CalcSPRF')
)


setMethod('CalcSPRF', c('stock', 'FleetList',  'ANY'), function(x, NPR=NULL, FSearch=NULL, TimeSteps=NULL) {
  
  if (!is.null(x@SRR@SPFrom) && x@SRR@SPFrom!=x@Name)
    return(NULL)
  
  out <- lapply(cli::cli_progress_along(seq_along(FSearch), 
                                        format='Calculating Spawning-Per-Recruit {.val {x@Name}} {cli::pb_bar} {cli::pb_percent} '),
                function(i) {
                  Fleet <- UpdateApicalF(NPR, FSearch[i]) 
                  FishedSurvival <- CalcFishedSurvival(x, Fleet, SP=TRUE, TimeSteps=TimeSteps)
                  
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
          function(x, NPR=NULL, FSearch=NULL, TimeSteps=NULL) {
            purrr::map2(x, NPR, CalcSPRF, FSearch=FSearch, TimeSteps=TimeSteps)
          })

setMethod('CalcSPRF', c('om', 'ANY',  'ANY'), 
          function(x, NPR=NULL, FSearch=NULL, TimeSteps=NULL) {
            if (is.null(FSearch))
              FSearch <- x@Control$Curves$FSearch
            purrr::map2(x@Stock, x@Fleet, CalcSPRF, FSearch=FSearch, TimeSteps=TimeSteps)
          })

setMethod('CalcSPRF', c('stock', 'array',  'ANY'), 
          function(x, NPR=NULL, FSearch=NULL, TimeSteps=NULL) {
            if (!is.null(x@SRR@SPFrom) && x@SRR@SPFrom!=x@Name)
              return(NULL)
            fecundity <- AddDimension(x@Fecundity@MeanAtAge, 'apicalF')
            ArrayMultiply(NPR, fecundity) |>
              apply(c(1,3,4), sum) 
          })

# ---- CalcSPR ---- 

CalcSPR <- function(OM, SPR0=NULL, NPR=NULL, FSearch=NULL, TimeSteps=NULL) {
  # TODO add option to specify Time Steps to calculate
  # currently does all
  
  # TODO  modify for herm species
  if (is.null(SPR0)) 
    SPR0 <- CalcSPR0(OM)
  if (is.null(FSearch))
    FSearch <- OM@Control$Curves$FSearch
  if (is.null(NPR)) 
    NPR <- CalcNPR(OM, FSearch=FSearch)
  if (is.null(TimeSteps)) 
    TimeSteps <- TimeSteps(OM, 'Historical')
  
  SPRF <- purrr::map2(OM@Stock, NPR, CalcSPRF, TimeSteps=TimeSteps)
  
  # add apicalF dimension for division
  SPR0 <- purrr::map(SPR0, AddDimension, 'apicalF') 
  purrr::map2(SPRF, SPR0, ArrayDivide)
}

