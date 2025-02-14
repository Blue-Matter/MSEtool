setGeneric('CalcNPR', function(x, Fleet=NULL, FSearch=NULL, TimeSteps=NULL)
  standardGeneric('CalcNPR')
)

setMethod('CalcNPR', c('stock', 'FleetList',  'ANY', 'ANY'), 
          function(x, Fleet=NULL, FSearch=NULL, TimeSteps=NULL) {
 
            
            
  out <- lapply(cli::cli_progress_along(seq_along(FSearch),
                                        format='Calculating Number-Per-Recruit {.val {x@Name}} {cli::pb_bar} {cli::pb_percent} '),
                CalcNPR_, Stock=x, Fleet=Fleet, TimeSteps=TimeSteps)
  
  l <- out[[1]][[1]]
  DimNames <- dimnames(l)
  DimNames$apicalF <- FSearch
  NPR <- lapply(out, '[[', 1)
  NPRS <- lapply(out, '[[', 2)
  NPRarray <- array(unlist(NPR), dim=c(dim(l), length(FSearch)))
  NPRSarray <- array(unlist(NPRS), dim=c(dim(l), length(FSearch)))
  dimnames(NPRarray) <- DimNames
  dimnames(NPRSarray) <- DimNames
  
  list(NPR=NPRarray,
       NPRS=NPRSarray)
})

setMethod('CalcNPR', c('StockList', 'StockFleetList',  'ANY', 'ANY'), 
          function(x, Fleet=NULL, FSearch=NULL, TimeSteps=NULL) {
            purrr::map2(x, Fleet, CalcNPR, FSearch=FSearch, TimeSteps=TimeSteps)
          })

setMethod('CalcNPR', c('om', 'ANY',  'ANY', 'ANY'), 
          function(x, Fleet=NULL, FSearch=NULL, TimeSteps=NULL) {
            if (is.null(FSearch))
              FSearch <- x@Control$Curves$FSearch
            if (is.null(TimeSteps))
              TimeSteps <- TimeSteps(x, 'Historical')
            
            purrr::map2(x@Stock, x@Fleet, CalcNPR, FSearch=FSearch, TimeSteps=TimeSteps)
          })

CalcNPR_ <- function(apicalF, Stock, Fleet, TimeSteps) {
  Fleet <- UpdateApicalF(Fleet, apicalF=apicalF, TimeSteps)
  Fleet <- CalcFatAge(Fleet, TimeSteps=TimeSteps, apicalF= GetApicalF(Fleet))
  NPR <- CalcFishedSurvival(Stock, Fleet=Fleet, TimeSteps=TimeSteps)
  if (x@SRR@SpawnTimeFrac !=0) {
    NPRS <- CalcFishedSurvival(Stock, Fleet=Fleet, SP=TRUE, TimeSteps=TimeSteps)
  } else {
    NPRS <- NPR
  }
  list(NPR=NPR, NPRS=NPRS)
}
