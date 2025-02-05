setGeneric('CalcNPR', function(x, Fleet=NULL, FSearch=NULL)
  standardGeneric('CalcNPR')
)

setMethod('CalcNPR', c('stock', 'FleetList',  'ANY'), function(x, Fleet=NULL, FSearch=NULL) {
  
  out <- lapply(cli::cli_progress_along(seq_along(FSearch),
                                        format='Calculating Number-Per-Recruit {.val {x@Name}} {cli::pb_bar} {cli::pb_percent} '),
                function(i) {
                 CalcFishedSurvival(x, Fleet=UpdateApicalF(Fleet, FSearch[i]) )
                })

  l <- out[[1]]
  DimNames <- dimnames(l)
  DimNames$apicalF <- FSearch
  array <- array(unlist(out), dim=c(dim(l), length(FSearch)))
  dimnames(array) <- DimNames
  array
})

setMethod('CalcNPR', c('StockList', 'StockFleetList',  'ANY'), 
          function(x, Fleet=NULL, FSearch=NULL) {
            purrr::map2(x, Fleet, CalcNPR, FSearch=FSearch)
          })


setMethod('CalcNPR', c('om', 'ANY',  'ANY'), 
          function(x, Fleet=NULL, FSearch=NULL) {
            if (is.null(FSearch))
              FSearch <- x@Control$Curves$FSearch
            purrr::map2(x@Stock, x@Fleet, CalcNPR, FSearch=FSearch)
          })