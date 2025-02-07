# devtools::load_all()

setGeneric('CalcYPR', function(x, Fleet=NULL, FSearch=NULL, TimeSteps=NULL)
  standardGeneric('CalcYPR')
)


# setMethod('CalcYPR', c('StockList', 'StockFleetList',  'ANY'), 
#           function(x, Fleet=NULL, FSearch=NULL) {
#             purrr::map2(x, Fleet, CalcYPR, FSearch=FSearch)
#           })
  
setMethod('CalcYPR', c('om', 'ANY',  'ANY'), 
          function(x, Fleet=NULL, FSearch=NULL, TimeSteps=NULL) {
            if (is.null(FSearch))
              FSearch <- x@Control$Curves$FSearch
            if (is.null(TimeSteps))
              TimeSteps <- TimeSteps(x, 'Historical')
            purrr::map2(x@Stock, x@Fleet, CalcYPR, FSearch=FSearch, TimeSteps=TimeSteps)
          })

setMethod('CalcYPR', c('stock', 'FleetList',  'ANY'), 
          function(x, Fleet=NULL, FSearch=NULL, TimeSteps=NULL) {
            
            out <- lapply(cli::cli_progress_along(seq_along(FSearch), 
                                                      format='Calculating Yield-Per-Recruit {.val {x@Name}} {cli::pb_bar} {cli::pb_percent} '), function(i) {
                                                        Fleet <- CalcFatAge(Fleet, TimeSteps, apicalF=FSearch[i]) 
                                                        NatAge <- CalcFishedSurvival(x, Fleet, TimeSteps=TimeSteps)
                                                        CalcCatch(x, Fleet, NatAge, TimeSteps=TimeSteps)
                                                      })
            
            RemovalBiomass <- lapply(out, '[[', 'RemovalBiomass')
            RemovalBiomass <- purrr::map(RemovalBiomass, \(x) apply(x, c(1,3), sum))
            l <- RemovalBiomass[[1]]
            DimNames <- dimnames(l)
            DimNames$apicalF <- FSearch
            RemovalBiomass <- array(unlist(RemovalBiomass), dim=c(dim(l), length(FSearch)))
            dimnames(RemovalBiomass) <- DimNames
            
            
            RetainBiomass <- lapply(out, '[[', 'RetainBiomass')
            RetainBiomass <- purrr::map(RetainBiomass, \(x) apply(x, c(1,3), sum))
            l <- RetainBiomass[[1]]
            DimNames <- dimnames(l)
            DimNames$apicalF <- FSearch
            RetainBiomass <- array(unlist(RetainBiomass), dim=c(dim(l), length(FSearch)))
            dimnames(RetainBiomass) <- DimNames
            
            list(Removal=RemovalBiomass,
                 Retain=RetainBiomass)
            
          })






