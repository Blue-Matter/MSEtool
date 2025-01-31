# devtools::load_all()

setGeneric('CalcYPR', function(x, Fleet=NULL, FSearch=NULL)
  standardGeneric('CalcYPR')
)

# x <- OM@Stock[[1]]
# Fleet <- OM@Fleet[[1]]
# 
# Fleet <- Fleet$SPN_1


setMethod('CalcYPR', 
          c('stock', 'FleetList',  'ANY'), 
          function(x, 
                   Fleet=NULL, 
                   FSearch=NULL) {
            
            out <- lapply(FSearch, function(i) {
              Fleet <- UpdateApicalF(Fleet, FSearch[i]) |>
                CalcFatAge()
              NatAge <- CalcFishedSurvival(x, Fleet)
              
              
              
              # TODO this should go in it's own function for calculating yield
              FDead <- GetFatAgeArray(Fleet)
              FRetain <- GetFatAgeArray(Fleet, 'Retain')
              
              FDeadTotal <- apply(FDead, 1:3, sum)
              ZDead <- AddArrays(x@NaturalMortality@MeanAtAge, FDeadTotal)
              
              # TODO - get empirical weight by fleet where available
              # add option to use empirical weight in control
              # calculate yield with baranov
              # add option for catch equation?
            
            })
            
            
          })


# YPRCurve <- function(OM, FSearch=NULL) {
#   
#   if (is.null(FSearch))
#     FSearch <- OM@Control$Curves$FSearch
#   
#   
#   # output - yield per recruit for each sim & time-step
#   OM@Stock[[1]]
#   N <- CalcFishedSurvival(OM, apicalF=FSearch[1])
#   
#   NatAge <- N$Female
#   WeightatAge <- OM@Stock[[1]]@Weight@MeanAtAge
#   MatAge <- OM@Stock[[1]]@NaturalMortality@MeanAtAge
#   
#   # TODO - empirical OM@Fleet[[1]][[1]]@Weight |> dim()
#   FatAge <- UpdateApicalF(OM@Fleet[[1]], FSearch[1])
#   
#   CalcBaranov <- function(NatAge, WeightatAge, MatAge, FatAge) {
#     
#   }
#   
# }



