
setGeneric('CalcPerRecruit', function(x, Fleet=NULL, FSearch=NULL, TimeSteps=NULL)
  standardGeneric('CalcPerRecruit')
)

setMethod('CalcPerRecruit', c('om'), 
          function(x, Fleet=NULL, FSearch=NULL,  TimeSteps=NULL) {
            if (is.null(FSearch))
              FSearch <- x@Control$Curves$FSearch
            
            if (is.null(TimeSteps))
              TimeSteps <- TimeSteps(x, 'Historical') |> tail(1)
            purrr::map2(x@Stock, x@Fleet, CalcPerRecruit, FSearch=FSearch, TimeSteps=TimeSteps)
          })

setMethod('CalcPerRecruit', c('stock', 'FleetList'), 
          function(x, Fleet=NULL, FSearch=NULL, TimeSteps=NULL) {
            
            if (is.null(TimeSteps))
              TimeSteps <- TimeSteps(x, 'Historical') |> tail(1)
            
            NMortAtAge <- GetNaturalMortalityAtAge(x, TimeSteps)
            NPR0 <- CalcUnfishedSurvival(x, TimeSteps=TimeSteps)
            NPR0S <- CalcUnfishedSurvival(x, SP=TRUE, TimeSteps=TimeSteps)
            
            SPR0 <- CalcSPR0(x, TimeSteps)
            
            out <- lapply(cli::cli_progress_along(seq_along(FSearch),
                                                  format='Per-Recruit Calculations: {.val {x@Name}} {cli::pb_bar} {cli::pb_percent} '), 
                          function(i) {
                            apicalF <- FSearch[i]
                            
                            Fleet <- UpdateApicalF(Fleet, apicalF=apicalF, TimeSteps)
                            Fleet <- CalcFatAge(Fleet, 
                                                TimeSteps=TimeSteps, 
                                                apicalF=GetApicalF(Fleet, TimeSteps=TimeSteps, array=FALSE))
                            
                            
                            # number per recruit
                            NPR <- CalcFishedSurvival(x, Fleet=Fleet, TimeSteps=TimeSteps)
                            
                            if (x@SRR@SpawnTimeFrac !=0) {
                              # number per recruit spawners
                              NPRS <- CalcFishedSurvival(x, Fleet=Fleet, SP=TRUE, TimeSteps=TimeSteps)
                            } else {
                              NPRS <- NPR 
                            }
                            
                            # yield per recruit
                            CatchNperRecruit <- CalcCatchN(FDeadAtAge=GetFatAgeArray(Fleet, TimeSteps),
                                                           FRetainAtAge=GetFatAgeArray(Fleet, TimeSteps, 'Retain'),
                                                           NatAge=NPR0,
                                                           NMortAtAge)
                            
                            WeightAtAge <- GetFleetWeightAtAge(x, Fleet, TimeSteps)
                         
                            RemovalPR <- ArrayMultiply(CatchNperRecruit$Removal, WeightAtAge) |> apply(c(1,3), sum)
                            RetainPR <- ArrayMultiply(CatchNperRecruit$Retain, WeightAtAge) |> apply(c(1,3), sum)
                            
                            NatAge <- NPR0
                            FDeadAtAge <- GetFatAgeArray(Fleet, TimeSteps) # need to add spatial dimension for pop dynamics
                            FRetainAtAge <- GetFatAgeArray(Fleet, TimeSteps, 'Retain')
                            
                            # spawning per recruit
                            SPRF <- ArrayMultiply(array1=NPRS, array2=x@Fecundity@MeanAtAge) |> 
                              apply(c(1,3), sum)
                            
                            SPR <- ArrayDivide(SPRF, SPR0)
                            
                            list(NPR=NPR,
                                 NPRS=NPRS,
                                 SPR=SPR,
                                 RemovalPR=RemovalPR,
                                 RetainPR=RetainPR)
                          })
            
            names(out) <- FSearch
            NPR <- lapply(out, '[[', 1) |> List2Array('ApicalF')
            NPRS <- lapply(out, '[[', 2) |> List2Array('ApicalF')
            SPR <- lapply(out, '[[', 3) |> List2Array('ApicalF')
            RemovalPR <- lapply(out, '[[', 4) |> List2Array('ApicalF')
            RetainPR <- lapply(out, '[[', 5) |> List2Array('ApicalF')
            
            list(NPR=NPR,
                 NPRS=NPRS,
                 SPR=SPR,
                 RemovalPR=RemovalPR,
                 RetainPR=RetainPR)
          }
)