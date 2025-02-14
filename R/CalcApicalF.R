
# Calculates apicalF (dead) from Effort
# accounting for discarding and discard mortality

# ---- CalcFatAge ----
setGeneric('CalcApicalF', function(x, TimeSteps=NULL)
  standardGeneric('CalcApicalF')
)

setMethod('CalcApicalF', c('FleetList', 'ANY'),
          function(x, TimeSteps=NULL) {
            purrr::map(x, CalcApicalF, TimeSteps=TimeSteps)
          })


setMethod('CalcApicalF', c('fleet', 'ANY'),
          function(x, TimeSteps=NULL) {
            selectivity <- GetSelectivityAtAge(x, TimeSteps)
            retention <- GetRetentionAtAge(x, TimeSteps)
            discardmortality <- GetDiscardMortalityAtAge(x, TimeSteps)
            effort <- GetEffort(x, TimeSteps)
            
            effort <- effort |> AddDimension('Age') |> aperm(c(1,3,2))
            interact <- ArrayMultiply(effort, selectivity)
            retain <- ArrayMultiply(interact, retention)
            discard <- ArraySubtract(interact,retain)
            deaddiscard <- ArrayMultiply(discard, discardmortality)
            
            apicalF <- apply(ArrayAdd(retain, deaddiscard), c(1,3), max)
            ArrayFill(x@FishingMortality@ApicalF) <- apicalF
            x
          })