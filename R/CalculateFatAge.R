CalcFatAge <- function(Fleet, return=c('All', 'FCaught', 'FRetain', 'FDiscard', 'FDead')) {
  if (!methods::is(Fleet, 'fleet'))
    cli::cli_abort('`Fleet` must be class `fleet`')
  
  apicalF <- Fleet@FishingMortality@ApicalF
  selectivity <-  Fleet@Selectivity@MeanAtAge
  DimSelectivity <- dim(selectivity)
  
  # add age dimension
  apicalF <- replicate(DimSelectivity[2], apicalF) |> aperm(c(1,3,2))
  
  ind <- match(dimnames(apicalF)$`Time Step`, dimnames(selectivity)$`Time Step`)
  
  dnames <- dimnames(selectivity)
  dnames[[3]] <- dnames[[3]][ind]
  dimnames(apicalF) <-  dnames
  
  FCaught <- MultiplyArrays(apicalF, selectivity[,,ind, drop=FALSE])
  
  EffectiveRetain <- MultiplyArrays(Fleet@Selectivity@MeanAtAge[,,ind, drop=FALSE], 
                                    Fleet@Retention@MeanAtAge[,,ind, drop=FALSE])
  
  FRetain <-  MultiplyArrays(apicalF, EffectiveRetain)
  
  FDiscard <-  FCaught-FRetain
  
  DiscardMort <- Fleet@DiscardMortality@MeanAtAge[,,ind, drop=FALSE]
  
  if (is.null(DiscardMort) || !all(is.finite(DiscardMort))) {
    FDead <- FRetain   
  } else {
    FDead <- FRetain + MultiplyArrays(FDiscard, DiscardMort)
  }
  
  out <- list(
    FCaught=FCaught,
    FRetain=FRetain,
    FDiscard=FDiscard,
    FDead=FDead
  )
  
  return <- match.arg(return)
  
  if (return=='All') return(out)
  
  out[[return]]
}


