CalcFatAge <- function(Fleet, return=c('All', 'FCaught', 'FRetain', 'FDiscard', 'FDead')) {
  if (!methods::is(Fleet, 'fleet'))
    cli::cli_abort('`Fleet` must be class `fleet`')
  
  apicalF <- Fleet@FishingMortality@ApicalF
  selectivity <-  Fleet@Selectivity@MeanAtAge
  DimSelectivity <- dim(selectivity)
  DimapicalF <- dim(apicalF)
  
  if (all(DimapicalF==1)) {
    # add sim, age, and time-step dimensions
    apicalF <- array(apicalF, dim=c(1,DimSelectivity[2], DimSelectivity[3])) |>
      AddDimNames(names=names(dimnames(selectivity)), TimeSteps =  dimnames(selectivity)$`Time Step`)
  } else {
    # add age dimension
    apicalF <- replicate(DimSelectivity[2], apicalF, simplify = 'array') |> 
      aperm(c(1,3,2))
    names(dimnames(apicalF))[2] <- 'Age'
    dimnames(apicalF)[[2]] <- 1:DimSelectivity[2]
    
  }
  
  
  # Inflate ApicalF to account for discard mortality
  # ApicalF = apicalF of Dead Fish 
  # TODO - this can be made more efficient by only calculating when necessary
  
  SelectivityRetention <- MultiplyArrays(Fleet@Selectivity@MeanAtAge, Fleet@Retention@MeanAtAge)
  SelectivityDiscardMort <- MultiplyArrays(Fleet@Selectivity@MeanAtAge, Fleet@DiscardMortality@MeanAtAge)
  SelectivityDiscardMortRetention <- MultiplyArrays(SelectivityDiscardMort, Fleet@Retention@MeanAtAge)
  
  InflateApicalF <- SubtractArrays(
    AddArrays(SelectivityRetention, SelectivityDiscardMort),
    SelectivityDiscardMortRetention
  )
  
  apicalF <- DivideArrays(apicalF, InflateApicalF)
  
  FCaught <- MultiplyArrays(apicalF, Fleet@Selectivity@MeanAtAge)
  FRetain <- MultiplyArrays(Fleet@Retention@MeanAtAge, FCaught)
  FDiscard <- SubtractArrays(FCaught, FRetain)
  

  if (is.null(DiscardMort) || !all(is.finite(DiscardMort))) {
    FDead <- FRetain   
  } else {
    FDead <- AddArrays(array1=FRetain, 
                       array2=MultiplyArrays(FDiscard, SelectivityDiscardMort))
  }
  
  FCaught[1,,71] |> max()
  FRetain[1,,71] |> max()
  FDead[1,,71] |> max()
  

  
  out <- list(
    FCaught=FCaught,
    FRetain=FRetain,
    FDiscard=FDiscard,
    FDead=FDead
  )
  
  return <- match.arg(return)
  
  if (return=='All') 
    return(out)
  
  out[[return]]
}



