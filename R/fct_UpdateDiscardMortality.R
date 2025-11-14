DiscardMortalityUnchanged <- function(MPAdvice, MPAdvicePrevious) {
  !is.null(MPAdvicePrevious) && 
    (IdenticalS4(MPAdvice@DiscardMortality, MPAdvicePrevious@DiscardMortality)) ||
    EmptyObject(MPAdvice@DiscardMortality)
  
}

UpdateDiscardMortality <- function(ProjSim, MPAdviceList, MPAdviceList_Previous, Year, YearsProj) {
  
  FleetNames <- FleetNames(ProjSim@OM)
  Complexes <- ProjSim@OM@Complexes
  YearsProj <- YearsProj[YearsProj>=Year]
  
  for (complex in seq_along(MPAdviceList)) {
    stocks <- Complexes[[complex]]
    MPAdvice <- MPAdviceList[[complex]]
    MPAdvicePrevious <- MPAdviceList_Previous[[complex]]
    
    if (DiscardMortalityUnchanged(MPAdvice, MPAdvicePrevious))
      next()
    
    DiscardMortalityList <- MPAdvice@DiscardMortality
    if (!is.list(DiscardMortalityList)) 
      DiscardMortalityList <- list(DiscardMortalityList)
    
    if (length(DiscardMortalityList)>1 && length(DiscardMortalityList)!=length(FleetNames)) 
      cli::cli_abort('`Advice@DiscardMortality` must be a `DiscardMortality()` object or an `nFleet` long list of `DiscardMortality()` objects')
    
    for (st in stocks) {
      Ages <- ProjSim@OM@Stock[[st]]@Ages
      for (fl in seq_along(FleetNames)) {
        if (length(SelectivityList)>1) {
          DiscardMortality <- DiscardMortalityList[[fl]]
        } else {
          DiscardMortality <- DiscardMortalityList[[1]]
        }
        
        if (length(DiscardMortality@MeanAtAge)) {
          AgeClasses <- Ages@Classes
          if (length(DiscardMortality@MeanAtAge) != length(AgeClasses))
            cli::cli_abort(c('x'="`Advice@DiscardMortality@MeanAtAge` must be a numeric vector with length `nAges` ({.val {length(AgeClasses)}})",
                             'i'='Currently length {.val {length(DiscardMortality@MeanAtAge)}}'),
                           call=NULL
            )
          
          MeanAtAge <- array(DiscardMortality@MeanAtAge, dim=c(length(AgeClasses), 1),
                             dimnames = list(
                               Age=AgeClasses,
                               Year=YearsProj[1]
                             ))
          
          dimnames(MeanAtAge) <- list(Age=Ages@Classes,
                                      Year=YearsProj[1]) 
          
          ArrayFill(slot(ProjSim@OM@Fleet[[st]],type)@DiscardMortality[,,fl]) <- MeanAtAge |>
            ExtendYears(YearsProj)
          
        } else if (length(DiscardMortality@MeanAtLength)) {
          Length <- ProjSim@OM@Stock[[st]]@Length 
          if (!is.null(DiscardMortality@Classes))
            Length@Classes <- DiscardMortality@Classes
          if (is.null(Length@Classes))
            Length@Classes <- slot(ProjSim@OM@Fleet[[st]], type)@Classes[[fl]]
          if (is.null(Length@Classes))
            Length@Classes <- ProjSim@OM@Fleet[[st]]@DiscardMortality@Classes[[fl]]
          
          DiscardMortality@Classes <- Length@Classes
          
          if (length(DiscardMortality@MeanAtLength) != length(DiscardMortality@Classes)) 
            cli::cli_abort(c('x'='=`DiscardMortality@MeanAtLength` must be the same length as `DiscardMortality@Classes`'))
          
          Length@MeanAtAge <- Length@MeanAtAge |> ArraySubsetYear(YearsProj)
          Length@CVatAge <- Length@CVatAge |> ArraySubsetYear(YearsProj)
          SDatAge <- ArrayMultiply(Length@MeanAtAge, Length@CVatAge)
          
          Length@ASK <- CalcAgeSizeKey_(Length@MeanAtAge, 
                                        SDatAge, 
                                        Length@Classes, 
                                        Length@TruncSD, 
                                        Length@Dist)
          
          DiscardMortality@MeanAtLength <- array(DiscardMortality@MeanAtLength, 
                                            dim=c(1, length(DiscardMortality@MeanAtLength),1),
                                            dimnames=list(
                                              Sim=1,
                                              Class=DiscardMortality@Classes,
                                              Year=YearsProj[1])
          ) |> 
            ExtendYears(YearsProj)
          DiscardMortality <- MeanAtLength2MeanAtAge(DiscardMortality, Length, Ages, nsim=1, Years=YearsProj)                              
          
          MeanAtLength <- DiscardMortality@MeanAtLength |> DropDimension('Sim')
          MeanAtAge <- DiscardMortality@MeanAtAge |> DropDimension('Sim')
          
          ArrayFill(ProjSim@OM@Fleet[[st]]@DiscardMortality@MeanAtAge[,,fl]) <- MeanAtAge
          ArrayFill(ProjSim@OM@Fleet[[st]]@DiscardMortality@MeanAtLength[,,fl]) <- MeanAtLength
          }
      }
    }
  }
  ProjSim
}

