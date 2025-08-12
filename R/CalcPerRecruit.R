CalcPerRecruit <- function(apicalF, Stock, Fleet, Allocation, TimeSteps) {
 
  stop()
  # TODO replace Allocation with CatchFrac 
  if (is.null(Allocation))
    Allocation <- 1
  
  NaturalMortality <- Stock@NaturalMortality@MeanAtAge |> ArraySubsetTimeStep(TimeSteps) 
  NPR0 <- CalcUnfishedSurvival(Stock, TimeSteps=TimeSteps)
  NPR0_SP <- CalcUnfishedSurvival(Stock, SP=TRUE, TimeSteps=TimeSteps)
  
  Selectivity <- Fleet@Selectivity@MeanAtAge |> ArraySubsetTimeStep(TimeSteps) 
  Retention <- Fleet@Retention@MeanAtAge |> ArraySubsetTimeStep(TimeSteps) 
  DiscardMortality <- Fleet@DiscardMortality@MeanAtAge |> ArraySubsetTimeStep(TimeSteps) 
  
  dd <- dim(Selectivity)
  
  apicalFAge <- matrix(apicalF * Allocation, dd[1], dd[3], byrow=TRUE)
  dimnames(apicalFAge) <- list(Age=dimnames(Selectivity)[[1]],
                               Fleet=dimnames(Selectivity)[[3]])
  
  apicalFAgeFleet <- AddDimension(apicalFAge, 'TimeStep', TimeSteps[1]) |>
    aperm(c('Age', 'TimeStep', 'Fleet'))
  
  FInteract <- ArrayMultiply(apicalFAgeFleet, Selectivity)
  FRetain <- ArrayMultiply(FInteract, Retention)  
  FDiscardTotal <- ArraySubtract(FInteract, FRetain)
  FDiscardDead <- ArrayMultiply(FDiscardTotal, DiscardMortality)
  FDead <- FRetain + FDiscardDead
  
  FDeadTotal <- apply(FDead, c('Age', 'TimeStep'), sum)
  
  ActualApicalF <- apply(FDeadTotal, 'TimeStep', max)
  TSInd <- which(ActualApicalF != apicalF)
  
  # adjust for retention and discard mortality & different selectivity patterns by fleet
  if (length(TSInd)>0) {
    adjust <- apicalF/ActualApicalF
    
    adjust <- matrix(adjust, dd[1], dd[2], byrow=TRUE)
    dimnames(adjust) <- list(Age=dimnames(Selectivity)[[1]],
                             TimeStep=TimeSteps)
    adjust <- replicate(dd[3], adjust)
    names(dimnames(adjust))[3] <- 'Fleet'
    dimnames(adjust)[[3]] <- dimnames(Selectivity)[[3]]
    
    FInteract <- ArrayMultiply(adjust, FInteract)
    FRetain <- ArrayMultiply(FInteract, Retention)  
    FDiscardTotal <- ArraySubtract(FInteract, FRetain)
    FDiscardDead <- ArrayMultiply(FDiscardTotal, DiscardMortality)
    FDead <- FRetain + FDiscardDead
    FDeadTotal <- apply(FDead, c('Age', 'TimeStep'), sum)
    ActualApicalF <- apply(FDeadTotal, 'TimeStep', max)
  }
  
  ZDeadTotal <- ArrayAdd(NaturalMortality, FDeadTotal)
  
  Semelparous <- Stock@Maturity@Semelparous |> ArraySubsetTimeStep(TimeSteps)
  
  
  # TODO SP for spawning biomass ....
  NPR_F <- CalcSurvival(NaturalMortalityAtAge=NaturalMortality, 
                        FishingMortalityAtAge=FDeadTotal, 
                        PlusGroup=Stock@Ages@PlusGroup,
                        SpawnTimeFrac=Stock@SRR@SpawnTimeFrac,
                        Semelparous=Semelparous)
  
  NDead <- ArrayMultiply(NPR_F, (1-exp(-ZDeadTotal)))
  
  # Calc SPR
  SPR0 <- CalcSPR0_Stock(Stock, TimeSteps)
  Fecundity <- Stock@Fecundity@MeanAtAge |> ArraySubsetTimeStep(TimeSteps)
  
  SPR_F <- ArrayMultiply(NPR_F, Fecundity) |> 
    apply('TimeStep', sum)
  SPR_F <- array(SPR_F, dimnames=list(TimeStep=TimeSteps))
  SPR <- ArrayDivide(SPR_F, SPR0)
  
  # Calc Removals and Landings
  ZDeadTotalFleet <- AddDimension(ZDeadTotal, 'Fleet')
  FishingDead <- ArrayDivide(FDead, ZDeadTotalFleet)
  FishingRetain <- ArrayDivide(FRetain, ZDeadTotalFleet)
  
  NDeadFleet <- AddDimension(NDead, 'Fleet') 
  WeightFleet <- Fleet@WeightFleet |> ArraySubsetTimeStep(TimeSteps)
  
  # WeightFleet <- Stock@Weight@MeanAtAge |> ArraySubsetTimeStep(TimeSteps) |>
  #   AddDimension('Fleet')
  
  Removals <- ArrayMultiply(FishingDead, NDeadFleet) |>
    ArrayMultiply(WeightFleet) |> 
    apply(c('TimeStep'), sum) |>
    array(dimnames=list(TimeStep=TimeSteps))
  
  Landings <- ArrayMultiply(FishingRetain, NDeadFleet) |>
    ArrayMultiply(WeightFleet) |> 
    apply(c('TimeStep'), sum) |>
    array(dimnames=list(TimeStep=TimeSteps))
  
  Weight <- Stock@Weight@MeanAtAge |> ArraySubsetTimeStep(TimeSteps)
  BiomassPR <- ArrayMultiply(NPR_F, Weight) |> apply('TimeStep', sum) |>
    array(dimnames=list(TimeStep=TimeSteps))
  
  SBiomassPR <- ArrayMultiply(NPR_F, Fecundity) |> apply('TimeStep', sum) |>
    array(dimnames=list(TimeStep=TimeSteps))
  
  list(F=apicalF,
       SPR=SPR,
       SPR0=SPR0,
       SBiomassPR=SBiomassPR,
       BiomassPR=BiomassPR,
       RemovalsPR=Removals,
       LandingsPR=Landings)
  
}