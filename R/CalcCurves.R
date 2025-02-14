# NOTE: Curves do not account for spatial closures or MICE interactions


CalcCurves <- function(OM, TimeSteps=NULL, FSearch=NULL) {
  
  if (is.null(FSearch))
    FSearch <- OM@Control$Curves$FSearch
  
  if (is.null(TimeSteps)) 
    TimeSteps <- TimeSteps(OM, 'Historical') |> tail(1)

  Curves <- new('curves')
  Curves@FValues <- FSearch

  # Calculate Number, Spawning, and Yield per Recruit
  PerRecruit <- CalcPerRecruit(OM, FSearch=FSearch, TimeSteps=TimeSteps)
  
  Curves@NPR <- lapply(PerRecruit, '[[','NPR')
  Curves@NPRS <- lapply(PerRecruit, '[[','NPRS')
  Curves@SPR <- lapply(PerRecruit, '[[','SPR')
  Curves@YPR <- lapply(PerRecruit, '[[','RetainPR')
  Curves@RPR <- lapply(PerRecruit, '[[','RemovalPR')
  Curves@RelRec <- CalcRelRec(OM, SPR=Curves@SPR)
  
  # Absolute 
  R0 <- purrr::map(GetR0(OM), AddDimension, name='ApicalF')
  Curves@Recruit <- purrr::map2(R0, Curves@RelRec, ArrayMultiply)
  
  Curves@Yield <- purrr::map2(Curves@YPR, Curves@Recruit, ArrayMultiply)
  Curves@Removal <- purrr::map2(Curves@RPR, Curves@Recruit, ArrayMultiply)
  
  Recruit <- purrr::map(Curves@Recruit, AddDimension, 'Age') |>
    purrr::map(\(x) aperm(x, c(1,4,2,3)))
  
  NumberAtAge <- purrr::map2(Recruit, Curves@NPR, ArrayMultiply)
  SPNumberAtAge <- purrr::map2(Recruit, Curves@NPRS, ArrayMultiply)
  WeightAtAge <- purrr::map(GetWeightAtAge(OM, TimeSteps), AddDimension, 'ApicalF')
  MaturityAtAge <- purrr::map(GetMaturityAtAge(OM, TimeSteps), AddDimension, 'ApicalF')
  FecundityAtAge <- purrr::map(GetFecundityAtAge(OM, TimeSteps), AddDimension, 'ApicalF')
  
  Curves@Biomass <- purrr::map2(NumberAtAge, WeightAtAge, ArrayMultiply) |>
    purrr::map(\(x) apply(x, c(1,3,4), sum))
 
  Curves@SBiomass <- purrr::map2(SPNumberAtAge, WeightAtAge, ArrayMultiply) |>
    purrr::map2(MaturityAtAge, ArrayMultiply) |> 
    purrr::map(\(x) apply(x, c(1,3,4), sum))
  
  Curves@SP <- purrr::map2(SPNumberAtAge, FecundityAtAge, ArrayMultiply) |>
    purrr::map2(MaturityAtAge, ArrayMultiply) |> 
    purrr::map(\(x) apply(x, c(1,3,4), sum))
  Curves
}

