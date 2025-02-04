# NOTE: Curves do not account for spatial closures or MICE interactions


CalcCurves <- function(OM, SPR0=NULL, FSearch=NULL) {
  if (is.null(SPR0)) 
    SPR0 <- CalcSPR0(OM) 
  
  if (is.null(FSearch))
    FSearch <- OM@Control$Curves$FSearch
  
  Curves <- new('curves')
  Curves@FValues <- FSearch
  # per-recruit
  Curves@NPR <- CalcNPR(OM, FSearch=Curves@FValues) 
  Curves@SPR <- CalcSPR(OM, SPR0, NPR=Curves@NPR)
  Curves@SPR$Female |> dim()
  
  Curves@RelRec <- CalcRelRec(OM, SPR=Curves@SPR)
  
  ypr <- CalcYPR(OM, FSearch=Curves@FValues)
  Curves@RPR <- lapply(ypr, '[[', 1)
  Curves@YPR <- lapply(ypr, '[[', 2)
  
  # absolute
  R0 <- purrr::map(GetR0(OM), AddDimension, name='apicalF')
  Curves@Recruit <- purrr::map2(R0, Curves@RelRec, ArrayMultiply)
  
  Curves@Yield <- purrr::map2(Curves@YPR, Curves@Recruit, ArrayMultiply)
  Curves@Removal <- purrr::map2(Curves@RPR, Curves@Recruit, ArrayMultiply)
  
  Recruit <- purrr::map(Curves@Recruit, AddDimension, 'Age') |>
    purrr::map(\(x) aperm(x, c(1,4,2,3)))
  
  NumberAtAge <- purrr::map2(Recruit, Curves@NPR, ArrayMultiply)
  WeightAtAge <- purrr::map(GetWeightAtAge(OM), AddDimension, 'apicalF')
  MaturityAtAge <- purrr::map(GetMaturityAtAge(OM), AddDimension, 'apicalF')
  FecundityAtAge <- purrr::map(GetFecundityAtAge(OM), AddDimension, 'apicalF')
  
  Curves@Biomass <- purrr::map2(NumberAtAge, WeightAtAge, ArrayMultiply) |>
    purrr::map(\(x) apply(x, c(1,3,4), sum))
  
  Curves@SBiomass <- purrr::map2(NumberAtAge, WeightAtAge, ArrayMultiply) |>
    purrr::map2(MaturityAtAge, ArrayMultiply) |> 
    purrr::map(\(x) apply(x, c(1,3,4), sum))
  
  Curves@SP <- purrr::map2(NumberAtAge, WeightAtAge, ArrayMultiply) |>
    purrr::map2(FecundityAtAge, ArrayMultiply) |> 
    purrr::map(\(x) apply(x, c(1,3,4), sum))
  Curves
}

