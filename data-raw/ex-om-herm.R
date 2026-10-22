library(MSEtool)

## ---- herm-create-transition ----
# Cumulative fraction remaining in the female stock by age (starts near 1,
# decreases toward 0 at older ages - see stocktransition-class). A smooth
# logistic transition centred on age 10, mostly complete by age 16.
GrouperAges <- Ages(GrouperFemaleExStock)@MaxAge |> seq(from = 0, by = 1)
GrouperFrac <- array(
  1 / (1 + exp((GrouperAges - 10) / 1.5)),
  dim = c(1, length(GrouperAges)),
  dimnames = list(Sim = 1, Age = GrouperAges)
)

GrouperHerm <- Herm(
  From = "Example Grouper Female Stock",
  To   = "Example Grouper Male Stock",
  Frac = GrouperFrac
)

## ---- herm-create-om ----
HermOM <- OM(
  Name   = "Hermaphroditic Grouper",
  nSim   = 8,
  nYear  = 20,
  pYear  = 30,
  Stock  = list(GrouperFemaleExStock, GrouperMaleExStock),
  Fleet  = list(
    list(AsympExFleet, DomeExFleet),
    list(AsympExFleet, DomeExFleet)
  ),
  Obs    = list(
    list(AgeStructuredObs, CommercialFleetObs)
  ),
  Imp       = FullComplianceImp,
  Herm      = list(GrouperHerm),
  Complexes = list(Grouper = 1:2)
)

## ---- save object ----
usethis::use_data(HermOM, overwrite = TRUE)
