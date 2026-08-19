library(MSEtool)

## ---- herm-create-transition ----
# Cumulative fraction remaining in the female stock by age (starts near 1,
# decreases toward 0 at older ages - see stocktransition-class). A smooth
# logistic transition centred on age 8, mostly complete by age 12.
WrasseAges <- Ages(WrasseFemaleExStock)@MaxAge |> seq(from = 0, by = 1)
WrasseFrac <- array(
  1 / (1 + exp((WrasseAges - 8) / 1.2)),
  dim = c(1, length(WrasseAges)),
  dimnames = list(Sim = 1, Age = WrasseAges)
)

WrasseHerm <- Herm(
  From = "Example Wrasse Female Stock",
  To   = "Example Wrasse Male Stock",
  Frac = WrasseFrac
)

## ---- herm-create-om ----
HermOM <- OM(
  Name   = "Hermaphroditic Wrasse",
  nSim   = 8,
  nYear  = 20,
  pYear  = 30,
  Stock  = list(WrasseFemaleExStock, WrasseMaleExStock),
  Fleet  = list(
    list(AsympExFleet, DomeExFleet),
    list(AsympExFleet, DomeExFleet)
  ),
  Obs    = list(
    list(AgeStructuredObs, CommercialFleetObs),
    list(AgeStructuredObs, CommercialFleetObs)
  ),
  Imp    = FullComplianceImp,
  Herm   = list(WrasseHerm)
)

## ---- save object ----
usethis::use_data(HermOM, overwrite = TRUE)
