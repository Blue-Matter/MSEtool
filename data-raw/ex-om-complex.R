library(MSEtool)

## ---- complex-create-om ----
ComplexOM <- OM(
  Name      = "Stock Complex - Single Fleet",
  nSim      = 8,
  nYear     = 20,
  pYear     = 30,
  Stock     = list(AlbacoreExStock, ButterfishExStock),
  Fleet     = AsympExFleet,
  Obs       = AgeStructuredObs,
  Imp       = FullComplianceImp,
  Complexes = list(StockComplex = 1:2)
)

## ---- save object ----
usethis::use_data(ComplexOM, overwrite = TRUE)
