library(MSEtool)

## ---- multistock-create-om ----
MultiStockOM <- OM(
  Name   = "Multi Stock - Multi Fleet",
  nSim   = 8,
  nYear  = 20,
  pYear  = 30,
  Stock  = list(AlbacoreExStock, ButterfishExStock),
  Fleet  = list(
    list(AsympExFleet, DomeExFleet),
    list(AsympExFleet, DomeExFleet)
  ),
  Obs    = list(
    list(AgeStructuredObs, CommercialFleetObs),
    list(CatchAndSurveyObs, LengthStructuredObs)
  ),
  Imp    = FullComplianceImp
)

## ---- save object ----
usethis::use_data(MultiStockOM, overwrite = TRUE)
