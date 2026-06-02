library(MSEtool)

MultiStockOM <- OM(
  "Multi Stock - Multi Fleet",
  nSim  = 8,
  nYear = 20,
  pYear = 30
)

Stock(MultiStockOM) <- list(AlbacoreExStock, ButterfishExStock)

Fleet(MultiStockOM) <- list(
  list(AsympExFleet, DomeExFleet),
  list(AsympExFleet, DomeExFleet)
)

Obs(MultiStockOM) <- list(
  list(AgeStructuredObs,    CommercialFleetObs),
  list(CatchAndSurveyObs,   LengthStructuredObs)
)

usethis::use_data(MultiStockOM, overwrite = TRUE)