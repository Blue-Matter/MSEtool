library(MSEtool)

## ---- two-fleet-create-om ----
TwoFleetOM <- OM(
  Name   = "Single Stock - Two Fleets",
  nSim   = 8,
  nYear  = 20,
  pYear  = 30,
  Stock  = AlbacoreExStock,
  Fleet  = list(
    list(AsympExFleet, DomeExFleet)
  ),
  Obs    = list(
    list(AgeStructuredObs, CatchAndSurveyObs)
  )
)

## ---- save object ----
usethis::use_data(TwoFleetOM, overwrite = TRUE)
