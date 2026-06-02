library(MSEtool)

TwoFleetOM <- OM(
  "Single Stock - Two Fleets",
  nSim  = 8,
  nYear = 20,
  pYear = 30
)

Stock(TwoFleetOM) <- AlbacoreExStock

Fleet(TwoFleetOM) <- list(
  list(AsympExFleet, DomeExFleet)
)

Obs(TwoFleetOM) <- list(
  list(AgeStructuredObs, CatchAndSurveyObs)
)

usethis::use_data(TwoFleetOM, overwrite = TRUE)