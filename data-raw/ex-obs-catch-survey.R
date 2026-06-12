library(MSEtool)

## ---- catch-survey-create-obs ----
CatchAndSurveyObs <- Obs("CatchAndSurveyObs")

Landings(CatchAndSurveyObs) <- CatchObs(
  CV   = c(0.10, 0.25),
  Bias = c(0.80, 1.00)
)

Survey(CatchAndSurveyObs) <- IndicesObs(
  CV          = c(0.15, 0.30),
  Selectivity = "Biomass",
  AC          = c(0.0,  0.2)
)

## ---- save object ----
usethis::use_data(CatchAndSurveyObs, overwrite = TRUE)
