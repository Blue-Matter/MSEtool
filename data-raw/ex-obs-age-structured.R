library(MSEtool)

AgeStructuredObs <- Obs("AgeStructuredObs")

Landings(AgeStructuredObs) <- CatchObs(
  CV   = c(0.05, 0.10),
  Bias = c(0.95, 1.05)
)

Discards(AgeStructuredObs) <- CatchObs(
  CV   = c(0.15, 0.25),
  Bias = c(0.95, 1.05)
)

Survey(AgeStructuredObs) <- IndicesObs(
  CV          = c(0.10, 0.20),
  Selectivity = "SBiomass",
  AC          = c(0.0,  0.1)
)

LandingsAtAge(AgeStructuredObs) <- CompObs(
  SampleSize = c(200, 400),
  ESS        = c(50,  100),
  Theta      = c(0.5, 1.0)
)

DiscardsAtAge(AgeStructuredObs) <- CompObs(
  SampleSize = c(100, 200),
  ESS        = c(30,   80),
  Theta      = c(0.4,  0.8)
)

usethis::use_data(AgeStructuredObs, overwrite = TRUE)