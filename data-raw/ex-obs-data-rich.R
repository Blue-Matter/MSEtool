library(MSEtool)

DataRichObs <- Obs("DataRichObs")

Landings(DataRichObs) <- CatchObs(
  CV   = c(0.03, 0.08),
  Bias = c(0.98, 1.02)
)

Discards(DataRichObs) <- CatchObs(
  CV   = c(0.10, 0.20),
  Bias = c(0.90, 1.10)
)

Effort(DataRichObs) <- EffortObs(
  CV   = c(0.02, 0.05),
  Bias = c(0.98, 1.02)
)

CPUE(DataRichObs) <- IndicesObs(
  CV = c(0.10, 0.20),
  AC = c(0.0,  0.2)
)

Survey(DataRichObs) <- IndicesObs(
  CV          = c(0.08, 0.15),
  Selectivity = "SBiomass",
  AC          = c(0.0,  0.1)
)

LandingsAtAge(DataRichObs) <- CompObs(
  SampleSize = c(300, 500),
  ESS        = c(80,  150),
  Theta      = c(0.6, 1.0)
)

DiscardsAtAge(DataRichObs) <- CompObs(
  SampleSize = c(150, 250),
  ESS        = c(50,  100),
  Theta      = c(0.5, 0.9)
)

LandingsAtSize(DataRichObs) <- CompObs(
  SampleSize = c(400, 700),
  ESS        = c(100, 180),
  Theta      = c(0.6, 1.0)
)

DiscardsAtSize(DataRichObs) <- CompObs(
  SampleSize = c(200, 350),
  ESS        = c(60,  120),
  Theta      = c(0.5, 0.9)
)

usethis::use_data(DataRichObs, overwrite = TRUE)
