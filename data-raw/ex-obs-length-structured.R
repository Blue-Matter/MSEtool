library(MSEtool)

LengthStructuredObs <- Obs("LengthStructuredObs")

Landings(LengthStructuredObs) <- CatchObs(
  CV   = c(0.10, 0.20),
  Bias = c(0.90, 1.05)
)

Discards(LengthStructuredObs) <- CatchObs(
  CV   = c(0.15, 0.25),
  Bias = c(0.95, 1.05)
)


Survey(LengthStructuredObs) <- IndicesObs(
  CV          = c(0.15, 0.30),
  Selectivity = "Biomass",
  AC          = c(0.0,  0.2)
)

LandingsAtSize(LengthStructuredObs) <- CompObs(
  SampleSize = c(300, 600),
  ESS        = c(60,  120),
  Theta      = c(0.5, 1.0)
)


DiscardsAtSize(LengthStructuredObs) <- CompObs(
  SampleSize = c(150, 300),
  ESS        = c(40,   80),
  Theta      = c(0.4,  0.8)
)

usethis::use_data(LengthStructuredObs, overwrite = TRUE)

