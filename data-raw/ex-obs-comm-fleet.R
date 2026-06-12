library(MSEtool)


## ---- comm-create-obs ----
CommercialFleetObs <- Obs("CommercialFleetObs")

Landings(CommercialFleetObs) <- CatchObs(
  CV   = c(0.05, 0.15),
  Bias = c(1.00, 1.15)
)

Discards(CommercialFleetObs) <- CatchObs(
  CV   = c(0.20, 0.40),
  Bias = c(0.80, 1.20)
)

Effort(CommercialFleetObs) <- EffortObs(
  CV   = c(0.02, 0.08),
  Bias = c(0.95, 1.05)
)

CPUE(CommercialFleetObs) <- IndicesObs(
  CV = c(0.15, 0.25),
  AC = c(0.0,  0.3)
)


## ---- save object ----
usethis::use_data(CommercialFleetObs, overwrite = TRUE)
