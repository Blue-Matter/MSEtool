library(MSEtool)

# ---- Example Fleet ----

## ---- create-fleet ----
AsympExFleet <- Fleet("AsympExFleet")


## ---- stochastic effort ----
Effort(AsympExFleet) <- Effort(Effort = data.frame(
  Year = c(0, 0.3, 0.6, 1.0),
  Lower = c(0, 0.4, 1, 1),
  Upper = c(0, 0.6, 1, 1),
  CV = 0.1
))


## ---- selectivity ----
Selectivity(AsympExFleet) <- Selectivity(Pars = list(
  L5 = c(0.4, 0.5),
  LFS = c(0.7, 0.8),
  Vmaxlen = 1
), isRel = TRUE)

usethis::use_data(AsympExFleet, overwrite = TRUE)
