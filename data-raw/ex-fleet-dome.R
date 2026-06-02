
library(MSEtool)

# ---- Example Fleet ----

## ---- create-fleet ----
DomeExFleet <- Fleet("DomeExFleet")


## ---- stochastic effort ----
Effort(DomeExFleet) <- Effort(Effort = data.frame(
  Year = c(0, 0.3, 0.6, 1.0),
  Lower = c(0, 0.4, 0.4, 1),
  Upper = c(0, 0.6, 0.6, 1),
  CV = 0.1
))


## ---- selectivity ----
Selectivity(DomeExFleet) <- Selectivity(Pars = list(
  L5 = c(0.2, 0.4),
  LFS = c(0.75, 1.1),
  Vmaxlen = c(0.5, 1)
), isRel = TRUE)

usethis::use_data(DomeExFleet, overwrite = TRUE)
