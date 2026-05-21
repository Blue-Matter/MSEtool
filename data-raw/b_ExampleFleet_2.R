library(MSEtool)

# ---- Example Fleet ----

## ---- create-fleet ----
ExampleFleet2 <- Fleet("Example Fleet 2")


## ---- stochastic effort ----
Effort(ExampleFleet2) <- Effort(Effort = data.frame(
  Year = c(0, 0.3, 0.6, 1.0),
  Lower = c(0, 0.4, 1, 1),
  Upper = c(0, 0.6, 1, 1),
  CV = 0.1
))


## ---- selectivity ----
Selectivity(ExampleFleet2) <- Selectivity(Pars = list(
  L5 = c(0.4, 0.5),
  LFS = c(0.7, 0.8),
  Vmaxlen = 1
), isRel = TRUE)

usethis::use_data(ExampleFleet2, overwrite = TRUE)