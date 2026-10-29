# Empty obs object
obs <- Obs()
obs

# Obs with catch observation error specified
obs <- Obs(
  Name     = "MyObs",
  Landings = CatchObs(CV = 0.2, Bias = 1.0),
  CPUE     = IndicesObs(CV = 0.3)
)
