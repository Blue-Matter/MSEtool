# Default multinomial draw: 200 fish sampled, ESS = 50
co <- CompObs(SampleSize = 200, ESS = 50)

# With Dirichlet-Multinomial overdispersion (Theta = 0.5)
co_dm <- CompObs(SampleSize = 200, ESS = 50, Theta = 0.5)

# Stochastic sample size drawn from Uniform(150, 250) across simulations
co_stoch <- CompObs(SampleSize = c(150, 250), ESS = 50)

# Bin-specific shift (e.g. 7 age classes): inflate older ages
co_shift <- CompObs(SampleSize = 200, ESS = 50, Shift = c(-2, -1, 0, 0, 1, 2, 3))

# Attach to an obs object
obs <- Obs(
  LandingsAtAge  = CompObs(SampleSize = 200, ESS = 50),
  LandingsAtSize = CompObs(SampleSize = 150, ESS = 40, Theta = 0.5)
)
