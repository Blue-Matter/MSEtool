# Biomass CPUE with uniform 30% CV
io <- IndicesObs(CV = 0.3, Units = "Biomass")

# Stochastic CV — unique CV per simulation
io <- IndicesObs(CV = c(0.3, 0.05), Units = "Biomass")

# Spawning biomass survey using maturity as selectivity
survey_obs <- IndicesObs(CV = 0.2, Selectivity = "SBiomass", TruncSD = 3)

# Index covering only areas 1 and 2
io <- IndicesObs(CV = 0.25, Areas = c(1L, 2L))

# Attach to an obs object
obs <- Obs(CPUE   = IndicesObs(CV = 0.3),
           Survey = IndicesObs(CV = 0.2, Selectivity = "SBiomass"))
