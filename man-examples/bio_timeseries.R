Hist <- Simulate(SingleStockOM)
MSE <- Project(Hist, 'CurrentEffort')

# Tidy data frames
Biomass(Hist, df = TRUE)
Biomass(MSE,  df = TRUE)

# Retain age and area structure
Biomass(MSE, df = TRUE, byAge = TRUE, byArea = TRUE)

# Spawning biomass and production follow the same structure
SBiomass(MSE, df = TRUE, byAge = TRUE)
SProduction(MSE, df = TRUE, byArea = TRUE)
