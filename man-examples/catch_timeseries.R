Hist <- Simulate(SingleStockOM)
MSE <- Project(Hist, 'CurrentEffort')

# Tidy data frames
Interactions(Hist)
Landings(MSE)
Discards(MSE)
Removals(MSE)

# Retain fleet, age, and area structure
Landings(MSE, byFleet = TRUE, byAge = TRUE, byArea = TRUE)

# Direct slot access for obs and data objects
obs <- Obs(Landings = CatchObs(CV = 0.2))
Landings(obs)
Landings(obs) <- CatchObs(CV = 0.3)
