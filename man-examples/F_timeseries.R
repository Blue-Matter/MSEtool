Hist <- Simulate(SingleStockOM)
MSE <- Project(Hist, 'CurrentEffort')

# Raw array slots
FInteract(Hist)
FDead(MSE)
FRetain(MSE)

# Age-structured F, as a tidy data frame (area always included)
FInteract(Hist, df = TRUE)
FDead(MSE, df = TRUE)

# Apical F (max over ages)
FDead(MSE, df = TRUE, byAge = FALSE)

# Retain fleet and area dimensions
FDead(MSE, df = TRUE, byFleet = TRUE)
FDead(MSE, df = TRUE, byAge = FALSE, byArea = TRUE, byFleet = TRUE)
