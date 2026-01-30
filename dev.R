library(MSEtool)

la()

testOM@nsim <- 100

OM <- ConvertOM(testOM)


LoadArgs(Simulate_om)


# optimize depletion
# ref yield ..


Hist <- Simulate(testOM)



PopulateCatchObs