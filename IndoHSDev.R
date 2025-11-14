library(MSEtool)

dir <- "C:/Users/Admin/Documents/GitHub/IndonesiaHarvestStrategies"
source(file.path(dir, '0a. Settings.R'))
source(file.path(dir, "0b. HarvestStrategies.r"))

Hist <-  readRDS(file.path(dir, 'Objects_Hist/Lobster.hist'))
OM <- readRDS(file.path(dir, 'Objects_OM/Lobster_FourArea_25.om'))

Hist <- Simulate(OM,
                 RefPointsMSY=Hist@RefPointsMSY,
                 RefLandings=Hist@RefLandings)



LoadArgs('Simulate_om')
Simulate_om

MSE <- Project_hist(Hist, MPs='ML60')


LoadArgs('Project_hist')
MSE <- Project_hist(Hist, MPs='ML60')


la()

OM <- readRDS(file.path(dir, 'Objects_OM/Lobster_FourArea_25.om'))
OM@Stock@Spatial@UnfishedDist
OM@Stock@Spatial@Movement
OM  <- ReduceNSim(OM, nSim=3)
OM@Stock@Spatial@UnfishedDist
OM@Stock@Spatial@FracOther |> dim()

OM <- PopulateOM(OM) |> ReduceNSim(3)
OM@Stock$`Painted Spiny Lobster`@Spatial@RelativeSize

OM@Stock[[st]]@Spatial@UnfishedDist
OM@Stock$`Painted Spiny Lobster`@Spatial@FracOther |> dimnames()
