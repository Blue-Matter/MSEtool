# Test CalcPopDynamics C++ code

library(callr)
library(MSEtool)


testOM@nsim <- 3

OM <- Convert(testOM)

# ---- Calculate Unfished Dynamics ----
Unfished <- MSEtool:::CalcUnfishedDynamics(OM)

# ---- Make Lists of Arrays ----
PopulationList <- MSEtool:::MakePopulationList(OM, Unfished=Unfished)
FleetList <- MSEtool:::MakeFleetList(OM) # everything with a fleet dimension

# ---- Calculate Reference Points ----
# RefPoints <- CalcRefPoints(OM)

# ---- Number-at-Age at Beginning of Initial Time Step ----
PopulationList <- MSEtool:::CalcInitialTimeStep(PopulationList, Unfished) 


sim <- 1
PopulationListsim <- MSEtool:::MakeSimList(PopulationList, sim)
FleetListsim <- MSEtool:::MakeSimList(FleetList, sim)
TimeSteps <- TimeSteps(OM, 'Historical')

# Biomass
BiomassAreaList <- PopulationListsim$BiomassArea
NumberAtAgeAreaList <- PopulationListsim$NumberAtAgeArea
WeightList <- PopulationListsim$Weight$MeanAtAge

Test <- MSEtool:::CalcBiomass_(BiomassAreaList,
                               NumberAtAgeAreaList,
                               WeightList,
                               TSindex=0)


PopulationListsim$Spatial$Movement$Albacore |> dimnames()
