library(MSEtool)

devtools::load_all()

# ---- Multi Stock & Multi Fleet (MOM) ----

SSdir <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/grid_2022/000_base_case'

MOM <- SS2MOM(SSdir=SSdir,nsim=3, Name='North Atlantic Swordfish') 

multiHist <- Simulate(MOM)

OM <- Convert(MOM, Populate = FALSE)  # convert from `MOM` to `om`

OM <- Populate(OM)


OM@Fleet[[1]][[2]]@Selectivity@MeanAtAge |> dim()

OM@Fleet[[1]][[1]]@FishingMortality@ApicalF






x <- om@Fleet[[1]][[1]]
t <- ReduceArraysTS(x)


OM@Stock$Female@Length@MeanAtAge
OM@Stock$Female@Weight@MeanAtAge |> dim()

multiHist[[1]][[1]]@Ref$ByYear$F_SPR[1,,1]



array <- OM@Fleet[[1]][[4]]@FishingMortality@ApicalF 




OM@Fleet$Female$SPN_1@FishingMortality@ApicalF



multiHist[[1]][[1]]@Ref$ByYear$SSB0/multiHist[[1]][[1]]@Ref$ByYear$R0


# Unfished Equilibrium

multiHist[[1]][[1]]@TSdata$Unfished_Equilibrium$N_at_age[1,,1,] |> rowSums()
multiHist[[1]][[1]]@TSdata$Unfished_Equilibrium$N_at_age[1,,2,] |> rowSums()


multiHist[[1]][[1]]@Misc$MOM@Allocation[[1]][1,]
multiHist[[1]][[1]]@Misc$MOM@Allocation[[2]][1,]

multiHist[[1]][[1]]@Ref$ReferencePoints |> names()

multiHist[[1]][[1]]@Ref$ByYear |> names()




# Historical

# - Unfished
# - Reference Points
# - Initial Time Step
#   - optimize for initial depletion
# - Optimize catchability given Depletion
# - Simulate Population Dynamics
#   - N-at-age at beginning of next time step - age, growth, and mortality
#   - move N-at-age across areas
#   - calculate B etc by area
#   - calc F distribution by area and fleet
#   - calculate overall F
#   - repeat for all hist time-steps
# - Simulate Fishery Data
# - Return Historical



parallel=FALSE
messages='default'
nSim=NULL
silent=FALSE


SimulateDEV


# Projection 

# TODO
# OM@Allocation - dimension length
# OM@Efactor - dimension length
# Hermaphroditism do in Populate



# newMOM <- Convert(OM)  # convert `om` back to `MOM`


# setClass("hist",
#          slots=c(OM='om',
#                  Unfished='unfished',
#                  RefPoints='list',
#                  BatAge='list',
#                  SBatAge='list',
#                  SPatAge='list',
#                  Misc='list'
#          ),
#          contains='Created_ModifiedClass'
# )
# 
# hist <- new('hist')
# hist@Unfished@Equilibrium@SBatAge

# OM - Operating Model
# Reference Points
# Unfished
# PopulationDynamics
# FleetDynamics


# Hist |> Unfished() |> Equilibrium() |> NatAge() # TODO




