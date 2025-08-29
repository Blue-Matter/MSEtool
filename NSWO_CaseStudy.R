library(MSEtool)

devtools::load_all()

# ---- Multi Stock & Multi Fleet (MOM) ----

SSdir <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/grid_2022/000_base_case'

MOM <- SS2MOM(SSdir=SSdir,nsim=5, Name='North Atlantic Swordfish') 

MOM@SexPars


OM <- Convert(MOM)  # convert from `MOM` to `om`


multiHist <- Simulate(MOM)

# Unfished Equilibrium

multiHist[[1]][[1]]@TSdata$Unfished_Equilibrium$N_at_age[1,,1,] |> rowSums()
multiHist[[1]][[1]]@TSdata$Unfished_Equilibrium$N_at_age[1,,2,] |> rowSums()


multiHist[[1]][[1]]@Misc$MOM@Allocation[[1]][1,]
multiHist[[1]][[1]]@Misc$MOM@Allocation[[2]][1,]

newMOM <- Convert(OM)  # convert `om` back to `MOM`


parallel=FALSE
messages='default'
nSim=NULL
silent=FALSE


SimulateDEV





