library(MSEtool)

devtools::load_all()

# ---- Multi Stock & Multi Fleet (MOM) ----

SSdir <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/grid_2022/000_base_case'

MOM <- SS2MOM(SSdir=SSdir,nsim=5, Name='North Atlantic Swordfish') 

multiHist <- Simulate(MOM)



OM <- Convert(MOM)  # convert from `MOM` to `om`





newMOM <- Convert(OM)  # convert `om` back to `MOM`






