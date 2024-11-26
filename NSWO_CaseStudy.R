library(MSEtool)

devtools::load_all()

# ---- Single Stock & Single Fleet (OM) ----
testOM@nsim <- 50
OM <- Convert(testOM) # convert from `OM` to `om`

newtestOM <- Convert(OM) # convert `om` back to `OM`


# ---- Multi Stock & Multi Fleet (MOM) ----

SSdir <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/grid_2022/000_base_case'

MOM <- SS2MOM(SSdir=SSdir,nsim=5, Name='North Atlantic Swordfish') 

OM <- Convert(MOM)  # convert from `MOM` to `om`

object.size(MOM) |> format(units='Mb')
object.size(OM)  |> format(units='Mb')

newMOM <- Convert(OM)  # convert `om` back to `MOM`

multiHist <- Simulate(MOM)
multiHist1 <- Simulate(newMOM)





