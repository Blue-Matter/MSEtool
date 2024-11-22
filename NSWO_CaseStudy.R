library(MSEtool)

devtools::load_all()

# ---- Single Stock & Single Fleet (OM) ----

Hist <- Simulate(testOM)

Hist@SampPars$Fleet$Vmaxlen_y

OM <- Convert(testOM)

newtestOM <- Convert(OM)


Populate(OM)
object = OM


# add selectivity model
# add retention model 




SSdir <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/grid_2022/000_base_case'

MOM <- SS2MOM(SSdir=SSdir,nsim=5, Name='North Atlantic Swordfish') 

OM <- Convert(MOM)

object.size(MOM) |> format(units='Mb')
object.size(OM)  |> format(units='Mb')

newMOM <- Convert(OM)
class(newMOM)







Hist <- Simulate(testOM)
Hist1 <- Simulate(OM)

