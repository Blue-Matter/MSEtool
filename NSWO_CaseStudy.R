library(MSEtool)

devtools::load_all()

SSdir <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/grid_2022/000_base_case'

MOM <- SS2MOM(SSdir=SSdir,nsim=5) 


OM <- OM2om(MOM)

# TODO - add Data


OM <- Populate(OM)


object.size(MOM) |> format(units='Mb')
object.size(OM)  |> format(units='Mb')



devtools::load_all()





