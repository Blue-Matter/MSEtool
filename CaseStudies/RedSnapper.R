library(MSEtool)

la <- devtools::load_all

la()

OM <- ImportBAM('Red Snapper')



messages='default'
nSim=NULL
parallel=FALSE
silent=FALSE

SimulateDEV


# # temporary copy over from existing
# MOM <- readRDS('../SAFMC-MSE/OM_Objects/BaseCase_RS.OM')
# MOM@nsim <- 2
# 
# OM2 <- Convert(MOM)  
# 
# OM@Stock$`SA Red Snapper`@Spatial <- OM2@Stock$`Red Snapper`@Spatial
# 
# 
