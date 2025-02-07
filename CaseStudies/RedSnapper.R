library(MSEtool)

la <- devtools::load_all

la()

# temporary copy over from existing
MOM <- readRDS('../SAFMC-MSE/OM_Objects/BaseCase_RS.OM')
MOM@nsim <- 2
OM2 <- Convert(MOM)  
spatial <- OM2@Stock$`Red Snapper`@Spatial

OM <- ImportBAM('Red Snapper')

# Add Spatial 
OM@Stock$`SA Red Snapper`@Spatial <- spatial

OM@Fleet$`SA Red Snapper`$cHL@Effort@Catchability

messages='default'
nSim=NULL
parallel=FALSE
silent=FALSE

SimulateDEV


multiHist <- Simulate(MOM)

multiHist$`Red Snapper`$`Commercial Line`@Ref$Dynamic_Unfished$N0[1,1]

multiHist$`Red Snapper`$`Commercial Line`@AtAge$Number[1,,1,]

multiHist$`Red Snapper`$`Commercial Line`@AtAge$Number[1,,1,] |> sum()
