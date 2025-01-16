# SAFMC Case Study

devtools::load_all()

MOM <- readRDS('../SAFMC-MSE/OM_Objects/BaseCase_RS.OM')
MOM@nsim <- 4

multiHist <- Simulate(MOM)


multiHist[[1]][[1]]@TSdata$Unfished_Equilibrium$N_at_age[1,1,70,] 

multiHist[[1]][[1]]@SampPars$Stock$mov[1,1,,,1]
multiHist[[1]][[1]]@SampPars$Stock$initdist[1,1,] |> round(2)


multiHist[[1]][[1]]@SampPars$Stock$[1,21,] |> round(2)

t <- multiHist[[1]][[1]]@AtAge$Number[1,1,70,]
round(t/sum(t),2)

OM <- Convert(MOM)  # convert from `MOM` to `om`


OM@Stock$`Red Snapper`@Spatial@FracOther
parallel=FALSE
messages='default'
nSim=NULL
silent=FALSE


SimulateDEV

