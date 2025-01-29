# TODO 
# add Spat_Targ to Convert - add slot in distribution object
# - replace methods::is with inherits
# test for large number of Sims
# code nSim option properly to reduce sims in all arrays

devtools::load_all()

testOM@nsim <- 5

OM <- testOM
OM <- Convert(OM)

#OM <- Convert(OM, Populate = FALSE)
# OM <- Populate(OM)

parallel=FALSE
messages='default'
nSim=NULL
silent=FALSE


SimulateDEV()


Hist <- Simulate(testOM)


# ---- Reference Points ----

# - F - SPR Curve 

Hist@Ref$ByYear$F_SPR[1,,1]



sim <- 3
age <- 12
ts <- 50
Hist@TSdata$Unfished_Equilibrium$N_at_age[sim,age,ts,]
HistOut@Unfished@Equilibrium@Number[[1]][sim,age,ts,]



stock <- OM@Stock
stock@Length@MeanAtAge[1,,1]
Hist@SampPars$Stock$Len_age[1,,1] 

Hist@SampPars$Stock$initdist[1,1,]
Hist@SampPars$Stock$mov[1,1,,,1]
stock@Spatial@Movement[1,,,1,1]

CalcAsymptoticDist(Hist@SampPars$Stock$mov[1,1,,,1])
CalcAsymptoticDist(stock@Spatial@Movement[1,,,1,1])


OM |> Stock() |> NaturalMortality() |> MeanAtAge() 




