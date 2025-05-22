
library(MSEtool)

la <- devtools::load_all

la()

OM <- testOM
OM@nsim <- 100

OM <- Convert(OM)




parallel=FALSE
messages='default'
nSim=NULL
silent=FALSE

# SimulateDEV

Stock <- OM@Stock$Albacore

MeanAtAge = Stock@Length@MeanAtAge
CVatAge <- Stock@Length@CVatAge
Classes <- Stock@Length@Classes
TruncSD=2
Dist='normal'
Ages=NULL
silent=FALSE
type='Length'



CalcASK(Stock@Length)




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




