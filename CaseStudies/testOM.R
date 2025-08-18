
library(MSEtool)

la <- devtools::load_all

la()

OM <- testOM
OM@nsim <- 10

OM <- Convert(OM)

OM@Stock$Albacore@Spatial <- Spatial()
OM@Fleet$Albacore$`Stock:Albacore  Fleet:Generic_Fleet  Obs model:Generic_Obs  Imp model:Perfect_Imp`@Distribution <- Distribution()

OM@Interval <- 1 # TODO test with different
tictoc::tic()
Hist <- SimulateDEV(OM)
tictoc::toc()


FixedTAC1000 <- function(...) {
  advice <- Advice()
  advice@TAC <- 10000
  advice
}

MSE <- ProjectDEV(Hist,
                  MPs='FixedTAC1000')

MSE@Removals[1,1,,1,1] |> round(2)

MSE@Removals[2,1,,1,1] |> round(2)


silent=FALSE
parallel=FALSE






OM2 <- testOM
OM2@nsim <- 100
tictoc::tic()
Hist2 <- Simulate(OM2)
tictoc::toc()


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




