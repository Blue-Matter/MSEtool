
# pak::pkg_install('blue-matter/MSEtool@prerelease')

library(MSEtool)

SSDir <- "G:/Shared drives/BM shared/1. Projects/openMSE v2/CaseStudies/BrazilLobster/msetools_lobster/Fitok_Scenario1_LowL_OneGrowth_COMEX_90"

nSim <- 5 # number of simulations - set low for testing
pYear <- 10 # number of projection years

RepList <- ImportSSReport(SSDir)

OM <- ImportSS(SSDir,
               Name='Lobster',
               nSim = nSim,
               pYear = pYear)

Hist <- Simulate(OM)

CompareSS_Number(RepList, Hist)

Hist@OM@Obs$`Female Male`$Fleet3@Landings@Error
Hist@OM@Obs$`Female Male`$Fleet3@Landings@Bias

Data <- Hist@Data$`1`$`Female Male`
Data@Landings@Value
sum(Hist@Landings[1,,70,3])

?Advice



CurrentCatch <- function(Data) {
  LHind <- match(Data@YearLH,Data@Years)
  HistLandings <- Data@Landings@Value[LHind, ]
  HistDiscards <- Data@Discards@Value[LHind,]
  Advice(TAC=HistLandings+HistDiscards)
}
class(CurrentCatch) <- 'mp'

MPs <- 'CurrentCatch'

MSE <- Project(Hist, MPs=MPs)

MSE@Hist@Landings[1,,,3]
MSE@Landings[5,,,,1] |> colSums()

colSums(MSE@Biomass[1,,,1]) |> plot()

MSE@Landings[1,1,,,1]
MSE@Discards[1,1,,,1]

MSE@PPD$CurrentCatch$`1`$`Female Male`@Advice@TAC

Landings(MSE)


# ---------------------- DEBUG ----------------------


la()
LoadArgs(Project_hist)


stop("DEBUG COMMENT BLOCK")

# -------------------- END DEBUG --------------------



