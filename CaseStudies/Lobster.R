
# pak::pkg_install('blue-matter/MSEtool@prerelease')

library(MSEtool)

SSDir <- "G:/Shared drives/BM shared/1. Projects/openMSE v2/CaseStudies/BrazilLobster/msetools_lobster/Fitok_Scenario1_LowL_OneGrowth_COMEX_90"

nSim <- 5 # number of simulations - set low for testing
pYear <- 10 # number of projection years

OM <- ImportSS(SSDir,
               Name='Lobster',
               nSim = nSim,
               pYear = pYear)

Hist <- Simulate(OM)

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


# ---------------------- DEBUG ----------------------


la()
LoadArgs(Project_hist)


stop("DEBUG COMMENT BLOCK")

# -------------------- END DEBUG --------------------



