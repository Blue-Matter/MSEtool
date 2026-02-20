
# pak::pkg_install('blue-matter/MSEtool@dev')

library(MSEtool)

SSDir <- "G:/Shared drives/BM shared/1. Projects/openMSE v2/CaseStudies/BrazilLobster/MSE_Basecase_red_tail34.6_sp73.1_exp80/MSE_Basecase_red_tail34.6_sp73.1_exp80"

RepList <- ImportSSReport(SSDir)

replist <- RepList$`1`
replist$natage$Seas |> unique() # only one season?

n <- dplyr::filter(replist$natage, Sex == 1, `Beg/Mid` == "B", Era == "VIRG")
n$Seas
n$Morph





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



