library(MSEtool)

la()

SSDir <- '../WCNPOSWO-2023/Final Base-case'

RepList <- ImportSSReport(SSDir)

# Correct M-at-Age for initial age class
# for some unknown reason, M for age-0 is exactly half the actual value
RepList[[1]]$M_at_age[,4] <- RepList[[1]]$M_at_age[,4] * 2 

OM <- ImportSS(RepList, nSim=2)

Hist <- Simulate(OM, Reduce=FALSE)

# TODO
# - test and fix MP projections
# - test SALB
# - test NSWO

test <- function(Data) {
  advice <- Advice()
  advice@Effort <- 1
  advice
}
class(test) <- 'mp'

MSE <- Project(Hist, MPs='test')


replist <- RepList[[1]]
CompareSSNumber(replist, Hist)

CompareSSLandings(replist, Hist)
