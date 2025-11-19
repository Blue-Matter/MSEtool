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
# - check if catches are really in numbers for some fleets
# - check projected catches under constant effort/catch scenarios - should be on same scale as historical
# - test SALB
# - test NSWO

test <- function(Data) {
  advice <- Advice()
  advice@Effort <- 1
  advice
}
class(test) <- 'mp'

MSE <- Project_hist(Hist, MPs='test')



# Check projected catches - data and real 
# Check conditioning for catch in numbers 
Hist@OM@Obs$`Female Male`$F1_JPN_WCNPO_OSDWCOLL_late_Area1@Landings@Error[1,]
OM@Data$`Female Male`@Landings@Value[,1]

LoadArgs('Project_hist')

replist <- RepList[[1]]

CompareSSNumber(replist, Hist)

CompareSSLandings(replist, Hist)
