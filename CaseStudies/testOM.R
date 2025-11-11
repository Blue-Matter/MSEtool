
library(MSEtool)

OM <- Convert(testOM)
Hist <- Simulate(OM)

FixedTAC <- function(...) {
  advice <- Advice()
  advice@TAC <- 1000
  advice
}
class(FixedTAC) <- 'mp'

FixedEffort <- function(...) {
  advice <- Advice(Effort)
  advice@Effort <- 1
  advice
}
class(FixedEffort) <- 'mp'

# Make a set of test/demo MPs 
# - finish update Selectivity, etc 

# Test all case study stocks

MSE <- Project_hist(Hist,      
               MPs=c('FixedTAC', 'FixedEffort'))

MSE@Effort[1,1,,1,]



