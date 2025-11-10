
library(MSEtool)

OM <- Convert(testOM)
Hist <- Simulate(OM)

FixedTAC <- function(...) {
  advice <- Advice()
  advice@TAC <- 10000
  advice
}
class(FixedTAC) <- 'mp'

# Make a set of test/demo MPs 
# - finish update Selectivity, etc 

# Test all case study stocks

MSE <- Project_hist(Hist,      
               MPs='FixedTAC')

