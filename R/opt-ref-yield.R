

CalcRefLandings <- function(Hist, 
                            IdenticalSim=FALSE,
                            type=c('Landings', 'Removals')) {
  type <- match.arg(type, c('Landings', 'Removals'))
  
  
  if (IdenticalSim) {
    Hist_1 <- Subset(Hist, Sims=1) |> ExtendHist()
    
  }
  
  
  
  
}