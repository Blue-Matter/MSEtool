#' Calculate Unfished Distribution
#' 
#' @export
CalcUnfishedDist <- function(Spatial,
                             Ages=NULL,
                             Years=NULL) {
  
  Ages <- DefaultAges(Ages)
  Years <- DefaultYears(Years)
  
  dims <- dim(Spatial@Movement)
  if (is.null(dims)) {
    return(Spatial) 
  }
  nSim <- dims[1]
  nArea <- dims[2]

  nAge <- dims[4]
  nYear <- dims[5]
  UnfishedDist <- array(1, dim=c(nSim, nArea, nAge, nYear),
                        dimnames = list(
                          Sim=1:nSim,
                          Area=1:nArea,
                          Age=Ages@Classes[1:nAge],
                          Year=Years[1:nYear]
                        ))
  
  if (nArea==1) {
    Spatial@UnfishedDist <- UnfishedDist
    return(Spatial)
  }
  
  for (s in 1:nSim) {
    for (ts in 1:nYear) {
      for (age in 1:nAge) {
        UnfishedDist[s,,age,ts] <- CalcAsymDist(Movement=Spatial@Movement[s,,,age,ts])
      }
    }
  }
  
  Spatial@UnfishedDist <- UnfishedDist
  Spatial
}
