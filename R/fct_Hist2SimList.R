Hist2SimList <- function(Hist) {
  
  SimList <- purrr::map(1:nSim(Hist@OM), \(x) {
    hist <- SubsetSim(Hist, Sim=x, drop=TRUE)
    hist@Log$OptDepletionRatio <- SubsetSim(Hist@Log$OptDepletionRatio, x)
    if (length(Hist@Data)<1)
      return(hist)
    if (length(Hist@Data)<x) {
      hist@Data <- Hist@Data[[1]]
    } else {
      hist@Data <- Hist@Data[[x]]  
    }
    hist
  }, .progress = 'Building internal object')
  names(SimList) <- 1:nSim(Hist@OM)
  
  nstock <- nStock(SimList[[1]]@OM)
  
  SimList <- purrr::map(SimList, \(HistSim) {
    HistSim@FDeadArea <- purrr::map(HistSim@FDeadArea, Array2List, 2)
    HistSim@FRetainArea <- purrr::map(HistSim@FRetainArea, Array2List, 2)
    HistSim@Landings <- purrr::map(HistSim@Landings, Array2List, 2)
    HistSim@Discards <- purrr::map(HistSim@Discards, Array2List, 2)
    
    HistSim@OM@Stock <- purrr::map(HistSim@OM@Stock, \(Stock) {
      Stock@Spatial@Movement <- Array2List(Stock@Spatial@Movement,4)
      Stock
    })
    
    HistSim
  }, .progress = 'Processing internal object')
  
  class(SimList) <- 'simlist'
  SimList
}

