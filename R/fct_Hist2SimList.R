Hist2SimList <- function(Hist) {
  
  # Create a length `nSim` list, each element a `Hist` object with the 
  # `sim` dimension removed from all 
  
  SimList <- purrr::map(1:nSim(Hist@OM), \(x) {
    hist <- SubsetSim(Hist, Sims = x)
    hist@Log$OptDepletionRatio <- SubsetSim(Hist@Log$OptDepletionRatio, x)
    if (length(Hist@Data) < 1) {
      return(hist)
    }
    if (length(Hist@Data) < x) {
      hist@Data <- Hist@Data[[1]]
    } else {
      hist@Data <- Hist@Data[[x]]
    }
    hist
  }, .progress = "Building internal object")
  names(SimList) <- 1:nSim(Hist@OM)

  nstock <- nStock(SimList[[1]]@OM)

  SimList <- purrr::map(SimList, \(HistSim) {
    HistSim@FDeadArea <- purrr::map(HistSim@FDeadArea, Array2List, 2)
    HistSim@FRetainArea <- purrr::map(HistSim@FRetainArea, Array2List, 2)
    HistSim@LandingsAtAge <- purrr::map(HistSim@LandingsAtAge, Array2List, 2)
    HistSim@DiscardsAtAge <- purrr::map(HistSim@DiscardsAtAge, Array2List, 2)

    HistSim@LandingsAtSize <- purrr::map(HistSim@LandingsAtSize, \(stock) {
      purrr::map(stock, \(fleet)
      Array2List(fleet, 2))
    })
    HistSim@DiscardsAtSize <- purrr::map(HistSim@DiscardsAtSize, \(stock) {
      purrr::map(stock, \(fleet)
      Array2List(fleet, 2))
    })

    HistSim@OM@Stock <- purrr::map(HistSim@OM@Stock, \(Stock) {
      Stock@Spatial@Movement <- Array2List(Stock@Spatial@Movement, 4)
      Stock
    })

    HistSim
  }, .progress = "Processing internal object")

  class(SimList) <- "simlist"
  SimList
}
