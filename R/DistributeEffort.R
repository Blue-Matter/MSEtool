DistributeEffort <- function(Hist, TimeStep) {
  Density  <- GetDensity(Hist, TimeStep)
  Effort <- GetEffort(Hist, TimeStep) |>
    purrr::map(AddDimension, 'Area')
  EffortDist <- purrr::map2(Effort, Density, ArrayMultiply)
  
  for (st in 1:nStock(Hist)) {
    ArrayFill(Hist@EffortArea[[st]]) <- EffortDist[[st]]
  }
  Hist
}