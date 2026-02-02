#' Generate Fishery Data for a single projection time step
#'
#' Internal wrapper function to generate projection fishery data
#' for a single timestep. Existing data will not be replaced with simulated data
#' 
#'
#' @param Proj A `Hist` object populated with fishery dynamics up to `Year`
#' @param Year The 'current' year when the function is being called
#' @param YearsHist Numeric vector of historical years
#' @param YearsProj Numeric vector of projection years
#' 
#' Generates data up to the time-step before `Year`:
#' 
#'  * `YearsAll <- c(YearsHist, YearsProj)`
#'  * `TSIndex <- match(Year, YearsAll) - 1`
#'  * `DataYear <- YearsAll[TSIndex]`
#'
#' @return Updated `Hist` object with `Hist@Data` updated with the simulated data
#'
#' @keywords internal
GenerateProjectionData <- function(Proj, Year, YearsHist, YearsProj) {
  
  YearsAll <- c(YearsHist, YearsProj)
  TSIndex <- match(Year, YearsAll) - 1
  DataYear <- YearsAll[TSIndex]
  
  nSim <- Proj@OM@nSim
  nArea <- nArea(Proj)
  
  FleetNames <- FleetNames(Proj@OM)
  StockNames <- StockNames(Proj@OM)
  
  # replicate if only 1 sim for historical
  if (length(Proj@Data) < nSim) {
    Proj@Data <- replicate(nSim, Proj@Data)
  }
  
  
  
  SimDataList <- purrr::map(1:nSim, \(x)
                            GenerateProjectionData_Sim(x, Hist, HistYears,
                                                        nArea, FleetNames, StockNames, silent, id)
  )
  names(SimDataList) <- 1:nSim
  
  
  
}

GenerateProjectionData_Sim <- function(x, Proj, Year, YearsHist, YearsProj) {
  
  DataList <- Proj@Data[[x]]
  
  Complexes <- Proj@OM@Complexes
  
  
  for (i in seq_along(Complexes)) {
    Data <- DataList[[i]]
                     
    stocks <- Complexes[[i]]
    if (max(Data@Years) >= DataYear) # data already exists for this time step
      next()
    
    
    # update `Year`
    Data@Years <- YearsAll[1:TSIndex]
    
    
    Data@Effort <- GenHistData_Effort(x, Data, Hist, HistYears, i, stocks, FleetNames)
    
  }
  
  
}
