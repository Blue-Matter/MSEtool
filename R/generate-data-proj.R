#' Generate Fishery Data for a single projection time step
#'
#' Internal wrapper function to generate projection fishery data
#' for a single timestep. Existing data will not be replaced with simulated data
#' 
#'
#' @param Proj A `hist` object populated with fishery dynamics up to `Year`
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
#' @return Updated `hist` object with `Hist@Data` updated with the simulated data
#'
#' @keywords internal
.GenerateProjectionData <- function(Proj, Year, YearsHist, YearsProj) {
  
  YearsAll <- c(YearsHist, YearsProj)
  TSIndex <- match(Year, YearsAll) - 1
  DataYear <- YearsAll[TSIndex]
  
  nSim <- Proj@OM@nSim
  nArea <- nArea(Proj)
  
  FleetNames <- FleetNames(Proj@OM)
  StockNames <- StockNames(Proj@OM)
  
  # replicate if only 1 sim for historical
  if (length(Proj@Data) == 1) {
    Proj@Data <- replicate(nSim, Proj@Data)
    names(Proj@Data) <- seq_len(nSim)
  }
  
  SimDataList <- purrr::map(seq_len(nSim), \(x)
                            .GenerateProjectionDataSim(x, 
                                                       Proj, 
                                                       DataYear, 
                                                       YearsAll,
                                                       StockNames,
                                                       FleetNames)
  )
  names(SimDataList) <- seq_len(nSim)
  
  Proj@Data <- SimDataList
  Proj
}

.GenerateProjectionDataSim <- function(x, Proj, DataYear, YearsAll,
                                       StockNames,
                                       FleetNames) {
  
  if (!length(Proj@Data))
    return(NULL)
  
  DataList <- Proj@Data[[x]]
  
  Complexes <- Proj@OM@Complexes
  
  for (i in seq_along(Complexes)) {
    Data   <- DataList[[i]]
    stocks <- Complexes[[i]]
    
    # data already exists for this time step
    if (max(Data@Years) >= DataYear)  next
  
    if (!DataYear %in% Data@Years) 
      Data@Years <- c(Data@Years, DataYear)
    
    # TODO
    # Data@LifeHistory 
    # Data@Exploitation
    
    Data@Effort         <- .GenProjDataEffort(x, Proj, DataYear, YearsAll, i)
    
    Data@Landings       <- .GenProjDataCatch(x, Proj, DataYear, YearsAll, i,
                                             stocks, type = 'Landings')
    
    Data@Discards       <- .GenProjDataCatch(x, Proj, DataYear, YearsAll, i,
                                             stocks, type = 'Discards')
    
    Data@CPUE           <- .GenProjDataIndex(x, Proj, DataYear, YearsAll, i,
                                             stocks, StockNames, FleetNames,
                                             type = 'CPUE')
    
    Data@Survey         <- .GenProjDataIndex(x, Proj, DataYear, YearsAll, i,
                                             stocks, StockNames, FleetNames,
                                             type = 'Survey')
    
    Data@LandingsAtAge  <- .GenProjDataAgeComp(x, Proj, DataYear, YearsAll, i,
                                               stocks,
                                               type = 'LandingsAtAge')
    
    Data@DiscardsAtAge  <- .GenProjDataAgeComp(x, Proj, DataYear, YearsAll, i,
                                               stocks,
                                               type = 'DiscardsAtAge')
    
    Data@LandingsAtSize <- .GenProjDataSizeComp(x, Proj, DataYear, YearsAll, i,
                                                stocks,
                                                type = 'LandingsAtSize')
    
    Data@DiscardsAtSize <- .GenProjDataSizeComp(x, Proj, DataYear, YearsAll, i,
                                                stocks,
                                                type = 'DiscardsAtSize')
    
    DataList[[i]] <- Data
    
  }  # end loop over stocks/complexes
  
  DataList
}



