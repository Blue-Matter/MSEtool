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

  if (!length(Proj@Data)) return(Proj)

  Complexes <- Proj@OM@Complexes

  for (i in seq_along(Complexes)) {
    stocks <- Complexes[[i]]
    Data1  <- Proj@Data[[1]][[i]]

    # data already exists for this time step
    if (max(Data1@Years) >= DataYear) next

    NewYear <- !DataYear %in% Data1@Years

    EffortAll         <- .GenProjDataEffortAll(Proj, DataYear, YearsAll, i, nSim)
    LandingsAll       <- .GenProjDataCatchAll(Proj, DataYear, YearsAll, i, stocks, nSim, type = 'Landings')
    DiscardsAll       <- .GenProjDataCatchAll(Proj, DataYear, YearsAll, i, stocks, nSim, type = 'Discards')
    CPUEAll           <- .GenProjDataIndexAll(Proj, DataYear, YearsAll, i, stocks, StockNames, FleetNames, nSim, type = 'CPUE')
    SurveyAll         <- .GenProjDataIndexAll(Proj, DataYear, YearsAll, i, stocks, StockNames, FleetNames, nSim, type = 'Survey')
    LandingsAtAgeAll  <- .GenProjDataAgeCompAll(Proj, DataYear, YearsAll, i, stocks, nSim, type = 'LandingsAtAge')
    DiscardsAtAgeAll  <- .GenProjDataAgeCompAll(Proj, DataYear, YearsAll, i, stocks, nSim, type = 'DiscardsAtAge')
    LandingsAtSizeAll <- .GenProjDataSizeCompAll(Proj, DataYear, YearsAll, i, stocks, nSim, type = 'LandingsAtSize')
    DiscardsAtSizeAll <- .GenProjDataSizeCompAll(Proj, DataYear, YearsAll, i, stocks, nSim, type = 'DiscardsAtSize')

    for (x in seq_len(nSim)) {
      Data <- Proj@Data[[x]][[i]]

      if (NewYear) Data@Years <- c(Data@Years, DataYear)

      Data@Effort         <- EffortAll[[x]]
      Data@Landings       <- LandingsAll[[x]]
      Data@Discards       <- DiscardsAll[[x]]
      Data@CPUE           <- CPUEAll[[x]]
      Data@Survey         <- SurveyAll[[x]]
      Data@LandingsAtAge  <- LandingsAtAgeAll[[x]]
      Data@DiscardsAtAge  <- DiscardsAtAgeAll[[x]]
      Data@LandingsAtSize <- LandingsAtSizeAll[[x]]
      Data@DiscardsAtSize <- DiscardsAtSizeAll[[x]]

      Proj@Data[[x]][[i]] <- Data
    }
  }  # end loop over stocks/complexes

  Proj
}



