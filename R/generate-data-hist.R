
#' Generate Historical Fishery Data
#'
#' Internal wrapper function to generate historical fishery data
#' for all simulation replicates in an operating model history object.
#'
#' Iterates over simulation replicates, generating effort, landings,
#' discards, CPUE, and survey data for each stock or stock complex. If all
#' simulation replicates produce identical data, only the first
#' replicate is returned.
#'
#'
#' @param Hist `Hist` object populated with historical fishery dynamics
#' @param silent Logical; if `TRUE`, suppress progress bars and status messages
#'
#' @return Updated `Hist` object with `Hist@Data` containing simulated historical data
#'
#' @keywords internal
GenerateHistoricalData <- function(Hist, silent=FALSE) {
  
  HistYears <- Years(Hist,'H')
  nSim <- Hist@OM@nSim
  nArea <- nArea(Hist)

  FleetNames <- FleetNames(Hist@OM)
  StockNames <- StockNames(Hist@OM)
  
  id <- NULL
  if (!silent) 
    id <- cli::cli_progress_bar("Generating Historical {.val Data}")
  
  SimDataList <- purrr::map(1:nSim, \(x)
                            GenerateHistoricalData_Sim(x, Hist, HistYears,
                                                        nArea, FleetNames,
                                                       StockNames, silent, id)
                            )
  names(SimDataList) <- 1:nSim
  
 
  # Check if all `nSim` data objects are identical and if so, only return sim 1
  identical <- rep(TRUE, nSim)
  for (sim in 2:nSim) {
    identical[sim] <- IdenticalS4(SimDataList[[1]], SimDataList[[sim]])
  }
  
  if(prod(identical)) {
    Hist@Data <- list("1"= SimDataList[[1]])
  } else {
    Hist@Data <- SimDataList
  }
  
  if (!silent)
    cli::cli_alert_success("Generated Historical {.val Data}")
 
  Hist
}

#' Generate Historical Data for a Single Simulation
#'
#' Internal function to generate historical data for a single simulation
#' replicate, including effort, catch (landings and discards), and indices
#' (CPUE and survey) for each stock complex.
#'
#' Applies the appropriate observation bias, error, and selectivity for
#' each data type and updates metadata such as names, years, and seasons.
#'
#' @param x Integer index of the simulation replicate
#' @param Hist Operating model history object
#' @param HistYears Numeric vector of historical years
#' @param nArea Integer number of spatial areas
#' @param FleetNames Character vector of fleet names
#' @param StockNames Character vector of stock names
#' @param silent Logical; if `TRUE`, suppress progress bars and status messages
#' @param id Optional progress bar ID from `cli::cli_progress_bar`
#'
#' @return A list of `data` objects for each stock complex, containing
#'   simulated historical values for effort, catch, and indices
#'
#' @keywords internal
GenerateHistoricalData_Sim <- function(x, Hist, HistYears, 
                                       nArea, FleetNames, StockNames, 
                                       silent=FALSE, 
                                       id=NULL) {
  
  OM <- Hist@OM
  Complexes <- Hist@OM@Complexes
  
  if (!is.null(Hist@OM@Data)) {
    # real data exists
   DataList <- Hist@OM@Data
  } else {
    # make Data object for each stock complex
    DataList <- MakeNamedList(names(Complexes), new('data'))  
  }
  
  nStockData <- length(DataList)
  
  for (i in seq_len(nStockData)) {
    
    if (!silent) cli::cli_progress_update(id=id)

    stocks <- Complexes[[i]]
    Data <- DataList[[i]]
    Data@nArea <- nArea
    
    # Effort 
    Data@Effort <- GenHistData_Effort(x, Data, Hist, HistYears, i, stocks, FleetNames)
    
    # Landings
    Data@Landings <- GenHistData_Catch(x, Data, Hist, HistYears, i, stocks, FleetNames)
    
    # Discards
    Data@Discards <- GenHistData_Catch(x, Data, Hist, HistYears, i, stocks, FleetNames,
                                       type='Discards')
    
    # CPUE
    Data@CPUE <- GenHistData_Indices(x, Data, Hist, HistYears, i, stocks, StockNames,
                                     nArea)
    
    # Survey
    Data@Survey <-  GenHistData_Indices(x, Data, Hist, HistYears, i, stocks, StockNames,
                                        nArea, type='Survey')
    
    

    
    # CAA - TODO
    
    # CAL - TODO
    
    # Life-History - TODO
    
  
    # Metadata
    
    if (is.null(Data@Name)) 
      Data@Name <- purrr::map(Hist@OM@Stock[stocks], slot, 'Name') |> unlist() |> paste(collapse='-')
    
    if (is.null(Data@Years)) 
      Data@Years <- HistYears
    
    if (is.null(Data@Seasons)) 
      Data@Seasons <- Hist@OM@Seasons
    
    if (is.null(Data@YearLH)) 
      Data@YearLH <- Data@Years[length(Data@Years)]
    
    # Add Pop Dyn if specified 
    Data <- AddPopDyn(Data, Hist, x)
    
  
    DataList[[i]] <- Data
  }
  
  DataList
  
}