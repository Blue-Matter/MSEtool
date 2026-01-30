

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
                            GenerateHistoricalData_Sim (x, Hist, HistYears,
                                                        nArea, FleetNames, StockNames, silent, id)
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
 
  Hist
}

GenerateHistoricalData_Sim <- function(x, Hist, HistYears, 
                                       nArea, FleetNames, StockNames, silent=FALSE, id=NULL) {
  
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
    
  
    DataList[[i]] <- Data
  }
  
  DataList
  
}