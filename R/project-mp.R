# TODO
# - make logs informative errors for interactive application of CalcAdvice
#   and store as logged error messages for MSE runs

# TODO
# - apply BioEconomic to Effort


# Project Hist object for a single MP 
Project_MP <- function(Proj,
                       MSE,
                       MPName,
                       MPfunction,
                       mp = 1, 
                       YearsHist, 
                       YearsProj,
                       silent=FALSE) {
  
  # Calc management years/time steps
  ManagementYears <- CalcManagementYears(YearsProj, Proj@OM@Interval)
  
  YearsAll <- c(YearsHist, YearsProj) 
  
  StockNames <- StockNames(MSE)
  FleetNames <- FleetNames(MSE)
  if (is.list(FleetNames))
    FleetNames <- FleetNames[[1]]
  Areas <- 1:nArea(Proj)
  
  Complexes <- Proj@OM@Complexes
  nSim <- Proj@OM@nSim
  
  StartTime <- Sys.time()
  
  # for debugging
  Year <- YearsProj[1]; ts =1;
  
  if (!silent) 
    cli::cli_progress_bar(format = "Projecting MP {.val {MPName}} {cli::pb_bar} {cli::pb_percent}",  total = length(YearsProj))
  
    
  for (ts in seq_along(YearsProj)) {
    
    if (!silent) cli::cli_progress_update()
    
    Year <- YearsProj[ts]
    TSIndex <- match(Year, YearsAll)
    
    # Simulate data for the previous time step 
    Proj <- GenerateProjectionData(Proj, Year, YearsHist, YearsProj)
      
    # Get previous advice
    LastAdviceSimList <- GetLastMPAdvice(Proj) 
    
    # Data year accounting for lag 
    DataYear <- CalcDataYear(Year = Year, 
                             YearsAll = YearsAll, 
                             DataLag = Proj@OM@DataLag,
                             Seasons = Proj@OM@Seasons)
      
    # Trim Data to `DataYear` if applicable
    DataSimList <- TrimMPData(Proj, DataYear)
      
    # Check data exists for every stock/complex
    CheckMPDataCompleteness(DataSimList, Complexes=Proj@OM@Complexes)

    # Run MP and return nested list of Advice objects
    AdviceSimList <- RunMPIfNeeded(Year, 
                                   ManagementYears, 
                                   LastAdviceSimList,
                                   MPName,
                                   MPfunction,
                                   DataSimList,
                                   Proj,
                                   YearsProj,
                                   mp,
                                   FleetNames,
                                   Areas)

    # Save MP Advice 
    Proj <- StoreMPAdvice(Proj, Year, AdviceSimList)
   
    # Save Advice@Misc to Data@Misc for each sim and stock
    Proj@Data <- purrr::map2(Proj@Data, AdviceSimList, ApplyAdviceMiscToData)
    
    # Save Advice@Log to Proj@Log for each sim and stock
    Proj@Log[[as.character(Year)]] <- ExtractAdviceLogs(AdviceSimList)
    
    # Save TAC and Effort
    Proj@Data <- purrr::map2(Proj@Data, AdviceSimList,\(DataList, AdviceList) {
        purrr::map2(DataList, AdviceList, AddAdviceToData, Year = Year)
      }
    )
    
    # Update Pop Dynamics in Proj with MP Advice
    Proj <- Proj |>
      Update_Closure(Year, AdviceSimList, LastAdviceSimList, 
                     YearsProj, Areas, FleetNames, StockNames) |>
      
      Update_Selectivity(Year, AdviceSimList, LastAdviceSimList, 
                         YearsProj, Areas, FleetNames) |>
      
      Update_Retention(Year, AdviceSimList, LastAdviceSimList, 
                       YearsProj, Areas, FleetNames) |>
      
      Update_DiscardMortality(Year, AdviceSimList, LastAdviceSimList, 
                              YearsProj, Areas, FleetNames) |>
      
      Update_Effort(Year, AdviceSimList, LastAdviceSimList, 
                    YearsHist, YearsProj, Areas, FleetNames) |>
      
      Update_TAC(Year, AdviceSimList, LastAdviceSimList, 
                 YearsHist, YearsProj, Areas, FleetNames)
    
    
    # Simulate Pop Dynamics for this Time Step
    Proj <- CalcFisheryDynamics(Proj, Year)
  
  }
  EndTime <- Sys.time()
  
  # Checks
  
  # up to here - why is testOM landings == ?
  Proj@Landings[1,1,,1]
  Proj@Landings[1,1,,1] |> plot()
  AdviceSimList$`1`$Albacore@TAC
  
  
  # update MSE object
  
  
  MSE
}







