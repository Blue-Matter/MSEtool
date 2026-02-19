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
    
    # TODO - this can be optimized to avoid the repeated calls to CalcFisheryDynamics
    
    # Update Pop Dynamics in Proj with MP Advice
    update_steps <- list(
      Update_Closure,
      Update_Selectivity,
      Update_Retention,
      Update_DiscardMortality,
      Update_Effort,
      Update_TAC
    )
    
    Error <- FALSE
    ErrorMessage <- NULL
    for (fun in update_steps) {
      
      tmp <- try(
        fun(
          Proj,
          Year,
          AdviceSimList,
          LastAdviceSimList,
          YearsHist = YearsHist,
          YearsProj = YearsProj,
          Areas = Areas,
          FleetNames = FleetNames,
          StockNames = StockNames
        ),
        silent = TRUE
      )
      
      if (inherits(tmp, "try-error")) {
        Error <- TRUE
        ErrorMessage <- tmp
        
        # Proj@Log[[as.character(Year)]]$UpdateError <- list(
        #   Step = deparse(substitute(fun)),
        #   Message = as.character(tmp)
        # )
        
        # if (!silent) {
        #   cli::cli_alert_danger(
        #     "Update step failed in year {Year}: {deparse(substitute(fun))}"
        #   )
        # }
        
        break
      }
      
      Proj <- tmp
    }
    
    if (Error)
      break
    

    # Simulate Pop Dynamics for this Time Step
    Proj <- CalcFisheryDynamics(Proj, Year)
    
  }
  
  EndTime <- Sys.time()
  
  Proj <- CheckMSERun(Proj, MSE, MPName, StartTime, EndTime, Error, ErrorMessage)
  
  if (!Error) 
    MSE <- UpdateMSEObject(MSE, Proj, MPName, mp,YearsHist, YearsProj, StockNames, FleetNames)
  
  MSE
}







