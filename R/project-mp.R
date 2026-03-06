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
  YearsAll    <- c(YearsHist, YearsProj) 
  StockNames  <- StockNames(MSE)
  FleetNames  <- FleetNames(MSE)
  Areas       <- 1:nArea(Proj)
  StartTime   <- Sys.time()
  
  # for debugging
  Year <- YearsProj[1]; ts <- 1;
  
  if (!silent) 
    cli::cli_progress_bar(
      format = "Projecting MP {.val {MPName}} {cli::pb_bar} {cli::pb_percent}",
      total = length(YearsProj)
      )
  
  Error        <- FALSE
  ErrorMessage <- NULL
  
  update_funs <- list(
    Update_Closure          = Update_Closure,
    Update_Selectivity      = Update_Selectivity,
    Update_Retention        = Update_Retention,
    Update_DiscardMortality = Update_DiscardMortality,
    Update_Effort           = Update_Effort,
    Update_TAC              = Update_TAC
  )

  for (ts in seq_along(YearsProj)) {
    
    if (!silent) cli::cli_progress_update()
    
    Year <- YearsProj[ts]
    
    # Simulate data for the previous time step 
    Proj <- GenerateProjectionData(Proj, Year, YearsHist, YearsProj)
      
    # Get previous advice
    LastAdviceSimList <- GetLastMPAdvice(Proj) 
    
    # Data year accounting for lag 
    DataYear <- CalcDataYear(Year     = Year, 
                             YearsAll = YearsAll, 
                             DataLag  = Proj@OM@DataLag,
                             Seasons  = Proj@OM@Seasons)
      
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
    Proj@Data <- purrr::map2(Proj@Data, AdviceSimList, \(DataList, AdviceList)
                             ApplyAdviceMiscToData(DataList, AdviceList)
    )
    
    # Save Advice@Log to Proj@Log for each sim and stock
    Proj@Log[[as.character(Year)]] <- ExtractAdviceLogs(AdviceSimList)
    
    # Check all failed 
    AllSimsFailed <- purrr::map(Proj@Log[[as.character(Year)]], \(sim) {
      purrr::map(sim, \(i) !is.null(i))
    }) |> unlist() |> all()
    
    if (AllSimsFailed) break
    
    # Save TAC and Effort
    Proj@Data <- purrr::map2(Proj@Data, AdviceSimList,\(DataList, AdviceList) {
      if (!inherits(AdviceList, 'try-error'))
        purrr::map2(DataList, AdviceList, \(Data, Advice) 
                    AddAdviceToData(Data, Advice, Year)
        )
      }
    )
    
    # Update Pop Dynamics in Proj with MP Advice
    # TODO - this could probably be optimized to avoid the repeated calls to CalcFisheryDynamics
    # and also add parallel processing over simulations
    
    for (fun_name in names(update_funs)) {
   
      result <- run_update_step(fun=update_funs[[fun_name]], 
                                fun_name,
                                Proj, Year, AdviceSimList, LastAdviceSimList,
                                YearsHist, YearsProj, Areas, FleetNames, StockNames)

      if (inherits(result, "update_error")) {
        Error        <- TRUE
        ErrorMessage <- sprintf("Error in %s (Year %d): %s",
                                result$step, Year, result$message)
        Proj@Log[[as.character(Year)]]$UpdateError <- list(
          Step    = result$step,
          Message = result$message
        )
        break
      }
      Proj <- result
    }
    
    if (Error) break
    
    # Simulate Pop Dynamics for this Time Step
    Proj <- CalcFisheryDynamics(Proj, Year, clone=1)
   
  }
  
  EndTime <- Sys.time()
  
  Proj <- CheckMSERun(Proj, MSE, MPName, 
                      StartTime, EndTime, 
                      Error, ErrorMessage)
  
  if (!Error) 
    MSE <- UpdateMSEObject(MSE, 
                           Proj,
                           MPName, 
                           mp, 
                           YearsHist, 
                           YearsProj, 
                           StockNames, 
                           FleetNames)
  
  MSE
}


run_update_step <- function(fun, fun_name, Proj, Year, AdviceSimList, LastAdviceSimList,
                            YearsHist, YearsProj, Areas, FleetNames, StockNames) {
  tryCatch(
    fun(Proj, 
        Year,
        AdviceSimList, 
        LastAdviceSimList,
        YearsHist,
        YearsProj, 
        Areas, 
        FleetNames, 
        StockNames),
    error = function(e) structure(
      list(step = fun_name, message = conditionMessage(e)),
      class = "update_error"
    )
  )
}






