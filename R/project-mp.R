
# TODO
# - apply BioEconomic to Effort
# - improve multi-stock TAC Effort allocation and optimization

#' Project a `Hist` Object for a Single Management Procedure
#'
#' Runs the projection loop for a single MP over all projection years.
#' In each year, data are simulated, the MP is applied (if a management
#' year), advice is stored, population dynamics are updated, and fishery
#' dynamics are simulated. Returns an updated `MSE` object.
#'
#' @param Proj      `Hist` object containing the operating model state.
#' @param MSE       `MSE` object to be updated with projection results.
#' @param MPName    Character. Name of the management procedure.
#' @param MPfunction Function. The MP to apply; must accept a `Data` object
#'                  and return an `Advice` object.
#' @param mp        Integer. Index of the MP within the `MSE` object.
#'                  Default: `1`.
#' @param YearsHist Integer vector. Historical (conditioning) years.
#' @param YearsProj Integer vector. Projection years to iterate over.
#' @param silent    Logical. If `TRUE`, suppresses the progress bar.
#'                  Default: `FALSE`.
#'
#' @return An updated `MSE` object. If an error occurs during projection,
#'   the error is logged and the partial `MSE` object is returned.
#'
#' @keywords internal
Project_MP <- function(Proj,
                       MSE,
                       MPName,
                       MPfunction,
                       mp = 1, 
                       YearsHist, 
                       YearsProj,
                       silent=FALSE) {
  ManagementYears <- CalcManagementYears(YearsProj, Proj@OM@Interval)
  YearsAll   <- c(YearsHist, YearsProj)
  StockNames <- StockNames(MSE)
  FleetNames <- FleetNames(MSE)
  Areas      <- 1:nArea(Proj)
  StartTime  <- Sys.time()
  
  # initialise for debugging convenience
  Year <- YearsProj[1]; ts <- 1
  
  if (!silent)
    cli::cli_progress_bar(
      format = "Projecting MP {.val {MPName}} | Year {.val {cli::pb_extra$year}} ({cli::pb_current}/{cli::pb_total})",
      total  = length(YearsProj),
      extra  = list(year = YearsProj[1])  
    )
  
  Error <- FALSE
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
    
    Year <- YearsProj[ts]
    if (!silent) cli::cli_progress_update(extra = list(year = Year))
    
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
    AdviceSimList <- Apply_MP(Year, 
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
    
    # Check all failed - 
    # TRUE if every sim/stock has a non-NULL log entry (i.e. all failed)
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
    
    # Update population dynamics with MP advice
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


#' Execute a Single Update Step with Error Handling
#'
#' Calls one of the `Update_*` functions within a `tryCatch()` block.
#' If the function throws an error, returns a structured `update_error`
#' object instead of propagating the condition, allowing [Project_MP()]
#' to log the failure and exit the projection loop cleanly.
#'
#' @param fun      Function. One of the `Update_*` population dynamics
#'                 update functions.
#' @param fun_name Character. Name of `fun`, used for error reporting.
#' @param Proj     `Hist` object containing the current operating model state.
#' @param Year     Integer. Current projection year.
#' @param AdviceSimList  Nested list of `Advice` objects for the current year.
#' @param LastAdviceSimList Nested list of `Advice` objects from the previous year.
#' @param YearsHist Integer vector. Historical years.
#' @param YearsProj Integer vector. Projection years.
#' @param Areas     Integer vector. Area indices.
#' @param FleetNames Character vector. Fleet names.
#' @param StockNames Character vector. Stock names.
#'
#' @return Either the updated `Hist` object returned by `fun`, or a list
#'   of class `"update_error"` with elements `step` (character) and
#'   `message` (character) if `fun` throws an error.
#'
#' @keywords internal
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






