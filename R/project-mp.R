
# TODO
# - apply Bioeconomic to Effort


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
.ProjectMP <- function(Proj,
                       MSE,
                       MPName,
                       MPfunction,
                       mp = 1,
                       YearsHist,
                       YearsProj,
                       silent=FALSE) {

  result <- .ProjectMPCompute(Proj, MPName, MPfunction, YearsHist, YearsProj,
                              StockNames = StockNames(MSE), 
                              FleetNames = FleetNames(MSE), 
                              silent)
  .MergeMPResult(MSE, result, MPName, mp, YearsHist, YearsProj, silent)
}


.ProjectMPCompute <- function(Proj, MPName, MPfunction, YearsHist, YearsProj,
                              StockNames, FleetNames, silent = FALSE) {

  Interval        <- .ResolveInterval(Proj@OM@Interval, MPName, MPfunction)
  ManagementYears <- .CalcManagementYears(YearsProj, Interval)
  YearsAll        <- c(YearsHist, YearsProj)
  Areas           <- 1:nArea(Proj)
  StartTime       <- Sys.time()

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
    .UpdateClosure          = .UpdateClosure,
    .UpdateSelectivity      = .UpdateSelectivity,
    .UpdateRetention        = .UpdateRetention,
    .UpdateDiscardMortality = .UpdateDiscardMortality,
    .UpdateEffort           = .UpdateEffort,
    .UpdateTAC              = .UpdateTAC,
    .UpdateBagLimit         = .UpdateBagLimit
  )

  for (ts in seq_along(YearsProj)) {
    
    Year <- YearsProj[ts]
    if (!silent) cli::cli_progress_update(extra = list(year = Year))

    # Simulate data for the previous time step 
    Proj <- .GenerateProjectionData(Proj, Year, YearsHist, YearsProj)
  
    # Get previous advice
    LastAdviceSimList      <- .GetLastMPAdvice(Proj)
    LastAggBagLimitSimList <- .GetLastMPAggBagLimit(Proj)

    # Data year accounting for lag
    DataYear <- .CalcDataYear(Year     = Year,
                             YearsAll = Proj@Data[[1]][[1]]@Years,
                             DataLag  = Proj@OM@DataLag,
                             Seasons  = Proj@OM@Seasons)

    # Trim Data to `DataYear` if applicable
    DataSimList <- .TrimMPData(Proj, DataYear)

    # Check data exists for every stock/complex
    .CheckMPDataCompleteness(DataSimList, Complexes=Proj@OM@Complexes)

    # Run MP and return nested lists of Advice and aggregate bag limit objects
    MPResult <- .ApplyMP(Year,
                         ManagementYears,
                         LastAdviceSimList,
                         LastAggBagLimitSimList,
                         MPName,
                         MPfunction,
                         DataSimList,
                         Proj,
                         YearsProj,
                         mp,
                         FleetNames,
                         Areas)
    
    AdviceSimList      <- MPResult$AdviceSimList
    AggBagLimitSimList <- MPResult$AggBagLimitSimList

    # If neither TAC or Effort are set, set Effort = 1
    # (keep same as last historical time step - matching
    #  both seasonal and spatial distributions)
    Seasons   <- max(1L, as.integer(Proj@OM@Seasons))
    LHIndLast <- match(max(YearsHist), YearsAll)
    SeasonInd <- ((match(Year, YearsAll) - 1L) %% Seasons) + 1L
    LHInd     <- LHIndLast - Seasons + SeasonInd

    AdviceSimList <- .CheckTACEffort(AdviceSimList,
                                    Proj,
                                    LHInd = LHInd,
                                    FleetNames)

    # Save MP Advice
    Proj <- .StoreMPAdvice(Proj, Year, AdviceSimList, AggBagLimitSimList)
   
    # Save Advice@Misc to Data@Misc for each sim and stock
    Proj@Data <- purrr::map2(Proj@Data, AdviceSimList, \(DataList, AdviceList)
                             .ApplyAdviceMiscToData(DataList, AdviceList)
    )
    
    # Save Advice@Log to Proj@Log for each sim and stock
    ExtractResult <- .ExtractAdviceLogs(AdviceSimList, Proj, Year, MPName)
    Proj <- ExtractResult$Proj

    if (ExtractResult$AllFailed) {
      Error        <- TRUE
      ErrorMessage <- sprintf("MP '%s' failed for all simulations (Year %d)", MPName, Year)
      break
    }
    
    # Save TAC and Effort
    Proj@Data <- purrr::map2(Proj@Data, AdviceSimList,\(DataList, AdviceList) {
      if (!inherits(AdviceList, 'try-error'))
        purrr::map2(DataList, AdviceList, \(Data, Advice) 
                    .AddAdviceToData(Data, Advice, Year)
        )
      }
    )
    
    # Update population dynamics with MP advice
    for (fun_name in names(update_funs)) {
   
      result <- .RunUpdateStep(fun=update_funs[[fun_name]], 
                                fun_name,
                                Proj, Year, AdviceSimList, LastAdviceSimList,
                                YearsHist, YearsProj, Areas, FleetNames, StockNames)

      if (inherits(result, "update_error")) {
        Error        <- TRUE
        ErrorMessage <- sprintf("Error in %s (Year %d): %s",
                                result$step, Year, result$message)
        Proj <- .CaptureLog(Proj,
                          string = ErrorMessage,
                          name = 'UpdateError',
                          type = 'error',
                          year = Year,
                          mp   = MPName)
        break
      }
      Proj <- result
    }
    
    if (Error) break

    # Simulate Pop Dynamics for this Time Step
    Proj <- .CalcFisheryDynamics(Proj, Year, clone=1,
                                 DoBackCalcEffort = .BackCalcEffortFlag(Proj))

    # Compute Catch & Discards at Size for this Time Step
    Proj <- .CalcCatchAtSize(Proj, Years = Year)

  }

  EndTime <- Sys.time()

  list(Proj = Proj, Error = Error, ErrorMessage = ErrorMessage,
       StartTime = StartTime, EndTime = EndTime,
       StockNames = StockNames, FleetNames = FleetNames)
}

# Applies a completed .ProjectMPCompute() result to the shared `MSE` object:
# runs .CheckMSERun()'s console/log reporting, writes the projected `Proj`
# into MSE's `mp` slice via .UpdateMSEObject(), and merges Proj@Log into
# MSE@Log. Kept sequential (called once per MP, in the main process, even
# when the compute step ran in parallel workers) since it mutates the one
# shared `MSE` object and is cheap relative to .ProjectMPCompute().
.MergeMPResult <- function(MSE, result, MPName, mp, YearsHist, YearsProj, silent = FALSE) {

  CheckResult <- .CheckMSERun(result$Proj, MSE, MPName,
                              result$StartTime, result$EndTime,
                              result$Error, result$ErrorMessage,
                              silent = silent)
  Proj  <- CheckResult$Proj
  Error <- result$Error || CheckResult$AllFailed

  if (!Error)
    MSE <- .UpdateMSEObject(MSE,
                           Proj,
                           MPName,
                           mp,
                           YearsHist,
                           YearsProj,
                           result$StockNames,
                           result$FleetNames)

  for (type in c('error', 'warning', 'assumption')) {
    if (is.null(Proj@Log[[type]])) next
    MSE@Log[[type]] <- c(MSE@Log[[type]], Proj@Log[[type]])
  }

  MSE
}


#' Execute a Single Update Step with Error Handling
#'
#' Calls one of the `Update_*` functions within a `tryCatch()` block.
#' If the function throws an error, returns a structured `update_error`
#' object instead of propagating the condition, allowing `.ProjectMP()`
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
.RunUpdateStep <- function(fun, fun_name, Proj, Year, AdviceSimList, LastAdviceSimList,
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




