#' Project a Hist Object Across Management Procedures
#'
#' Runs the projection loop for one or more management procedures (MPs),
#' returning a completed [mse-class] object with projection results for all
#' MPs.
#'
#' @param Hist A [hist-class] object containing the conditioned operating
#'   model and historical dynamics, as returned by [Simulate()].
#' @param MPs Character vector of MP names to project. If `NULL` (default),
#'   all MPs attached to `Hist` are used. MP names must correspond to
#'   functions available in the current environment. See `.CheckMPClass()` for
#'   validation details.
#' @param parallel Logical. If `TRUE`, projects each MP in parallel using a
#'   `future` plan established by [SetupParallel()]. Errors if
#'   `TRUE` and no parallel plan is active. Default `FALSE`.
#' @param silent Logical. Suppress progress messages if `TRUE`. Default
#'   `FALSE`.
#' @param nSim Integer. If provided, reduces the number of simulations to
#'   `nSim` before projecting. If `NULL` (default), all simulations in `Hist`
#'   are used.
#' @param Reduce Logical. Reduce object size after simulation for memory
#'   efficiency? Default `TRUE`. See [ReduceDims()].
#'
#' @return A [mse-class] object containing projection results for all MPs in
#'   `MPs`.
#'
#' @keywords internal
.ProjectHist <- function(Hist,
                         MPs = NULL, 
                         parallel=FALSE, 
                         silent=FALSE, 
                         nSim=NULL, 
                         Reduce=TRUE) {

  StartTime <- Sys.time()
  Hist <- UpdateObject(Hist)
  
  .OnExit()
  .CheckClass(Hist, 'hist', 'Hist')
  .CheckMPClass(MPs)
  
  YearsHist <- Years(Hist@OM, "Historical")
  YearsProj <- Years(Hist@OM, "Projection")
  nMPs <- length(MPs)
  
  if (!silent) {
    cli::cli_text('')
    cli::cli_alert_info(' Starting `Project` for OM {.val {Hist@OM@Name}}')
  }

  Proj <- Hist |> ReduceNSim(nSim)
  
  Proj <- .CheckAllocation(Proj)
  
  Proj <- .PrepHistMisc(Proj)

  Proj <- .ExtendHist(Proj, Years = c(YearsHist, YearsProj))
  
  Proj <- .CalcFisheryDynamics(Proj, 
                              Years = c(utils::tail(YearsHist,1)), 
                              clone = 1) 
  
  MSE <- .Hist2MSE(Proj, MPNames = MPs)
  SaveLog <- Proj@Log 
  Proj@Log <- list()

  mp <- 1 # initialise for debugging

  if (!silent)
    cli::cli_alert('Projecting {.val {nMPs}} MP{?s}')

  parallel <- CheckParallel(parallel)

  if (parallel && nMPs > 1) {
    CheckPackage('furrr')
    StockNamesMSE <- StockNames(MSE)
    FleetNamesMSE <- FleetNames(MSE)

    results <- furrr::future_map(
      seq_along(MPs), \(mp) {
        .ProjectMPCompute(Proj, MPs[mp], MSE@MPs[[MPs[mp]]], YearsHist, YearsProj,
                         StockNamesMSE, FleetNamesMSE, silent = TRUE)
      },
      .options = furrr::furrr_options(
        globals  = c('Proj', 'MPs', 'MSE', 'YearsHist', 'YearsProj',
                     'StockNamesMSE', 'FleetNamesMSE'),
        packages = "MSEtool",
        seed     = 101
      )
    )

    for (mp in seq_along(MPs))
      MSE <- .MergeMPResult(MSE, results[[mp]], MPs[mp], mp, YearsHist, YearsProj, silent)

  } else {
    for (mp in seq_along(MPs)) {
      MPName <- MPs[mp]
      MPfunction <- MSE@MPs[[MPName]]

      MSE <- .ProjectMP(Proj,
                        MSE,
                        MPName,
                        MPfunction,
                        mp,
                        YearsHist,
                        YearsProj,
                        silent)

    }
  }


  EndTime <- Sys.time()
  elapse_auto <- round(difftime(time1 = EndTime, time2 = StartTime, units = "auto"),2) |> format()
  if (!silent)
    cli::cli_alert_success('Completed {.val Project} for OM {.val {Hist@OM@Name}} ({elapse_auto})') 
  
  MSE <- .RestoreHistMisc(MSE)

  MSE@Log <- .JoinLog(SaveLog, MSE@Log)

  if (!silent)
    .CheckLog(MSE, 'MSE')

  .ReduceMSE(MSE, Reduce) 
}
