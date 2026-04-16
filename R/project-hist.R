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
#'   functions available in the current environment. See [CheckMPClass()] for
#'   validation details.
#' @param parallel Logical. Not currently used. Default `FALSE`. 
#' @param silent Logical. Suppress progress messages if `TRUE`. Default
#'   `FALSE`.
#' @param nSim Integer. If provided, reduces the number of simulations to
#'   `nSim` before projecting. If `NULL` (default), all simulations in `Hist`
#'   are used.
#' @param Reduce Logical. Reserved for future use. Default `TRUE`.
#'
#' @return A [mse-class] object containing projection results for all MPs in
#'   `MPs`.
#'
#' @keywords internal
Project_hist <- function(Hist,
                         MPs = NULL, 
                         parallel=FALSE, 
                         silent=FALSE, 
                         nSim=NULL, 
                         Reduce=TRUE) {

  # ---- Initial Checks and Setup ----
  StartTime <- Sys.time()
  Hist <- UpdateObject(Hist)
  
  OnExit()
  CheckClass(Hist, 'hist', 'Hist')
  CheckMPClass(MPs)
  
  YearsHist <- Years(Hist@OM, "Historical")
  YearsProj <- Years(Hist@OM, "Projection")
  nMPs <- length(MPs)
  
  if (!silent) {
    cli::cli_text('')
    cli::cli_alert_info(' Starting  {.val Project} for OM {.val {Hist@OM@Name}}')
  }

  # ---- Reduce nSim if provided ----
  Proj <- Hist |> ReduceNSim(nSim)
  
  # ---- Add temporary lists and arrays to Hist@Misc ----
  Proj <- PrepHistMisc(Proj)

  # ---- Extend Arrays with Projection Years ----
  Proj <- ExtendHist(Proj, Years = c(YearsHist, YearsProj))
  
  # ---- Populate Number-at-Age at Beginning of Projection Year ----
  Proj <- CalcFisheryDynamics(Proj, 
                              Years=c(utils::tail(YearsHist,1)), 
                              clone=1) 
  
  # ---- Create MSE Object ----
  MSE <- Hist2MSE(Proj, MPNames = MPs)

  # ---- Project MPs ----
  mp <- 1 # initialise for debugging
  
  if (!silent) 
    cli::cli_alert('Projecting {.val {nMPs}} MP{?s}')
  
  for (mp in seq_along(MPs)) {
    MPName <- MPs[mp]
    MPfunction <- MSE@MPs[[MPName]]
    
    MSE <- Project_MP(Proj, 
                      MSE,
                      MPName,
                      MPfunction,
                      mp,
                      YearsHist,
                      YearsProj,
                      silent)
    
  }

  EndTime <- Sys.time()
  elapse_auto <- round(difftime(time1 = EndTime, time2 = StartTime, units = "auto"),2) |> format()
  if (!silent)
    cli::cli_alert_success('Completed {.val Project} for OM {.val {Hist@OM@Name}} ({elapse_auto})') 
  
  MSE <- RestoreHistMisc(MSE)
  
  MSE
}
