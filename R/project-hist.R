
#' Project a `Hist` Object Across Multiple Management Procedures
#'
#' Runs the projection loop for all MPs in `MPs`, either sequentially or
#' in parallel across MPs using `furrr`. Returns a completed `MSE` object
#' with results for all MPs.
#'
#' @param Hist     `Hist` object containing the conditioned operating model.
#' @param MPs      Character vector of MP names to project. Default: `NULL`
#'                 (uses all MPs attached to `Hist`).
#' @param parallel Logical. Not used.
#' @param silent   Logical. If `TRUE`, suppresses progress messages.
#'                 Default: `FALSE`.
#' @param nSim     Integer or `NULL`. If provided, reduces the number of
#'                 simulations to `nSim` before projecting. Default: `NULL`
#'                 (use all simulations).
#' @param Reduce   Logical. Reserved for future use. Default: `TRUE`.
#'
#' @return A completed `MSE` object with projection results for all MPs.
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
                              Years=c(tail(YearsHist,1)), 
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
