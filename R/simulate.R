#' Simulate Operating Model Dynamics
#'
#' Simulate historical fishery dynamics, fisheries, and reference points
#' for an [om-class] or an [OM-class] object 
#'
#' @param OM An [om-class] or an [OM-class] object. If missing, defaults to [ExampleOM]
#' @param parallel Logical. Should the simulation use parallel processing? Default `FALSE`.
#' @param silent Logical. Suppress progress messages if `TRUE`. Default `FALSE`.
#' @param nSim Integer. Number of simulation replicates (used for `om` objects). Default uses all available replicates in `OM`.
#' @param nsim Integer. Synonym for `nSim`. 
#' @param DoDynamicUnfished Logical. Simulate dynamic unfished population? Default `TRUE`.
#' @param DoRefMSY Logical. Calculate MSY reference points? Default `TRUE`.
#' @param DoRefLandings Logical. Calculate reference landings? Default `TRUE`.
#' @param DoRefRemovals Logical. Calculate reference removals? Default `FALSE`.
#' @param DoConditionObs Logical. Condition observation objects on historical data? Default `TRUE`.
#' @param DoGenerateData Logical. Generate historical data? Default `TRUE`.
#' @param Reduce Logical. Reduce object size after simulation for memory efficiency? Default `TRUE`.
#' @param ... Additional arguments passed to the sub-functions. Not currently used. 
#'
#'
#' @return A [hist-class] or a [Hist-class] object
#'
#' @examples
#' \dontrun{
#' # Simulate a default test OM object
#' simOM <- Simulate()
#'
#' # Using additional arguments
#' simOM <- Simulate(MSEtool::ExampleOM,
#'                   parallel = TRUE,
#'                   DoRefLandings = FALSE,
#'                   DoGenerateData = FALSE)
#' }
#'
#' @export
Simulate <- function(OM=NULL, 
                     parallel = FALSE,
                     silent = FALSE,
                     nSim = NULL,
                     nsim = NULL,
                     DoDynamicUnfished = TRUE,
                     DoRefMSY = TRUE,
                     DoRefLandings = TRUE,
                     DoRefRemovals = FALSE,
                     DoConditionObs = TRUE,
                     DoGenerateData = TRUE,
                     Reduce = TRUE, 
                     ...) {
  
  if (is.null(OM))
    OM <- MSEtool::ExampleOM
  
  if (inherits(OM, 'om'))
    return(
      Simulate_om(OM = OM,
                  parallel = parallel,
                  silent = silent,
                  nSim = nSim,
                  DoDynamicUnfished = DoDynamicUnfished,
                  DoRefMSY = DoRefMSY,
                  DoRefLandings = DoRefLandings,
                  DoRefRemovals = DoRefRemovals,
                  DoConditionObs = DoConditionObs,
                  DoGenerateData = DoGenerateData,
                  Reduce = Reduce, 
                  ...)
      
      )
  
  
  
  if (!is.null(nSim) && is.null(nsim))
    nsim <- nSim
    
  SimulateOM(OM = OM,
             parallel = parallel,
             silent = silent,
             nsim = nsim
             )
}