#' Run a Management Strategy Evaluation
#'
#' The primary user-facing function for running a complete MSE. Simulates
#' historical dynamics with [Simulate()], then projects forward under one or
#' more management procedures (MPs) with [Project()], returning a completed
#' `MSE` object.
#'
#' Accepts either an operating model ([om-class] or [OM-legacy-class]) or a
#' pre-conditioned historical object ([hist-class] or [Hist-legacy-class]).
#' 
#' When a historical object is provided, [Simulate()] is skipped and the existing
#' historical dynamics are used directly, saving computation time when
#' projecting new MPs.
#'
#' If `OM = NULL`, uses [ExampleOM] and projects example MPs [CurrentEffort] and 
#' [CurrentCatch] as a quick demonstration. 
#'
#' @param OM An [om-class], [OM-legacy-class], [hist-class], or [Hist-legacy-class] object.
#'   If `NULL` (default), uses [ExampleOM].
#' @param MPs Character vector of MP names to project. MPs must be functions
#'   available in the current environment. If `NULL` (default), projects
#'   `c("CurrentEffort", "CurrentCatch")`.
#' @param Hist Logical. If `TRUE`, returns the [hist-class] or [Hist-legacy-class]
#'   object from [Simulate()] without running forward projections. Useful for
#'   inspecting historical dynamics or saving the conditioned OM for later use
#'   with [Project()]. Default `FALSE`.
#' @param silent Logical. Suppress progress messages if `TRUE`. Default
#'   `FALSE`.
#' @param parallel Logical or named list controlling parallel execution of
#'   MPs. For legacy `OM` and `Hist` objects only:
#'   * if `TRUE`, all MPs are run in parallel. 
#'   * if a named list, names correspond to individual MPs to run in parallel. 
#'   * pass `"sac"` to use the SAC parallel backend ([OM-legacy-class] only). 
#'   Default `FALSE`.
#' @param extended Logical. If `TRUE`, stores full age- and area-structured
#'   arrays for all years in `MSE@Misc$extended` (legacy only). Substantially increases
#'   object size. Default `FALSE`.
#' @param checkMPs Logical. Validate MP names and availability before
#'   projecting. Default `FALSE`. Legacy only.
#' @param nSim Integer. Reduce the number of simulations to `nSim`. Passed to
#'   both [Simulate()] and [Project()]. If `NULL` (default), all simulations
#'   are used.
#' @param nsim Integer. Alternative to `nSim` for [OM-legacy-class] objects.
#'   Sets `OM@nsim` before simulating. Default `NULL`.
#' @param DoDynamicUnfished Logical. Calculate dynamic unfished reference
#'   points? Passed to [Simulate()]. Default `TRUE`. `om` and `hist` class only.
#' @param DoRefMSY Logical. Calculate MSY-based reference points? Passed to
#'   [Simulate()]. Default `TRUE`. `om` and `hist` class only.
#' @param DoRefLandings Logical. Calculate landings-based reference points?
#'   Passed to [Simulate()]. Default `TRUE`. `om` and `hist` class only.
#' @param DoRefRemovals Logical. Calculate removals-based reference points?
#'   Passed to [Simulate()]. Default `FALSE`. `om` and `hist` class only.
#' @param DoConditionObs Logical. Condition observation model on historical
#'   data? Passed to [Simulate()]. Default `TRUE`. `om` and `hist` class only.
#' @param DoGenerateData Logical. Generate observed data for MPs? Passed to
#'   [Simulate()]. Default `TRUE`.  `om` and `hist` class only.
#' @param Reduce Logical. Reserved for future use. Default `TRUE`.  `om` and `hist` class only.
#' @param ... Additional arguments. Reserved for future use.
#'
#' @return If `Hist = TRUE`, a [hist-class] or [Hist-legacy-class] object containing
#'   the conditioned historical dynamics. Otherwise, a [MSE-legacy-class] object
#'   containing projection results for all MPs. If an error occurs during
#'   [Project()], the historical simulations are returned with a warning.
#'
#' @seealso [Simulate()], [Project()]
#'
#' @export
runMSE <- function(OM = NULL,
                   MPs = NULL,
                   Hist = FALSE,
                   silent = FALSE,
                   parallel = FALSE,
                   extended = FALSE,
                   checkMPs = FALSE,
                   nSim = NULL,
                   nsim = NULL, 
                   DoDynamicUnfished = TRUE, 
                   DoRefMSY = TRUE,
                   DoRefLandings = TRUE, 
                   DoRefRemovals = FALSE, 
                   DoConditionObs = TRUE,
                   DoGenerateData = TRUE, 
                   Reduce = TRUE, ...) {
  
  # ---- Initial Checks and Setup ----
  
  if (is.null(OM))
    OM <- MSEtool::ExampleOM
  
  if (is.null(MPs))
    MPs <- c('CurrentEffort', 'CurrentCatch')
  
  is_new_om   <- inherits(OM, 'om')
  is_new_hist <- inherits(OM, 'hist')
  is_leg_om   <- inherits(OM, 'OM') && !is_new_om && !is_new_hist
  is_leg_hist <- inherits(OM, 'Hist') && !is_new_hist
  
  if (!is_new_om && !is_new_hist && !is_leg_om && !is_leg_hist)
    cli::cli_abort("You must provide an {.cls om}, {.cls OM}, {.cls hist}, or {.cls Hist} object.")
  
  if (is_leg_om && OM@nsim <= 1)
    cli::cli_abort("{.code OM@nsim} must be greater than 1.")
  
  if (is_leg_hist && !silent)
    cli::cli_inform("Using {.cls Hist} object to reproduce historical dynamics.")
  
  if (is_new_hist && !silent)
    cli::cli_inform("Using {.cls hist} object to reproduce historical dynamics.")
  
  # ---- Check MPs ----
  if (checkMPs && !Hist && (is_leg_hist || is_leg_om))
    MPs <- CheckMPs(MPs = MPs, silent = silent)
  
  # ---- SAC parallel path (legacy OM only) ----
  if (is.character(parallel) && parallel == "sac") {
    if (!is_leg_om)
      cli::cli_abort('parallel = "sac" is only supported for {.cls OM} objects.')
    return(runMSE_sac(OM, MPs, Hist = Hist, silent = silent, extended = extended))
  }
  
  # ---- Run Historical Simulations ----
  if (is_new_om || is_leg_om) {
    HistSims <- Simulate(OM, 
                         parallel = parallel, 
                         silent = silent, 
                         nSim = nSim, 
                         nsim = nsim, 
                         DoDynamicUnfished = DoDynamicUnfished,
                         DoRefMSY = DoRefMSY, 
                         DoRefLandings = DoRefLandings, 
                         DoRefRemovals = DoRefRemovals,
                         DoConditionObs = DoConditionObs, 
                         DoGenerateData = DoGenerateData, 
                         Reduce = Reduce)
    
  } else {
    # hist or Hist passed directly - use as-is
    HistSims <- OM
  }
  
  if (Hist) {
    if (!silent) cli::cli_alert("Returning historical simulations.")
    return(HistSims)
  }
  
  # ---- Run Forward Projections ----
  if (!silent) cli::cli_alert("Running forward projections.")
  
  MSEout <- try(
    Project(Hist     = HistSims,
            MPs      = MPs,
            parallel = parallel,
            silent   = silent,
            nSim     = nSim,
            Reduce   = Reduce,
            extended = extended,
            checkMPs = FALSE),
    silent = TRUE
  )
  
  if (inherits(MSEout, 'try-error')) {
    cli::cli_warn(c(
      "An error occurred during forward projections:",
      "x" = conditionMessage(attr(MSEout, 'condition')),
      "i" = "Returning historical simulations ({.cls {class(HistSims)}}).",
      "i" = "Re-run projections with {.code Project(HistSims, MPs, ...)} to avoid re-simulating."
    ))
    return(HistSims)
  }
  
  MSEout
}