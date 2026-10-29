#' Run a Management Strategy Evaluation
#'
#' The primary user-facing function for running a complete MSE. Simulates
#' historical dynamics with [Simulate()], then projects forward under one or
#' more management procedures (MPs) with [Project()], returning the
#' projection results.
#'
#' Accepts either an operating model ([om-class] or [OM-legacy-class]) or a
#' pre-conditioned historical object ([hist-class] or [Hist-legacy-class]).
#' 
#' When a historical object is provided, [Simulate()] is skipped and the existing
#' historical dynamics are used directly, saving computation time when
#' projecting new MPs.
#'
#' If `OM = NULL`, uses [SingleStockOM] and projects example MPs [CurrentEffort] and 
#' [CurrentCatch] as a quick demonstration. 
#'
#' @param OM An [om-class], [OM-legacy-class], [hist-class], or [Hist-legacy-class] object.
#'   If `NULL` (default), uses [SingleStockOM].
#' @param MPs MPs to project: a character vector of names of MP functions
#'   available in the current environment, or (for [hist-class] objects) a
#'   named list of MP functions, optionally mixed with MP names, e.g.
#'   `list(IR5 = SetMPArgs(IndexRate, CalibYears = 5), 'IndexTarget')`. List
#'   names are used as the MP names. If `NULL` (default), projects
#'   `c("CurrentEffort", "CurrentCatch")`.
#' @param Hist Logical. If `TRUE`, returns the [hist-class] or [Hist-legacy-class]
#'   object from [Simulate()] without running forward projections. Useful for
#'   inspecting historical dynamics or saving the conditioned OM for later use
#'   with [Project()]. Default `FALSE`.
#' @param silent Logical. Suppress progress messages if `TRUE`. Default
#'   `FALSE`.
#' @param parallel Logical, named list, or `"sac"`, controlling parallel
#'   execution. Passed to [Simulate()] and [Project()]; see those functions for
#'   details. Parallel execution for [om-class] and [hist-class] objects
#'   requires a plan established by [SetupParallel()]. A named list is only
#'   accepted for [hist-class], [OM-legacy-class], and [Hist-legacy-class]
#'   objects. For [OM-legacy-class] objects, `"sac"` splits the simulations
#'   into chunks and runs [Simulate()] and [Project()] for each chunk in
#'   parallel. Default `FALSE`.
#' @param extended Logical. If `TRUE`, stores full age- and area-structured
#'   arrays for all years in `MSE@Misc$extended`. Substantially increases
#'   object size. Only used for [OM-legacy-class] and [Hist-legacy-class]
#'   objects. Default `FALSE`.
#' @param checkMPs Logical. Validate MP names and availability before
#'   projecting. Only used for [OM-legacy-class] and [Hist-legacy-class]
#'   objects. Default `FALSE`.
#' @param nSim Integer. Reduce the number of simulations to `nSim`. Passed to
#'   [Simulate()] and [Project()]. Used for [om-class], [hist-class], and
#'   [OM-legacy-class] objects (for the latter only if `nsim` is `NULL`). If
#'   `NULL` (default), all simulations are used.
#' @param nsim Integer. Reduce the number of simulations of an
#'   [OM-legacy-class] object to `nsim` if it is less than `OM@nsim`. Only used
#'   for [OM-legacy-class] objects. Default `NULL`.
#' @param Reduce Logical. Reduce object size after simulation for memory
#'   efficiency? Passed to [Simulate()] and [Project()]. See [ReduceDims()].
#'   Only used for [om-class] and [hist-class] objects. Default `TRUE`.
#'
#' @details
#' `runMSE()` runs [Simulate()] with the default [SimControl()] settings. To
#' control which optional calculations are performed (e.g., MSY or
#' landings-based reference points, observation model conditioning, data
#' generation), call [Simulate()] with `control = SimControl(...)` and pass the
#' result to [Project()].
#'
#' @return
#' - If `Hist = TRUE`: a [hist-class] object for an [om-class] or [hist-class]
#'   input, or a [Hist-legacy-class] object for an [OM-legacy-class] or
#'   [Hist-legacy-class] input.
#' - Otherwise: an [mse-class] object for an [om-class] or [hist-class] input,
#'   or an [MSE-legacy-class] object for an [OM-legacy-class] or
#'   [Hist-legacy-class] input.
#'
#' If an error occurs during [Project()], the historical simulations are
#' returned with a warning.
#'
#' @seealso [Simulate()], [Project()], [OMControl]
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
                   Reduce = TRUE) {
  
  
  if (is.null(OM))
    OM <- MSEtool::SingleStockOM
  
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
  
  if (checkMPs && !Hist && (is_leg_hist || is_leg_om))
    MPs <- CheckMPs(MPs = MPs, silent = silent)
  
  if (is.character(parallel) && parallel == "sac") {
    if (!is_leg_om)
      cli::cli_abort('parallel = "sac" is only supported for {.cls OM} objects.')
    return(runMSE_sac(OM, MPs, Hist = Hist, silent = silent, extended = extended))
  }
  
  if (is_new_om || is_leg_om) {
    HistSims <- Simulate(OM, 
                         parallel = parallel, 
                         silent   = silent, 
                         nSim     = nSim, 
                         nsim     = nsim, 
                         Reduce   = Reduce)
    
  } else {
    # hist or Hist passed directly - use as-is
    HistSims <- OM
  }
  
  if (Hist) {
    if (!silent) cli::cli_alert("Returning historical simulations.")
    return(HistSims)
  }
  

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
