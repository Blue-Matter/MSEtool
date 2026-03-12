#' Project an Operating Model Over the Projection Period
#'
#' Runs the MSE projection loop for one or more management procedures (MPs)
#' and returns a completed [MSE-class] object. Dispatches to the appropriate
#' internal projection engine based on the class of `Hist`: [hist-class]
#' objects use the new `om`-based engine; [Hist-legacy-class] objects use the legacy
#' engine.
#'
#' @param Hist A [hist-class] or [Hist-legacy-class] object containing the
#'   conditioned operating model and historical dynamics, as returned by
#'   [Simulate()].
#' @param MPs Character vector of MP names to project. MPs must be functions
#'   available in the current environment. f `NULL` (default), projects
#'   `c("CurrentEffort", "CurrentCatch")`.
#' @param parallel Logical or named list controlling parallel execution of
#'   MPs. If `TRUE`, all MPs are run in parallel. If a named list, names
#'   correspond to individual MPs to run in parallel. Default `FALSE`.
#' @param silent Logical. Suppress progress messages if `TRUE`. Default
#'   `FALSE`.
#' @param nSim Integer. If provided, reduces the number of simulations to
#'   `nSim` before projecting. Only used for [hist-class] objects. If `NULL`
#'   (default), all simulations in `Hist` are used.
#' @param Reduce Logical. Reserved for future use. Default `TRUE`.
#' @param extended Logical. If `TRUE`, stores full age- and area-structured
#'   arrays for all years in `MSE@Misc$extended`. Only used for [Hist-class]
#'   objects. Substantially increases object size. Default `FALSE`.
#' @param checkMPs Logical. Validate MP names and availability before
#'   projecting. Only used for [Hist-legacy-class] objects. Default `FALSE`.
#'
#' @return An [mse-class] or a [MSE-legacy-class] object containing projection results for all MPs.
#'
#' @seealso [Simulate()], [RunMSE()]
#' 
#' @export
Project <- function(Hist,
                    MPs = NULL, 
                    parallel=FALSE, 
                    silent=FALSE, 
                    nSim=NULL, 
                    Reduce=TRUE,
                    extended=FALSE,
                    checkMPs=FALSE) {
  
  CheckClass(Hist, c('hist', 'Hist'), 'Hist')
  
  if (is.null(MPs))
    MPs <- c('CurrentEffort', 'CurrentCatch')
  
  if (inherits(Hist, 'hist'))
    return(
      Project_hist(Hist,
                   MPs = MPs, 
                   parallel=parallel, 
                   silent=silent, 
                   nSim=nSim, 
                   Reduce=Reduce)
    )
  
  Project_Hist(Hist,
               MPs = MPs, 
               parallel=parallel, 
               silent=silent, 
               extended=extended, 
               checkMPs=checkMPs)
}





  