
#' Internal OM Initilization 
#'
#' Internal helper that initializes an [OM()] object prior to simulation.
#'
#' Models using hermaphroditism are not yet supported and will result
#' in an error.
#'
#' @param OM An [OM()] object.
#' @param nSim Optional integer specifying the maximum number of simulations 
#' to set in the OM (`nSim` must be < `OM@nsim` to have any impact).
#' If `NULL`, the number of simulations is unchanged.
#' @param silent Logical. Currently unused. Included for future support
#'   of suppressed messaging.
#'
#' @return An initialized [OM()] object.
#'
#' @keywords internal
StartUp <- function(OM, nSim=NULL, silent=FALSE) {
  CheckClass(OM)
                 
  if (!is.null(OM@Herm)) {
    stop('Herm not done yet!')
  }
    
  
  OM |> 
    PopulateOM() |>
    ReduceNSim(nSim) 
  
  
  
}