#' Populate a Depletion Object
#'
#' Populate a `Depletion` object by generating initial and final depletion 
#' values across simulation replicates, and validating the reference biomass.
#'
#' @param Depletion A [Depletion()] object to populate.
#' @param nSim Integer. Number of simulation replicates.
#' @param seed Integer. Random seed used for stochastic generation.
#' @param silent Logical. If `TRUE`, suppress informational messages.
#' @param force Logical. If `TRUE`, force re-population even if the object
#'   digest is unchanged.
#'
#' @details
#' `PopulateDepletion()` handles population of initial and final depletion
#' values. Steps include:
#'
#' * Generating stochastic values if required  for `Initial` and 
#'   `Final` slots.
#' * Validating that the `Reference` slot is one of the accepted types:
#'   `"B0"` or `"SB0"`.
#' * Returning a fully populated `Depletion` object.
#'
#' @return
#' A populated [Depletion()] object.
#'
#' @examples
#' D_pop <- PopulateDepletion(AlbacoreExStock@@Depletion, nSim = 10, seed = 123)
#'
#' @export
PopulateDepletion <- function(Depletion,
                              nSim = 5,
                              seed = NULL,
                              silent = FALSE,
                              force = FALSE) {
  nSim    <- .GetNSim(Depletion, nSim)
  argList <- list(nSim, seed)
  
  if (EmptyObject(Depletion)) 
    return(Depletion)
  
  if (.CheckDigest(Depletion, argList) & !force) 
    return(Depletion)
  
  .CheckInputs(Depletion)
  .CheckSimsNotIncreased(Depletion, nSim)
  .SetSeed(seed)
  
  Depletion@Initial <- .PopulateInitial(Depletion@Initial, nSim)
  Depletion@Final <- .PopulateInitial(Depletion@Final, nSim, "Final")
  .SetDigest(.CheckOutput(Depletion, nSim), argList)
}


.PopulateInitial <- function(Initial, nSim = NA, name = "Initial") {
  if (length(Initial) < 1) {
    return(Initial)
  }
  if (all(is.na(Initial))) {
    return(Initial)
  }
  # already populated per-sim values; don't treat length 2 as bounds
  if ("Sim" %in% names(dimnames(Initial))) {
    return(Initial)
  }

  if (length(Initial) == 1) {
    nSim <- 1
  }
  
  if (length(Initial) == 2 && identical(as.numeric(nSim), 2))
    cli::cli_inform(c("i" = "With {.code nSim = 2}, a length-2 parameter value is used as one value per simulation, not as uniform bounds."),
                    .frequency = "once", .frequency_id = "MSEtool_length2_nSim2")

  if (length(Initial) == 2 && !identical(as.numeric(nSim), 2)) {
    # sample from uniform distribution
    if (is.null(nSim) || is.na(nSim)) {
      cli::cli_abort(c("`nSim` required to generate stochastic values",
                       "i" = "Provide number of simulations to `nSim` argument"
      ))
    }
    Initial <- sort(Initial)
    Initial <- .SimRunif(nSim, Initial[1], Initial[2])
  }
  nSim <- length(Initial)
  array(Initial, dim = nSim, dimnames = list(Sim = 1:nSim))
}
