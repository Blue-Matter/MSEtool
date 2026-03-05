#' Depletion 
#'
#' Construct and manipulate a [depletion-class] object defining depletion assumptions
#' for a [Stock()] object.
#'
#' @param Initial Numeric initial depletion (optional).
#' @param Final Numeric final depletion (optional).
#' @param Reference Character reference biomass used to calculate depletion.
#'   One of `"B0"` (default) or `"BMSY"`.
#' @param Stock A [Stock()] object.
#' @param x A `depletion` object.
#' @param value Replacement value.
#'
#' @details
#' The `Depletion` class defines the depletion state of a [Stock()] at the
#' beginning and end of the historical period.
#'
#' It is optional and only required if users wish to manually set the depletion
#' for a given [Stock()] object in the initial or terminal historical year.
#'
#' Printing a `Depletion` object provides a concise summary of the specified
#' initial and final depletion assumptions.
#'
#' ## Initial
#'
#' If `Initial` is not supplied, the stock is assumed to be unfished at the
#' beginning of the historical period.
#'
#' When `Initial` is provided, model initialization adjusts early recruitment
#' deviations so that biomass relative to `Reference` in the first time step
#' matches the specified value.
#'
#' Valid inputs:
#' * numeric length 1: constant over all simulations
#' * numeric length `nSim`: simulation-specific values
#' * numeric length 2: lower and upper bounds of a uniform distribution from
#'   which `nSim` values will be sampled
#'
#' ## Final
#'
#' If `Final` is not supplied, depletion in the terminal year is determined by
#' the interaction between [Stock()] and [Fleet()] parameters used to simulate
#' the historical fishery. In this case, [Catchability()] must be populated.
#'
#' If `Final` is supplied, the model optimizes `Efficiency` in the
#' [Catchability()] object (ignoring existing values) to force the model to
#' achieve the specified depletion level, if possible.
#'
#' Valid inputs for `Final` match those for `Initial`.
#'
#' ## Reference
#'
#' `Reference` defines the biomass used to scale depletion:
#'
#' * `"B0"`: equilibrium unfished biomass (default)
#' * `"BMSY"`: equilibrium biomass at maximum sustainable yield
#'
#' ## Accessors and assignment
#'
#' - `GetDepletion()` / `SetDepletion()` retrieve or assign the `Depletion`
#'   component of a [Stock()] object.
#' - `Initial()`, `Final()`, and `Reference()` access individual components of a
#'   `depletion` object.
#' - Replacement functions (e.g. `Initial<-`) update the corresponding component
#'   and validate the object.
#'
#' @return A [depletion-class] object.
#'
#' @seealso [Populate()]
#'
#' @example man-examples/class-Depletion.R
#' @export
Depletion <- function(Initial,
                      Final,
                      Reference = "B0") {
  
  
  
  if (missing(Initial)) {
    object <- methods::new("depletion")
    object@Reference <- Reference
    return(object)
  }
  
  if (inherits(Initial, 'stock'))
    return(Initial@Depletion)
  
  
  methods::new("depletion",
               Initial = if (missing(Initial)) numeric() else Initial,
               Final = if (missing(Final))   numeric() else Final,
               Reference = Reference)
}



#' @rdname Depletion
#' @export
Initial <- function(x) {
  CheckClass(x, "depletion", "Depletion")
  x@Initial
}


#' @rdname Depletion
#' @export
`Initial<-` <- function(x, value) {
  CheckClass(x, "depletion", "Depletion")
  x@Initial <- value
  methods::validObject(x)
  x
}


#' @rdname Depletion
#' @export
Final <- function(x) {
  CheckClass(x, "depletion", "Depletion")
  x@Final
}


#' @rdname Depletion
#' @export
`Final<-` <- function(x, value) {
  CheckClass(x, "depletion", "Depletion")
  x@Final <- value
  methods::validObject(x)
  x
}

#' @rdname Depletion
#' @export
`Depletion<-`<- function(x, value) {
  CheckClass(x, "stock", "x")
  x@Depletion <- value
  methods::validObject(x)
  x
}
