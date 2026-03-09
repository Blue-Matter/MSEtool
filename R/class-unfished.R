#' `unfished` Class
#'
#' Stores unfished equilibrium and dynamic population states, used as the
#' baseline reference for calculating biological reference points.
#'
#' Three accessor functions are provided:
#' - `Unfished()` extracts the `unfished` object from a `hist` or `mse` object
#' - `Equilibrium()` extracts the equilibrium population dynamics
#' - `Dynamic()` extracts the dynamic population dynamics
#'
#' @slot Equilibrium Unfished equilibrium population dynamics.
#'   A [popdynamics-class] object.
#' @slot Dynamic Unfished dynamic population dynamics.
#'   A [popdynamics-class] object.
#' @slot Misc Miscellaneous list for additional outputs.
#'
#' @param object A `hist` or `mse` object.
#' @param unfished An `unfished` object.
#'
#' @return
#' - `Unfished()` returns an `unfished` object
#' - `Equilibrium()` returns a [popdynamics-class] object
#' - `Dynamic()` returns a [popdynamics-class] object
#'
#' @examples
#' \dontrun{
#' unf  <- Unfished(Hist)
#' eq   <- Equilibrium(unf)
#' dyn  <- Dynamic(unf)
#' }
#'
#' @seealso [popdynamics-class]
#' @include class-unions.R
#' @export
#' @name unfished-class
#' @aliases unfished
setClass("unfished",
         slots = c(
           Equilibrium = "popdynamics",
           Dynamic     = "popdynamics",
           Misc        = "list"
         )
)

#' @rdname unfished-class
#' @export
Unfished <- function(object) {
  CheckClass(object, c('hist', 'mse'), 'object')
  AccessSlot(object, 'Unfished')
}

#' @rdname unfished-class
#' @export
Equilibrium <- function(unfished) {
  AccessSlot(unfished, 'Equilibrium')
}

#' @rdname unfished-class
#' @export
Dynamic <- function(unfished) {
  AccessSlot(unfished, 'Dynamic')
}