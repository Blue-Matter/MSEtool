
#' Depletion Class and Constructor
#'
#' The `Depletion` class defines the depletion state of a stock at the beginning
#' and end of the historical period.
#'
#'
#' @param x
#' * For `Depletion()`: missing, numeric, or a [Stock()] object.
#' * For `Depletion<-`: a [Stock()] object.
#'
#' @param Initial Numeric vector or array specifying depletion at the first
#'   historical time step, expressed relative to `Reference`.
#'
#' @param Final Numeric vector or array specifying depletion at the final
#'   historical time step, expressed relative to `Reference`.
#'
#' @param Reference Reference biomass used to calculate depletion. A
#'  character string (`"B0"` or `"BMSY"`) 
#'
#' @param value A `Depletion` object to assign to a [Stock()] object.
#'
#' @details
#' 
#' The `Depletion` generic is used to:
#' * construct new `Depletion` objects;
#' * access `Depletion` from a [Stock()] object;
#' * assign a `Depletion` object to a [Stock()] object.
#'
#' Depletion is defined as stock biomass divided by a reference biomass.
#' Lower values indicate a more depleted stock relative to its reference state.
#'
#' ## Initial depletion
#'
#' If `Initial` is not supplied, the stock is assumed to be unfished at the
#' beginning of the historical period.
#'
#' When `Initial` is provided, model initialization adjusts early recruitment
#' deviations so that biomass relative to `Reference` in the first time step
#' matches the specified value.
#'
#' ## Final depletion
#'
#' `Final` specifies the depletion level in the last historical time step and is
#' typically used as a constraint or tuning target during model fitting.
#'
#' ## Reference biomass
#'
#' `Reference` defines the biomass used to scale depletion:
#'
#' * `"B0"`: equilibrium unfished biomass (default);
#' * `"BMSY"`: equilibrium biomass at maximum sustainable yield;
#'
#' @return
#' * `Depletion()`: returns an empty `Depletion` object.
#' * `Depletion(x)`: returns the `Depletion` object from a [Stock()] object.
#' * `Depletion(x) <- value`: returns the modified [Stock()] object.
#'
#' @seealso [Stock()]
#'
#' @name Depletion
#' @rdname Depletion
#'
#' @examples
#' Depletion()
#'
#' Depletion(Initial = 0.8, Final = 0.3)
#'
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
NULL

setClass('depletion',
         slots=c(Initial='num.array.null',
                 Final='num.array.null',
                 Reference='array.char.null',
                 Misc='list')
)

setValidity('depletion', function(object) {
  # TODO 
  TRUE
})

setMethod("initialize", "depletion", function(.Object,
                                              Initial=numeric(),
                                              Final=numeric(),
                                              Reference='B0') {
  .Object@Initial <- Initial
  .Object@Final <- Final
  .Object@Reference <- Reference
  .Object
})



