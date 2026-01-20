#' Depletion class
#'
#' An S4 class representing initial and/or final depletion relative to
#' a reference biomass.
#'
#' @slot Initial   Numeric vector of initial depletion values.
#' @slot Final     Numeric vector of final depletion values.
#' @slot Reference Character scalar identifying the reference biomass.
#'
#' @export
#' @include 00_Class_unions.R
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




