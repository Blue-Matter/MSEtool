#' Depletion class
#'
#' An S4 class representing initial and/or final depletion relative to
#' a reference biomass for a [Stock()] object.
#'
#' @slot Initial   Numeric vector of initial depletion values.
#' @slot Final     Numeric vector of final depletion values.
#' @slot Reference Character scalar identifying the reference biomass.
#' @slot Misc List for internal use.
#' 
#' @details
#' See [Depletion()] for details.
#' 
#'
#' @export
#' @include class-unions.R
#' @name depletion-class
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




