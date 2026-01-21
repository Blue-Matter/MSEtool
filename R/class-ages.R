#' Ages class
#'
#' An S4 class representing the age structure for a [Stock()] object.
#'
#' @slot MaxAge Numeric scalar giving the maximum age. If `PlusGroup == TRUE`,
#'   this represents the plus group age.
#' @slot MinAge  Numeric scalar giving the minimum age.#' 
#' @slot Units Character string describing the time units used in the definition
#' of `MaxAge` and `MinAge` (e.g. `"year"`).
#' @slot PlusGroup Logical; indicates whether the maximum age is treated
#'   as a plus group.
#' @slot Classes Numeric vector of age classes expressed in years
#' @export
#' @include class-unions.R
setClass('ages',
         slots=c(MaxAge='numeric',
                 MinAge='numeric',
                 Units='character',
                 PlusGroup='logical',
                 Classes='num.null')
         
)

setValidity('ages', function(object) {
  # TODO 
  TRUE
})
