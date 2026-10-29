#' The `stocktransition` S4 Class
#'
#' The `stocktransition` object defines an age-dependent reclassification of
#' individuals from one stock's numbers-at-age bookkeeping into another's,
#' e.g. sequential hermaphroditism (protogynous/protandrous sex change), or
#' any other ontogenetic-stage or stock-of-origin transition modelled as
#' separate [stock-class] objects. 
#' 
#' Only raw numbers-at-age move between
#' stocks; weight, maturity, natural mortality, and selectivity are all
#' functions of (age, year, area, stock) applied fresh to whatever `Number`
#' is present, so a transitioned individual is indistinguishable from one
#' that started life in the `To` stock. Constructed via [Herm()].
#'
#' @slot From `character(1)` or `numeric(1)`. Source stock, specified by
#'   stock name (`character`) or 1-based integer index (`numeric`).
#' @slot To `character(1)` or `numeric(1)`. Destination stock, specified by
#'   stock name (`character`) or 1-based integer index (`numeric`).
#' @slot Frac `array` or `NULL`. Cumulative "fraction remaining in `From` by
#'   age" curve - `[sim, age]` or `[sim, age, year]`, values in `[0, 1]`.
#'   This is *not* transitioned/mover fraction: it starts near `1` (almost
#'   no one has transitioned yet) and decreases toward `0` as age increases
#'   (nearly everyone has transitioned out of `From` by the oldest ages).
#'   Converted internally to a per-age hazard rate applied each year.
#' @slot Misc `list`. Reserved for additional information or user-defined
#'   extensions. Not used internally.
#'
#' @seealso [Herm()]
#' @family om
#'
#' @include class-unions.R
#' @export
setClass("stocktransition", representation(
  From = "char.num",
  To   = "char.num",
  Frac = "array.null",
  Misc = "list"
))

setValidity("stocktransition", function(object) {
  errors <- character()

  if (length(object@From) != 1) errors <- c(errors, "`From` must be length 1")
  if (length(object@To) != 1) errors <- c(errors, "`To` must be length 1")
  if (!is.null(object@Frac) && any(object@Frac < 0 | object@Frac > 1, na.rm = TRUE))
    errors <- c(errors, "`Frac` must be within [0, 1]")

  if (length(errors)) return(errors)
  TRUE
})
