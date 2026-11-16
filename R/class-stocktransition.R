#' The `stocktransition` S4 Class
#'
#' The `stocktransition` object defines an age- or length-dependent reclassification of
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
#'   Converted internally to a per-age hazard rate applied each year. In a
#'   plus group the hazard applies every year to the whole plus group; see
#'   *Transition in the Plus Group* in [Herm()]. Populated from
#'   `FracAtLength` during [Populate()] when `FracAtLength` is used.
#' @slot FracAtLength `array`, `numeric`, or `NULL`. Cumulative "fraction
#'   remaining in `From` by length" curve - a numeric vector of length
#'   `length(Classes)`, or an array with named dimensions `Sim`, `Class`,
#'   and `Year`, values in `[0, 1]`. Converted to `Frac` with the age-length
#'   key (`Length@ALK`) of the `From` stock during [Populate()], replacing
#'   any existing values in `Frac`. See *Length-Based Transitions* in
#'   [Herm()].
#' @slot Classes `numeric` or `NULL`. Lower bounds of the length bins for
#'   `FracAtLength`. Defaults to `Length@Classes` of the `From` stock when
#'   `NULL`. Values in `FracAtLength` are evaluated at bin midpoints.
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
  FracAtLength = "num.array.null",
  Classes = "num.null",
  Misc = "list"
))

setValidity("stocktransition", function(object) {
  errors <- character()

  if (length(object@From) != 1) errors <- c(errors, "`From` must be length 1")
  if (length(object@To) != 1) errors <- c(errors, "`To` must be length 1")
  if (!is.null(object@Frac) && any(object@Frac < 0 | object@Frac > 1, na.rm = TRUE))
    errors <- c(errors, "`Frac` must be within [0, 1]")
  if (!is.null(object@FracAtLength) && (!is.numeric(object@FracAtLength) ||
      any(object@FracAtLength < 0 | object@FracAtLength > 1, na.rm = TRUE)))
    errors <- c(errors, "`FracAtLength` must be numeric and within [0, 1]")
  if (!is.null(object@FracAtLength) && !is.null(object@Classes) &&
      is.null(dim(object@FracAtLength)) &&
      length(object@FracAtLength) != length(object@Classes))
    errors <- c(errors, "`FracAtLength` must have length matching `Classes` when it is a vector")

  if (length(errors)) return(errors)
  TRUE
})
