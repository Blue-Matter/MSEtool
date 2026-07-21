#' Catchability Constructor and Accessors
#'
#' Construct and manipulate a [catchability-class] object defining fishing
#' gear or vessel efficiency for a [Fleet()] object.
#'
#' @param Efficiency Numeric, array, or `NULL`. Gear or vessel catchability
#'   coefficient (`q`) relating fishing effort to fishing mortality
#'  (encounters, see [Interactions()]). Accepted  forms:
#'   - `NULL` (default): creates an empty [catchability-class] object.
#'     [PopulateCatchability()] initialises `Efficiency` to 1 across all
#'     simulations and years.
#'   - Scalar numeric (e.g. `0.01`): constant efficiency applied across all
#'     simulations and years.
#'   - Numeric array with dimensions `Sim x Year` and named dimnames. The
#'     `Sim` dimension may be length 1 (replicated internally) or match
#'     `nSim`. The `Year` dimension may be length 1 or span a subset of years;
#'     [PopulateCatchability()] extends it to cover all historical and
#'     projected years.
#'
#'   If `Efficiency` is a [fleet-class] object, the `Catchability` slot of
#'   that fleet is returned (see Details).
#'
#' @param qCV Numeric or `NULL`. Coefficient of variation for lognormal
#'   stochastic variation in catchability. Applied to projected years only
#'   (see Details). Default `NULL`. Included primarily for backwards
#'   compatibility; users may find it simpler to supply a fully specified
#'   `Efficiency` array directly.
#'   
#' @param qInc Numeric or `NULL`. Annual percentage increase in catchability
#'   (e.g. `2` = 2% per year; negative values indicate declining efficiency).
#'   Applied to projected years only (see Details). Default `NULL`. Included
#'   primarily for backwards compatibility; users may find it simpler to
#'   supply a fully specified `Efficiency` array directly.
#'   
#' @param Misc List. Miscellaneous additional inputs. Default `list()`.
#' 
#' @param x A [catchability-class] object for accessor and replacement
#'   functions.
#' @param value The replacement value for the corresponding slot.
#'
#' @details
#' 
#' A [catchability-class] object defines the efficiency with which a fleet
#' converts fishing effort into fishing mortality (encounters/interactions, see
#' [Interactions()]). 
#' 
#' A constant scalar is sufficient for most applications;
#' time-varying or simulation-varying efficiency can be specified via a full
#' `Sim x Year` array.
#' 
#'
#' ## Efficiency Array Format
#'
#' When supplied as a scalar, `Efficiency` is wrapped internally into a
#' `1 x 1` array and replicated across all simulations and years during
#' [PopulateCatchability()], producing constant efficiency. When supplied as
#' an array, the `Sim` dimension may be length 1 or match `nSim`, and the
#' `Year` dimension is extended to cover all historical and projected years
#' if it does not already do so.
#'
#' When `Efficiency` is `NULL`, [PopulateCatchability()] initialises it to
#' 1 across all simulations and years, equivalent to supplying
#' `Efficiency = 1`.
#'
#' ## Projected-Year Adjustments: `qInc` and `qCV`
#'
#' `qInc` and `qCV` modify efficiency in projected years only and have no
#' effect on historical years.
#'
#' `qInc` specifies a percentage annual increase compounded over projection
#' years: efficiency in projection year index `t` is scaled by
#' `(1 + qInc / 100)^t`. Negative values model declining gear efficiency
#' over the projection period.
#'
#' `qCV` introduces lognormal inter-annual variation in efficiency during
#' projection years. Each projected year's efficiency is multiplied by a
#' mean-1 lognormal deviate with the specified coefficient of variation.
#'
#' Both parameters are included primarily for backwards compatibility. For
#' most applications, supplying a fully specified `Efficiency` array gives
#' more direct control over time-varying catchability across both historical
#' and projected periods.
#'
#' ## Attaching to a Fleet
#'
#' A [catchability-class] object can be attached to a [Fleet()] with
#' `Catchability(Fleet) <- MyCatchability` and retrieved with
#' `Catchability(Fleet)`.
#'
#' @return
#' - `Catchability()` returns a [catchability-class] object. If `Efficiency`
#'   is a [fleet-class] object, the `Catchability` slot of that fleet is
#'   returned.
#' - `Catchability<-` returns `x` with the `Catchability` slot replaced by
#'   `value`.
#' - `Efficiency()`, `qCV()`, `qInc()` return the corresponding slot from
#'   the [catchability-class] object `x`.
#' - Their replacement forms return `x` with the corresponding slot updated.
#'
#' @seealso
#' - [catchability-class] for the class definition and slot-level
#'   documentation.
#' - [Fleet()] for the enclosing fleet constructor.
#' - [Effort()] for the effort object that is scaled by efficiency to produce
#'   fishing mortality.
#' - [PopulateCatchability()] for how efficiency arrays are extended and
#'   stochastic adjustments are applied.
#'
#' @family fleet
#'
#' @example man-examples/class-Catchability.R
#'
#' @export
Catchability <- function(Efficiency = NULL,
                         qCV        = NULL,
                         qInc       = NULL,
                         Misc       = list()) {
  
  if (.IsFleetOrList(Efficiency))
    return(.ExtractFleetSlot(Efficiency, 'Catchability'))
    
  object <- methods::new(
    "catchability",
    Efficiency = Efficiency,
    qCV        = qCV,
    qInc       = qInc,
    Misc       = Misc
  )
  
  methods::validObject(object)
  object
}


#' @rdname Catchability
#' @export
`Catchability<-` <- function(x, value) {
  .AssignFleetSlot(x, value, 'Catchability')
}

#' @rdname Catchability
#' @export
Efficiency <- function(x) {
  .CheckClass(x, "catchability", "x")
  x@Efficiency
}

#' @rdname Catchability
#' @export
`Efficiency<-` <- function(x, value) {
  .CheckClass(x, "catchability", "x")
  x@Efficiency <- value
  methods::validObject(x)
  x
}

#' @rdname Catchability
#' @export
qCV <- function(x) {
  .CheckClass(x, "catchability", "x")
  x@qCV
}

#' @rdname Catchability
#' @export
`qCV<-` <- function(x, value) {
  .CheckClass(x, "catchability", "x")
  x@qCV <- value
  methods::validObject(x)
  x
}

#' @rdname Catchability
#' @export
qInc <- function(x) {
  .CheckClass(x, "catchability", "x")
  x@qInc
}

#' @rdname Catchability
#' @export
`qInc<-` <- function(x, value) {
  .CheckClass(x, "catchability", "x")
  x@qInc <- value
  methods::validObject(x)
  x
}
