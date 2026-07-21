#' Discard Mortality
#'
#' Construct and manipulate a [discardmortality-class] object defining the
#' proportion of discarded catch that dies, for use in a [Fleet()] object.
#' Discard mortality is optional; if not specified, all discarded fish are
#' assumed to survive.
#'
#' @param MeanAtAge Numeric, numeric vector, or array, or `NULL`. Mean discard
#'   mortality at age. Values must be between 0 (all discards survive) and 1
#'   (all discards die). Accepted forms:
#'   - `NULL` (default): creates an empty [discardmortality-class] object.
#'     [PopulateDiscardMortality()] sets discard mortality to 0 for all age
#'     and length classes.
#'   - Scalar: constant mortality applied to all ages. Wrapped internally to a
#'     `1 x nAge x 1` array.
#'   - Numeric vector of length `nAge`: mortality specified per age class.
#'     Wrapped internally to a `1 x nAge x 1` array.
#'   - Numeric array with dimensions `Sim x Age x Year` and named dimnames.
#'
#'   If `MeanAtAge` is a [fleet-class] object, the `DiscardMortality` slot of
#'   that fleet is returned.
#'
#'   Unlike [Selectivity()] and [Retention()], `DiscardMortality` does not
#'   support model-based population via `Pars` and `Model`. Values must be
#'   supplied directly as arrays or derived from `MeanAtLength`.
#'
#' @param MeanAtLength Numeric array or `NULL`. Mean discard mortality at
#'   length with dimensions `Sim x Length x Year`. If provided, takes
#'   precedence over `MeanAtAge`; `MeanAtAge` is derived from it via the
#'   age-length key during [PopulateDiscardMortality()].
#' @param Classes Numeric vector or `NULL`. Length class midpoints
#'   corresponding to the second dimension of `MeanAtLength`. Default `NULL`.
#' @param Misc List. Miscellaneous additional inputs. Default `list()`.
#' @param x A [discardmortality-class] object, or a [fleet-class],
#'   `FleetList`, `StockFleetList`, or [om-class] object for
#'   `DiscardMortality<-`.
#' @param value For `DiscardMortality<-`: a [discardmortality-class] object
#'   when `x` is a [fleet-class]; a flat list of [discardmortality-class]
#'   objects when `x` is a `FleetList`; a nested stock-fleet list of
#'   [discardmortality-class] objects when `x` is a `StockFleetList` or
#'   [om-class].
#'
#' @details
#' Discard mortality defines the proportion of discarded catch that dies.
#' Fish that are selected but not retained (as defined by [Retention()]) are
#' treated as discards; `DiscardMortality` determines how many of those fish
#' die as a result of the capture-and-release process.
#'
#' Unlike [Selectivity()] and [Retention()], discard mortality does not
#' support model-based population via `Pars` and `Model`. Values must be
#' supplied directly as `MeanAtAge` or `MeanAtLength` arrays. If neither is
#' supplied, [PopulateDiscardMortality()] defaults to 0 (all discards survive)
#' for all age and length classes.
#'
#' ## Array Format
#'
#' `MeanAtAge` may be a scalar, a vector of length `nAge`, or a full
#' `Sim x Age x Year` array. Scalars and vectors are wrapped internally to
#' arrays with a single simulation and year, then extended to cover all
#' simulations and years during population. If `MeanAtLength` is supplied, it
#' takes precedence and `MeanAtAge` is derived from it using the age-length
#' key.
#'
#' ## Attaching to a Fleet
#'
#' A [discardmortality-class] object can be attached to a [Fleet()] with
#' `DiscardMortality(Fleet) <- MyDiscardMortality` and retrieved with
#' `DiscardMortality(Fleet)`.
#'
#' Individual slots may be accessed or modified using [MeanAtAge()],
#' [MeanAtLength()], and [Classes()].
#'
#' @return
#' - `DiscardMortality()` returns a [discardmortality-class] object. If
#'   `MeanAtAge` is a [fleet-class] object, the `DiscardMortality` slot of
#'   that fleet is returned.
#' - `DiscardMortality<-` returns `x` with the `DiscardMortality` slot
#'   replaced by `value`. When `x` is an [om-class] or `StockFleetList`,
#'   each fleet's slot is updated from the corresponding element of the
#'   nested list `value`. When `x` is a `FleetList`, each fleet's slot is
#'   updated from the corresponding element of the flat list `value`.
#'
#' @seealso
#' - [discardmortality-class] for the class definition and slot-level
#'   documentation.
#' - [Fleet()] for the enclosing fleet constructor.
#' - [Selectivity()], [Retention()] for related fleet components.
#' - [PopulateDiscardMortality()] for population details.
#'
#' @family fleet
#'
#' @example man-examples/class-DiscardMortality.R
#'
#' @export
DiscardMortality <- function(MeanAtAge    = NULL,
                             MeanAtLength = NULL,
                             Classes      = NULL,
                             Misc         = list()) {
  
  if (.IsFleetOrList(MeanAtAge))
    return(.ExtractFleetSlot(MeanAtAge, 'DiscardMortality'))

  if (inherits(MeanAtAge, 'advice'))
    return(.AccessSlot(MeanAtAge, 'DiscardMortality'))

  methods::new(
    "discardmortality",
    MeanAtAge    = MeanAtAge,
    MeanAtLength = MeanAtLength,
    Classes      = Classes,
    Misc         = Misc
  )
}


#' @rdname DiscardMortality
#' @export
`DiscardMortality<-` <- function(x, value) {
  .AssignFleetSlot(x, value, 'DiscardMortality')
}
  
