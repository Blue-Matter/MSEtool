#' Data Slot Accessors
#'
#' Access and replace the component slots of [data-class] and [obs-class]
#' objects, and the slots of the data components ([catchdata-class],
#' [effortdata-class], [indicesdata-class], [compdata-class]). Functions work
#' on any S4 object that contains the corresponding slot, and on a list of
#' [data-class] objects, where they return a list with the slot of each.
#'
#' | Function | Slot of |
#' |---|---|
#' | `CPUE()`, `Survey()`, `LifeHistory()`, `Exploitation()` | `data`, `obs` |
#' | `LandingsAtAge()`, `LandingsAtSize()`, `DiscardsAtAge()`, `DiscardsAtSize()`, `SurveyAtAge()`, `SurveyAtSize()` | `data`, `obs`, [hist-class], [mse-class], [timeseries-class] |
#' | `Reference()` | `data`, `hist`, `mse`, [depletion-class] |
#' | `Value()` | the data components |
#' | `Ref()` | `catchdata`, `indicesdata`; also the observation components and [pm-class] |
#' | `RefCV()` | `catchdata`, `indicesdata` |
#'
#' The other slots of a [data-class] object are accessed with
#' [Landings()], [Discards()], [Effort()], [Advice()], [Years()], [YearLH()],
#' [Seasons()], [nArea()], and the [Metadata] accessors.
#'
#' @param x An S4 object with the corresponding slot.
#' @param value The value to assign to the slot.
#'
#' @return
#' - Accessor functions return the value stored in the named slot of `x`.
#' - Replacement functions return `x` with the named slot updated to `value`.
#'
#' @seealso [Data()], [Obs()], [CatchData()], [EffortData()], [IndicesData()],
#'   [CompData()]
#'
#' @family slot-accessors
#'
#' @name Data-accessors
NULL

#' @rdname Data-accessors
#' @export
CPUE <- function(x, byStock = FALSE) {
  if (inherits(x, 'data') && !byStock)
    return(.CombineIndexElements(x@CPUE))
  .AccessSlot(x, 'CPUE')
}

#' @rdname Data-accessors
#' @export
`CPUE<-` <- function(x, value) {
  .AssignSlot(x, value, 'CPUE')
}

#' @rdname Data-accessors
#' @param byAge,bySize For `Survey()` of a [data-class] object: `TRUE`
#'   returns the survey age (`SurveyAtAge`) or size (`SurveyAtSize`)
#'   compositions as a tidy `data.frame` (see [Landings()]).
#' @param byFleet For `Survey()` compositions: `FALSE` sums over surveys.
#' @param byStock For `CPUE()` and `Survey()` of a [data-class] object: `TRUE`
#'   returns the slot as stored, with indices or compositions held by stock
#'   kept separate (see [data-class]); `FALSE` (default) returns the indices
#'   of every element in one object, or compositions summed over stocks.
#' @export
Survey <- function(x, byAge = FALSE, bySize = FALSE, byFleet = TRUE, byStock = FALSE) {
  if (inherits(x, 'data') && (byAge || bySize))
    return(.ExtractDataCompTimeseries(x, if (byAge) 'SurveyAtAge' else 'SurveyAtSize',
                                      byFleet = byFleet, byStock = byStock))
  if (inherits(x, 'data') && !byStock)
    return(.CombineIndexElements(x@Survey))
  .AccessSlot(x, 'Survey')
}

#' @rdname Data-accessors
#' @export
`Survey<-` <- function(x, value) {
  .AssignSlot(x, value, 'Survey')
}

#' @rdname Data-accessors
#' @export
LifeHistory <- function(x) {
  .AccessSlot(x, 'LifeHistory')
}

#' @rdname Data-accessors
#' @export
`LifeHistory<-` <- function(x, value) {
  .AssignSlot(x, value, 'LifeHistory')
}

#' @rdname Data-accessors
#' @export
Exploitation <- function(x) {
  .AccessSlot(x, 'Exploitation')
}

#' @rdname Data-accessors
#' @export
`Exploitation<-` <- function(x, value) {
  .AssignSlot(x, value, 'Exploitation')
}

#' @rdname Data-accessors
#' @export
LandingsAtAge <- function(x) {
  .AccessSlot(x, 'LandingsAtAge')
}

#' @rdname Data-accessors
#' @export
`LandingsAtAge<-` <- function(x, value) {
  .AssignSlot(x, value, 'LandingsAtAge')
}

#' @rdname Data-accessors
#' @export
LandingsAtSize <- function(x) {
  .AccessSlot(x, 'LandingsAtSize')
}

#' @rdname Data-accessors
#' @export
`LandingsAtSize<-` <- function(x, value) {
  .AssignSlot(x, value, 'LandingsAtSize')
}

#' @rdname Data-accessors
#' @export
DiscardsAtAge <- function(x) {
  .AccessSlot(x, 'DiscardsAtAge')
}

#' @rdname Data-accessors
#' @export
`DiscardsAtAge<-` <- function(x, value) {
  .AssignSlot(x, value, 'DiscardsAtAge')
}

#' @rdname Data-accessors
#' @export
DiscardsAtSize <- function(x) {
  .AccessSlot(x, 'DiscardsAtSize')
}

#' @rdname Data-accessors
#' @export
`DiscardsAtSize<-` <- function(x, value) {
  .AssignSlot(x, value, 'DiscardsAtSize')
}

#' @rdname Data-accessors
#' @export
SurveyAtAge <- function(x) {
  .AccessSlot(x, 'SurveyAtAge')
}

#' @rdname Data-accessors
#' @export
`SurveyAtAge<-` <- function(x, value) {
  .AssignSlot(x, value, 'SurveyAtAge')
}

#' @rdname Data-accessors
#' @export
SurveyAtSize <- function(x) {
  .AccessSlot(x, 'SurveyAtSize')
}

#' @rdname Data-accessors
#' @export
`SurveyAtSize<-` <- function(x, value) {
  .AssignSlot(x, value, 'SurveyAtSize')
}

#' @rdname Data-accessors
#' @export
Value <- function(x) {
  .AccessSlot(x, 'Value')
}

#' @rdname Data-accessors
#' @export
`Value<-` <- function(x, value) {
  .AssignSlot(x, value, 'Value')
}

#' @rdname Data-accessors
#' @export
Ref <- function(x) {
  .AccessSlot(x, 'Ref')
}

#' @rdname Data-accessors
#' @export
`Ref<-` <- function(x, value) {
  .AssignSlot(x, value, 'Ref')
}

#' @rdname Data-accessors
#' @export
RefCV <- function(x) {
  .AccessSlot(x, 'RefCV')
}

#' @rdname Data-accessors
#' @export
`RefCV<-` <- function(x, value) {
  .AssignSlot(x, value, 'RefCV')
}

#' @rdname Data-accessors
#' @export
Reference <- function(x) {
  .AccessSlot(x, 'Reference')
}

#' @rdname Data-accessors
#' @export
`Reference<-` <- function(x, value) {
  .AssignSlot(x, value, 'Reference')
}
