#' Access Per-Recruit and Replacement-Line Reference Point Slots
#'
#' Accessor functions for reference points stored in `Hist@Reference`,
#' populated by [CalcRefPoints()].
#'
#' @param Hist A [hist-class] object.
#'
#' @return A numeric array with dimensions `Sim x Stock x Year` (`FSPR()`
#'   additionally has a `Target` dimension). Returns `NULL` if the slot has
#'   not yet been populated (i.e. [CalcRefPoints()] has not been run).
#'
#' @details
#' - `F01()`: apical F at 10% of the yield-per-recruit slope at the origin.
#' - `FMax()`: apical F maximising yield-per-recruit.
#' - `FSPR()`: apical F at each requested SPR target.
#' - `FMed()`: apical F at the median historical replacement line.
#' - `FCrash()`: apical F at which the stock can no longer replace itself.
#' - `SPRcrash()`: spawning potential ratio at `FCrash()`.
#' - `BLow()`: spawning biomass rebuilding threshold (see [CalcBLow()]).
#'   Only populated when computed explicitly, since it is substantially
#'   more expensive than the other reference points here.
#'
#' @seealso [CalcRefPoints()], [CalcBLow()], [reference-class]
#' @name refpoints-accessors
NULL

#' @rdname refpoints-accessors
#' @export
F01 <- function(Hist) {
  .CheckClass(Hist, 'hist', 'Hist')
  .AccessSlot(Hist@Reference, 'F01')
}

#' @rdname refpoints-accessors
#' @export
FMax <- function(Hist) {
  .CheckClass(Hist, 'hist', 'Hist')
  .AccessSlot(Hist@Reference, 'FMax')
}

#' @rdname refpoints-accessors
#' @export
FSPR <- function(Hist) {
  .CheckClass(Hist, 'hist', 'Hist')
  .AccessSlot(Hist@Reference, 'FSPR')
}

#' @rdname refpoints-accessors
#' @export
FMed <- function(Hist) {
  .CheckClass(Hist, 'hist', 'Hist')
  .AccessSlot(Hist@Reference, 'FMed')
}

#' @rdname refpoints-accessors
#' @export
FCrash <- function(Hist) {
  .CheckClass(Hist, 'hist', 'Hist')
  .AccessSlot(Hist@Reference, 'FCrash')
}

#' @rdname refpoints-accessors
#' @export
SPRcrash <- function(Hist) {
  .CheckClass(Hist, 'hist', 'Hist')
  .AccessSlot(Hist@Reference, 'SPRcrash')
}

#' @rdname refpoints-accessors
#' @export
BLow <- function(Hist) {
  .CheckClass(Hist, 'hist', 'Hist')
  .AccessSlot(Hist@Reference, 'BLow')
}
