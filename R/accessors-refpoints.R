#' Reference Point Accessors
#'
#' Extract the MSY, per-recruit, replacement-line, and unfished reference
#' points from a [hist-class] or [mse-class] object.
#'
#' @param Hist A [hist-class] object (the output of [Simulate()]) or an
#'   [mse-class] object (the output of [Project()]). The MSY and per-recruit
#'   accessors also accept a [reference-class] object, e.g.
#'   `Reference(Hist) |> FMSY()`, and the MSY accessors a
#'   [refpointsMSY-class] or [referencedata-class] object.
#' @param type Character. One of `'Equilibrium'` or `'Dynamic'`; partial
#'   matching is supported.
#'
#'   - `'Equilibrium'`: unfished values assuming equilibrium conditions — no
#'     recruitment deviations and all life-history parameters constant over
#'     time.
#'   - `'Dynamic'`: unfished values calculated by running the full population
#'     dynamics model with no fishing, including recruitment deviations and any
#'     time-varying parameters, over the historical and projection years. The
#'     projection-year values are the same for all management procedures.
#' @param Reduce Logical. If `TRUE` (default), calls [ReduceDims()] to
#'   collapse any `Sim`, `Year`, or `Age` dimensions where values are
#'   identical across that dimension, reducing array size. If `FALSE`, the
#'   slot contents are returned as-is with all dimensions intact.
#' @param value The value to assign to the slot of a [refpointsMSY-class] or
#'   [referencedata-class] object, e.g. `Data@Reference`.
#'
#' @details
#' ## MSY Reference Points
#'
#' Calculated by [CalcMSY()] and stored in `Hist@Reference@MSY`.
#'
#' - `MSYRefs()`, `MSY()`: the full [refpointsMSY-class] object.
#' - `FMSY()`: apical fishing mortality at MSY, defined at the complex
#'   level.
#' - `BMSY()`: total biomass at MSY.
#' - `SBMSY()`: spawning biomass at MSY.
#' - `SPMSY()`: spawning production at MSY.
#' - `SPRMSY()`: spawning potential ratio at MSY.
#' - `MSYLandings()`: landed catch at MSY.
#' - `MSYDiscards()`: dead discards at MSY.
#'
#' The replacement functions (e.g. `FMSY<-`) set the reference points of a
#' [refpointsMSY-class] or [referencedata-class] object, such as the
#' `Reference` slot of a [data-class] object. The reference points of a
#' [hist-class] or [mse-class] object are calculated by [Simulate()] and
#' cannot be replaced.
#'
#' ## Per-Recruit and Replacement-Line Reference Points
#'
#' Calculated by [CalcRefPoints()] and stored in `Hist@Reference`.
#'
#' - `F01()`: apical F at 10% of the complex's yield-per-recruit slope at the
#'   origin.
#' - `FMax()`: apical F maximising the complex's yield-per-recruit.
#' - `FSPR()`: apical F at each requested SPR target.
#' - `FMed()`: apical F at the median historical replacement line.
#' - `FCrash()`: apical F at which the stock can no longer replace itself.
#' - `SPRcrash()`: spawning potential ratio at `FCrash()`.
#' - `BLow()`: spawning biomass rebuilding threshold (see [CalcBLow()]).
#'   Only populated when computed explicitly, since it is substantially
#'   more expensive than the other reference points here.
#'
#' ## Unfished Reference Points
#'
#' Stored in `Hist@Unfished`.
#'
#' - `B0()`: total biomass.
#' - `SB0()`: spawning biomass.
#' - `SP0()`: spawning production.
#' - `N0()`: numbers-at-age.
#'
#' @return
#' - The MSY accessors return a numeric array with dimensions
#'   `Sim x Stock x Year`, except `FMSY()` which returns
#'   `Sim x Complex x Year`. `MSYRefs()` and `MSY()` return a
#'   [refpointsMSY-class] object.
#' - The per-recruit accessors return a numeric array with dimensions
#'   `Sim x Stock x Year` (`FSPR()` additionally has a `Target` dimension).
#'   For `F01()` and `FMax()` the `Stock` dimension is labelled by complex
#'   name.
#' - `B0()`, `SB0()`, and `SP0()` return an array with dimensions
#'   `[Sim, Stock, Year]`, and `N0()` a named list with one element per stock,
#'   each an array with dimensions `[Sim, Age, Year]`. The `Sim` and `Year`
#'   dimensions are collapsed by [ReduceDims()] when `Reduce = TRUE` and
#'   values do not vary across those dimensions.
#' - The MSY and per-recruit accessors return `NULL` when the slot has not
#'   been populated.
#' - Replacement functions return `Hist` with the slot updated to `value`.
#'
#' @seealso [CalcMSY()], [CalcRefPoints()], [CalcBLow()], [reference-class],
#'   [refpointsMSY-class], [unfished-class]
#'
#' @family slot-accessors
#'
#' @name RefPoint-accessors
NULL

#' @rdname RefPoint-accessors
#' @export
MSYRefs <- function(Hist) {
  .CheckClass(Hist, c('hist', 'mse'), 'Hist')
  .AccessSlot(Hist@Reference, 'MSY')
}

#' @rdname RefPoint-accessors
#' @export
MSY <- function(Hist) {
  .ResolveRefpointsMSY(Hist, 'MSY')
}

.ResolveRefpointsMSY <- function(Hist, fn_name = "MSY accessor") {
  supported <- c('hist', 'mse', 'reference', 'refpointsMSY')
  .CheckClass(Hist, supported, fn_name)

  if (inherits(Hist, 'refpointsMSY')) return(Hist)
  if (inherits(Hist, 'reference'))    return(Hist@MSY)
  Hist@Reference@MSY
}

.AccessMSYSlot <- function(Hist, SlotName) {
  .AccessSlot(.ResolveRefpointsMSY(Hist, SlotName), SlotName)
}

.AssignMSYSlot <- function(Hist, value, SlotName) {
  .CheckClass(Hist, 'refpointsMSY', 'Hist')
  .AssignSlot(Hist, value, SlotName)
}

#' @rdname RefPoint-accessors
#' @export
FMSY <- function(Hist) .AccessMSYSlot(Hist, 'FMSY')

#' @rdname RefPoint-accessors
#' @export
`FMSY<-` <- function(Hist, value) .AssignMSYSlot(Hist, value, 'FMSY')

#' @rdname RefPoint-accessors
#' @export
BMSY <- function(Hist) .AccessMSYSlot(Hist, 'BMSY')

#' @rdname RefPoint-accessors
#' @export
`BMSY<-` <- function(Hist, value) .AssignMSYSlot(Hist, value, 'BMSY')

#' @rdname RefPoint-accessors
#' @export
SBMSY <- function(Hist) .AccessMSYSlot(Hist, 'SBMSY')

#' @rdname RefPoint-accessors
#' @export
`SBMSY<-` <- function(Hist, value) .AssignMSYSlot(Hist, value, 'SBMSY')

#' @rdname RefPoint-accessors
#' @export
SPMSY <- function(Hist) .AccessMSYSlot(Hist, 'SPMSY')

#' @rdname RefPoint-accessors
#' @export
`SPMSY<-` <- function(Hist, value) .AssignMSYSlot(Hist, value, 'SPMSY')

#' @rdname RefPoint-accessors
#' @export
SPRMSY <- function(Hist) .AccessMSYSlot(Hist, 'SPRMSY')

#' @rdname RefPoint-accessors
#' @export
`SPRMSY<-` <- function(Hist, value) .AssignMSYSlot(Hist, value, 'SPRMSY')

#' @rdname RefPoint-accessors
#' @export
MSYLandings <- function(Hist) .AccessMSYSlot(Hist, 'MSYLandings')

#' @rdname RefPoint-accessors
#' @export
`MSYLandings<-` <- function(Hist, value) .AssignMSYSlot(Hist, value, 'MSYLandings')

#' @rdname RefPoint-accessors
#' @export
MSYDiscards <- function(Hist) .AccessMSYSlot(Hist, 'MSYDiscards')

#' @rdname RefPoint-accessors
#' @export
`MSYDiscards<-` <- function(Hist, value) .AssignMSYSlot(Hist, value, 'MSYDiscards')

.AccessRefSlot <- function(Hist, SlotName) {
  .CheckClass(Hist, c('hist', 'mse', 'reference'), 'Hist')
  if (!inherits(Hist, 'reference'))
    Hist <- Hist@Reference
  .AccessSlot(Hist, SlotName)
}

#' @rdname RefPoint-accessors
#' @export
F01 <- function(Hist) .AccessRefSlot(Hist, 'F01')

#' @rdname RefPoint-accessors
#' @export
FMax <- function(Hist) .AccessRefSlot(Hist, 'FMax')

#' @rdname RefPoint-accessors
#' @export
FSPR <- function(Hist) .AccessRefSlot(Hist, 'FSPR')

#' @rdname RefPoint-accessors
#' @export
FMed <- function(Hist) .AccessRefSlot(Hist, 'FMed')

#' @rdname RefPoint-accessors
#' @export
FCrash <- function(Hist) .AccessRefSlot(Hist, 'FCrash')

#' @rdname RefPoint-accessors
#' @export
SPRcrash <- function(Hist) .AccessRefSlot(Hist, 'SPRcrash')

#' @rdname RefPoint-accessors
#' @export
BLow <- function(Hist) .AccessRefSlot(Hist, 'BLow')

.UnfishedSlot <- function(Hist, type, slot_name, Reduce) {
  .CheckClass(Hist, c('hist', 'mse'), 'Hist')
  type <- match.arg(type, c('Equilibrium', 'Dynamic'))

  obj <- if (type == 'Equilibrium') {
    slot(Hist@Unfished@Equilibrium, slot_name)
  } else {
    slot(Hist@Unfished@Dynamic, slot_name)
  }

  if (Reduce) ReduceDims(obj, IncYear = FALSE) else obj
}

#' @rdname RefPoint-accessors
#' @export
B0 <- function(Hist, type = c('Equilibrium', 'Dynamic'), Reduce = TRUE)
  .UnfishedSlot(Hist, type, 'Biomass', Reduce)

#' @rdname RefPoint-accessors
#' @export
SB0 <- function(Hist, type = c('Equilibrium', 'Dynamic'), Reduce = TRUE)
  .UnfishedSlot(Hist, type, 'SBiomass', Reduce)

#' @rdname RefPoint-accessors
#' @export
SP0 <- function(Hist, type = c('Equilibrium', 'Dynamic'), Reduce = TRUE)
  .UnfishedSlot(Hist, type, 'SProduction', Reduce)

#' @rdname RefPoint-accessors
#' @export
N0 <- function(Hist, type = c('Equilibrium', 'Dynamic'), Reduce = TRUE)
  .UnfishedSlot(Hist, type, 'Number', Reduce)
